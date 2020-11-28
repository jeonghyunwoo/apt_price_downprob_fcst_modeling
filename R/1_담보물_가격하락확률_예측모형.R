source('0_init.R',encoding='utf-8')
library(furrr)
library(rsample)
library(recipes)
library(naniar)
library(caret)
library(doParallel)
library(pROC)
library(clipr)

# 문제인식: 지역별로 나눠서 개발한 모형결과의 통합
# target: 3년후 가격 5%이상 하락여부 

# train: 매각대상건 리스트
mbslst = read_rds('data/mbslist.rds')
# 법정동코드 없는 건 많음
# 좌표는 모형에 이용하기 어려운 형태임 
dfx1 = mk_x(201502) # 5분 소요 
dfx2 = mk_x(201506)
dfx3 = mk_x(201511)
dfx4 - mk_x(201606)

dfx = bind_rows(dfx1,dfx2,dfx3,dfx4)
dfy = map_dfr(c(201502,201506,201511,201606),mk_y)

# 인구증감율 
# 기준월 두달전 데이터를 이용해야 함 (말일자에 전월말 데이터 업데이트)
# 8월에 작업중이라면 인구증감율은 6월기준 데이터를 이용
# pop_n은 0_init.R에서 생성 
dfz = dfx %>% 
  left_join(dfx_city_id,by=c('시도','구군')) %>% 
  left_join(select(pop_g,id,mm_2,인구증감율),by=c('id','마감년월'='mm_2')) %>% 
  left_join(select(dfy,마감년월,아팥코드,가격증감율,grp),by=c('마감년월','아파트코드'))
# 추가항목 생기면 for_scoring에서 수정할것 
rm(dfx,dfy);gc()
data1 = filter(dfz,매각가능==1) %>% for_scoring()
# train,test split ----
train_df = filter(data1,마감년월!='201606') %>% 
  drop_na(가격증감율) 
test_df = filter(data1,마감년월=='201606')
# target
quantile(train_df$가격증감율,probs=seq(0,1,.1),na.rm=T) %>% 
  round(.,3)
# down = -5%이상 하락, up = +5%이상 상승 

# preproc ----
train_df1 = train_df %>% 
  mutate(target = case_when(가격증감율>=0.05~'up',
                            가격증감율<=-0.05~'down',
                            TRUE~'else')) %>% 
  filter(target!='else') %>% 
  mutate(target = for_relevel(target,'up'))

set.seed(7)
spl = initial_split(train_df1,prop=0.35)
tra = training(spl)
valid = testing(spl)

test_df1 = test_df %>% 
  mutate(target = case_when(가격증감율>=0.05~'up',
                                 가격증감율<=-0.05~'down',
                                 TRUE~'else')) %>% 
  filter(target!='else') %>% 
  mutate(target = for_relevel(target,'up'))

tr_rec = recipe(target~지역구분+브랜드여부+총세대수+대단지여부+
                  입주후경과년수+전용면적+면적구분+
                  방수+욕실수+계단식여부+
                  역세권여부+버스역세권여부+
                  주변시설여부+인구증감율+
                  매매가증감율+전세비율+전세비율증감1+
                  대출번호+시도+구군,data=tra) %>% 
  step_string2factor(지역구분,계단식여부,주변시설여부) %>% 
  step_dummy(all_nominal(),-target,-대출번호,-시도,-구군,one_hot=T) %>% 
  prep(retain=T,strings_as_facors=F)
saveRDS(tr_rec,'model/tr_rec.rds')

tr = juice(tr_rec) %>% select(-대출번호,-시도,-구군)
val = bake(tr_rec,valid)
te = bake(tr_rec,test_df1)
miss_var_summary(te)

# modeling----
ctrl = trainControl(method='cv',number=3,classProbs = T,
                    summaryFunction = twoClassSummary)
registerDoParallel(4)
fit1 = train(target~.-대출번호,data=tr,
             method='xgbTree',trControl=ctrl,
             tuneLength = 3)
stopImplicitCluster() # 약 3분소요 
pred1 = map(list(val,te),~predict(fit1,.))
confusionMatrix(pred1[[1]],val$target)
confusionMatrix(pred1[[2]],te$target)
varImp(fit1) %>% plot
prob1 = map(list(val,te),~predict(fit1,.,type='prob')[,2])
roc(val$target,prob1[[1]])
roc(te$target,prob1[[2]])
# fit1은 지역구분_지방의 중요도가 너무 높음

registerDoParallel(4)
fit2 = train(target~.-대출번호-지역구분_지방,data=tr,
             method='xgbTree',trControl=ctrl,
             tuneLength = 3)
stopImplicitCluster() # 약 3분소요 
pred2 = map(list(val,te),~predict(fit2,.))
confusionMatrix(pred2[[1]],val$target)
confusionMatrix(pred2[[2]],te$target)
varImp(fit2) %>% plot
prob2 = map(list(val,te),~predict(fit2,.,type='prob')[,2])
roc(val$target,prob2[[1]])
roc(te$target,prob2[[2]])
# fit2는 인구증감율의 중요도가 너무 높음 
# fit3(과정생략) : 매매가증감율 중요도가 너무 높음 

# fit4 : 최종모델 
registerDoParallel(4)
fit4 = train(target~.-대출번호-지역구분_지방-인구증감-매매증감율,data=tr,
             method='xgbTree',trControl=ctrl,
             tuneLength = 3)
stopImplicitCluster() # 약 3분소요 
pred4 = map(list(val,te),~predict(fit4,.))
confusionMatrix(pred4[[1]],val$target)
confusionMatrix(pred4[[2]],te$target)
varImp(fit4) %>% plot
prob4 = map(list(val,te),~predict(fit4,.,type='prob')[,2])
roc(val$target,prob4[[1]])
roc(te$target,prob4[[2]])

# fit4로 추정한 5%이상 하락확률로 가격증감율 <0 확률 예측 
prob_df = train_df %>% 
  mutate(target = ifelse(가격증감율<0,'down','up'),
         target = fct_relevel(target,'up')) %>% 
  bake(tr_rec,.)
prob_df$down_p = predict(fit4,prob_df,type='prob')[,2]
fit4_prob = glm(target~down_p,data=prob_df,family='binomial')
prob_df$down_prob = predict(fit4_prob,prob_df,type='response')
saveRDS(fit4_prob,'model/fit4_prob.rds')

# model performance
library(DALEX)
p_fun = function(object,newdata){
  predict(object,newdata=newdata,type='prob')[,2]
}
target = ifelse(te$target=='down',1,0)
explainer_fit1 = explain(fit1,label='fit1',data=te,y=target,predict_fun=p_fun)
explainer_fit2 = explain(fit2,label='fit2',data=te,y=target,predict_fun=p_fun)
explainer_fit3 = explain(fit3,label='fit3',data=te,y=target,predict_fun=p_fun)
explainer_fit4 = explain(fit4,label='fit4',data=te,y=target,predict_fun=p_fun)
mp_1 = model_performance(explainer_fit1)
mp_2 = model_performance(explainer_fit2)
mp_3 = model_performance(explainer_fit3)
mp_4 = model_performance(explainer_fit4)
plot(mp_1,mp_2,mp_3,mp_4,geom='boxplot')

names(tr) %>% write_clip()
fit4$bestTune %>% as.data.frame() %>% gather() %>% write_clip()

# scoring 적용----
dfx2019 = mk_x(201906)
dfx2019 = dfx2019 %>% filter(매각가능==1)
scr2019 = for_scoring(dfx2019)

scr2019$인구증감율 = .1 # 인구증감율 항목 없으면 에러나므로 임의의 값 입력 
scr = bake(tr_rec,scr2019)
# 아파트코드 없어서 모형값 미산출된 건은 시도,구군별 평균값으로 대체 
idx = is.na(scr$총세대수)
scra = scr[!idx,]
scrb = scr[idx,]
scra[,'down_p'] = predict(fit4,scra,type='prob')[,2]
scrb[,'down_p'] = NA
scr1 = bind_rows(scra,scrb)
choro = dfx2019 %>% 
  left_join(select(scr1,대출번호,down_p),by='대출번호') %>% 
  group_by(시도,구군) %>% 
  mutate(down_p = ifelse(is.na(down_p),mean(down_p,na.rm=T),down_p))
choro$down_prob = predict(fit4_prob,choro,type='response')