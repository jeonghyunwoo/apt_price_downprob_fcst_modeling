#' 매각대상  선정모형적용(가격하락확률 산출) 단계
#' 기초변수 분석을 위해 3가지 데이터 생성 
#' 순서: mk_x --> for_score --> pred_df ----
#' mk_x : 기초데이터
#' for_score : 스코어링에 필요한 항목 추가 
#' pred_df : recipes 적용한 데이터, rec<-read_rds('model/mod1_rec.rds')
#' mk_y : 향후 새 모형만들기 위한 target 생성 
#' 모형 : models <-read_rds('model/models_xgbtree.rds')
#' 모형그룹정보: grpx <-read_rds('model/grpx.rds')

library(pacman)
p_load(plyr,tidyverse,clipr,lubridate,janitor,scales,naniar,skimr)
p_load(readxl,writexl,ggExtra,ggridges,ggthemes,ggforce)
p_load(sticklylabeller)

# cbf함수----
# 데이터준비함수 (생략)

# dobo_f : 역세권여부 함수----
dobo_f = function(tr){
  sta = tr %>% 
    transmute_at(vars(contains('역에서도보')),~ifelse(.==0,NA,.))
  bus = tr %>% 
    transmute_at(vars(contains('역에서버스')),~ifelse(.==0,NA,.))
  sta_min = apply(sta,1,min,na.rm=T)
  sta_min = ifelse(is.infinite(sta_min),NA,sta_min)
  bus_min = ifelse(is.infinite(bus_min),NA,bus_min)
  tr$역에서도보소요=sta_min
  tr$역에서버스소요=bus_min
  tr$역세권여부 = if_else(sta_min<=10,1,0,0)
  return(tr)
}
# preproc함수----
# xdf로부터 모형적합 및 예측치산출에 필요한 형태로 가공
# option = 1: 모형개발용, options=2: 스코어링용
preproc = function(xdf=xdf, yyyymm, option=1){
  require(simputation)
  ym = yyyymm
  brandc = c('래미안','자이','푸르지오','더샵','더샾','아이파크','이편한세상',
             '힐스테이트','롯데캐슬','위브','e편한세상')
  brand = brandc %>% str_c(collapse='|')
  te = filter(xdf, 마감년월==ym) %>% 
    mutate(가격증감율 = prc3/prc0 - 1) %>% 
    mutate(가격증감율03 = prc0/prc_3-1,
           가격증감율02 = prc0/prc_2-1,
           가격증감율01 = prc0/prc_1-1) %>% 
    mutate(브랜드 = str_extract(아파트명,brand),
           브랜드 = str_replace(브랜드,'이편한세상','e편한세상'),
           브랜드 = ifelse(is.na(브랜드),'기타',브랜드)) %>% 
    mutate(브랜드 = fct_relevel(브랜드,brandc)) %>% 
    mutate(브랜드여부=ifelse(브랜드=='기타','기타','브랜드'),
           브랜드여부=fct_relevel(브랜드여부,'브랜드')) %>% 
    mutate(지역구분=case_when(시도 %in% c('서울','경기','인천')~'수도권',
                              TRUE~'지방')) %>% 
    mutate(매매가 = 매매일반거래금액/100000000) %>% 
    mutate(시도 = ifelse(is.na(시도),구군,시도)) %>% 
    rename(전세비율=jpr0) %>% 
    mutate(전세비율증감1 = 전세비율-jpr_1,
           전세비율증감2 = 전세비율-jpr_2,
           전세비율증감3 = 전세비율-jpr_3) %>% 
    # KB아파트시세의 분류 참조
    mutate(면적구분 = case_when(전용면적>135~'대형',
                                전용면적>102~'중대형',
                                전용면적>85~'중대형',
                                전용면적>60~'중형',
                                TRUE~'소형'),
           면적구분=fct_relevel(면적구분,'대형','중대형','중형')) %>% 
    mutate(`2년내매매가증감율`=prc0/prc_2-1,
           `1년내매매가증감율`=prc0/prc_1-1) %>% 
    mutate(매매가증감율=ifelse(is.na(`2년내매매가증감율`),`1년내매매가증감율`,
                         `2년내매매가증감율`))

  te = dobo_f(te)
  # 가격증감율 여부에 따라 분리가능함
  varname = c('시도','브랜드여부','총세대수','대단지여부','세대별주차공간수',
              '입주후경과년수','전용면적','면적구분','전세비율','전세비율증감1',
              '역세권여부','매매가증감율','가격증감율')
  # option=1: 모형검증용, option=2: 스코어링용
  if(option==1){
    varn = varname
  } else if(option==2){
    varn = append(varname,c('마감년월','대출번호'))
  }
  
  te = select(te,varn) %>% 
    mutate(세대별주차공간수=ifelse(세대별주차공간수==0,NA,세대별주차공간수)) %>% 
    impute_rlm(전세비율증감1-브랜드여부+대단지여부+입주후경과년수+역세권여부) %>% 
    impute_rlm(매매가증감율~브랜드여부+대단지여부+입주후경과년수+역세권여부) %>% 
    select(-세대별주차공간수) %>% 
    group_by(grp) %>% 
    mutate(targ_bk=ntile(가격증감율,10)) %>% 
    # 가격증감율 1~4분위(bad),7~10분위(good),5~6분위(판단미정:indeterminante)
    mutate(target = case_when(targ_bk<=4~'bad',
                              targ_bk>=7~'good',
                              TRUE~'ind')) %>% 
    ungroup() %>% 
    mutate(target = factor(target))
  return(te)
}
# pred_df 함수 ----
# 모형에 적용가능하도록 더미화 등 recipe 적용하는 함수 
pred_df = function(rec,te){
  require(recipes)
  te1 = bake(rec,te)
  train_var = rec$var_info$variable
  add_var = dplyr::setdiff(names(te),train_var)
  te1 = bind_cols(select(te,add_var),te1)
  return(te1)
}

# for_scoring 함수----
# 스코어링을 위한 데이터가공 함수로 target생성을 위한
# 가격증감율 변수생성은 배제됨
for_scoring = function(xdf=xdf,yyyymm=NULL){
  require(simputation)
  # grpx : 지역_클러스터링.R 참조 
  if(!exists('grpx')) grpx = read_rds('model/grpx.rds')
  if(is.null(yyyymm)){
    ym = unique(xdf$마감년월)
  } else {
    ym = yyyymm
  }
  brandc = c('래미안','자이','푸르지오','더샵','더샾','아이파크','이편한세상',
             '힐스테이트','롯데캐슬','위브','e편한세상')
  brand = brandc %>% str_c(collapse='|')
  te = filter(xdf,마감년월==ym) %>% 
    mutate(브랜드 = str_extract(아파트명,brand),
           브랜드 = str_replace(브랜드,'이편한세상','e편한세상'),
           브랜드 = ifelse(is.na(브랜드),'기타',브랜드)) %>% 
    mutate(브랜드 = fct_relevel(브랜드,brandc)) %>% 
    mutate(브랜드여부=ifelse(브랜드=='기타','기타','브랜드'),
                브랜드여부=fct_relevel(브랜드여부,'브랜드')) %>% 
    mutate(지역구분=case_when(시도 %in% c('서울','경기','인천')~'수도권',
                            TRUE~'지방')) %>% 
    mutate(매매가 = 매매일반거래금액/100000000) %>% 
    mutate(시도 = ifelse(is.na(시도),구군,시도)) %>% 
    rename(전세비율=jpr0) %>% 
    mutate(전세비율증감1 = 전세비율-jpr_1,
                 전세비율증감2 = 전세비율-jpr_2,
                 전세비율증감3 = 전세비율-jpr_3) %>% 
    # KB아파트시세의 분류 참조
    mutate(면적구분 = case_when(전용면적>135~'대형',
                                전용면적>102~'중대형',
                                전용면적>85~'중대형',
                                전용면적>60~'중형',
                                TRUE~'소형'),
               면적구분=fct_relevel(면적구분,'대형','중대형','중형')) %>% 
    mutate(`2년내매매가증감율`=prc0/prc_2-1,
           `1년내매매가증감율`=prc0/prc_1-1) %>% 
    mutate(매매가증감율=ifelse(is.na(`2년내매매가증감율`),`1년내매매가증감율`,
                         `2년내매매가증감율`)) %>% 
    mutate(총세대수 =as.numeric(총세대수),
           전용면적 =as.numeric(전용면적)) %>% 
    mutate_at(vars(contains('인근'),주변시설기타),
              -str_trim(.) %>% str_squish()) %>% 
    mutate_at(vars(contians('인근'),주변시설기타),
              list(~na_if(.,""))) %>% 
    mutate_at(vars(contains('인근'),주변시설기타),
              list(~ifelse(is.na(.),0,1))) %>% 
    mutate(주변시설여부=ifelse(pmax(인근병원명,인근쇼핑센터명,인근학교명,
                                   인근공원명,주변시설기타)>0,'yes','no')) %>% 
    mutate(지역구분=case_when(시도 %in% c('서울','경기','인천')~시도,
                            시도 %in% c('울산','대구','대전','광주','부산','세종')~'광역자치',
                            TRUE~'지방')) %>% 
    mutate(계단식여부 = if_else(아파트현관구분명=='계단식','yes','no','no'))
  
  # 역세권여부
  te = dobo_f(te)
  varname = c('마감년월','대출번호','시도','구군','브랜드여부','총세대수','대단지여부',
              '세대별주차공간수','입주후경과년수','전용면적','면적구분','전세비율',
              '전세비율증감1','역세권여부','버스역세권여부','매매가증감율',
              '역에서버스소요','주변시설여부','아파트코드','인구증감율','가격증감율',
              '지역구분','계단식여부','방수','욕실수','target')
  # 매매가증감율 : 직전1~2년간 매매가격증감율
  # 가격증감율 : target용, 향후 3년간 매매가격증감율
  te = select(te,one_of(varname)) %>% 
    mutate(세대별주차공간수=ifelse(세대별주차공간수==0,NA,세대별주차공간수)) %>% 
    impute_rlm(전세비율증감1-브랜드여부+대단지여부+입주후경과년수+역세권여부) %>% 
    impute_rlm(매매가증감율~브랜드여부+대단지여부+입주후경과년수+역세권여부) %>% 
    select(-세대별주차공간수) %>% 
    left_join(grpx, by='시도') # grpx : 지역_클러스터링.R 
  
  return(te)
}

# mk_x 함수 ----
# recipe 적용 전까지 만든다 (feature 생성)
# 포함된 내용 
#' 1.ltvdata에서 기초항목 가져오기
#' 2.아파트특성정보가져오기(kb아파트) :기준월,아파트코드,아파트명,평형 등
#' 3.아파트가격 시계열(직전3년) 생성

# mk_y 함수 ----
#' 가격상승율을 이용해서 target 생성