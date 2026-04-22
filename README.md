# Apartment Price-Decline Probability Modeling for Mortgage Risk Management

아파트 가격 하락확률을 활용해 담보물 위험을 정교하게 평가하려는 모기지 리스크 분석 프로젝트입니다.
단순 LTV나 지역 기준만으로는 포착하기 어려운 담보물의 하방위험을 데이터 기반으로 구조화했습니다.
유동화 대상 자산 선정과 담보 리스크 모니터링에 활용할 수 있는 실무형 모델링 사례입니다.

This project models the probability of apartment price decline to support mortgage asset selection and securitization decisions.

Instead of relying only on simple rules such as current LTV or regional classification, the project aims to incorporate property-level, location-level, and market-level information into a more structured risk signal.

---

## 1. Business Context

In mortgage portfolio management, asset selection is often based on broad indicators such as LTV, region, or operational screening rules.

However, these rules may not fully capture the forward-looking downside risk of the underlying collateral.

This project was designed to improve asset-selection criteria by estimating the probability that an apartment's price will decline materially over a future horizon.

---

## 2. Objective

The objective of this project is to build a model that predicts whether an apartment price will fall by at least 5% over the next 3 years.

The resulting probability can be used to support:

- mortgage asset selection
- securitization candidate screening
- collateral-risk monitoring
- more refined interpretation of property-side downside risk

---

## 3. Project Idea

The core idea is simple:

- identify mortgage assets eligible for review
- construct collateral-, location-, and market-related features
- predict future downside in apartment prices
- convert the model output into an interpretable probability signal

This makes the selection process less dependent on static rule-based criteria alone.

---

## 4. Data Structure

The model combines multiple types of information related to the collateral asset, including:

- apartment/property attributes
- building size and layout information
- brand and complex size
- years since move-in
- transportation accessibility
- nearby facilities
- population-change indicators
- sale-price trend indicators
- jeonse-related indicators

The target is defined using future apartment price change over a multi-year horizon.

---

## 5. Methodology

### Step 1. Base data construction
Build a modeling dataset for eligible mortgage assets.

### Step 2. Feature engineering
Generate collateral-related, location-related, and market-related variables for scoring.

### Step 3. Target definition
Label each case based on whether the apartment price declines by at least 5% over the target horizon.

### Step 4. Preprocessing
Apply categorical encoding and preprocessing through a recipe-based pipeline.

### Step 5. Model training
Train an XGBoost-based classification model.

### Step 6. Probability calibration
Convert model output into a more interpretable price-decline probability.

---

## 6. Repository Structure

```text
R/
├─ 0_init.R                     # helper functions and feature engineering utilities
├─ 1_담보물_가격하락확률_예측모형.R   # model training and probability estimation
└─ README.md
```

---

## 7. Practical Relevance

This is not just a housing-price prediction exercise.

Its practical value lies in connecting collateral-side market risk to real mortgage portfolio decisions, especially where simple LTV or broad regional rules may be too coarse.

Potential use cases include:

- selecting assets for securitization
- refining collateral screening criteria
- identifying assets with elevated downside risk
- supporting mortgage risk monitoring and communication

---

## 8. Public Repository Notes

This repository is a simplified public version of a practical internal analysis project.

Because the original work depended on internal mortgage portfolio data and environment-specific preparation steps, the public repository does not include:

- source datasets
- internal screening data
- full production-ready scoring pipeline
- all execution dependencies needed for direct reproducibility

The repository is intended to show:

- the business problem
- the modeling logic
- the feature-engineering direction
- the practical use case

rather than provide a turnkey public package.

---

## 9. Tech Stack

- R
- tidyverse
- recipes
- caret
- xgboost
- pROC
- DALEX

---

## 10. Future Improvements

- add synthetic sample data
- separate feature engineering and modeling into modular scripts
- clean environment-specific code
- document example outputs and interpretation guide
- include figures for feature importance and model workflow
