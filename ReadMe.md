## 분석 목표
- 청구데이터를 활용해서 비만의 중증도를 판단할 수 있는 지표를 발견하는 것을 목표로 함 

### 1. 정답셋 구축
- G1E 데이터로 비만 중증도 정량화 (등급)

### 2. 연관성 분석
- BNC, M20, M30, M40, M60의 어떤 변수가 비만 중증도와 연관있는지 파악
- 각 테이블별 중요 변수 순위 도출

### 3. 예측 모델 개발
- 입력: BNC, M20, M30, M40, M60 데이터
- 출력: 비만 중증도 (G1E 기반)
- 목표: 청구데이터만으로 중증도 예측 

## 분석 계획 
### 1단계: 정답셋 생성 

##### 대사지표 이상 기준 (1개 이상 해당)
- 공복혈당 ≥ 100 mg/dL
- 혈압: 수축기 ≥ 130 또는 이완기 ≥ 85
- 중성지방 ≥ 150 mg/dL
- HDL: 남성 < 40, 여성 < 50 mg/dL
- 허리둘레: 남성 ≥ 90cm, 여성 ≥ 85cm

##### 필요 변수
- G1E: G1E_BMI, G1E_WSTC, G1E_FBS, G1E_BP_SYS, G1E_BP_DIA, G1E_TG, G1E_HDL
- BNC: SEX, RN_INDI

### 2단계: 피처 생성 및 연관성 분석

##### 피처 생성
- BNC: 가입자유형, 보험료분위, 장애여부
- M20: 진료건수, 입원/외래건수, 진료비, 수술여부
- M30: 진료항목건수, 진료비구분별 금액
- M40: 상병건수, 질환플래그(비만/고혈압/당뇨/이상지질혈증)
- M60: 처방건수, 처방일수, 약제비

##### 연관성 분석
- 카이제곱 검정 (범주형), Kruskal-Wallis (연속형)
- Random Forest 변수 중요도

### 3단계: 예측 모델

##### 모델
- 다항 로지스틱 회귀 (Multinomial Logistic Regression)
- Stepwise AIC 변수 선택

##### 평가
- Train/Test 분할 (7:3)
- Accuracy, Confusion Matrix

## 실행 코드



```r
source("scripts/analysis/create_label_data.R")    # 1. 정답셋 생성
source("scripts/analysis/create_feature_data.R")  # 2. 피처 생성
source("scripts/analysis/analyze_association.R")  # 3. 연관성 분석
source("scripts/analysis/prepare_model_data.R")   # 4. 모델 데이터 준비
source("scripts/analysis/train_model.R")          # 5. 모델 학습
```