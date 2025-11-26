# 모델링 데이터 준비 스크립트
# 피처 데이터 + 정답셋 조인, 결측치 처리, Train/Test 분할

library(dplyr)
library(readr)
library(tidyr)

# ============================================================================
# 설정
# ============================================================================

OUTPUT_PATH <- "data/output/"
TRAIN_RATIO <- 0.7
SEED <- 42

# ============================================================================
# 1. 데이터 로드
# ============================================================================

cat("=== 데이터 로드 ===\n")

# 피처 데이터
df_features <- read_csv(paste0(OUTPUT_PATH, "feature_data.csv"))
cat(sprintf("피처 데이터: %d 행, %d 열\n", nrow(df_features), ncol(df_features)))

# 정답셋
df_label <- read_csv(paste0(OUTPUT_PATH, "label_obesity_severity.csv"))
cat(sprintf("정답셋: %d 행\n", nrow(df_label)))

# ============================================================================
# 2. 데이터 조인
# ============================================================================

cat("\n=== 데이터 조인 ===\n")

df_model <- df_features %>%
  inner_join(
    df_label %>% select(RN_INDI, SEX, EXMD_BZ_YYYY, OBESITY_GRADE, OBESITY_GRADE_LABEL),
    by = "RN_INDI"
  )

cat(sprintf("조인 후 데이터: %d 행\n", nrow(df_model)))

# ============================================================================
# 3. 결측치 처리
# ============================================================================

cat("\n=== 결측치 처리 ===\n")

# 결측치 현황 확인
na_summary <- df_model %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count))

if (nrow(na_summary) > 0) {
  cat("결측치가 있는 변수:\n")
  print(na_summary)
} else {
  cat("결측치 없음\n")
}

# 숫자형 변수 결측치를 0으로 대체
numeric_cols <- df_model %>%
  select(where(is.numeric), -RN_INDI, -OBESITY_GRADE, -EXMD_BZ_YYYY) %>%
  names()

df_model <- df_model %>%
  mutate(across(all_of(numeric_cols), ~replace_na(., 0)))

# 범주형 변수 결측치를 "Unknown"으로 대체
if ("BNC_GAIBJA_GROUP" %in% names(df_model)) {
  df_model <- df_model %>%
    mutate(BNC_GAIBJA_GROUP = replace_na(BNC_GAIBJA_GROUP, "Unknown"))
}

if ("BNC_SIDO" %in% names(df_model)) {
  df_model <- df_model %>%
    mutate(BNC_SIDO = replace_na(BNC_SIDO, "00"))
}

cat("결측치 처리 완료\n")

# ============================================================================
# 4. 피처 변환
# ============================================================================

cat("\n=== 피처 변환 ===\n")

# 범주형 변수를 더미 변수로 변환
df_model <- df_model %>%
  mutate(
    # 성별 더미
    SEX_MALE = if_else(SEX == "1", 1L, 0L),
    
    # 가입자 유형 더미 (기준: 직장)
    GAIBJA_LOCAL = if_else(BNC_GAIBJA_GROUP == "지역", 1L, 0L),
    GAIBJA_MEDICAID = if_else(BNC_GAIBJA_GROUP == "의료급여", 1L, 0L)
  )

# 타겟 변수를 팩터로 변환
df_model <- df_model %>%
  mutate(OBESITY_GRADE = as.factor(OBESITY_GRADE))

cat("피처 변환 완료\n")

# ============================================================================
# 5. Train/Test 분할
# ============================================================================

cat("\n=== Train/Test 분할 ===\n")

set.seed(SEED)

# 층화 샘플링 (등급별 비율 유지)
train_indices <- df_model %>%
  mutate(row_id = row_number()) %>%
  group_by(OBESITY_GRADE) %>%
  sample_frac(TRAIN_RATIO) %>%
  pull(row_id)

df_train <- df_model %>% slice(train_indices)
df_test <- df_model %>% slice(-train_indices)

cat(sprintf("Train 데이터: %d 행 (%.1f%%)\n", nrow(df_train), 100 * nrow(df_train) / nrow(df_model)))
cat(sprintf("Test 데이터: %d 행 (%.1f%%)\n", nrow(df_test), 100 * nrow(df_test) / nrow(df_model)))

# 등급별 분포 확인
cat("\nTrain 데이터 등급 분포:\n")
print(df_train %>% count(OBESITY_GRADE, OBESITY_GRADE_LABEL) %>% mutate(pct = round(100 * n / sum(n), 1)))

cat("\nTest 데이터 등급 분포:\n")
print(df_test %>% count(OBESITY_GRADE, OBESITY_GRADE_LABEL) %>% mutate(pct = round(100 * n / sum(n), 1)))

# ============================================================================
# 6. 모델링용 피처 선택
# ============================================================================

cat("\n=== 모델링용 피처 선택 ===\n")

# 모델에 사용할 피처 목록
model_features <- c(
  # BNC 피처
  "BNC_CTRB_Q10", "BNC_DSB_YN",
  "SEX_MALE", "GAIBJA_LOCAL", "GAIBJA_MEDICAID",
  
  # M20 피처
  "M20_VISIT_CNT", "M20_INPATIENT_CNT", "M20_OUTPATIENT_CNT",
  "M20_MDCARE_DD_SUM", "M20_VSHSP_DD_SUM", "M20_PRSC_DD_SUM",
  "M20_TOT_AMT_SUM", "M20_SBA_SUM", "M20_HAS_SURGERY",
  
  # M30 피처
  "M30_ITEM_CNT", "M30_AMT_SUM",
  
  # M40 피처
  "M40_SICK_CNT", "M40_SICK_UNIQUE_CNT",
  "M40_HAS_OBESITY", "M40_HAS_HTN", "M40_HAS_DM", "M40_HAS_DLD",
  
  # M60 피처
  "M60_PRSC_CNT", "M60_PRSC_DD_SUM", "M60_AMT_SUM"
)

# 실제로 존재하는 피처만 선택
available_features <- intersect(model_features, names(df_model))
cat(sprintf("사용 가능한 피처: %d 개\n", length(available_features)))

# Train/Test 데이터에서 피처만 선택
X_train <- df_train %>% select(all_of(available_features))
y_train <- df_train$OBESITY_GRADE

X_test <- df_test %>% select(all_of(available_features))
y_test <- df_test$OBESITY_GRADE

cat("\n피처 목록:\n")
for (i in seq_along(available_features)) {
  cat(sprintf("  %2d. %s\n", i, available_features[i]))
}

# ============================================================================
# 7. 결과 저장
# ============================================================================

cat("\n=== 결과 저장 ===\n")

# Train 데이터 저장
write_csv(df_train, paste0(OUTPUT_PATH, "model_train_data.csv"))
cat("Train 데이터 저장 완료\n")

# Test 데이터 저장
write_csv(df_test, paste0(OUTPUT_PATH, "model_test_data.csv"))
cat("Test 데이터 저장 완료\n")

# 피처 목록 저장
write_lines(available_features, paste0(OUTPUT_PATH, "model_features.txt"))
cat("피처 목록 저장 완료\n")

# ============================================================================
# 8. 요약
# ============================================================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n모델링 데이터 준비 완료\n")
cat("=" |> rep(60) |> paste(collapse = ""))

cat(sprintf("\n- 전체 데이터: %d 행\n", nrow(df_model)))
cat(sprintf("- Train 데이터: %d 행 (%.0f%%)\n", nrow(df_train), 100 * TRAIN_RATIO))
cat(sprintf("- Test 데이터: %d 행 (%.0f%%)\n", nrow(df_test), 100 * (1 - TRAIN_RATIO)))
cat(sprintf("- 피처 수: %d 개\n", length(available_features)))
cat(sprintf("- 타겟 클래스: %d 개\n", n_distinct(y_train)))

cat("\n=== 데이터 준비 완료 ===\n")

