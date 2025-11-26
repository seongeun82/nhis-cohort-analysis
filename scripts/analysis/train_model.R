# 예측 모델 학습 스크립트
# 다항 로지스틱 회귀를 사용한 비만 중증도 예측 모델

library(dplyr)
library(readr)
library(nnet)       # multinom (다항 로지스틱 회귀)
library(caret)      # confusionMatrix
library(MASS)       # stepAIC

# ============================================================================
# 설정
# ============================================================================

OUTPUT_PATH <- "data/output/"

# ============================================================================
# 1. 데이터 로드
# ============================================================================

cat("=== 데이터 로드 ===\n")

# Train/Test 데이터 로드
df_train <- read_csv(paste0(OUTPUT_PATH, "model_train_data.csv"))
df_test <- read_csv(paste0(OUTPUT_PATH, "model_test_data.csv"))

# 피처 목록 로드
model_features <- read_lines(paste0(OUTPUT_PATH, "model_features.txt"))

cat(sprintf("Train 데이터: %d 행\n", nrow(df_train)))
cat(sprintf("Test 데이터: %d 행\n", nrow(df_test)))
cat(sprintf("피처 수: %d 개\n", length(model_features)))

# 타겟 변수를 팩터로 변환
df_train <- df_train %>% mutate(OBESITY_GRADE = as.factor(OBESITY_GRADE))
df_test <- df_test %>% mutate(OBESITY_GRADE = as.factor(OBESITY_GRADE))

# ============================================================================
# 2. 다항 로지스틱 회귀 모델 학습
# ============================================================================

cat("\n=== 다항 로지스틱 회귀 모델 학습 ===\n")

# 모델 공식 생성
formula_str <- paste("OBESITY_GRADE ~", paste(model_features, collapse = " + "))
model_formula <- as.formula(formula_str)

cat("모델 공식:\n")
cat(formula_str, "\n\n")

# 다항 로지스틱 회귀 학습
cat("모델 학습 중...\n")
model_full <- multinom(
  model_formula,
  data = df_train,
  maxit = 500,
  trace = FALSE
)

cat("학습 완료!\n")

# 모델 요약
cat("\n모델 요약:\n")
print(summary(model_full))

# ============================================================================
# 3. 변수 선택 (Stepwise)
# ============================================================================

cat("\n=== 변수 선택 (Stepwise AIC) ===\n")

# Stepwise 변수 선택
cat("Stepwise 변수 선택 중...\n")
model_step <- stepAIC(model_full, direction = "both", trace = FALSE)

cat("선택 완료!\n")

# 선택된 변수 확인
selected_vars <- attr(terms(model_step), "term.labels")
cat(sprintf("\n선택된 변수 (%d 개):\n", length(selected_vars)))
for (i in seq_along(selected_vars)) {
  cat(sprintf("  %2d. %s\n", i, selected_vars[i]))
}

# ============================================================================
# 4. 모델 평가
# ============================================================================

cat("\n=== 모델 평가 ===\n")

# Train 데이터 예측
pred_train <- predict(model_step, newdata = df_train, type = "class")
acc_train <- mean(pred_train == df_train$OBESITY_GRADE)

# Test 데이터 예측
pred_test <- predict(model_step, newdata = df_test, type = "class")
acc_test <- mean(pred_test == df_test$OBESITY_GRADE)

cat(sprintf("\nTrain Accuracy: %.2f%%\n", 100 * acc_train))
cat(sprintf("Test Accuracy: %.2f%%\n", 100 * acc_test))

# Confusion Matrix (Test 데이터)
cat("\n[Test 데이터 Confusion Matrix]\n")
cm <- confusionMatrix(pred_test, df_test$OBESITY_GRADE)
print(cm$table)

cat("\n[클래스별 성능]\n")
print(cm$byClass[, c("Sensitivity", "Specificity", "Balanced Accuracy")])

# ============================================================================
# 5. 계수 및 Odds Ratio
# ============================================================================

cat("\n=== 회귀 계수 및 Odds Ratio ===\n")

# 계수 추출
coef_matrix <- coef(model_step)
se_matrix <- summary(model_step)$standard.errors

# Odds Ratio 계산
or_matrix <- exp(coef_matrix)

# 결과 정리
coef_df <- data.frame()

for (class_name in rownames(coef_matrix)) {
  for (var_name in colnames(coef_matrix)) {
    coef_val <- coef_matrix[class_name, var_name]
    se_val <- se_matrix[class_name, var_name]
    or_val <- or_matrix[class_name, var_name]
    
    # Z-value 및 P-value 계산
    z_val <- coef_val / se_val
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    coef_df <- rbind(coef_df, data.frame(
      Class = class_name,
      Variable = var_name,
      Coefficient = round(coef_val, 4),
      SE = round(se_val, 4),
      OddsRatio = round(or_val, 4),
      Z = round(z_val, 2),
      PValue = round(p_val, 4),
      Significant = if_else(p_val < 0.05, "*", "")
    ))
  }
}

# 유의미한 변수만 출력
cat("\n유의미한 변수 (p < 0.05):\n")
sig_vars <- coef_df %>%
  filter(Significant == "*", Variable != "(Intercept)") %>%
  arrange(Class, desc(abs(Coefficient)))

print(sig_vars)

# ============================================================================
# 6. 예측 확률
# ============================================================================

cat("\n=== 예측 확률 예시 ===\n")

# Test 데이터 예측 확률
prob_test <- predict(model_step, newdata = df_test, type = "probs")

# 예측 확률 데이터프레임
df_predictions <- df_test %>%
  select(RN_INDI, OBESITY_GRADE, OBESITY_GRADE_LABEL) %>%
  bind_cols(as.data.frame(prob_test)) %>%
  rename(
    PROB_1 = `1`,
    PROB_2 = `2`,
    PROB_3 = `3`,
    PROB_4 = `4`
  ) %>%
  mutate(PRED_GRADE = as.character(pred_test))

cat("\n예측 결과 샘플 (10행):\n")
print(head(df_predictions, 10))

# ============================================================================
# 7. 결과 저장
# ============================================================================

cat("\n=== 결과 저장 ===\n")

# 계수 및 Odds Ratio 저장
write_csv(coef_df, paste0(OUTPUT_PATH, "model_coefficients.csv"))
cat("회귀 계수 저장 완료\n")

# 예측 결과 저장
write_csv(df_predictions, paste0(OUTPUT_PATH, "model_predictions.csv"))
cat("예측 결과 저장 완료\n")

# 선택된 변수 저장
write_lines(selected_vars, paste0(OUTPUT_PATH, "model_selected_features.txt"))
cat("선택된 변수 저장 완료\n")

# 모델 객체 저장
saveRDS(model_step, paste0(OUTPUT_PATH, "model_logistic.rds"))
cat("모델 객체 저장 완료\n")

# ============================================================================
# 8. 최종 리포트
# ============================================================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n예측 모델 학습 결과 리포트\n")
cat("=" |> rep(60) |> paste(collapse = ""))

cat("\n\n[1] 모델 정보\n")
cat(sprintf("  - 모델 유형: 다항 로지스틱 회귀 (Multinomial Logistic Regression)\n"))
cat(sprintf("  - 전체 피처 수: %d 개\n", length(model_features)))
cat(sprintf("  - 선택된 피처 수: %d 개\n", length(selected_vars)))
cat(sprintf("  - 타겟 클래스: 4 개 (정상, 경도, 중등도, 고도)\n"))

cat("\n[2] 예측 성능\n")
cat(sprintf("  - Train Accuracy: %.2f%%\n", 100 * acc_train))
cat(sprintf("  - Test Accuracy: %.2f%%\n", 100 * acc_test))
cat(sprintf("  - Overall Kappa: %.3f\n", cm$overall["Kappa"]))

cat("\n[3] 주요 예측 변수 (유의미한 변수 수)\n")
sig_count <- coef_df %>%
  filter(Significant == "*", Variable != "(Intercept)") %>%
  count(Variable) %>%
  arrange(desc(n))

for (i in 1:min(10, nrow(sig_count))) {
  cat(sprintf("  %2d. %s (클래스 %d개에서 유의)\n", 
              i, sig_count$Variable[i], sig_count$n[i]))
}

cat("\n[4] 저장된 파일\n")
cat("  - model_coefficients.csv: 회귀 계수 및 Odds Ratio\n")
cat("  - model_predictions.csv: Test 데이터 예측 결과\n")
cat("  - model_selected_features.txt: 선택된 변수 목록\n")
cat("  - model_logistic.rds: 학습된 모델 객체\n")

cat("\n=== 예측 모델 학습 완료 ===\n")

