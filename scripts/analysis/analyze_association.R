# 연관성 분석 스크립트
# 피처 데이터와 비만 중증도 등급 간의 연관성 분석

library(dplyr)
library(readr)
library(tidyr)
library(randomForest)

# ============================================================================
# 설정
# ============================================================================

OUTPUT_PATH <- "data/output/"

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

# 데이터 조인
df_analysis <- df_features %>%
  inner_join(
    df_label %>% select(RN_INDI, EXMD_BZ_YYYY, OBESITY_GRADE, OBESITY_GRADE_LABEL),
    by = "RN_INDI"
  )

cat(sprintf("분석 데이터: %d 행\n", nrow(df_analysis)))

# ============================================================================
# 2. 기술통계: 등급별 변수 분포
# ============================================================================

cat("\n=== 등급별 기술통계 ===\n")

# 숫자형 변수 목록
numeric_vars <- df_analysis %>%
  select(where(is.numeric), -RN_INDI, -OBESITY_GRADE) %>%
  names()

# 등급별 평균
desc_stats <- df_analysis %>%
  group_by(OBESITY_GRADE, OBESITY_GRADE_LABEL) %>%
  summarise(
    N = n(),
    across(all_of(numeric_vars), ~mean(., na.rm = TRUE)),
    .groups = "drop"
  )

cat("\n등급별 평균값:\n")
print(desc_stats %>% select(OBESITY_GRADE_LABEL, N, starts_with("M20_VISIT"), starts_with("M20_TOT_AMT")))

# ============================================================================
# 3. 카이제곱 검정: 범주형 변수
# ============================================================================

cat("\n=== 카이제곱 검정 (범주형 변수) ===\n")

# 범주형 변수 목록
categorical_vars <- c("BNC_GAIBJA_GROUP", "BNC_DSB_YN", "M20_HAS_SURGERY", 
                      "M40_HAS_OBESITY", "M40_HAS_HTN", "M40_HAS_DM", "M40_HAS_DLD")

chi_results <- list()

for (var in categorical_vars) {
  if (var %in% names(df_analysis)) {
    tryCatch({
      tbl <- table(df_analysis[[var]], df_analysis$OBESITY_GRADE)
      test <- chisq.test(tbl)
      
      chi_results[[var]] <- data.frame(
        Variable = var,
        ChiSquare = round(test$statistic, 2),
        DF = test$parameter,
        PValue = format.pval(test$p.value, digits = 3),
        Significant = if_else(test$p.value < 0.05, "*", "")
      )
      
      cat(sprintf("%s: χ² = %.2f, p = %s %s\n", 
                  var, test$statistic, format.pval(test$p.value, digits = 3),
                  if_else(test$p.value < 0.05, "*", "")))
    }, error = function(e) {
      cat(sprintf("%s: 검정 불가 (%s)\n", var, e$message))
    })
  }
}

chi_summary <- bind_rows(chi_results)

# ============================================================================
# 4. ANOVA/Kruskal-Wallis: 연속형 변수
# ============================================================================

cat("\n=== Kruskal-Wallis 검정 (연속형 변수) ===\n")

continuous_vars <- c("M20_VISIT_CNT", "M20_INPATIENT_CNT", "M20_OUTPATIENT_CNT",
                     "M20_MDCARE_DD_SUM", "M20_TOT_AMT_SUM", "M20_SBA_SUM",
                     "M30_ITEM_CNT", "M30_AMT_SUM",
                     "M40_SICK_CNT", "M40_SICK_UNIQUE_CNT",
                     "M60_PRSC_CNT", "M60_AMT_SUM",
                     "BNC_CTRB_Q10")

kw_results <- list()

for (var in continuous_vars) {
  if (var %in% names(df_analysis)) {
    tryCatch({
      test <- kruskal.test(df_analysis[[var]] ~ df_analysis$OBESITY_GRADE)
      
      kw_results[[var]] <- data.frame(
        Variable = var,
        HStatistic = round(test$statistic, 2),
        DF = test$parameter,
        PValue = format.pval(test$p.value, digits = 3),
        Significant = if_else(test$p.value < 0.05, "*", "")
      )
      
      cat(sprintf("%s: H = %.2f, p = %s %s\n", 
                  var, test$statistic, format.pval(test$p.value, digits = 3),
                  if_else(test$p.value < 0.05, "*", "")))
    }, error = function(e) {
      cat(sprintf("%s: 검정 불가 (%s)\n", var, e$message))
    })
  }
}

kw_summary <- bind_rows(kw_results)

# ============================================================================
# 5. Random Forest 변수 중요도
# ============================================================================

cat("\n=== Random Forest 변수 중요도 ===\n")

# 모델링용 데이터 준비
df_rf <- df_analysis %>%
  select(OBESITY_GRADE, all_of(numeric_vars)) %>%
  mutate(OBESITY_GRADE = as.factor(OBESITY_GRADE)) %>%
  drop_na()

# 범주형 변수 추가
if ("BNC_GAIBJA_GROUP" %in% names(df_analysis)) {
  df_rf <- df_rf %>%
    bind_cols(
      df_analysis %>% 
        select(BNC_GAIBJA_GROUP) %>%
        slice(which(!is.na(df_analysis$OBESITY_GRADE)))
    ) %>%
    mutate(BNC_GAIBJA_GROUP = as.factor(BNC_GAIBJA_GROUP))
}

cat(sprintf("Random Forest 학습 데이터: %d 행\n", nrow(df_rf)))

# Random Forest 학습
set.seed(42)
rf_model <- randomForest(
  OBESITY_GRADE ~ ., 
  data = df_rf,
  ntree = 100,
  importance = TRUE
)

# 변수 중요도 추출
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseGini))

cat("\n변수 중요도 (상위 15개):\n")
print(head(importance_df, 15))

# ============================================================================
# 6. 결과 저장
# ============================================================================

cat("\n=== 결과 저장 ===\n")

# 카이제곱 결과 저장
if (nrow(chi_summary) > 0) {
  write_csv(chi_summary, paste0(OUTPUT_PATH, "association_chi_square.csv"))
  cat("카이제곱 검정 결과 저장 완료\n")
}

# Kruskal-Wallis 결과 저장
if (nrow(kw_summary) > 0) {
  write_csv(kw_summary, paste0(OUTPUT_PATH, "association_kruskal_wallis.csv"))
  cat("Kruskal-Wallis 검정 결과 저장 완료\n")
}

# 변수 중요도 저장
write_csv(importance_df, paste0(OUTPUT_PATH, "association_feature_importance.csv"))
cat("변수 중요도 저장 완료\n")

# 기술통계 저장
write_csv(desc_stats, paste0(OUTPUT_PATH, "association_descriptive_stats.csv"))
cat("기술통계 저장 완료\n")

# ============================================================================
# 7. 요약 리포트
# ============================================================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n연관성 분석 요약 리포트\n")
cat("=" |> rep(60) |> paste(collapse = ""))

cat("\n\n[1] 유의미한 범주형 변수 (p < 0.05):\n")
if (nrow(chi_summary) > 0) {
  sig_chi <- chi_summary %>% filter(Significant == "*")
  if (nrow(sig_chi) > 0) {
    for (i in 1:nrow(sig_chi)) {
      cat(sprintf("  - %s (p = %s)\n", sig_chi$Variable[i], sig_chi$PValue[i]))
    }
  } else {
    cat("  없음\n")
  }
}

cat("\n[2] 유의미한 연속형 변수 (p < 0.05):\n")
if (nrow(kw_summary) > 0) {
  sig_kw <- kw_summary %>% filter(Significant == "*")
  if (nrow(sig_kw) > 0) {
    for (i in 1:nrow(sig_kw)) {
      cat(sprintf("  - %s (p = %s)\n", sig_kw$Variable[i], sig_kw$PValue[i]))
    }
  } else {
    cat("  없음\n")
  }
}

cat("\n[3] Random Forest 변수 중요도 TOP 10:\n")
top10 <- head(importance_df, 10)
for (i in 1:nrow(top10)) {
  cat(sprintf("  %2d. %s (Gini: %.2f)\n", i, top10$Variable[i], top10$MeanDecreaseGini[i]))
}

cat("\n=== 연관성 분석 완료 ===\n")

