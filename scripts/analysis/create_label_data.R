# 비만 중증도 정답셋 생성 스크립트
# G1E 건강검진 데이터와 BNC 자격 데이터를 활용하여
# 4단계 비만 중증도 등급을 계산하고 정답셋을 생성

library(dplyr)
library(readr)

# 공통 함수 로드
source("scripts/common/load_data_info_nhs.R")

# ============================================================================
# 설정
# ============================================================================

# 데이터 경로 (실제 데이터 경로로 수정 필요)
DATA_PATH <- "data/"

# 출력 경로
OUTPUT_PATH <- "data/output/"

# ============================================================================
# 1. 데이터 로드
# ============================================================================

cat("=== 데이터 로드 시작 ===\n")

# G1E 건강검진 데이터 로드
df_g1e <- read_nhs_datafile("G1E", DATA_PATH)
cat(sprintf("G1E 데이터 로드 완료: %d 행\n", nrow(df_g1e)))

# BNC 자격 데이터 로드 (성별 정보용)
df_bnc <- read_nhs_datafile("BNC", DATA_PATH)
cat(sprintf("BNC 데이터 로드 완료: %d 행\n", nrow(df_bnc)))

# ============================================================================
# 2. 데이터 전처리 및 조인
# ============================================================================

cat("\n=== 데이터 전처리 시작 ===\n")

# BNC에서 성별 정보 추출
df_sex <- df_bnc %>%
  select(RN_INDI, SEX) %>%
  distinct()

cat(sprintf("고유 개인 수: %d 명\n", nrow(df_sex)))

# G1E와 성별 정보 조인
df_g1e_with_sex <- df_g1e %>%
  inner_join(df_sex, by = "RN_INDI")

cat(sprintf("조인 후 G1E 데이터: %d 행\n", nrow(df_g1e_with_sex)))

# ============================================================================
# 3. 대사지표 이상 여부 계산
# ============================================================================

cat("\n=== 대사지표 이상 여부 계산 ===\n")

df_metabolic <- df_g1e_with_sex %>%
  mutate(
    # 1) 공복혈당 이상: >= 100 mg/dL
    FBS_ABNORMAL = if_else(G1E_FBS >= 100, 1L, 0L, missing = NA_integer_),
    
    # 2) 혈압 이상: 수축기 >= 130 OR 이완기 >= 85
    BP_ABNORMAL = if_else(
      G1E_BP_SYS >= 130 | G1E_BP_DIA >= 85, 
      1L, 0L, 
      missing = NA_integer_
    ),
    
    # 3) 중성지방 이상: >= 150 mg/dL
    TG_ABNORMAL = if_else(G1E_TG >= 150, 1L, 0L, missing = NA_integer_),
    
    # 4) HDL 이상: 남성 < 40, 여성 < 50 mg/dL
    HDL_ABNORMAL = case_when(
      is.na(G1E_HDL) ~ NA_integer_,
      SEX == "1" & G1E_HDL < 40 ~ 1L,  # 남성
      SEX == "2" & G1E_HDL < 50 ~ 1L,  # 여성
      TRUE ~ 0L
    ),
    
    # 5) 허리둘레 이상: 남성 >= 90cm, 여성 >= 85cm
    WSTC_ABNORMAL = case_when(
      is.na(G1E_WSTC) ~ NA_integer_,
      SEX == "1" & G1E_WSTC >= 90 ~ 1L,  # 남성
      SEX == "2" & G1E_WSTC >= 85 ~ 1L,  # 여성
      TRUE ~ 0L
    ),
    
    # 대사지표 이상 개수 (NA는 제외하고 계산)
    METABOLIC_ABNORMAL_CNT = rowSums(
      across(c(FBS_ABNORMAL, BP_ABNORMAL, TG_ABNORMAL, HDL_ABNORMAL, WSTC_ABNORMAL)),
      na.rm = TRUE
    ),
    
    # 대사지표 이상 여부 (1개 이상이면 이상)
    HAS_METABOLIC_ABNORMAL = if_else(METABOLIC_ABNORMAL_CNT >= 1, TRUE, FALSE)
  )

# 대사지표 이상 현황 출력
cat("\n대사지표 이상 현황:\n")
cat(sprintf("  - 공복혈당 이상: %d 명 (%.1f%%)\n", 
    sum(df_metabolic$FBS_ABNORMAL == 1, na.rm = TRUE),
    100 * mean(df_metabolic$FBS_ABNORMAL == 1, na.rm = TRUE)))
cat(sprintf("  - 혈압 이상: %d 명 (%.1f%%)\n", 
    sum(df_metabolic$BP_ABNORMAL == 1, na.rm = TRUE),
    100 * mean(df_metabolic$BP_ABNORMAL == 1, na.rm = TRUE)))
cat(sprintf("  - 중성지방 이상: %d 명 (%.1f%%)\n", 
    sum(df_metabolic$TG_ABNORMAL == 1, na.rm = TRUE),
    100 * mean(df_metabolic$TG_ABNORMAL == 1, na.rm = TRUE)))
cat(sprintf("  - HDL 이상: %d 명 (%.1f%%)\n", 
    sum(df_metabolic$HDL_ABNORMAL == 1, na.rm = TRUE),
    100 * mean(df_metabolic$HDL_ABNORMAL == 1, na.rm = TRUE)))
cat(sprintf("  - 허리둘레 이상: %d 명 (%.1f%%)\n", 
    sum(df_metabolic$WSTC_ABNORMAL == 1, na.rm = TRUE),
    100 * mean(df_metabolic$WSTC_ABNORMAL == 1, na.rm = TRUE)))

# ============================================================================
# 4. 비만 중증도 등급 분류
# ============================================================================

cat("\n=== 비만 중증도 등급 분류 ===\n")

# 등급 기준:
# 1 (정상): BMI < 25
# 2 (경도): BMI 25-30 + 대사지표 정상
# 3 (중등도): BMI 30-35 OR (BMI 25-30 + 대사지표 이상)
# 4 (고도): BMI >= 35 OR (BMI 30-35 + 대사지표 이상)

df_labeled <- df_metabolic %>%
  mutate(
    OBESITY_GRADE = case_when(
      is.na(G1E_BMI) ~ NA_integer_,
      
      # 고도 비만 (등급 4)
      G1E_BMI >= 35 ~ 4L,
      G1E_BMI >= 30 & G1E_BMI < 35 & HAS_METABOLIC_ABNORMAL ~ 4L,
      
      # 중등도 비만 (등급 3)
      G1E_BMI >= 30 & G1E_BMI < 35 ~ 3L,
      G1E_BMI >= 25 & G1E_BMI < 30 & HAS_METABOLIC_ABNORMAL ~ 3L,
      
      # 경도 비만 (등급 2)
      G1E_BMI >= 25 & G1E_BMI < 30 & !HAS_METABOLIC_ABNORMAL ~ 2L,
      
      # 정상 (등급 1)
      G1E_BMI < 25 ~ 1L,
      
      TRUE ~ NA_integer_
    ),
    
    # 등급 라벨
    OBESITY_GRADE_LABEL = case_when(
      OBESITY_GRADE == 1 ~ "정상",
      OBESITY_GRADE == 2 ~ "경도비만",
      OBESITY_GRADE == 3 ~ "중등도비만",
      OBESITY_GRADE == 4 ~ "고도비만",
      TRUE ~ NA_character_
    )
  )

# 등급별 현황 출력
cat("\n비만 중증도 등급 분포:\n")
grade_summary <- df_labeled %>%
  filter(!is.na(OBESITY_GRADE)) %>%
  count(OBESITY_GRADE, OBESITY_GRADE_LABEL) %>%
  mutate(pct = round(100 * n / sum(n), 1))

for (i in 1:nrow(grade_summary)) {
  cat(sprintf("  - 등급 %d (%s): %d 명 (%.1f%%)\n",
      grade_summary$OBESITY_GRADE[i],
      grade_summary$OBESITY_GRADE_LABEL[i],
      grade_summary$n[i],
      grade_summary$pct[i]))
}

# ============================================================================
# 5. 결과 저장
# ============================================================================

cat("\n=== 결과 저장 ===\n")

# 출력 폴더 생성
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
  cat(sprintf("출력 폴더 생성: %s\n", OUTPUT_PATH))
}

# 최종 정답셋 생성
df_final <- df_labeled %>%
  select(
    RN_INDI,
    SEX,
    EXMD_BZ_YYYY,
    OBESITY_GRADE,
    OBESITY_GRADE_LABEL
  ) %>%
  filter(!is.na(OBESITY_GRADE))  # BMI 결측 제외

# CSV 저장
output_file <- paste0(OUTPUT_PATH, "label_obesity_severity.csv")
write_csv(df_final, output_file)

cat(sprintf("정답셋 저장 완료: %s\n", output_file))
cat(sprintf("총 레코드 수: %d 행\n", nrow(df_final)))
cat(sprintf("고유 개인 수: %d 명\n", n_distinct(df_final$RN_INDI)))

# ============================================================================
# 6. 요약 통계 출력
# ============================================================================

cat("\n=== 요약 통계 ===\n")

# 성별 × 등급 교차표
cat("\n성별별 비만 등급 분포:\n")
sex_grade_table <- df_final %>%
  mutate(SEX_LABEL = if_else(SEX == "1", "남성", "여성")) %>%
  count(SEX_LABEL, OBESITY_GRADE_LABEL) %>%
  tidyr::pivot_wider(names_from = OBESITY_GRADE_LABEL, values_from = n, values_fill = 0)

print(sex_grade_table)

# 연도별 분포
cat("\n연도별 레코드 수:\n")
year_summary <- df_final %>%
  count(EXMD_BZ_YYYY) %>%
  arrange(EXMD_BZ_YYYY)

print(year_summary)

cat("\n=== 정답셋 생성 완료 ===\n")

