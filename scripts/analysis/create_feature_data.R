# 피처 데이터셋 생성 스크립트
# 개인(RN_INDI)별로 BNC, M20, M30, M40, M60 데이터를 요약하여 피처 생성

library(dplyr)
library(readr)
library(tidyr)

# 공통 함수 로드
source("scripts/common/load_data_info_nhs.R")

# ============================================================================
# 설정
# ============================================================================

DATA_PATH <- "data/"
OUTPUT_PATH <- "data/output/"

# ============================================================================
# 1. 데이터 로드
# ============================================================================

cat("=== 데이터 로드 시작 ===\n")

df_bnc <- read_nhs_datafile("BNC", DATA_PATH)
cat(sprintf("BNC 데이터: %d 행\n", nrow(df_bnc)))

df_m20 <- read_nhs_datafile("M20", DATA_PATH)
cat(sprintf("M20 데이터: %d 행\n", nrow(df_m20)))

df_m30 <- read_nhs_datafile("M30", DATA_PATH)
cat(sprintf("M30 데이터: %d 행\n", nrow(df_m30)))

df_m40 <- read_nhs_datafile("M40", DATA_PATH)
cat(sprintf("M40 데이터: %d 행\n", nrow(df_m40)))

df_m60 <- read_nhs_datafile("M60", DATA_PATH)
cat(sprintf("M60 데이터: %d 행\n", nrow(df_m60)))

# 정답셋 로드 (대상자 필터링용)
df_label <- read_csv(paste0(OUTPUT_PATH, "label_obesity_severity.csv"))
cat(sprintf("정답셋: %d 행\n", nrow(df_label)))

# 대상자 목록
target_rn_indi <- df_label %>% select(RN_INDI) %>% distinct()

# ============================================================================
# 2. BNC 피처 생성
# ============================================================================

cat("\n=== BNC 피처 생성 ===\n")

feat_bnc <- df_bnc %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  group_by(RN_INDI) %>%
  arrange(desc(STD_YYYY)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    # 시도 코드
    SIDO = substr(SGG, 1, 2),
    
    # 가입자 유형 그룹화
    GAIBJA_GROUP = case_when(
      GAIBJA_TYPE %in% c("1", "2") ~ "지역",
      GAIBJA_TYPE %in% c("5", "6") ~ "직장",
      GAIBJA_TYPE %in% c("7", "8") ~ "의료급여",
      TRUE ~ "기타"
    ),
    
    # 장애 여부
    DSB_YN = if_else(!is.na(DSB_SVRT_CD) & DSB_SVRT_CD != "", 1L, 0L)
  ) %>%
  select(
    RN_INDI,
    BNC_GAIBJA_GROUP = GAIBJA_GROUP,
    BNC_CTRB_Q10 = CTRB_Q10,
    BNC_DSB_YN = DSB_YN,
    BNC_SIDO = SIDO
  )

cat(sprintf("BNC 피처: %d 명\n", nrow(feat_bnc)))

# ============================================================================
# 3. M20 피처 생성 (진료내역)
# ============================================================================

cat("\n=== M20 피처 생성 ===\n")

feat_m20 <- df_m20 %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  mutate(
    # 입원/외래 구분
    IS_INPATIENT = if_else(FORM_CD %in% c("01", "11", "21"), 1L, 0L),
    IS_OUTPATIENT = if_else(FORM_CD %in% c("02", "12", "22"), 1L, 0L),
    
    # 수술 여부
    HAS_SURGERY = if_else(OPRTN_YN == "9", 1L, 0L)
  ) %>%
  group_by(RN_INDI) %>%
  summarise(
    # 진료 건수
    M20_VISIT_CNT = n(),
    M20_INPATIENT_CNT = sum(IS_INPATIENT, na.rm = TRUE),
    M20_OUTPATIENT_CNT = sum(IS_OUTPATIENT, na.rm = TRUE),
    
    # 일수
    M20_MDCARE_DD_SUM = sum(MDCARE_DD_CNT, na.rm = TRUE),
    M20_VSHSP_DD_SUM = sum(VSHSP_DD_CNT, na.rm = TRUE),
    M20_PRSC_DD_SUM = sum(TOT_PRSC_DD_CNT, na.rm = TRUE),
    
    # 진료비
    M20_TOT_AMT_SUM = sum(ED_RC_TOT_AMT, na.rm = TRUE),
    M20_SBA_SUM = sum(EDC_SBA, na.rm = TRUE),
    M20_INSUR_AMT_SUM = sum(EDC_INSUR_BRDN_AMT, na.rm = TRUE),
    
    # 수술 여부
    M20_HAS_SURGERY = max(HAS_SURGERY, na.rm = TRUE),
    
    .groups = "drop"
  )

cat(sprintf("M20 피처: %d 명\n", nrow(feat_m20)))

# ============================================================================
# 4. M30 피처 생성 (진료내역 세부)
# ============================================================================

cat("\n=== M30 피처 생성 ===\n")

feat_m30 <- df_m30 %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  group_by(RN_INDI) %>%
  summarise(
    # 총 항목 수
    M30_ITEM_CNT = n(),
    
    # 총 금액
    M30_AMT_SUM = sum(AMT, na.rm = TRUE),
    
    .groups = "drop"
  )

# 진료비 구분별 금액 (MCEXP_TYPE_CD: 1=기본진료료, 2=진료행위료, 3=약품비, 4=치료재료료, 5=정액수가)
feat_m30_by_type <- df_m30 %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  group_by(RN_INDI, MCEXP_TYPE_CD) %>%
  summarise(AMT_SUM = sum(AMT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = MCEXP_TYPE_CD,
    values_from = AMT_SUM,
    names_prefix = "M30_TYPE_",
    values_fill = 0
  )

feat_m30 <- feat_m30 %>%
  left_join(feat_m30_by_type, by = "RN_INDI")

cat(sprintf("M30 피처: %d 명\n", nrow(feat_m30)))

# ============================================================================
# 5. M40 피처 생성 (상병내역)
# ============================================================================

cat("\n=== M40 피처 생성 ===\n")

feat_m40 <- df_m40 %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  mutate(
    SICK_3 = substr(MCEX_SICK_SYM, 1, 3),
    
    # 비만 관련 상병
    IS_OBESITY = if_else(SICK_3 == "E66", 1L, 0L),
    
    # 대사증후군 관련 상병
    IS_HTN = if_else(SICK_3 %in% c("I10", "I11", "I12", "I13", "I14", "I15"), 1L, 0L),  # 고혈압
    IS_DM = if_else(SICK_3 %in% c("E10", "E11", "E12", "E13", "E14"), 1L, 0L),          # 당뇨
    IS_DLD = if_else(SICK_3 == "E78", 1L, 0L)                                            # 이상지질혈증
  ) %>%
  group_by(RN_INDI) %>%
  summarise(
    # 총 상병 건수
    M40_SICK_CNT = n(),
    M40_SICK_UNIQUE_CNT = n_distinct(SICK_3),
    
    # 질환 플래그
    M40_HAS_OBESITY = max(IS_OBESITY, na.rm = TRUE),
    M40_HAS_HTN = max(IS_HTN, na.rm = TRUE),
    M40_HAS_DM = max(IS_DM, na.rm = TRUE),
    M40_HAS_DLD = max(IS_DLD, na.rm = TRUE),
    
    .groups = "drop"
  )

cat(sprintf("M40 피처: %d 명\n", nrow(feat_m40)))

# ============================================================================
# 6. M60 피처 생성 (처방전)
# ============================================================================

cat("\n=== M60 피처 생성 ===\n")

feat_m60 <- df_m60 %>%
  inner_join(target_rn_indi, by = "RN_INDI") %>%
  group_by(RN_INDI) %>%
  summarise(
    # 총 처방 건수
    M60_PRSC_CNT = n(),
    
    # 총 처방일수
    M60_PRSC_DD_SUM = sum(TOT_MCNT, na.rm = TRUE),
    
    # 총 약제비
    M60_AMT_SUM = sum(AMT, na.rm = TRUE),
    
    .groups = "drop"
  )

cat(sprintf("M60 피처: %d 명\n", nrow(feat_m60)))

# ============================================================================
# 7. 피처 통합
# ============================================================================

cat("\n=== 피처 통합 ===\n")

# 모든 피처 조인
df_features <- target_rn_indi %>%
  left_join(feat_bnc, by = "RN_INDI") %>%
  left_join(feat_m20, by = "RN_INDI") %>%
  left_join(feat_m30, by = "RN_INDI") %>%
  left_join(feat_m40, by = "RN_INDI") %>%
  left_join(feat_m60, by = "RN_INDI")

# 결측값을 0으로 대체 (숫자형 컬럼만)
numeric_cols <- df_features %>%
  select(where(is.numeric), -RN_INDI) %>%
  names()

df_features <- df_features %>%
  mutate(across(all_of(numeric_cols), ~replace_na(., 0)))

cat(sprintf("총 피처 수: %d 개\n", ncol(df_features) - 1))
cat(sprintf("총 대상자 수: %d 명\n", nrow(df_features)))

# ============================================================================
# 8. 결과 저장
# ============================================================================

cat("\n=== 결과 저장 ===\n")

if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}

output_file <- paste0(OUTPUT_PATH, "feature_data.csv")
write_csv(df_features, output_file)

cat(sprintf("피처 데이터 저장 완료: %s\n", output_file))

# 피처 요약 출력
cat("\n=== 피처 요약 ===\n")
cat("\n[BNC 피처]\n")
cat("- BNC_GAIBJA_GROUP: 가입자 유형 그룹\n")
cat("- BNC_CTRB_Q10: 보험료 분위\n")
cat("- BNC_DSB_YN: 장애 여부\n")
cat("- BNC_SIDO: 시도 코드\n")

cat("\n[M20 피처]\n")
cat("- M20_VISIT_CNT: 총 진료 건수\n")
cat("- M20_INPATIENT_CNT: 입원 건수\n")
cat("- M20_OUTPATIENT_CNT: 외래 건수\n")
cat("- M20_MDCARE_DD_SUM: 총 요양일수\n")
cat("- M20_VSHSP_DD_SUM: 총 입내원일수\n")
cat("- M20_TOT_AMT_SUM: 총 진료비\n")
cat("- M20_HAS_SURGERY: 수술 여부\n")

cat("\n[M30 피처]\n")
cat("- M30_ITEM_CNT: 진료항목 건수\n")
cat("- M30_AMT_SUM: 총 금액\n")
cat("- M30_TYPE_*: 진료비 구분별 금액\n")

cat("\n[M40 피처]\n")
cat("- M40_SICK_CNT: 총 상병 건수\n")
cat("- M40_HAS_OBESITY: 비만 상병 여부 (E66)\n")
cat("- M40_HAS_HTN: 고혈압 상병 여부\n")
cat("- M40_HAS_DM: 당뇨 상병 여부\n")
cat("- M40_HAS_DLD: 이상지질혈증 상병 여부\n")

cat("\n[M60 피처]\n")
cat("- M60_PRSC_CNT: 총 처방 건수\n")
cat("- M60_PRSC_DD_SUM: 총 처방일수\n")
cat("- M60_AMT_SUM: 총 약제비\n")

cat("\n=== 피처 데이터셋 생성 완료 ===\n")

