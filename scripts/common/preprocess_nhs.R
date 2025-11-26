# 국민건강보험공단 표본코호트 데이터 전처리 함수
# load_data_info_nhs.R에서 이미 col_types로 타입을 지정하므로 별도 타입 변환 불필요

library(dplyr)

# 데이터 추출 핵심조건
# 1) 서식코드(FORM_CD)로 의과 입원/외래 구분
#    - 01: 의과입원, 02: 의과외래
#    - 11: 치과입원, 12: 치과외래  
#    - 21: 한방입원, 22: 한방외래
# 2) 주상병/부상병으로 질환자 필터링

# M20 데이터에서 특정 상병코드로 필터링된 개인고유번호 추출
get_filtered_rn_indi <- function(df_m20, sick_cd, form_cd = c("01", "02")) {
  # df_m20: M20 진료내역 데이터
  # sick_cd: 상병코드 벡터 (예: c("I21", "I22"))
  # form_cd: 서식코드 (기본값: 의과 입원/외래)
  
  filtered_rn_indi <- df_m20 %>%
    filter(SICK_SYM1 %in% sick_cd | SICK_SYM2 %in% sick_cd) %>%
    filter(FORM_CD %in% form_cd) %>%
    select(RN_INDI) %>%
    distinct()
  
  return(filtered_rn_indi)
}

# M40 상병내역에서 특정 상병코드로 필터링된 개인고유번호 추출
get_filtered_rn_indi_from_m40 <- function(df_m40, sick_cd, form_cd = c("01", "02")) {
  # df_m40: M40 상병내역 데이터
  # sick_cd: 상병코드 벡터
  # form_cd: 서식코드 (기본값: 의과 입원/외래)
  
  filtered_rn_indi <- df_m40 %>%
    filter(MCEX_SICK_SYM %in% sick_cd) %>%
    filter(FORM_CD %in% form_cd) %>%
    select(RN_INDI) %>%
    distinct()
  
  return(filtered_rn_indi)
}

# M20: 진료내역 전처리
preprocess_m20 <- function(df_m20, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_m20 <- df_m20 %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_m20 <- df_m20 %>%
    mutate(
      # 주상병/부상병 3자리 코드
      SICK_SYM1_3 = substr(SICK_SYM1, 1, 3),
      SICK_SYM2_3 = substr(SICK_SYM2, 1, 3),
      
      # 입원/외래 구분
      INOUT = case_when(
        FORM_CD %in% c("01", "11", "21") ~ "1",  # 입원 (의과, 치과, 한방)
        FORM_CD %in% c("02", "12", "22") ~ "2",  # 외래 (의과, 치과, 한방)
        TRUE ~ "9"  # 기타
      ),
      
      # 진료과 구분
      CARE_TYPE = case_when(
        FORM_CD %in% c("01", "02") ~ "1",  # 의과
        FORM_CD %in% c("11", "12") ~ "2",  # 치과
        FORM_CD %in% c("21", "22") ~ "3",  # 한방
        TRUE ~ "9"  # 기타
      ),
      
      # 요양개시일자를 날짜 형식으로 변환
      MDCARE_STRT_DT_DATE = as.Date(MDCARE_STRT_DT, format = "%Y%m%d")
    )
  
  return(df_m20)
}

# M30: 진료내역 세부 전처리
preprocess_m30 <- function(df_m30, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_m30 <- df_m30 %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_m30 <- df_m30 %>%
    mutate(
      # 약효분류 대분류 (앞 2자리)
      EFMDC_CLSF_2 = substr(EFMDC_CLSF_NO, 1, 2),
      
      # 요양개시일자를 날짜 형식으로 변환
      MDCARE_STRT_DT_DATE = as.Date(MDCARE_STRT_DT, format = "%Y%m%d")
    )
  
  return(df_m30)
}

# M40: 상병내역 전처리
preprocess_m40 <- function(df_m40, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_m40 <- df_m40 %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_m40 <- df_m40 %>%
    mutate(
      # 상병코드 3자리
      MCEX_SICK_SYM_3 = substr(MCEX_SICK_SYM, 1, 3),
      
      # 요양개시일자를 날짜 형식으로 변환
      MDCARE_STRT_DT_DATE = as.Date(MDCARE_STRT_DT, format = "%Y%m%d")
    )
  
  return(df_m40)
}

# M60: 처방전내역 전처리
preprocess_m60 <- function(df_m60, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_m60 <- df_m60 %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_m60 <- df_m60 %>%
    mutate(
      # 약효분류 대분류 (앞 2자리)
      EFMDC_CLSF_2 = substr(EFMDC_CLSF_NO, 1, 2),
      
      # 요양개시일자를 날짜 형식으로 변환
      MCARE_STRT_DT_DATE = as.Date(MCARE_STRT_DT, format = "%Y%m%d")
    )
  
  return(df_m60)
}

# BNC: 자격 및 보험료 전처리
preprocess_bnc <- function(df_bnc, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_bnc <- df_bnc %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_bnc <- df_bnc %>%
    mutate(
      # 가입자 유형 그룹
      GAIBJA_GROUP = case_when(
        GAIBJA_TYPE %in% c("1", "2") ~ "1",  # 지역가입자
        GAIBJA_TYPE %in% c("5", "6") ~ "2",  # 직장가입자
        GAIBJA_TYPE %in% c("7", "8") ~ "3",  # 의료급여수급권자
        TRUE ~ "9"  # 기타
      ),
      
      # 시도 코드
      SIDO = substr(SGG, 1, 2),
      
      # 장애 여부
      DSB_YN = if_else(!is.na(DSB_SVRT_CD) & DSB_SVRT_CD != "", "Y", "N")
    )
  
  return(df_bnc)
}

# G1E: 건강검진 전처리
preprocess_g1e <- function(df_g1e, filtered_rn_indi = NULL) {
  # filtered_rn_indi: 필터링된 개인고유번호 데이터프레임 (NULL이면 전체 사용)
  
  # 필터링된 개인만 선택
  if (!is.null(filtered_rn_indi)) {
    df_g1e <- df_g1e %>%
      inner_join(filtered_rn_indi, by = "RN_INDI")
  }
  
  # 추가 컬럼 생성
  df_g1e <- df_g1e %>%
    mutate(
      # BMI 카테고리 (WHO 아시아-태평양 기준)
      BMI_CATEGORY = case_when(
        is.na(G1E_BMI) ~ NA_character_,
        G1E_BMI < 18.5 ~ "저체중",
        G1E_BMI < 23 ~ "정상",
        G1E_BMI < 25 ~ "과체중",
        G1E_BMI < 30 ~ "비만",
        TRUE ~ "고도비만"
      ),
      
      # 고혈압 여부 (수축기 ≥140 또는 이완기 ≥90)
      HTN_YN = if_else(G1E_BP_SYS >= 140 | G1E_BP_DIA >= 90, "Y", "N", missing = NA_character_),
      
      # 당뇨 여부 (공복혈당 ≥126)
      DM_YN = if_else(G1E_FBS >= 126, "Y", "N", missing = NA_character_),
      
      # 이상지질혈증 여부 (총콜레스테롤 ≥240 또는 LDL ≥160 또는 HDL <40)
      DLD_YN = if_else(
        G1E_TOT_CHOL >= 240 | G1E_LDL >= 160 | G1E_HDL < 40, 
        "Y", "N", 
        missing = NA_character_
      ),
      
      # 흡연 상태 단순화
      SMOKE_STATUS = case_when(
        Q_SMK_YN == 1 ~ "비흡연",
        Q_SMK_YN == 2 ~ "과거흡연",
        Q_SMK_YN == 3 ~ "현재흡연",
        TRUE ~ NA_character_
      )
    )
  
  return(df_g1e)
}

# 특정 상병코드 3자리 패턴으로 필터링
filter_by_sick_pattern <- function(df_m20, sick_pattern) {
  # sick_pattern: 상병코드 3자리 패턴 (예: c("I21", "I22", "I23"))
  
  df_filtered <- df_m20 %>%
    filter(
      substr(SICK_SYM1, 1, 3) %in% sick_pattern | 
      substr(SICK_SYM2, 1, 3) %in% sick_pattern
    )
  
  return(df_filtered)
}

# 연도별 데이터 요약 함수
summarize_by_year <- function(df, year_col = "STD_YYYY") {
  # df: 데이터프레임
  # year_col: 연도 컬럼명
  
  summary <- df %>%
    group_by(across(all_of(year_col))) %>%
    summarise(
      n_records = n(),
      n_patients = n_distinct(RN_INDI),
      .groups = "drop"
    )
  
  return(summary)
}

