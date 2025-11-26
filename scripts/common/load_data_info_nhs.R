# 국민건강보험공단 표본코호트 데이터 컬럼 정보

# BNC: 자격 및 보험료 테이블 (10개 필드)
colnames_BNC <- c(
  "STD_YYYY",           # 기준년도
  "RN_INDI",            # 개인고유번호
  "SEX",                # 성
  "SGG",                # 주소(시군구)
  "GAIBJA_TYPE",        # 건강보장유형(가입자 구분)
  "CTRB_Q10",           # 보험료분위 10분위
  "DSB_SVRT_CD",        # 장애중증도
  "DSB_TYPE_CD",        # 장애유형
  "G1E_OBJ_YN",         # 건강검진(일반1차) 대상자 여부
  "SMPL_TYPE_CD"        # 표본구분
)

# BNC 컬럼 타입 (c: 문자, i: 정수, d: 실수)
coltypes_BNC <- "cicccicccc"  # STD_YYYY(c), RN_INDI(i), SEX(c), SGG(c), GAIBJA_TYPE(c), CTRB_Q10(i), DSB_SVRT_CD(c), DSB_TYPE_CD(c), G1E_OBJ_YN(c), SMPL_TYPE_CD(c)

# M20: 진료내역 명세서 테이블 (21개 필드)
colnames_M20 <- c(
  "RN_INDI",            # 개인고유번호
  "RN_KEY",             # 청구고유번호
  "RN_INST",            # 요양기관고유번호
  "MDCARE_STRT_DT",     # 요양개시일자
  "FORM_CD",            # 서식코드
  "MCARE_SUBJ_CD",      # 진료과목코드
  "SICK_SYM1",          # 주상병
  "SICK_SYM2",          # 부상병
  "HSPTZ_PATH_TYPE",    # 입원경로구분
  "OFIJ_TYPE",          # 공상 등 구분
  "OPRTN_YN",           # 수술여부
  "MDCARE_DD_CNT",      # 요양일수
  "VSHSP_DD_CNT",       # 입내원일수
  "TOT_PRSC_DD_CNT",    # 총처방일수
  "MCARE_RSLT_TYPE",    # 진료결과구분
  "FST_HSPTZ_DT",       # 최초입원일자
  "EDC_ADD_RT",         # 심결가산율
  "SPCF_SYM_TYPE",      # 특정기호구분
  "ED_RC_TOT_AMT",      # 심결요양급여비용총액
  "EDC_SBA",            # 심결본인부담금
  "EDC_INSUR_BRDN_AMT"  # 심결보험자부담금
)

# M20 컬럼 타입
coltypes_M20 <- "iiiccccccciiiccdcddd"

# M30: 진료내역 세부 테이블 (17개 필드)
colnames_M30 <- c(
  "RN_INDI",            # 개인고유번호
  "RN_KEY",             # 청구고유번호
  "MDCARE_STRT_DT",     # 요양개시일자
  "FORM_CD",            # 서식코드
  "MCARE_DESC_LN_NO",   # 줄번호
  "CLA_CD",             # 항코드
  "ITEM_CD",            # 목코드
  "CLSF_PTTN_CD",       # 분류유형코드
  "MCARE_DIV_CD",       # 분류코드
  "UPRC",               # 단가
  "DD1_MQTY_FREQ",      # 1일투여량또는실시횟수
  "TOT_MCNT",           # 총투여일수 또는 실시횟수
  "AMT",                # 금액
  "TIME1_MDCT_CPCT",    # 1회투여용량
  "MDCN_UD",            # 약제상한차액
  "MCEXP_TYPE_CD",      # 진료비 구분 코드
  "EFMDC_CLSF_NO"       # 약효분류번호
)

# M30 컬럼 타입
coltypes_M30 <- "iiciccccdddddddcc"

# M40: 상병내역 테이블 (7개 필드)
colnames_M40 <- c(
  "RN_INDI",            # 개인고유번호
  "RN_KEY",             # 청구고유번호
  "MDCARE_STRT_DT",     # 요양개시일자
  "FORM_CD",            # 서식코드
  "MCEX_SICK_SYM",      # 요양급여상병기호
  "DETAIL_TMSG_SUBJ_CD", # 세부전문과목코드
  "SICK_CLSF_TYPE"      # 상병분류구분코드
)

# M40 컬럼 타입
coltypes_M40 <- "iiccccc"

# M60: 처방전내역 테이블 (13개 필드)
colnames_M60 <- c(
  "RN_INDI",            # 개인고유번호
  "RN_KEY",             # 청구고유번호
  "MCARE_STRT_DT",      # 요양개시일자
  "FORM_CD",            # 서식코드
  "MCARE_DESC_LN_NO",   # 줄번호
  "CLSF_PTTN_CD",       # 분류유형코드
  "MPRSC_TIME1_TUYAK_CPCT", # 1회투여량횟수
  "MPRSC_DD1_TUYAK_CPCT",   # 1일투약량
  "TOT_MCNT",           # 총투여일수
  "UPRC",               # 단가
  "AMT",                # 금액
  "GNL_NM_CD",          # 일반명코드
  "EFMDC_CLSF_NO"       # 약효분류번호
)

# M60 컬럼 타입
coltypes_M60 <- "iiccicdddddcc"

# G1E: 건강검진 테이블 (2009~2015년) (57개 필드)
# 주의: JSON 스키마에는 59개로 되어있으나, 중복 필드 제거 후 57개
colnames_G1E <- c(
  "EXMD_BZ_YYYY",       # 검진년도 
  "RN_INDI",            # 개인고유번호
  "HME_YYYYMM",         # 건강검진년월
  "Q_PHX_DX_STK",       # (본인)뇌졸중과거병력유무
  "Q_PHX_DX_HTDZ",      # (본인)심장병과거병력유무
  "Q_PHX_DX_HTN",       # (본인)고혈압과거병력유무
  "Q_PHX_DX_DM",        # (본인)당뇨병과거병력유무
  "Q_PHX_DX_DLD",       # (본인)고지혈증(이상지질혈증)과거병력유무
  "Q_PHX_DX_PTB",       # (본인)폐결핵과거병력유무
  "Q_PHX_DX_ETC",       # (본인)기타(암포함)질환과거병력유무
  "Q_PHX_TX_STK",       # (본인)뇌졸중(중풍)약물치료여부
  "Q_PHX_TX_HTDZ",      # (본인)심장병(심근경색/협심증)약물치료여부
  "Q_PHX_TX_HTN",       # (본인)고혈압약물치료여부
  "Q_PHX_TX_DM",        # (본인)당뇨병약물치료여부
  "Q_PHX_TX_DLD",       # (본인)고지혈증약물치료여부
  "Q_PHX_TX_PTB",       # (본인)폐결핵약물치료여부
  "Q_PHX_TX_ETC",       # (본인)기타(암포함)약물치료여부
  "Q_FHX_STK",          # (가족력)뇌졸중(중풍)여부
  "Q_FHX_HTDZ",         # (가족력)심장병(심근경색/협심증)여부
  "Q_FHX_HTN",          # (가족력)고혈압여부
  "Q_FHX_DM",           # (가족력)당뇨병여부
  "Q_FHX_ETC",          # (가족력)기타(암포함)여부
  "Q_HBV_AG",           # B형간염항원보유자
  "Q_SMK_YN",           # 흡연상태
  "Q_SMK_PAST_DRT",     # (과거)흡연기간
  "Q_SMK_PAST_AMT_V09N", # (과거)하루흡연량
  "Q_SMK_NOW_DRT",      # (현재)흡연기간
  "Q_SMK_NOW_AMT_V09N", # (현재)하루흡연량
  "Q_DRK_FRQ_V09N",     # 주간음주일수
  "Q_DRK_AMT_V09N",     # 1회 음주량
  "Q_PA_VD",            # 1주_20분이상 격렬한 운동
  "Q_PA_MD",            # 1주_30분이상 중간정도 운동
  "Q_PA_WALK",          # 1주_총30분이상 걷기 운동
  "G1E_HGHT",           # 신장
  "G1E_WGHT",           # 체중
  "G1E_WSTC",           # 허리둘레
  "G1E_BMI",            # 체질량지수
  "G1E_VA_LT",          # 시력(좌)
  "G1E_VA_RT",          # 시력(우)
  "G1E_HA_LT",          # 청력(좌)
  "G1E_HA_RT",          # 청력(우)
  "G1E_BP_SYS",         # 수축기혈압
  "G1E_BP_DIA",         # 이완기혈압
  "G1E_URN_PROT",       # 요단백
  "G1E_HGB",            # 혈색소
  "G1E_FBS",            # 식전혈당(공복혈당)
  "G1E_TOT_CHOL",       # 총콜레스테롤
  "G1E_TG",             # 트리글리세라이드
  "G1E_HDL",            # HDL콜레스테롤
  "G1E_LDL",            # LDL콜레스테롤
  "G1E_CRTN",           # 혈청크레아티닌
  "G1E_SGOT",           # (혈청지오티)AST
  "G1E_SGPT",           # (혈청지피티)ALT
  "G1E_GGT",            # 감마지티피
  "G1E_GFR",            # 신사구체여과율(GFR)
  "G1E_GFR_MTHD",       # 신사구체여과율(GFR)측정방법
  "G1E_CHST_XRAY_RST"   # 흉부방사선검사 결과
)

# G1E 컬럼 타입 (c: 문자, i: 정수, d: 실수)
# EXMD_BZ_YYYY(c), RN_INDI(i), HME_YYYYMM(c), Q_* 설문 29개(i), 신체계측/검사 24개(d)
coltypes_G1E <- "ciciiiiiiiiiiiiiiiiiiiiiiiiiiiidddddddddddddddddddddddd"
# coltypes_G1E <- "ciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiddddddddddddddddddddddddd"

# 데이터 파일 읽기 함수
read_nhs_datafile <- function(table_name, data_path, file_name = NULL) {
  # table_name: BNC, M20, M30, M40, M60, G1E
  # data_path: 데이터가 있는 경로
  # file_name: 파일명 (NULL이면 테이블명.csv 사용)
  
  if (is.null(file_name)) {
    file_name <- sprintf("%s.csv", table_name)
  }
  
  file_path <- file.path(data_path, file_name)
  
  if (!file.exists(file_path)) {
    warning(sprintf("파일을 찾을 수 없습니다: %s", file_path))
    return(NULL)
  }
  
  # 컬럼명 및 컬럼타입 가져오기
  col_names <- get(paste0("colnames_", table_name))
  col_types <- get(paste0("coltypes_", table_name))
  
  # 데이터 읽기
  df <- read_csv(
    file_path, 
    col_names = col_names, 
    col_types = col_types,
    na = c("", "NA", "NULL"),
    locale = locale(encoding = "UTF-8")
  )
  
  return(df)
}

# 여러 파일 통합 읽기 함수
read_nhs_datafile_multi <- function(table_name, data_path, file_names) {
  # table_name: BNC, M20, M30, M40, M60, G1E
  # data_path: 데이터가 있는 경로
  # file_names: 파일명 벡터 (예: c("BNC_2002.csv", "BNC_2003.csv"))
  
  data_list <- lapply(file_names, function(file_name) {
    read_nhs_datafile(table_name, data_path, file_name)
  })
  
  # NULL이 아닌 데이터만 필터링
  data_list <- data_list[!sapply(data_list, is.null)]
  
  if (length(data_list) == 0) {
    warning(sprintf("테이블 %s에 대한 데이터를 찾을 수 없습니다", table_name))
    return(NULL)
  }
  
  # 데이터 통합
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# 디렉토리 내 테이블 관련 모든 파일 읽기 함수
read_nhs_datafile_all <- function(table_name, data_path, pattern = NULL) {
  # table_name: BNC, M20, M30, M40, M60, G1E
  # data_path: 데이터가 있는 경로
  # pattern: 파일 검색 패턴 (예: "BNC_.*\\.csv")
  
  if (is.null(pattern)) {
    pattern <- sprintf("^%s.*\\.csv$", table_name)
  }
  
  # 디렉토리 내 파일 목록 가져오기
  all_files <- list.files(data_path, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(sprintf("테이블 %s에 대한 파일을 찾을 수 없습니다: %s", table_name, data_path))
    return(NULL)
  }
  
  message(sprintf("총 %d개의 파일을 읽습니다: %s", length(all_files), paste(all_files, collapse = ", ")))
  
  # 각 파일 읽기
  data_list <- lapply(all_files, function(file_name) {
    read_nhs_datafile(table_name, data_path, file_name)
  })
  
  # NULL이 아닌 데이터만 필터링
  data_list <- data_list[!sapply(data_list, is.null)]
  
  if (length(data_list) == 0) {
    warning(sprintf("유효한 데이터를 찾을 수 없습니다"))
    return(NULL)
  }
  
  # 데이터 통합
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# 변수명으로 파일명으로 하여 추출해주는 함수
variable_to_file_with_filter <- function(variable, output_path, filter_condition = NULL) {
  variable_name <- deparse(substitute(variable))
  
  # 필터 적용
  if (!is.null(filter_condition)) {
    variable2 <- variable %>% filter(!!enquo(filter_condition))
  } else {
    variable2 <- variable
  }
  
  file_name <- sprintf("%s%s.txt", output_path, variable_name)
  
  # CSV 파일 작성
  write_csv(variable2, file_name)
  
  message(sprintf("파일 저장 완료: %s", file_name))
}

variable_to_file <- function(variable, output_path) {
  variable_name <- deparse(substitute(variable))
  file_name <- sprintf("%s%s.txt", output_path, variable_name)
  
  # CSV 파일 작성
  write_csv(variable, file_name)
  
  message(sprintf("파일 저장 완료: %s", file_name))
}

