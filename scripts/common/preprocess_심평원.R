# 데이터 추출 핵심조건
# 1) INSUP_TP_CD = 4 - 의료보험 만 추출 (그외 제외)
# 2) FOM_TP_CD = '021', '031' - 의과만 추출 (의과입원, 의과외래)

library(dplyr)

# 전처리는

get_filtered_mid <- function(df_200, sick_cd) {
  filtered_mid <- df_200 %>%
    filter(MAIN_SICK %in% sick_cd) %>%  
    filter(INSUP_TP_CD %in% c(4, "4")) %>%
    filter(FOM_TP_CD %in% c("021", "031", "02", "03")) %>%
    select(MID)
}

preprocess_raw_200 <- function(df_200) {
  df_200 <- df_200 %>%
    mutate(
      MAIN_SICK3 = substr(MAIN_SICK, 1, 3),
      SUB_SICK3 = substr(SUB_SICK, 1, 3)
    ) %>%
    mutate(INOUT = case_when(
      FOM_TP_CD %in% c(
        "021", "041", "061", "071", "072", "073", "101", "121",
        "02", "04", "06", "07", "10", "12"
      ) ~ "1",
      FOM_TP_CD %in% c(
        "031", "051", "081", "082", "083", "091", "111", "131", "151",
        "201", "211", "991",
        "03", "05", "08", "09", "11", "13", "15", "99"
      ) ~ "2",
      TRUE ~ "0" # 혹시라도 다른 코드가 있을 경우 NA 부여
    )) %>%
    mutate(
      VST_DDCNT = as.numeric(VST_DDCNT),
      RECU_DDCNT = as.numeric(RECU_DDCNT),
      OPRSC_DDCNT = as.numeric(OPRSC_DDCNT),
      RVD_RPE_TAMT_AMT = as.numeric(RVD_RPE_TAMT_AMT),
      RVD_SLF_BRDN_AMT = as.numeric(RVD_SLF_BRDN_AMT),
      RVD_INSUP_BRDN_AMT = as.numeric(RVD_INSUP_BRDN_AMT)
    )
  return(df_200)
}

preprocess_200 <- function(df_200, filtered_mid) {
  df_200 <- df_200 %>%
    inner_join(filtered_mid, by = "MID")
  
  df_200 <- preprocess_raw_200(df_200)
    
  return(df_200)
}

preprocess_300 <- function(df_300, filtered_mid) {
  df_300 <- df_300 %>%
    inner_join(filtered_mid, by = "MID") %>%
    mutate(
      FQ1_MDCT_QTY = as.numeric(FQ1_MDCT_QTY),
      DY1_MDCT_QTY = as.numeric(DY1_MDCT_QTY),
      DY1_INJC_QTY_EXEC_FQ = as.numeric(DY1_INJC_QTY_EXEC_FQ),
      TOT_INJC_DDCNT_EXEC_FQ = as.numeric(TOT_INJC_DDCNT_EXEC_FQ),
      TOT_USE_QTY_OR_EXEC_FQ = as.numeric(TOT_USE_QTY_OR_EXEC_FQ),
      UNPRC = as.numeric(UNPRC),
      AMT = as.numeric(AMT),
      ADDC_ADT_AMT = as.numeric(ADDC_ADT_AMT)
    )
  return(df_300)
}

preprocess_400 <- function(df_400, filtered_mid) {
  df_400 <- df_400 %>%
    inner_join(filtered_mid, by = "MID") %>%
    mutate(SICK_CD3 = substr(SICK_CD, 1, 3)) 
  return(df_400)
}

preprocess_530 <- function(df_530) {
  df_530 <- df_530 %>%
    inner_join(filtered_mid, by = "MID") %>%
    mutate(
      FQ1_MDCT_QTY = as.numeric(FQ1_MDCT_QTY),
      DY1_MDCT_QTY = as.numeric(DY1_MDCT_QTY),
      TOT_INJC_DDCNT_EXEC_FQ = as.numeric(TOT_INJC_DDCNT_EXEC_FQ),
      TOT_USE_QTY_OR_EXEC_FQ = as.numeric(TOT_USE_QTY_OR_EXEC_FQ),
      UNPRC = as.numeric(UNPRC),
      AMT = as.numeric(AMT)
    )
  return(df_530)
} 


longterm_columns <- list(
  col200 = c(
    "MID", "JID", 
    "SEX_TP_CD", "PAT_AGE", "CL_CD", "RVD_PLC_CD", "FOM_TP_CD",
    "MAIN_SICK", "SUB_SICK", 
    "DGSBJT_CD", "RECU_FR_DD",
    "RECU_TO_DD", "FST_DD", "VST_DDCNT", "RECU_DDCNT",
    "OPRSC_DDCNT", "RVD_RPE_TAMT_AMT", "RVD_SLF_BRDN_AMT", "RVD_INSUP_BRDN_AMT",
    "SOPR_YN", "OINJ_TP_CD", "PRCL_SYM_TP_CD", "INJ_EXA_TP_CD",
    "DGRSLT_TP_CD", "IPAT_ARIV_PTH_TP", "MAIDCL_CD", "DMD_TP_CD", "DGTGTP_TP_CD"
  ), 
  col300 = c(
    "MID", "JID", "CZ_ITEM_CD", "DIV_TY_CD",
    "DIV_CD", "FQ1_MDCT_QTY", "DY1_MDCT_QTY",
    "DY1_INJC_QTY_EXEC_FQ", "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ",
    "UNPRC", "AMT", "ADDC_ADT_AMT", "MEFT_DIV_NO", "EXP_TP_CD"
  ), 
  col400 = c(
    "MID",
    "JID",
    "SICK_SNO",
    "SICK_CD",
    "SICK_TY_CD",
    "IFLD_DTL_SPC_SBJT_CD"
  ), 
  col530 = c(
    "MID",
    "JID",
    "DIV_TY_CD",
    "DIV_CD",
    "FQ1_MDCT_QTY",
    "DY1_MDCT_QTY",
    "TOT_INJC_DDCNT_EXEC_FQ",
    "TOT_USE_QTY_OR_EXEC_FQ",
    "UNPRC",
    "AMT",
    "MEFT_DIV_NO"
  )
)
  
    
