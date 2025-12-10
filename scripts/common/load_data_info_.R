# <U+CEEC><U+B7FC> <U+C815><U+BCF4>!!
colnames_200 <- c(
  "MID", "JID", "INSUP_TP_CD", "SEX_TP_CD", "PAT_AGE", "YID",
  "CL_CD", "RVD_PLC_CD", "FOM_TP_CD", "MAIN_SICK", "SUB_SICK", "DGSBJT_CD",
  "RECU_FR_DD", "RECU_TO_DD", "FST_DD", "VST_DDCNT", "RECU_DDCNT", "OPRSC_DDCNT",
  "RVD_RPE_TAMT_AMT", "RVD_SLF_BRDN_AMT", "RVD_INSUP_BRDN_AMT", "SOPR_YN", "OINJ_TP_CD",
  "PRCL_SYM_TP_CD", "INJ_EXA_TP_CD", "DGRSLT_TP_CD", "IPAT_ARIV_PTH_TP", "MAIDCL_CD", "DMD_TP_CD",
  "DGTGTP_TP_CD"
)

colnames_300 <- c(
  "MID", "JID", "CZ_ITEM_CD", "DIV_TY_CD", "DIV_CD", "FQ1_MDCT_QTY",
  "DY1_MDCT_QTY", "DY1_INJC_QTY_EXEC_FQ", "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ",
  "UNPRC", "AMT", "ADDC_ADT_AMT", "MEFT_DIV_NO", "EXP_TP_CD"
)

colnames_400 <- c(
  "MID", "JID", "SICK_SNO", "SICK_CD", "SICK_TY_CD", "IFLD_DTL_SPC_SBJT_CD"
)

colnames_530 <- c(
  "MID", "JID", "DIV_TY_CD", "DIV_CD", "FQ1_MDCT_QTY", "DY1_MDCT_QTY",
  "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ", "UNPRC", "AMT", "MEFT_DIV_NO"
)

# <U+BB50><U+C5D0> <U+C0AC><U+C6A9><U+B418><U+B294> <U+D568><U+C218><U+C778><U+AC00>?
read_datafile <- function(dtype, yyyymm) {
  # dtype is the one of 200, 300, 400, 530
  df <- read_csv(sprintf("%s/%s", data_path, sprintf(filename_format, dtype, yyyymm)), col_names = FALSE) %>%
    select(-ncol(.))
  colnames(df) <- get(paste0("colnames_", dtype))

  return(df)
}

# <U+BCC0><U+C218><U+BA85><U+C73C><U+B85C> <U+D30C><U+C77C><U+BA85><U+C73C><U+B85C> <U+D558><U+C5EC> <U+CD94><U+CD9C><U+D574><U+C8FC><U+B294> <U+D568><U+C218>
variable_to_file_with_filter <- function(variable, output_path) {
  variable_name <- deparse(substitute(variable))
  variable2 <- variable %>% filter(mid_count > 2)
  file_name <- sprintf("%s%s.txt", output_path, variable_name)
  # CSV <U+D30C><U+C77C> <U+C791><U+C131>
  write_csv(variable2, file_name)
}

variable_to_file <- function(variable, output_path) {
  variable_name <- deparse(substitute(variable))
  file_name <- sprintf("%s%s.txt", output_path, variable_name)
  # CSV <U+D30C><U+C77C> <U+C791><U+C131>
  write_csv(variable, file_name)
}

# <U+D30C><U+C77C><U+BA85>
medi_file_name <- "freq_medicine_summary_%s.txt"
whole_medi_file_name <- "whole_freq_medi_summary.txt"
disease_file_name <- "freq_disease_summary_%s.txt"
whole_disease_file_name <- "whole_freq_disease_summary.txt"

whole_period_data <- "hira_whole_data_%s.txt" # <U+D328><U+D608><U+C99D>, <U+B1CC><U+C878><U+C911>
