# <U+B274><U+B85C><U+D54F> <U+C0C1><U+BCD1><U+BD84><U+C11D> <U+B0B4><U+C6A9>

library("readr")
library("dplyr")
library("glue")

source("./scripts/config/config.R")
source("./scripts/common/load_data_info.R")
source("./scripts/common/preprocess.R")
source("./scripts/common/disease_analysis.R")

# <U+D2B9><U+C815> 1<U+B144> <U+B370><U+C774><U+D130><U+B9CC> <U+B300><U+C0C1><U+C73C><U+B85C> <U+C9C4><U+D589>
from_yyyymm <- "201801"
to_yyyymm <- "201803"
fromto_yyyymm <- format(
  seq(as.Date(paste0(from_yyyymm, "01"), format = "%Y%m%d"), as.Date(paste0(to_yyyymm, "01"), format = "%Y%m%d"), by = "month"),
  "%Y%m"
)
# config <U+C5D0> <U+C874><U+C7AC> 3
disease_list <- c("I60", "I61", "I63", "I64")


# <U+C8FC><U+C0C1><U+BCD1><U+C774> I60, I61, I63, I64 <U+C774><U+ACE0> <U+ACB0><U+ACFC><U+AC00> <U+C0AC><U+B9DD><U+C778> <U+ACBD><U+C6B0> (<U+B77C><U+ACE0> <U+D558><U+AE30><U+C5D4>)
## <U+BC1C><U+BCD1><U+B960><U+C740> <U+D574><U+B2F9><U+C5F0><U+B3C4> <U+BC1C><U+C0DD><U+AC74><U+C218> / <U+D574><U+B2F9> <U+C5F0><U+B3C4> <U+C8FC><U+BBFC><U+B4F1><U+B85D><U+C5F0><U+C559><U+C778><U+AD6C><U+C218> * 10<U+B9CC>


#------
# <U+C6D4><U+BCC4><U+B85C> <U+CD94><U+CD9C><U+D558><U+AE30> # <U+C644><U+B8CC>
for (yyyymm in fromto_yyyymm) {
  df_200 <- read_datafile("200", yyyymm)
  # <U+C815><U+D574><U+C9C4> <U+C0C1><U+BCD1><U+B9CC> <U+CD94><U+CD9C>
  filtered_mid <- get_filtered_mid(df_200, disease_list)
  df_200 <- preprocess_200(df_200, filtered_mid = filtered_mid)
  my_freq_disease_summary(df_200 = df_200, output_path = output_path, disease_file_name = sprintf(disease_file_name, yyyymm))
}



#--------
# 0 <U+AE30><U+BCF8><U+C815><U+C81C> <U+B370><U+C774><U+D130><U+B97C> <U+B300><U+C0C1><U+C73C><U+B85C> <U+D574><U+B2F9> <U+B0B4><U+C6A9><U+C744> <U+CD9C><U+B825><U+D558><U+B294> <U+AC83>
# 1 <U+C0AC><U+B9DD><U+C790>, <U+C9C8><U+BCD1> <U+B300><U+C0C1><U+C790> , MID_COUNT, AMT_SUM  <U+C804><U+CCB4> <U+B370><U+C774><U+D130><U+B098> <U+C5D0><U+C11C> <U+BD84><U+C11D><U+D558><U+B294> <U+B370><U+C774><U+D130><U+B97C> <U+B9CC><U+B4E6>
# 2.<U+C0DD><U+C131><U+B41C> <U+D30C><U+C77C><U+C744> <U+BD88><U+B7EC><U+B4E4><U+C5EC> <U+D569><U+CCD0><U+C11C> <U+D544><U+C694><U+D55C> <U+CEEC><U+B7FC><U+B9CC> <U+B0A8><U+ACA8><U+C11C> <U+C804><U+CCB4> <U+D30C><U+C77C><U+B85C> <U+B0A8><U+AE40>
use_columns <- c(
  "JID", "MID", "RVD_RPE_TAMT_AMT", "MAIN_SICK3", "SEX_TP_CD", "INOUT", "PAT_AGE",
  "CL_CD", "RVD_PLC_CD", "DGRSLT_TP_CD", "RECU_FR_DD", "MAIN_SICK"
)
monthly_data <- list()
for (yyyymm in fromto_yyyymm) {
  df_200 <- read_datafile("200", yyyymm)
  # <U+C815><U+D574><U+C9C4> <U+C0C1><U+BCD1><U+B9CC> <U+CD94><U+CD9C>
  filtered_mid <- get_filtered_mid(df_200, disease_list)
  df_200 <- preprocess_200(df_200, filtered_mid = filtered_mid)
  monthly_data[[yyyymm]] <- df_200 %>%
    select(use_columns) %>%
    filter(MAIN_SICK %in% disease_list)
}

df_tmp <- bind_rows(monthly_data)
df_disease_combined <- df_tmp %>% mutate(YYYYMM = substr(RECU_FR_DD, 1, 6))
# 1. <U+C804><U+CCB4> <U+B300><U+C0C1>
# 1 <U+B370><U+C774><U+D130> <U+C804><U+CCB4><U+C5D0><U+C11C> <U+C804><U+CCB4> <U+B300><U+C0C1><U+C790> <U+C22B><U+C790> <U+D30C><U+C545><U+D558><U+AE30>
unique_jid_mid_sex_whole <- df_disease_combined %>%
  group_by(SEX_TP_CD) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(unique_jid_mid_sex_whole, output_path)

unique_jid_mid_sex_inout_whole <- df_disease_combined %>%
  group_by(SEX_TP_CD, INOUT, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(unique_jid_mid_sex_inout_whole, output_path)

df_in_all <- df_disease_combined %>% filter(INOUT == "1")

#<U+BC1C><U+C0DD><U+AC74><U+C218>
unique_jid_mid_sex_whole_in <- df_in_all %>%
  group_by(SEX_TP_CD) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(unique_jid_mid_sex_whole_in, output_path)

# <U+C0AC><U+B9DD><U+B960> -> <U+CD5C><U+C885><U+ACB0><U+ACFC><U+AC00> <U+C0AC><U+B9DD><U+C778><U+B370> <U+C8FC> <U+C9C4><U+B2E8><U+BA85><U+C774> <U+D574><U+B2F9> <U+C0C1><U+BCD1><U+C778> <U+ACBD><U+C6B0>
df_death_in <- df_in_all %>%
  filter(DGRSLT_TP_CD == 4) %>%
  group_by(SEX_TP_CD, PAT_AGE, YYYYMM) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(df_death_in, output_path)


df_place_in <- df_in_all %>%
  group_by(SEX_TP_CD, RVD_PLC_CD) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )
variable_to_file_with_filter(df_place_in, output_path)

df_age_in <- df_in_all %>%
  group_by(SEX_TP_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(df_age_in, output_path)

df_disease_in <- df_in_all %>%
  group_by(SEX_TP_CD, MAIN_SICK3) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(df_disease_in, output_path)
# -----------------------------------------------------

unique_jid_mid_sex_sick3_whole <- df_disease_combined %>%
  group_by(MAIN_SICK3, SEX_TP_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )


unique_jid_mid_sex_sick3_inout_whole <- df_disease_combined %>%
  group_by(MAIN_SICK3, SEX_TP_CD, INOUT, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_whole <- df_disease_combined %>%
  group_by(MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_plc_whole <- df_disease_combined %>%
  group_by(MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, RVD_PLC_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_plc_dgrslt_whole <- df_disease_combined %>%
  group_by(MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, RVD_PLC_CD, DGRSLT_TP_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(unique_jid_mid_sex_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_inout_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_plc_whole, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_plc_dgrslt_whole, output_path)
names(df_disease_combined)
# monthly
unique_jid_mid_sex_montly <- df_disease_combined %>%
  group_by(SEX_TP_CD, YYYYMM, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_inout_montly <- df_disease_combined %>%
  group_by(YYYYMM, SEX_TP_CD, INOUT, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_monthly <- df_disease_combined %>%
  group_by(YYYYMM, MAIN_SICK3, SEX_TP_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_montly <- df_disease_combined %>%
  group_by(YYYYMM, MAIN_SICK3, SEX_TP_CD, INOUT, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_monthly <- df_disease_combined %>%
  group_by(YYYYMM, MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_plc_montly <- df_disease_combined %>%
  group_by(YYYYMM, MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, RVD_PLC_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

unique_jid_mid_sex_sick3_inout_cl_plc_dgrslt_montly <- df_disease_combined %>%
  group_by(YYYYMM, MAIN_SICK3, SEX_TP_CD, INOUT, CL_CD, RVD_PLC_CD, DGRSLT_TP_CD, PAT_AGE) %>%
  summarise(
    jid_count = n_distinct(JID),
    mid_count = n_distinct(MID),
    AMT_SUM = sum(RVD_RPE_TAMT_AMT, na.rm = TRUE)
  )

variable_to_file_with_filter(unique_jid_mid_sex_montly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_inout_montly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_monthly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_montly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_monthly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_plc_montly, output_path)
variable_to_file_with_filter(unique_jid_mid_sex_sick3_inout_cl_plc_dgrslt_montly, output_path)
