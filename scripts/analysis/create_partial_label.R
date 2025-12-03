# 정답셋 샘플링 스크립트
# 전체 정답셋에서 일부 대상자만 무작위 추출하여 테스트용 정답셋 생성

library(dplyr)
library(readr)

# ============================================================================
# 설정
# ============================================================================

OUTPUT_PATH <- "data/output/"
INPUT_FILE <- "label_obesity_severity.csv"
OUTPUT_FILE <- "label_obesity_severity_part.csv"

# 샘플링 비율 (0.1 = 10%)
SAMPLE_RATE <- 0.1
# 또는 고정된 수 (예: 1000명)
# SAMPLE_N <- 1000

SEED <- 12345

# ============================================================================
# 실행
# ============================================================================

# 1. 정답셋 로드
input_path <- paste0(OUTPUT_PATH, INPUT_FILE)
if (!file.exists(input_path)) {
    stop(sprintf("파일을 찾을 수 없습니다: %s", input_path))
}

cat(sprintf("정답셋 로드 중: %s\n", input_path))
df_label <- read_csv(input_path, show_col_types = FALSE)
cat(sprintf("전체 데이터: %d 행\n", nrow(df_label)))

# 2. 고유 개인(RN_INDI) 추출
unique_indi <- unique(df_label$RN_INDI)
n_total <- length(unique_indi)
cat(sprintf("고유 개인 수: %d 명\n", n_total))

# 3. 샘플링
set.seed(SEED)

# 비율로 샘플링
n_sample <- round(n_total * SAMPLE_RATE)
sampled_indi <- sample(unique_indi, n_sample)

# 수로 샘플링하려면 아래 주석 해제
# n_sample <- min(n_total, SAMPLE_N)
# sampled_indi <- sample(unique_indi, n_sample)

cat(sprintf("샘플링된 개인 수: %d 명 (%.1f%%)\n", length(sampled_indi), 100 * length(sampled_indi) / n_total))

# 4. 샘플링된 개인의 데이터만 필터링
df_part <- df_label %>%
    filter(RN_INDI %in% sampled_indi)

cat(sprintf("추출된 데이터: %d 행\n", nrow(df_part)))

# 5. 저장
output_path <- paste0(OUTPUT_PATH, OUTPUT_FILE)
write_csv(df_part, output_path)

cat(sprintf("샘플 데이터 저장 완료: %s\n", output_path))
