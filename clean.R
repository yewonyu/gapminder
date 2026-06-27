# clean.R
# gapminder.csv 데이터 품질 점검 스크립트
# 실행: Rscript clean.R   (또는 R 콘솔에서 source("clean.R"))

# ---- 0. 설정 ----------------------------------------------------------------
input_path  <- file.path("data", "gapminder.csv")
output_path <- file.path("data", "gapminder_clean.csv")

stopifnot(file.exists(input_path))

# 문자열을 factor로 자동 변환하지 않도록 (R 4.0 미만 호환)
df <- read.csv(input_path, stringsAsFactors = FALSE)

cat("==============================================\n")
cat(" gapminder.csv 데이터 품질 리포트\n")
cat("==============================================\n\n")

# ---- 1. 기본 구조 -----------------------------------------------------------
cat("[1] 기본 구조\n")
cat(sprintf("  - 행 수      : %d\n", nrow(df)))
cat(sprintf("  - 열 수      : %d\n", ncol(df)))
cat(sprintf("  - 열 이름    : %s\n", paste(names(df), collapse = ", ")))
cat("\n  - 열별 자료형:\n")
for (col in names(df)) {
  cat(sprintf("      %-12s : %s\n", col, class(df[[col]])))
}
cat("\n")

# ---- 2. 결측치(NA) ----------------------------------------------------------
cat("[2] 결측치(NA) 개수\n")
na_counts <- sapply(df, function(x) sum(is.na(x)))
for (col in names(na_counts)) {
  cat(sprintf("      %-12s : %d\n", col, na_counts[col]))
}
cat(sprintf("  - 전체 NA 합계: %d\n\n", sum(na_counts)))

# ---- 3. 중복 행 -------------------------------------------------------------
cat("[3] 중복 점검\n")
dup_all <- sum(duplicated(df))
cat(sprintf("  - 완전 중복 행: %d\n", dup_all))

# country + year 조합은 유일해야 함 (gapminder의 키)
if (all(c("country", "year") %in% names(df))) {
  dup_key <- sum(duplicated(df[, c("country", "year")]))
  cat(sprintf("  - (country, year) 중복: %d\n", dup_key))
}
cat("\n")

# ---- 4. 수치형 변수 요약 및 범위 이상치 -------------------------------------
cat("[4] 수치형 변수 요약 통계\n")
num_cols <- names(df)[sapply(df, is.numeric)]
for (col in num_cols) {
  s <- summary(df[[col]])
  cat(sprintf("  - %s:\n", col))
  cat(sprintf("      min=%.3f, median=%.3f, mean=%.3f, max=%.3f\n",
              s["Min."], s["Median"], s["Mean"], s["Max."]))
}
cat("\n")

# ---- 5. 도메인 규칙 기반 이상치 검사 ----------------------------------------
cat("[5] 도메인 규칙 위반 검사\n")
flag <- function(name, idx) {
  cat(sprintf("  - %-28s : %d건\n", name, length(idx)))
  if (length(idx) > 0 && length(idx) <= 10) {
    print(df[idx, , drop = FALSE])
  }
}

if ("pop" %in% names(df))      flag("인구(pop) <= 0",          which(df$pop <= 0))
if ("lifeExp" %in% names(df))  flag("기대수명 범위 밖(0~120)", which(df$lifeExp <= 0 | df$lifeExp > 120))
if ("gdpPercap" %in% names(df)) flag("1인당 GDP <= 0",         which(df$gdpPercap <= 0))
if ("year" %in% names(df))     flag("연도 범위 밖(1800~2030)", which(df$year < 1800 | df$year > 2030))
cat("\n")

# ---- 6. 범주형 변수 점검 ----------------------------------------------------
cat("[6] 범주형 변수\n")
if ("continent" %in% names(df)) {
  cat("  - 대륙(continent) 분포:\n")
  print(table(df$continent, useNA = "ifany"))
}
if ("country" %in% names(df)) {
  cat(sprintf("\n  - 고유 국가 수: %d\n", length(unique(df$country))))
  # 국가별 관측 연도 수가 일정한지 확인 (gapminder는 보통 12)
  per_country <- table(df$country)
  cat(sprintf("  - 국가별 관측 수: 최소=%d, 최대=%d\n",
              min(per_country), max(per_country)))
  irregular <- per_country[per_country != as.integer(names(sort(table(per_country), decreasing = TRUE))[1])]
  if (length(irregular) > 0) {
    cat("  - 관측 수가 다른 국가:\n")
    print(irregular)
  }
}

# 문자열 앞뒤 공백 점검
cat("\n  - 문자열 앞뒤 공백 점검:\n")
chr_cols <- names(df)[sapply(df, is.character)]
for (col in chr_cols) {
  n_ws <- sum(df[[col]] != trimws(df[[col]]), na.rm = TRUE)
  cat(sprintf("      %-12s : %d건\n", col, n_ws))
}
cat("\n")

# ---- 7. 정리(clean)된 데이터 저장 -------------------------------------------
cat("[7] 정리 작업\n")
clean <- df

# 문자열 공백 제거
for (col in chr_cols) clean[[col]] <- trimws(clean[[col]])

# 완전 중복 행 제거
before <- nrow(clean)
clean <- clean[!duplicated(clean), ]
cat(sprintf("  - 중복 행 제거: %d건\n", before - nrow(clean)))

# (country, year) 기준 정렬
if (all(c("country", "year") %in% names(clean))) {
  clean <- clean[order(clean$country, clean$year), ]
}

write.csv(clean, output_path, row.names = FALSE)
cat(sprintf("  - 저장 완료: %s (%d행)\n\n", output_path, nrow(clean)))

cat("==============================================\n")
cat(" 리포트 종료\n")
cat("==============================================\n")
