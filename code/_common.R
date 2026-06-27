# code/_common.R
# 프로젝트 공유 설정: 라이브러리 · 헬퍼 함수 · 데이터 로더 · 색상 팔레트
# eda.R 와 모든 .qmd 문서가 이 파일을 source() 해서 중복 없이 재사용한다.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# ---- 통계 헬퍼 (외부 패키지 없이 직접 계산) --------------------------------
# 표본 왜도(조정): 분포의 비대칭. +는 오른쪽 꼬리.
skewness <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  m <- mean(x); s <- sd(x)
  if (s == 0 || n < 3) return(NA_real_)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}
# 초과 첨도: 0은 정규분포 수준, 음수는 평평/이봉 경향.
kurtosis_excess <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  m <- mean(x); s <- sd(x)
  if (s == 0 || n < 4) return(NA_real_)
  sum(((x - m) / s)^4) / n - 3
}
# 지니계수: 분포의 집중도를 0(완전균등)~1(완전불균등)로 요약.
gini <- function(x) {
  x <- sort(x[is.finite(x)]); n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

# ---- 데이터 로더 -----------------------------------------------------------
# 프로젝트 루트 / code / document 어디서 실행해도 data 를 찾도록 후보를 둔다.
find_data <- function() {
  cands <- c(
    file.path("data", "gapminder_clean.csv"),
    file.path("..", "data", "gapminder_clean.csv"),
    file.path("data", "gapminder.csv"),
    file.path("..", "data", "gapminder.csv")
  )
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) stop("gapminder 데이터(data/gapminder_clean.csv)를 찾을 수 없습니다.")
  hit[1]
}

load_gapminder <- function() {
  df <- read.csv(find_data(), stringsAsFactors = FALSE)
  df$continent <- factor(df$continent)
  df
}

# ---- 공유 색상 팔레트 -------------------------------------------------------
cont_pal <- c(Africa = "#e41a1c", Americas = "#377eb8", Asia = "#4daf4a",
              Europe = "#984ea3", Oceania = "#ff7f00")  # 대륙별
wu_col   <- c("비가중" = "#e41a1c", "인구가중" = "#1fb3c9")  # 가중 vs 비가중