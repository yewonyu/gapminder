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

# ---- 공유 색상 팔레트 (Tableau 10 계열 — 전문적·색약 친화) ------------------
cont_pal <- c(Africa = "#E15759", Americas = "#4E79A7", Asia = "#59A14F",
              Europe = "#B07AA1", Oceania = "#F28E2B")    # 대륙별
wu_col   <- c("비가중" = "#FC7D0B", "인구가중" = "#1170AA")  # 가중 vs 비가중

# ---- 공유 ggplot 테마 -------------------------------------------------------
# 일관된 시각 언어: 굵은 제목 좌측 정렬, 옅은 격자, 하단 범례, 출처 캡션.
theme_gap <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      plot.subtitle       = ggplot2::element_text(color = "grey35", size = ggplot2::rel(0.92),
                                                  margin = ggplot2::margin(b = 8)),
      plot.caption        = ggplot2::element_text(color = "grey60", size = ggplot2::rel(0.72)),
      plot.title.position = "plot",
      axis.title          = ggplot2::element_text(color = "grey35"),
      panel.grid.minor    = ggplot2::element_blank(),
      panel.grid.major    = ggplot2::element_line(color = "grey92"),
      legend.position     = "bottom",
      legend.title        = ggplot2::element_text(color = "grey35")
    )
}
GAP_CAPTION <- "출처: Gapminder · 142개국 · 1952–2007"