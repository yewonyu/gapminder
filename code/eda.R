# code/eda.R
# gapminder 데이터 탐색적 분석(EDA) 스크립트 — 개정판
# 실행: Rscript code/eda.R   (프로젝트 루트에서)
# 산출물: 콘솔 요약 + figures/ 폴더의 그림 파일들
#
# 설계 원칙
#   (1) 무결성 먼저: 분포·관계 분석에 앞서 데이터 품질을 점검한다.
#   (2) 통계량은 근사가 아니라 실제 값으로: 왜도/첨도/사분위수를 직접 계산한다.
#   (3) 통합(pooled) 분석의 함정을 명시: 국가 간 차이와 시간 추세를 분리해 본다.
#   (4) '좋은 이야기'뿐 아니라 '나쁜 이야기'도: 개선폭만이 아니라 후퇴/역전도 본다.

# ---- 0. 공통 설정 로드 ------------------------------------------------------
# code/_common.R 에서 라이브러리·헬퍼(skewness/kurtosis_excess/gini)·데이터 로더를 가져온다.
# 스크립트 위치를 기준으로 작업 디렉터리를 프로젝트 루트로 맞춰 data/·figures/ 경로를 안정화한다.
.script_dir <- tryCatch({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
if (basename(.script_dir) == "code") setwd(dirname(.script_dir))  # code/ 의 상위 = 루트
source(file.path(.script_dir, "_common.R"))

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

input_path <- find_data()
df <- load_gapminder()

cat("==============================================\n")
cat(" gapminder 탐색적 분석(EDA) 리포트 (개정판)\n")
cat("==============================================\n")
cat(sprintf(" 입력 파일: %s (%d행, %d열)\n", input_path, nrow(df), ncol(df)))
cat(sprintf(" 실행 시각: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

save_plot <- function(p, name, w = 8, h = 5) {
  path <- file.path(fig_dir, name)
  ggsave(path, p, width = w, height = h, dpi = 120)
  cat(sprintf("  [그림 저장] %s\n", path))
}

# ---- 1. 데이터 무결성 점검 --------------------------------------------------
# 분석 결론의 신뢰도는 입력 데이터의 품질을 넘어설 수 없다. 가장 먼저 확인한다.
cat("[1] 데이터 무결성 점검\n")

expected_cols <- c("country", "year", "pop", "continent", "lifeExp", "gdpPercap")
missing_cols  <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) {
  stop(sprintf("필수 컬럼 누락: %s", paste(missing_cols, collapse = ", ")))
}

df$continent <- factor(df$continent)

# 1-1. 결측치 / 중복 / 자료형
na_by_col  <- sapply(df, function(x) sum(is.na(x)))
n_dup_rows <- sum(duplicated(df))
n_dup_key  <- sum(duplicated(df[, c("country", "year")]))   # (국가,연도) 키 중복
cat(sprintf("  - 결측치(NA) 총합            : %d\n", sum(na_by_col)))
if (sum(na_by_col) > 0) print(na_by_col[na_by_col > 0])
cat(sprintf("  - 완전 중복 행               : %d\n", n_dup_rows))
cat(sprintf("  - (국가,연도) 키 중복        : %d\n", n_dup_key))

# 1-2. 값 범위의 타당성 (음수/0/비현실적 값)
range_flags <- c(
  lifeExp_음수_또는_과대 = sum(df$lifeExp <= 0 | df$lifeExp > 120, na.rm = TRUE),
  gdpPercap_비양수       = sum(df$gdpPercap <= 0, na.rm = TRUE),
  pop_비양수             = sum(df$pop <= 0, na.rm = TRUE)
)
cat("  - 값 범위 위반 건수:\n")
print(range_flags)

# 1-3. 범주(대륙) 타당성 — 따옴표 없는 원본에서 쉼표 포함 국가명이 열을 밀면 여기서 드러남
expected_conts <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
bad_conts <- setdiff(levels(df$continent), expected_conts)
cat(sprintf("  - 예상 밖 대륙 라벨          : %s\n",
            if (length(bad_conts) == 0) "없음(정상)" else paste(bad_conts, collapse = ", ")))

# 1-4. 패널 균형성 — 모든 국가가 같은 연도 집합을 갖는 균형 패널인가?
years_all   <- sort(unique(df$year))
per_country <- df %>% count(country, name = "n_years")
balanced    <- all(per_country$n_years == length(years_all))
cat(sprintf("  - 연도 집합 (%d개)          : %s\n",
            length(years_all), paste(years_all, collapse = ", ")))
cat(sprintf("  - 국가 수                    : %d\n", n_distinct(df$country)))
cat(sprintf("  - 균형 패널 여부             : %s\n",
            if (balanced) "예 (모든 국가가 동일 연도 보유)" else "아니오(불균형)"))
if (!balanced) {
  cat("    · 불완전 시계열 국가:\n")
  print(as.data.frame(per_country %>% filter(n_years != length(years_all))), row.names = FALSE)
}
cat("\n")

# 이후 분석에서 재사용할 파생 객체
latest_year <- max(df$year)
first_year  <- min(df$year)
latest      <- df %>% filter(year == latest_year)

# ---- 2. 변수별 분포 (정확한 요약 통계) --------------------------------------
cat("[2] 주요 변수 분포 요약\n")
num_vars <- c("lifeExp", "gdpPercap", "pop")
dist_tbl <- lapply(num_vars, function(v) {
  x <- df[[v]]
  q <- quantile(x, c(0, .25, .5, .75, 1), na.rm = TRUE)
  data.frame(
    변수     = v,
    평균     = mean(x, na.rm = TRUE),
    표준편차 = sd(x, na.rm = TRUE),
    최소     = q[[1]], Q1 = q[[2]], 중앙값 = q[[3]], Q3 = q[[4]], 최대 = q[[5]],
    왜도     = skewness(x),
    초과첨도 = kurtosis_excess(x)
  )
})
dist_tbl <- do.call(rbind, dist_tbl)
print(format(dist_tbl, digits = 4, scientific = FALSE), row.names = FALSE)
cat("  · 해석: gdpPercap·pop은 왜도가 크게 +이고 평균 >> 중앙값 → 강한 우편향. 로그 변환 권장.\n")
cat("  · 주의: lifeExp 분포는 1952~2007년을 '한데 모은' 것이라 시대 혼합으로 봉우리가 둘일 수 있음\n")
cat("          (아래 03 그림에서 연도 분리로 확인).\n\n")

p_life <- ggplot(df, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
  labs(title = "기대수명(lifeExp) 분포 — 전체 연도 통합",
       subtitle = "통합 분포는 시대 혼합으로 좌측(과거)·우측(현재) 봉우리가 나타날 수 있음",
       x = "기대수명", y = "빈도") +
  theme_minimal()
save_plot(p_life, "01_hist_lifeExp.png")

p_gdp <- ggplot(df, aes(gdpPercap)) +
  geom_histogram(bins = 30, fill = "#31a354", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "1인당 GDP 분포 (로그 스케일)", x = "gdpPercap (log10)", y = "빈도") +
  theme_minimal()
save_plot(p_gdp, "02_hist_gdpPercap_log.png")
cat("\n")

# ---- 3. 시간에 따른 추세 + 전 세계 불평등 -----------------------------------
# 가중 평균만 보면 '중심'은 보이지만 '격차'는 보이지 않는다. 분산/지니를 함께 본다.
cat("[3] 연도별 추세: 중심(인구 가중 평균) + 격차(분산·지니)\n")
trend <- df %>%
  group_by(year) %>%
  summarise(
    lifeExp_w   = weighted.mean(lifeExp, pop),   # 인구 가중: '평균적 인간'의 경험
    lifeExp_u   = mean(lifeExp),                 # 비가중: '평균적 국가'
    gdp_w       = weighted.mean(gdpPercap, pop),
    pop_total   = sum(pop),
    lifeExp_sd  = sd(lifeExp),                   # 국가 간 기대수명 격차(분산)
    lifeExp_gini= gini(lifeExp),
    gdp_gini    = gini(gdpPercap),               # 국가 간 소득 불평등
    .groups = "drop"
  )
print(as.data.frame(trend %>% mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)
cat(sprintf("  · 기대수명 가중평균: %.1f(%d) → %.1f(%d), %+.1f년\n",
            trend$lifeExp_w[1], first_year, tail(trend$lifeExp_w, 1), latest_year,
            tail(trend$lifeExp_w, 1) - trend$lifeExp_w[1]))
min_sd_year <- trend$year[which.min(trend$lifeExp_sd)]
cat(sprintf("  · 국가 간 기대수명 격차(SD): %.1f(%d) → 최저 %.1f(%d) → %.1f(%d)\n",
            trend$lifeExp_sd[1], first_year,
            min(trend$lifeExp_sd), min_sd_year,
            tail(trend$lifeExp_sd, 1), latest_year))
cat("    → 단조 수렴이 아니라 U자형: 1980년대 후반까지 수렴 후 HIV/AIDS·체제전환으로 재발산.\n")
cat("  · 가중평균 > 비가중평균이면 인구가 많은 국가가 상대적으로 양호함을 시사.\n\n")

p_trend_life <- ggplot(df, aes(factor(year), lifeExp)) +
  geom_boxplot(fill = "#a6bddb", outlier.size = 0.6) +
  labs(title = "연도별 기대수명 분포 변화", x = "연도", y = "기대수명") +
  theme_minimal()
save_plot(p_trend_life, "03_box_lifeExp_by_year.png")

p_disp <- ggplot(trend, aes(year, lifeExp_sd)) +
  geom_line(color = "#d95f0e", linewidth = 1) + geom_point(color = "#d95f0e") +
  labs(title = "국가 간 기대수명 격차(표준편차)의 시간 변화",
       subtitle = "하락 추세면 국가 간 '수렴', 상승이면 '발산'",
       x = "연도", y = "기대수명 표준편차") +
  theme_minimal()
save_plot(p_disp, "07_dispersion_lifeExp.png")
cat("\n")

# ---- 4. 대륙별 비교 + 대륙 내 격차 ------------------------------------------
cat(sprintf("[4] 대륙별 요약 (기준 연도: %d)\n", latest_year))
by_cont <- latest %>%
  group_by(continent) %>%
  summarise(
    n_countries = n(),
    lifeExp_med = median(lifeExp),
    lifeExp_iqr = IQR(lifeExp),        # 대륙 '내부' 격차
    gdp_med     = median(gdpPercap),
    pop_total   = sum(pop),
    .groups = "drop"
  ) %>%
  arrange(desc(lifeExp_med))
print(as.data.frame(by_cont %>% mutate(across(where(is.numeric), ~round(.x, 1)))), row.names = FALSE)
cat("  · 중앙값뿐 아니라 IQR로 대륙 '내부' 이질성도 확인 (아프리카·아시아는 내부 편차가 큼).\n\n")

p_cont_life <- ggplot(latest, aes(reorder(continent, lifeExp, median), lifeExp, fill = continent)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.15, size = 0.6, alpha = 0.4) +
  labs(title = sprintf("대륙별 기대수명 분포 (%d)", latest_year), x = "대륙", y = "기대수명") +
  coord_flip() + theme_minimal()
save_plot(p_cont_life, "04_box_lifeExp_by_continent.png")

p_cont_trend <- df %>%
  group_by(continent, year) %>%
  summarise(lifeExp = weighted.mean(lifeExp, pop), .groups = "drop") %>%
  ggplot(aes(year, lifeExp, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  labs(title = "대륙별 기대수명 추세 (인구 가중)", x = "연도", y = "기대수명", color = "대륙") +
  theme_minimal()
save_plot(p_cont_trend, "05_line_lifeExp_continent.png")
cat("\n")

# ---- 5. GDP–기대수명 관계: 통합 vs 분리 -------------------------------------
# 통합 상관은 '국가 간 차이'와 '시간에 따른 동반 상승'을 뒤섞는다(허위 강화 위험).
# 단일 연도 단면 상관과 비교해 효과를 분리한다.
cat("[5] 1인당 GDP와 기대수명의 상관관계\n")
corr_pool_raw <- cor(df$gdpPercap, df$lifeExp)
corr_pool_log <- cor(log10(df$gdpPercap), df$lifeExp)
corr_year_log <- cor(log10(latest$gdpPercap), latest$lifeExp)

# 연도별 단면 상관(log GDP) — 관계의 강도가 시대에 따라 변하는지
corr_by_year <- df %>%
  group_by(year) %>%
  summarise(cor_log = cor(log10(gdpPercap), lifeExp), .groups = "drop")

cat(sprintf("  - 통합 상관 (원자료)            : %.3f\n", corr_pool_raw))
cat(sprintf("  - 통합 상관 (log10 GDP)         : %.3f\n", corr_pool_log))
cat(sprintf("  - %d년 단면 상관 (log10 GDP)    : %.3f\n", latest_year, corr_year_log))
cat("  · 통합 상관은 '국가 간 차이'와 '연도별 동반 상승'을 혼합하므로 단독 해석에 주의.\n")
cat("  · 연도별 단면 상관(log GDP):\n")
print(as.data.frame(corr_by_year %>% mutate(cor_log = round(cor_log, 3))), row.names = FALSE)
cat("\n")

# 단면 회귀(lifeExp ~ log10 GDP)와 잔차로 '추세에서 벗어난' 국가 식별
fit <- lm(lifeExp ~ log10(gdpPercap), data = latest)
latest$resid <- resid(fit)
cat(sprintf("  - %d년 로그-선형 적합 R^2 = %.3f\n", latest_year, summary(fit)$r.squared))
cat("  · 소득 대비 기대수명이 '높은' 국가 (양의 잔차 Top 5):\n")
print(latest %>% arrange(desc(resid)) %>%
        transmute(country, continent, lifeExp = round(lifeExp, 1),
                  gdpPercap = round(gdpPercap), resid = round(resid, 1)) %>%
        head(5) %>% as.data.frame(), row.names = FALSE)
cat("  · 소득은 높지만 기대수명이 '낮은' 국가 (음의 잔차 Top 5; 2007년엔 HIV/AIDS 피해 남부 아프리카가 지배적):\n")
print(latest %>% arrange(resid) %>%
        transmute(country, continent, lifeExp = round(lifeExp, 1),
                  gdpPercap = round(gdpPercap), resid = round(resid, 1)) %>%
        head(5) %>% as.data.frame(), row.names = FALSE)
cat("\n")

outliers <- bind_rows(
  latest %>% arrange(desc(resid)) %>% head(3),
  latest %>% arrange(resid) %>% head(3)
)
p_scatter <- ggplot(latest, aes(gdpPercap, lifeExp)) +
  geom_point(aes(size = pop, color = continent), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "grey30", linetype = "dashed") +
  geom_text(data = outliers, aes(label = country), size = 3, vjust = -1, check_overlap = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_size(range = c(1, 12), guide = "none") +
  labs(title = sprintf("1인당 GDP vs 기대수명 (%d)", latest_year),
       subtitle = "점선=로그-선형 적합. 라벨=추세에서 가장 벗어난 국가",
       x = "gdpPercap (log10)", y = "기대수명", color = "대륙") +
  theme_minimal()
save_plot(p_scatter, "06_scatter_gdp_lifeExp.png", w = 9, h = 6)
cat("\n")

# ---- 6. 순위: 상위/하위 국가 ------------------------------------------------
cat(sprintf("[6] %d년 기대수명 상위/하위 10개국\n", latest_year))
cat("  [상위 10]\n")
print(latest %>% arrange(desc(lifeExp)) %>%
        transmute(country, continent, lifeExp = round(lifeExp, 1),
                  gdpPercap = round(gdpPercap)) %>% head(10) %>% as.data.frame(),
      row.names = FALSE)
cat("\n  [하위 10]\n")
print(latest %>% arrange(lifeExp) %>%
        transmute(country, continent, lifeExp = round(lifeExp, 1),
                  gdpPercap = round(gdpPercap)) %>% head(10) %>% as.data.frame(),
      row.names = FALSE)
cat("\n")

# ---- 7. 변화: 개선폭 '그리고' 후퇴/역전 -------------------------------------
# 기존 분석은 개선 상위만 보았다. 같은 비중으로 '악화'도 봐야 균형 잡힌 EDA다.
cat(sprintf("[7] %d→%d 기대수명 변화 — 개선 & 후퇴\n", first_year, latest_year))
change <- df %>%
  group_by(country, continent) %>%
  summarise(
    le_start = lifeExp[which.min(year)],
    le_end   = lifeExp[which.max(year)],
    le_peak  = max(lifeExp),                       # 기간 중 정점
    gain     = lifeExp[which.max(year)] - lifeExp[which.min(year)],
    drawdown = max(lifeExp) - lifeExp[which.max(year)],  # 정점 대비 최종 하락폭
    .groups = "drop"
  )

cat("  [개선폭 상위 10]\n")
print(change %>% arrange(desc(gain)) %>%
        transmute(country, continent, le_start = round(le_start, 1),
                  le_end = round(le_end, 1), gain = round(gain, 1)) %>%
        head(10) %>% as.data.frame(), row.names = FALSE)

cat("\n  [기간 내 순(純)후퇴 국가: le_end < le_start]\n")
declines <- change %>% filter(gain < 0) %>% arrange(gain)
if (nrow(declines) == 0) {
  cat("    없음\n")
} else {
  print(declines %>%
          transmute(country, continent, le_start = round(le_start, 1),
                    le_end = round(le_end, 1), gain = round(gain, 1)) %>%
          as.data.frame(), row.names = FALSE)
}

cat("\n  [정점 대비 최대 후퇴(drawdown) 상위 10 — 에이즈·분쟁 충격 탐지]\n")
print(change %>% arrange(desc(drawdown)) %>%
        transmute(country, continent, le_peak = round(le_peak, 1),
                  le_end = round(le_end, 1), drawdown = round(drawdown, 1)) %>%
        head(10) %>% as.data.frame(), row.names = FALSE)
cat("\n")

# 후퇴 충격이 큰 국가들의 궤적 시각화
shock <- change %>% arrange(desc(drawdown)) %>% head(6) %>% pull(country)
p_shock <- df %>% filter(country %in% shock) %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  labs(title = "기대수명 후퇴가 컸던 국가들의 궤적",
       subtitle = "정점 대비 하락폭(drawdown) 상위 — HIV/AIDS·분쟁의 흔적",
       x = "연도", y = "기대수명", color = "국가") +
  theme_minimal()
save_plot(p_shock, "08_reversals_lifeExp.png", w = 9, h = 5.5)
cat("\n")

# ---- 8. 마무리 -------------------------------------------------------------
cat("==============================================\n")
cat(sprintf(" EDA 종료 — 그림 %d개를 '%s/' 에 저장\n",
            length(list.files(fig_dir, pattern = "\\.png$")), fig_dir))
cat("==============================================\n")
cat("\n[세션 정보]\n")
print(sessionInfo()$R.version$version.string)
cat(sprintf("주요 패키지: ggplot2 %s, dplyr %s\n",
            as.character(packageVersion("ggplot2")),
            as.character(packageVersion("dplyr"))))
