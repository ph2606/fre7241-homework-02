# Run from repository root after Panagiotis_Housos_hw2.R.
results <- readRDS("hw2_results/results.rds")
inputs <- readRDS("data/hw2_inputs.rds")
dir.create("report/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)
fmt <- function(x, digits = 4) unname(formatC(x, digits = digits, format = "f"))
latex_table <- function(data, headers, file, alignment = NULL) {
  if (is.null(alignment)) alignment <- paste0("l", paste(rep("r", NCOL(data)-1), collapse = ""))
  rows <- apply(as.matrix(data), 1, function(row) paste0(paste(row, collapse = " & "), " \\\\"))
  writeLines(c(paste0("\\begin{tabular}{", alignment, "}"), "\\toprule",
    paste0(paste(headers, collapse = " & "), " \\\\"), "\\midrule", rows,
    "\\bottomrule", "\\end{tabular}"), file.path("report/tables", file))
}
vol <- results$vol
hampel <- results$hampel
pm <- hampel$portfolio_metrics
metric <- function(portfolio, period) pm$Sharpe[pm$Portfolio == portfolio & pm$Period == period]
fit_summary <- summary(hampel$regression)
macros <- c(
  HWBestLambda = fmt(vol$best_lambda, 1),
  HWBestSharpe = fmt(vol$best_sharpe),
  HWVolSharpe = fmt(vol$metrics[1, 2]), HWSpySharpe = fmt(vol$metrics[1, 1]),
  HWVolSortino = fmt(vol$metrics[2, 2]), HWSpySortino = fmt(vol$metrics[2, 1]),
  HWEligibleStocks = length(hampel$sharpe),
  HWMatchedStocks = length(hampel$sharpis),
  HWPersistenceCorrelation = fmt(hampel$correlation),
  HWPersistenceSlope = fmt(coef(hampel$regression)[2]),
  HWPersistencePValue = fmt(coef(fit_summary)[2, 4]),
  HWPersistenceRSquared = fmt(fit_summary$r.squared, 6),
  HWAllFullSharpe = fmt(metric("All stocks", "Full")),
  HWAllOOSSharpe = fmt(metric("All stocks", "OOS")),
  HWTopFullSharpe = fmt(metric("Top 100", "Full")),
  HWTopISSharpe = fmt(metric("Top 100", "IS")),
  HWTopOOSSharpe = fmt(metric("Top 100", "OOS")),
  HWNxpiSharpe = fmt(hampel$nxpi_metrics[1, 1]),
  HWNxpiHampelSharpe = fmt(hampel$nxpi_metrics[1, 2]),
  HWVolScale = fmt(vol$scale, 6),
  HWMinAvailable = min(hampel$available), HWMaxAvailable = max(hampel$available)
)
writeLines(paste0("\\newcommand{\\", names(macros), "}{", macros, "}"),
           "report/results_macros.tex")
vol_table <- data.frame(
  Series = c("SPY", "Strategy"),
  Sharpe = fmt(vol$metrics[1, ]),
  Sortino = fmt(vol$metrics[2, ]),
  AnnualVol = fmt(sqrt(252) * c(sd(vol$returns), sd(vol$pnls))),
  DailyMean = fmt(c(mean(vol$returns), mean(vol$pnls)), 6)
)
latex_table(vol_table,
  c("Series", "Sharpe", "Course Sortino", "Annualized volatility", "Mean daily PnL"),
  "vol_metrics.tex")
latex_table(data.frame(Lambda = fmt(vol$profile$lambda, 1),
                        Sharpe = fmt(vol$profile$Sharpe)),
            c("Decay factor $\\lambda$", "Net annualized Sharpe"), "lambda_profile.tex")
latex_table(data.frame(Rank = 1:10, Stock = names(head(hampel$sharpe, 10)),
                        Sharpe = fmt(head(hampel$sharpe, 10))),
            c("Rank", "Stock", "Full-sample Sharpe"), "hampel_top10.tex", "rlr")
persistence <- data.frame(
  Statistic = c("Matched stock pairs", "Correlation", "Intercept", "Slope",
                 "Conventional slope p-value", "$R^2$"),
  Value = c(length(hampel$sharpis), fmt(hampel$correlation),
              fmt(coef(hampel$regression)[1]), fmt(coef(hampel$regression)[2]),
              fmt(coef(fit_summary)[2, 4]), fmt(fit_summary$r.squared, 6))
)
latex_table(persistence, c("Statistic", "Value"), "persistence.tex")
portfolio_table <- data.frame(
  Portfolio = pm$Portfolio, Period = pm$Period, N = pm$N,
  Sharpe = fmt(pm$Sharpe), CumPnL = fmt(pm$CumulativePnL),
  AnnualVol = fmt(pm$AnnualizedVolatility)
)
latex_table(portfolio_table,
  c("Portfolio", "Period", "$N$", "Sharpe", "Cumulative PnL", "Annualized volatility"),
  "portfolio_metrics.tex", "llrrrr")
latex_table(data.frame(Series = c("NXPI", "Hampel"),
  Sharpe = fmt(hampel$nxpi_metrics[1, ]),
  Sortino = fmt(hampel$nxpi_metrics[2, ])),
  c("NXPI series", "Sharpe", "Course Sortino"), "nxpi_metrics.tex")
files <- list.files("hw2_results", pattern = "^strat_.*\\.pdf$", full.names = TRUE)
stopifnot(length(files) == 6L, all(file.copy(files, "report/figures", overwrite = TRUE)))
cat("Exported report macros, six tables, and six vector figures.\n")
