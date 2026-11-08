#################################
### FRE7241 Homework #2
### Panagiotis Housos | November 7, 2026
### Due: 6PM Tuesday November 10, 2026
#################################
# Run: Rscript --vanilla Panagiotis_Housos_hw2.R
# This file completes all six questions in FRE7241_hw2.R.
# Figures, interactive dygraphs, and numerical tables go to hw2_results/.
# The bundled input snapshot takes priority, so a clone reproduces the report.
# If only this R file is copied into the course's Homework_02 folder, it also
# runs using installed rutils data and ../../06_Data/R_Data/sp500_prices.RData.
# Different data/package snapshots can produce different numerical results.

suppressPackageStartupMessages(library(rutils))
stopifnot(requireNamespace("HighFreq", quietly = TRUE),
          requireNamespace("htmlwidgets", quietly = TRUE))

# Locate the submission without requiring the professor's absolute paths.
hw2_files <- c(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)),
               unlist(lapply(sys.frames(), function(x) x$ofile)))
hw2_files <- hw2_files[basename(hw2_files) == "Panagiotis_Housos_hw2.R"]
hw2_root <- if (length(hw2_files)) {
  dirname(normalizePath(tail(hw2_files, 1), mustWork = TRUE))
} else if (file.exists("Panagiotis_Housos_hw2.R")) {
  normalizePath(".")
} else {
  normalizePath(file.path("Homeworks", "Homework_02"), mustWork = TRUE)
}
results_dir <- file.path(hw2_root, "hw2_results")
for (subdir in c("", "tables", "interactive")) {
  dir.create(file.path(results_dir, subdir), recursive = TRUE, showWarnings = FALSE)
}
snapshot_file <- file.path(hw2_root, "data", "hw2_inputs.rds")
if (file.exists(snapshot_file)) {
  hw2_inputs <- readRDS(snapshot_file)
} else {
  stock_candidates <- c(
    file.path(hw2_root, "..", "..", "06_Data", "R_Data", "sp500_prices.RData"),
    file.path(hw2_root, "data", "sp500_prices.RData"),
    file.path(hw2_root, "sp500_prices.RData"),
    "/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
  stock_candidates <- stock_candidates[file.exists(stock_candidates)]
  if (!length(stock_candidates)) {
    stop("Missing stock data: use the repository data/ folder or course folder.")
  }
  stock_file <- stock_candidates[1]
  stock_env <- new.env()
  load(stock_file, envir = stock_env)
  hw2_inputs <- list(
    spy_returns = na.omit(get("SPY", rutils::etfenv$returns)),
    stock_prices = stock_env$pricestock["2006/"]
  )
}

# The course's Sortino uses SD of negative returns, not semideviation.
sharpe_ratio <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L || sd(x) == 0) return(NA_real_)
  sqrt(252) * mean(x) / sd(x)
}
course_ratios <- function(x) rutils::calc_sharpe(x, riskf = 0, nperiods = 252)
write_table <- function(x, filename, row_names = FALSE) {
  write.csv(x, file.path(results_dir, "tables", filename), row.names = row_names)
}

# Save each requested dygraph, including local JS/CSS dependencies.
save_dygraph <- function(x, filename, title) {
  widget <- dygraphs::dygraph(cumsum(x), main = title,
                              xlab = "Date", ylab = "Cumulative PnL (sum)")
  widget <- dygraphs::dyOptions(widget, colors = c("#183B56", "#C43C39"),
                                 strokeWidth = 1.25, drawGrid = TRUE)
  widget <- dygraphs::dyRangeSelector(widget)
  htmlwidgets::saveWidget(widget,
    file.path(results_dir, "interactive", paste0(filename, ".html")),
    selfcontained = FALSE, libdir = "lib")
  widget
}

# Save report-quality PDF and PNG versions alongside interactive output.
save_figure <- function(filename, draw) {
  pdf(file.path(results_dir, paste0(filename, ".pdf")), width = 9, height = 5.3)
  tryCatch(draw(), finally = dev.off())
  png(file.path(results_dir, paste0(filename, ".png")),
      width = 1800, height = 1060, res = 200)
  tryCatch(draw(), finally = dev.off())
}
plot_pnl <- function(x, title, boundary = FALSE) {
  y <- coredata(cumsum(x))
  matplot(as.Date(index(x)), y, type = "l", lty = 1, lwd = 1.1,
          col = c("#183B56", "#C43C39"), xlab = "Date",
          ylab = "Cumulative PnL (sum)", main = title)
  grid(col = "#DDDDDD")
  if (boundary) abline(v = as.numeric(as.Date("2017-01-01")),
                       lty = 2, col = "#116466", lwd = 1.5)
  legend("topleft", legend = colnames(x), lty = 1, lwd = 2,
         col = c("#183B56", "#C43C39"), bty = "n", cex = 0.85)
}

############## Part I: inverse EMA volatility timing

bidask <- 0.0001
symboln <- "SPY"
retp <- hw2_inputs$spy_returns
# The installed HighFreq argument is 'lambdaf'; positional use also works
# with versions that call it 'lambda'. Column 2 is the EMA variance.
volma <- sqrt(HighFreq::run_var(as.matrix(retp), 0.5)[, 2])
stopifnot(all(is.finite(volma)), all(volma > 0))

# I.1 (10pts): inverse volatility, lagged positions, costs, and ratios.
posv <- rutils::lagit(1 / volma)
turnover <- abs(rutils::diffit(posv))
costs <- 0.5 * bidask * turnover
pnls_raw <- posv * retp - costs
vol_scale <- as.numeric(sd(retp) / sd(pnls_raw))
pnls <- pnls_raw * vol_scale
vol_pnls <- pnls
vol_metrics <- course_ratios(cbind(SPY = retp, Strategy = pnls))
colnames(vol_metrics) <- c("SPY", "Strategy")
cat("\nPart I.1: inverse volatility positions\n")
print(NROW(posv)); print(head(posv)); print(tail(posv))
cat("Net, volatility-matched PnLs\n")
print(head(pnls)); print(tail(pnls)); print(vol_metrics)
vol_plot_data <- cbind(SPY = retp, Strategy = pnls)
colnames(vol_plot_data) <- c("SPY", "Strategy")
dy_voltiming <- save_dygraph(vol_plot_data, "strat_voltiming_ema",
  sprintf("Volatility timing: SPY Sharpe %.3f / strategy %.3f",
          vol_metrics[1, 1], vol_metrics[1, 2]))
save_figure("strat_voltiming_ema", function() {
  plot_pnl(vol_plot_data, "Inverse EMA volatility timing (lambda = 0.5)")
})

# I.2 (10pts): parallel lambda profile. PSOCK works on Windows and Unix.
library(parallel)
lambdav <- seq(0.1, 0.9, by = 0.1)
timing_sharpe <- function(lambda, retp, bidask) {
  vol <- sqrt(HighFreq::run_var(as.matrix(retp), lambda)[, 2])
  pos <- rutils::lagit(1 / vol)
  pnl <- pos * retp - 0.5 * bidask * abs(rutils::diffit(pos))
  # Positive constant volatility scaling leaves Sharpe unchanged.
  sqrt(252) * as.numeric(mean(pnl) / sd(pnl))
}
profile_parallel <- function(lambdav, retp, bidask) {
  workers <- min(2L, parallel::detectCores(logical = FALSE))
  if (is.na(workers)) workers <- 2L
  # Workers run without project startup profiles.
  cluster <- parallel::makePSOCKcluster(workers,
    rscript_args = "--vanilla", methods = FALSE)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  unlist(parallel::parLapply(cluster, lambdav, timing_sharpe,
                              retp = retp, bidask = bidask), use.names = FALSE)
}
sharpev <- profile_parallel(lambdav, retp, bidask)
lambda_profile <- data.frame(lambda = lambdav, Sharpe = sharpev)
best_sharpe <- max(sharpev)
best_lambda <- lambdav[which.max(sharpev)]
cat("\nPart I.2: parallel lambda profile\n")
print(round(sharpev, 4)); print(best_sharpe); print(best_lambda)
save_figure("strat_voltiming_profile_ema", function() {
  plot(lambdav, sharpev, type = "b", pch = 19, col = "#183B56", lwd = 1.5,
       xlab = "EMA decay parameter (lambda)", ylab = "Annualized Sharpe ratio",
       main = "Volatility timing: in-sample parameter profile")
  grid(col = "#DDDDDD")
  points(best_lambda, best_sharpe, pch = 19, col = "#C43C39", cex = 1.4)
})

############## Part II: Hampel strategy for single stocks

pricestock <- hw2_inputs$stock_prices["2006/"]
lookb <- 3L

# Course strategies_univariate.pdf pp. 88-90: use log prices, trailing
# median and scaled MAD; trade beyond +/-1, carry positions, then lag.
simulate_hampel <- function(prices, lookb = 3L, details = FALSE) {
  prices <- na.omit(prices)
  if (NROW(prices) < 2L * lookb) return(NULL)
  stopifnot(all(is.finite(prices)), all(prices > 0))
  pricev <- log(prices)
  nrows <- NROW(pricev)
  medianv <- HighFreq::roll_mean(as.matrix(pricev),
                                 lookb = lookb, method = "nonparametric")
  madv <- HighFreq::roll_var(as.matrix(pricev),
                              lookb = lookb, method = "nonparametric")
  zscores <- ifelse(madv > 0, (as.matrix(pricev) - medianv) / madv, 0)
  zscores[seq_len(lookb), ] <- 0
  signal <- rep(NA_integer_, nrows)
  signal[1] <- 0L
  signal[zscores > 1] <- -1L
  signal[zscores < -1] <- 1L
  signal <- as.numeric(zoo::na.locf(signal))
  pos <- rutils::lagit(signal)
  returns <- rutils::diffit(pricev)
  pnl <- returns * pos
  if (details) return(list(prices = pricev, median = medianv, mad = madv,
    zscores = zscores, signal = signal, positions = pos,
    returns = returns, pnl = pnl))
  pnl
}

# II.1 (10pts): one lapply() per column, omitting short histories.
pnls <- lapply(seq_len(NCOL(pricestock)), function(j) {
  simulate_hampel(pricestock[, j], lookb = lookb)
})
names(pnls) <- colnames(pricestock)
pnls <- Filter(Negate(is.null), pnls)
sharpev <- sort(vapply(pnls, sharpe_ratio, numeric(1)), decreasing = TRUE)
hampel_sharpe <- sharpev
cat("\nPart II.1: individual-stock PnLs and Sharpe ranking\n")
print(class(pnls)); print(dim(pnls[[1]]))
print(NROW(sharpev)); print(head(sharpev, 10))

# II.2 (10pts): date-aligned, equal-weight mean among available histories.
# merge() aligns dates; column binding by row number would mix listing dates.
pnl_all <- do.call(merge, c(unname(pnls), list(all = TRUE)))
colnames(pnl_all) <- names(pnls)
mean_available <- function(x) {
  values <- rowMeans(x, na.rm = TRUE)
  values[!is.finite(values)] <- NA_real_
  xts::xts(values, order.by = index(x))
}
pnlm <- mean_available(pnl_all)
colnames(pnlm) <- "All stocks"
available_counts <- rowSums(is.finite(pnl_all))
cat("\nPart II.2: mean PnLs\n")
print(class(pnlm)); print(dim(pnlm))
dy_hampel_mean <- save_dygraph(pnlm, "strat_hampel_mean",
  sprintf("Mean Hampel PnL: annualized Sharpe %.3f", sharpe_ratio(pnlm)))
save_figure("strat_hampel_mean", function() {
  plot_pnl(pnlm, "Hampel: mean PnL across available stocks")
})

symboln <- "NXPI"
nxpi_details <- simulate_hampel(pricestock[, symboln], lookb, details = TRUE)
nxpi_plot_data <- cbind(nxpi_details$returns, nxpi_details$pnl)
colnames(nxpi_plot_data) <- c("NXPI", "Hampel")
nxpi_metrics <- course_ratios(nxpi_plot_data)
dy_hampel_single <- save_dygraph(nxpi_plot_data, "strat_hampel_single",
  sprintf("NXPI: buy/hold Sharpe %.3f / Hampel %.3f",
          nxpi_metrics[1, 1], nxpi_metrics[1, 2]))
save_figure("strat_hampel_single", function() {
  plot_pnl(nxpi_plot_data, "Hampel strategy for NXPI (no transaction costs)")
})

# II.3 (10pts): in sample through 2016; out of sample from 2017.
# Run signals continuously, so the first 2017 holding uses 2016 information.
sharpis_all <- vapply(pnls, function(x) sharpe_ratio(x["/2016"]), numeric(1))
sharpos_all <- vapply(pnls, function(x) sharpe_ratio(x["2017/"]), numeric(1))
matched <- is.finite(sharpis_all) & is.finite(sharpos_all)
sharpis <- sharpis_all[matched]
sharpos <- sharpos_all[names(sharpis)]
stopifnot(identical(names(sharpis), names(sharpos)))
persistence_data <- data.frame(symbol = names(sharpis),
                                InSample = as.numeric(sharpis),
                                OutOfSample = as.numeric(sharpos))
persistence_fit <- lm(OutOfSample ~ InSample, data = persistence_data)
persistence_summary <- summary(persistence_fit)
persistence_correlation <- cor(sharpis, sharpos)
cat("\nPart II.3: matched in/out-of-sample Sharpe ratios\n")
print(NROW(sharpis)); print(head(sharpis, 10))
print(NROW(sharpos)); print(head(sharpos, 10)); print(persistence_summary)
save_figure("strat_hampel_scatter", function() {
  plot(sharpis, sharpos, pch = 19, col = adjustcolor("#183B56", alpha.f = 0.45),
       xlab = "In-sample Sharpe (2006-2016)",
       ylab = "Out-of-sample Sharpe (2017 onward)",
       main = sprintf("Hampel Sharpe persistence: correlation = %.3f",
                      persistence_correlation))
  grid(col = "#DDDDDD")
  abline(persistence_fit, col = "#C43C39", lwd = 2)
  abline(h = 0, v = 0, col = "#AAAAAA", lty = 3)
})
# A positive fitted slope describes cross-sectional historical persistence;
# common market exposures and selection prevent a causal interpretation.

# II.4 (10pts): choose the top 100 using ONLY in-sample Sharpe ratios.
ranked_is <- sort(sharpis_all[is.finite(sharpis_all)], decreasing = TRUE)
top_symbols <- head(names(ranked_is), 100L)
stopifnot(length(top_symbols) == 100L)
pnltop <- mean_available(pnl_all[, top_symbols])
colnames(pnltop) <- "Top 100 (selected through 2016)"
top_plot_data <- cbind(pnlm, pnltop)
colnames(top_plot_data) <- c("All stocks", "Top 100 selected through 2016")
dy_hampel_top <- save_dygraph(top_plot_data, "strat_hampel_top",
  "Top 100 by in-sample Sharpe: selection ends December 2016")
dy_hampel_top <- dygraphs::dyEvent(dy_hampel_top, "2017-01-01",
                                    "Out of sample", strokePattern = "dashed")
htmlwidgets::saveWidget(dy_hampel_top,
  file.path(results_dir, "interactive", "strat_hampel_top.html"),
  selfcontained = FALSE, libdir = "lib")
save_figure("strat_hampel_top", function() {
  plot_pnl(top_plot_data, "Hampel: top 100 selected on 2006-2016 data", TRUE)
})

# Historical selected-stock gains before 2017 are in-sample. Evaluate the
# fixed selection after 2016 separately. Costs and shorting fees are excluded
# in Part II, as in the assignment; the supplied universe may be survivorship
# biased. Cumulative plots are sums of daily PnL, not compounded wealth.
portfolio_metrics <- do.call(rbind, lapply(c("Full", "IS", "OOS"), function(period) {
  dates <- switch(period, Full = "/", IS = "/2016", OOS = "2017/")
  do.call(rbind, lapply(c("All stocks", "Top 100"), function(portfolio) {
    x <- if (portfolio == "All stocks") pnlm[dates] else pnltop[dates]
    data.frame(Portfolio = portfolio, Period = period,
      N = NROW(x), Sharpe = sharpe_ratio(x), CumulativePnL = sum(x),
      AnnualizedVolatility = sqrt(252) * sd(x))
  }))
}))
print(portfolio_metrics)

# Export the report's numerical inputs and reproducible result objects.
write_table(data.frame(Statistic = rownames(vol_metrics), vol_metrics,
                        check.names = FALSE), "vol_ratios.csv")
write_table(lambda_profile, "lambda_profile.csv")
write_table(data.frame(Symbol = names(sharpev), Sharpe = as.numeric(sharpev)),
            "hampel_sharpe_ranking.csv")
write_table(persistence_data, "hampel_is_oos.csv")
write_table(data.frame(Symbol = top_symbols, InSampleSharpe = ranked_is[top_symbols]),
            "top100_selection.csv")
write_table(portfolio_metrics, "portfolio_metrics.csv")
write_table(data.frame(Date = as.character(index(vol_plot_data)),
                        coredata(vol_plot_data)), "vol_daily_pnls.csv")
write_table(data.frame(Date = as.character(index(top_plot_data)),
                        coredata(top_plot_data), AvailableStocks = available_counts),
            "hampel_portfolio_daily_pnls.csv")
hw2_results <- list(
  vol = list(returns = retp, volma = volma, positions = posv,
    costs = costs, raw_pnls = pnls_raw, scale = vol_scale,
    pnls = vol_pnls, metrics = vol_metrics, profile = lambda_profile,
    best_lambda = best_lambda, best_sharpe = best_sharpe),
  hampel = list(prices_dim = dim(pricestock), lookb = lookb,
    first_pnl_dim = dim(pnls[[1]]), sharpe = sharpev,
    mean_pnl = pnlm, available = available_counts,
    nxpi = nxpi_details, nxpi_metrics = nxpi_metrics,
    sharpis = sharpis, sharpos = sharpos,
    regression = persistence_fit, correlation = persistence_correlation,
    top_symbols = top_symbols, ranked_is = ranked_is,
    top_pnl = pnltop, portfolio_metrics = portfolio_metrics)
)
saveRDS(hw2_results, file.path(results_dir, "results.rds"), compress = "xz")
cat("\nCompleted all six questions. Results saved in:\n", results_dir, "\n")
