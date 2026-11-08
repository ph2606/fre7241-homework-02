# Independent numerical verification of the saved Homework 2 submission.
# Run from any directory: Rscript --vanilla path/to/reproducibility/verify_hw2.R
# This reads the frozen inputs/results and never sources the solution script.

verify_files <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
verify_files <- c(verify_files, unlist(lapply(sys.frames(), function(x) x$ofile)))
verify_files <- verify_files[basename(verify_files) == "verify_hw2.R"]
verify_root <- if (length(verify_files)) {
  dirname(dirname(normalizePath(tail(verify_files, 1), mustWork = TRUE)))
} else if (file.exists("reproducibility/verify_hw2.R")) {
  normalizePath(".")
} else stop("Cannot locate Homework_02/reproducibility/verify_hw2.R")

local({
  log_file <- file.path(verify_root, "hw2_results", "verification.txt")
  sink(log_file, split = TRUE)
  on.exit({
    sink()
    file.copy(log_file, file.path(verify_root, "reproducibility", "verification.txt"),
              overwrite = TRUE)
  }, add = TRUE)
  cat("Homework 2 independent verification\n")
  cat("Inputs: data/hw2_inputs.rds\nResults: hw2_results/results.rds\n\n")
  checks <- 0L
  check <- function(condition, label) {
    if (!isTRUE(condition)) stop("FAIL: ", label, call. = FALSE)
    checks <<- checks + 1L
    cat(sprintf("PASS %02d: %s\n", checks, label))
    invisible(TRUE)
  }
  close <- function(actual, expected, label, tolerance = 1e-10) {
    actual <- as.numeric(actual)
    expected <- as.numeric(expected)
    same <- length(actual) == length(expected) &&
      identical(is.na(actual), is.na(expected)) &&
      identical(is.infinite(actual), is.infinite(expected))
    if (same) {
      finite <- is.finite(actual) & is.finite(expected)
      if (any(finite)) same <- max(abs(actual[finite] - expected[finite])) <=
        tolerance * max(1, abs(expected[finite]))
      if (any(is.infinite(actual))) same <- same &&
        identical(actual[is.infinite(actual)], expected[is.infinite(expected)])
    }
    check(same, label)
  }
  # Some xts Date indexes retain redundant tzone/tclass attributes after
  # subsetting; compare the calendar values independently of those attributes.
  dates <- function(x) as.Date(as.numeric(as.Date(zoo::index(x))), origin = "1970-01-01")
  independent_sharpe <- function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    if (length(x) < 2L || stats::sd(x) == 0) return(NA_real_)
    sqrt(252) * mean(x) / stats::sd(x)
  }
  independent_ratios <- function(x) {
    x <- as.numeric(x)
    c(Sharpe = sqrt(252) * mean(x) / stats::sd(x),
      Sortino = sqrt(252) * mean(x) / stats::sd(x[x < 0]))
  }
  lag_one <- function(x) c(0, head(x, -1L))
  difference <- function(x) c(0, diff(x))

  # Independent recurrence, including the installed HighFreq startup value.
  # The first variance uses observation 2; all causality checks below use
  # prefixes of length >= 2, and the first trading position is flat.
  independent_ema <- function(x, lambda) {
    x <- as.numeric(x)
    stopifnot(length(x) >= 2L)
    mu <- variance <- numeric(length(x))
    mu[1] <- x[1]
    variance[1] <- (x[2] - x[1])^2
    for (i in 2:length(x)) {
      mu[i] <- lambda * mu[i - 1L] + (1 - lambda) * x[i]
      variance[i] <- lambda^2 * variance[i - 1L] +
        (1 - lambda^2) * (x[i] - mu[i])^2
    }
    list(mean = mu, variance = variance)
  }
  independent_timing <- function(x, lambda, bidask = 0.0001) {
    ema <- independent_ema(x, lambda)
    volatility <- sqrt(ema$variance)
    positions <- lag_one(1 / volatility)
    costs <- 0.5 * bidask * abs(difference(positions))
    raw <- positions * as.numeric(x) - costs
    scale <- stats::sd(as.numeric(x)) / stats::sd(raw)
    list(volatility = volatility, positions = positions, costs = costs,
         raw = raw, scale = scale, pnl = raw * scale)
  }

  # A transparent base-R reference loop: no HighFreq, rutils, or zoo carry.
  # MAD has the stats::mad default normalization constant 1.4826.
  independent_hampel <- function(prices) {
    prices <- as.numeric(prices)
    x <- log(prices)
    n <- length(x)
    stopifnot(n >= 6L, all(is.finite(x)))
    center <- dispersion <- score <- signal <- numeric(n)
    for (i in seq_len(n)) {
      window <- x[max(1L, i - 2L):i]
      center[i] <- stats::median(window)
      dispersion[i] <- stats::mad(window, center = center[i], constant = 1.4826)
      if (i > 3L && dispersion[i] > 0)
        score[i] <- (x[i] - center[i]) / dispersion[i]
      signal[i] <- if (i == 1L) 0 else signal[i - 1L]
      if (score[i] > 1) signal[i] <- -1
      if (score[i] < -1) signal[i] <- 1
    }
    position <- lag_one(signal)
    returns <- difference(x)
    list(prices = x, median = center, mad = dispersion, zscores = score,
         signal = signal, positions = position, returns = returns,
         pnl = returns * position)
  }

  # Independently evaluate the whole universe efficiently for lookback 3.
  # Median(a,b,c) selects an input value; no numerical summation is needed.
  # For sorted a<=b<=c, raw MAD is min(b-a,c-b).
  independent_hampel_three <- function(prices) {
    x <- log(as.numeric(prices))
    n <- length(x)
    if (n < 6L) return(NULL)
    a <- x[seq_len(n - 2L)]
    b <- x[2:(n - 1L)]
    c <- x[3:n]
    mid <- pmax(pmin(a, b), pmin(pmax(a, b), c))
    dispersion <- 1.4826 * pmin(mid - pmin(a, b, c), pmax(a, b, c) - mid)
    score <- numeric(n)
    positive <- dispersion > 0
    score[which(positive) + 2L] <-
      (x[which(positive) + 2L] - mid[positive]) / dispersion[positive]
    score[1:3] <- 0
    signal <- ifelse(score > 1, -1, ifelse(score < -1, 1, NA_real_))
    signal[1] <- 0
    last_signal <- cummax(ifelse(is.na(signal), 0L, seq_len(n)))
    signal <- signal[last_signal]
    difference(x) * lag_one(signal)
  }

  tryCatch({
    check(requireNamespace("HighFreq", quietly = TRUE) &&
            requireNamespace("zoo", quietly = TRUE) &&
            requireNamespace("xts", quietly = TRUE),
          "Required numerical/time-series packages are available")
    input_file <- file.path(verify_root, "data", "hw2_inputs.rds")
    result_file <- file.path(verify_root, "hw2_results", "results.rds")
    check(file.exists(input_file) && file.exists(result_file),
          "Frozen input and saved result files exist")
    inputs <- readRDS(input_file)
    results <- readRDS(result_file)
    spy <- as.numeric(inputs$spy_returns)
    spy_dates <- dates(inputs$spy_returns)
    v <- results$vol
    h <- results$hampel
    check(length(spy) >= 100L && all(is.finite(spy)),
          "SPY input is finite and has sufficient observations")

    # Confirm the documented recurrence and the installed startup convention.
    probe <- c(0.01, -0.02, 0.03, -0.01)
    probe_expected <- independent_ema(probe, 0.5)
    probe_package <- HighFreq::run_var(matrix(probe), 0.5)
    close(probe_package[, 1], probe_expected$mean,
          "Independent EMA mean recurrence matches a known probe", 1e-13)
    close(probe_package[, 2], probe_expected$variance,
          "EMA variance uses squared decay and first squared difference", 1e-13)
    ema <- independent_ema(spy, 0.5)
    package_ema <- HighFreq::run_var(matrix(spy), 0.5)
    close(package_ema[, 1], ema$mean, "SPY EMA means match independent recurrence")
    close(package_ema[, 2], ema$variance,
          "SPY EMA variances match independent recurrence")
    timing <- independent_timing(spy, 0.5)
    close(v$volma, timing$volatility, "Saved EMA volatilities match independent recurrence")
    close(v$positions, timing$positions, "Inverse volatility positions are lagged one day")
    close(v$costs, timing$costs, "Costs equal half-spread times absolute position change")
    close(v$raw_pnls, timing$raw, "Net PnLs subtract turnover costs before normalization")
    close(v$scale, timing$scale, "Saved full-sample volatility scale is correct")
    close(v$pnls, timing$pnl, "Saved normalized timing PnLs match independent arithmetic")
    close(stats::sd(v$pnls), stats::sd(spy), "Timing volatility equals SPY volatility")
    check(as.numeric(v$positions[1]) == 0 && as.numeric(v$pnls[1]) == 0,
          "First timing holding and PnL are flat")
    check(identical(dates(v$pnls), spy_dates), "Timing PnL dates align with SPY returns")
    ratios <- cbind(SPY = independent_ratios(spy),
                    Strategy = independent_ratios(timing$pnl))
    close(v$metrics, ratios, "Sharpe and course Sortino match independent formulas")
    sequential_profile <- vapply(v$profile$lambda, function(lambda)
      independent_sharpe(independent_timing(spy, lambda)$raw), numeric(1))
    close(v$profile$Sharpe, sequential_profile,
          "Saved PSOCK lambda profile matches independent sequential recurrence")
    close(v$best_sharpe, max(sequential_profile), "Best Sharpe is profile maximum")
    close(v$best_lambda, v$profile$lambda[which.max(sequential_profile)],
          "Best lambda is associated with the maximum Sharpe")

    for (cut in unique(c(100L, length(spy) %/% 2L, length(spy) - 20L))) {
      prefix <- HighFreq::run_var(matrix(spy[seq_len(cut)]), 0.5)
      close(prefix, package_ema[seq_len(cut), ],
            paste("EMA prefix invariant at observation", cut))
      altered <- spy
      future <- (cut + 1L):length(spy)
      altered[future] <- 2 * altered[future] + 0.01
      changed <- HighFreq::run_var(matrix(altered), 0.5)
      close(changed[seq_len(cut), ], package_ema[seq_len(cut), ],
            paste("Future SPY changes preserve EMA history through", cut))
    }

    stock <- inputs$stock_prices["2006/"]
    stock_dates <- dates(stock)
    check(identical(as.integer(h$prices_dim), as.integer(dim(stock))) && h$lookb == 3L,
          "Saved stock dimensions and lookback match frozen inputs")
    check("NXPI" %in% colnames(stock), "NXPI is present in the frozen universe")
    nxpi_price <- stock[, "NXPI"]
    nxpi_valid <- is.finite(as.numeric(nxpi_price))
    nxpi_dates <- dates(nxpi_price)[nxpi_valid]
    nxpi <- independent_hampel(as.numeric(nxpi_price)[nxpi_valid])
    close(h$nxpi$prices, nxpi$prices, "NXPI uses logarithmic prices")
    close(h$nxpi$median, nxpi$median, "NXPI trailing medians match base median loop")
    # The installed HighFreq initializes its first two dispersion outputs
    # to squared log prices. These unused startup values are not stats::mad.
    close(as.numeric(h$nxpi$mad)[-(1:2)], nxpi$mad[-(1:2)],
          "NXPI scaled trailing MAD matches base mad after startup")
    for (field in c("zscores", "signal", "positions", "returns", "pnl"))
      close(h$nxpi[[field]], nxpi[[field]], paste("NXPI independent base loop:", field))
    check(identical(dates(h$nxpi$pnl), nxpi_dates),
          "NXPI PnL dates follow its own available price history")
    close(independent_hampel_three(as.numeric(nxpi_price)[nxpi_valid]), nxpi$pnl,
          "Vectorized median-of-three reference agrees with transparent NXPI loop")
    close(h$nxpi_metrics, cbind(independent_ratios(nxpi$returns),
                              independent_ratios(nxpi$pnl)),
          "NXPI buy/hold and strategy performance ratios match independently")

    nxpi_cut <- length(nxpi$prices) %/% 2L
    prefix_nxpi <- independent_hampel(as.numeric(nxpi_price)[nxpi_valid][seq_len(nxpi_cut)])
    close(prefix_nxpi$pnl, as.numeric(h$nxpi$pnl)[seq_len(nxpi_cut)],
          "NXPI PnL prefix remains invariant when later data are absent")
    original_logs <- nxpi$prices
    changed_logs <- original_logs
    changed_logs[(nxpi_cut + 1L):length(changed_logs)] <-
      changed_logs[(nxpi_cut + 1L):length(changed_logs)] +
      seq(0.2, 0.5, length.out = length(changed_logs) - nxpi_cut)
    changed_median <- HighFreq::roll_mean(matrix(changed_logs), 3L,
                                          method = "nonparametric")
    changed_mad <- HighFreq::roll_var(matrix(changed_logs), 3L,
                                     method = "nonparametric")
    close(changed_median[seq_len(nxpi_cut), ],
          as.numeric(h$nxpi$median)[seq_len(nxpi_cut)],
          "Future stock-price perturbation preserves installed trailing medians")
    close(changed_mad[seq_len(nxpi_cut), ],
          as.numeric(h$nxpi$mad)[seq_len(nxpi_cut)],
          "Future stock-price perturbation preserves installed trailing MAD")

    cat("\nRecomputing every eligible stock from independent median-of-three arithmetic...\n")
    independent_pnls <- matrix(NA_real_, nrow(stock), ncol(stock),
                              dimnames = list(NULL, colnames(stock)))
    eligible <- logical(ncol(stock))
    for (j in seq_len(ncol(stock))) {
      valid <- which(is.finite(as.numeric(stock[, j])))
      if (length(valid) < 6L) next
      independent_pnls[valid, j] <- independent_hampel_three(stock[valid, j])
      eligible[j] <- TRUE
    }
    independent_pnls <- independent_pnls[, eligible, drop = FALSE]
    counts <- rowSums(is.finite(independent_pnls))
    usable <- counts > 0L
    independent_pnls <- independent_pnls[usable, , drop = FALSE]
    stock_dates <- stock_dates[usable]
    counts <- counts[usable]
    check(identical(dates(h$mean_pnl), stock_dates),
          "All-stock aggregation aligns by calendar date")
    close(h$available, counts, "Available-stock counts match independently aligned histories")
    close(h$mean_pnl, rowMeans(independent_pnls, na.rm = TRUE),
          "Every all-stock mean PnL matches independent date-aligned arithmetic")
    all_sharpe <- apply(independent_pnls, 2, independent_sharpe)
    ranking <- sort(all_sharpe, decreasing = TRUE)
    check(identical(names(h$sharpe), names(ranking)),
          "Full-sample stock Sharpe ranking matches independent universe calculation")
    close(h$sharpe, ranking, "Every individual-stock Sharpe matches independent PnLs")

    insample <- stock_dates <= as.Date("2016-12-31")
    outsample <- stock_dates >= as.Date("2017-01-01")
    check(!any(insample & outsample) && all(insample | outsample),
          "2016/2017 sample split is disjoint and exhaustive")
    is_all <- apply(independent_pnls[insample, , drop = FALSE], 2, independent_sharpe)
    os_all <- apply(independent_pnls[outsample, , drop = FALSE], 2, independent_sharpe)
    matched <- is.finite(is_all) & is.finite(os_all)
    check(identical(names(h$sharpis), names(is_all)[matched]) &&
            identical(names(h$sharpos), names(os_all)[matched]),
          "IS/OOS cohorts match the same finite-history stock symbols in the same order")
    close(h$sharpis, is_all[matched], "All matched in-sample Sharpes recompute independently")
    close(h$sharpos, os_all[matched], "All matched out-of-sample Sharpes recompute independently")
    ranked_is <- sort(is_all[is.finite(is_all)], decreasing = TRUE)
    check(identical(names(h$ranked_is), names(ranked_is)),
          "Saved selection ranking uses all valid in-sample stock histories")
    close(h$ranked_is, ranked_is, "In-sample selection ranking values are correct")
    expected_top <- head(names(ranked_is), 100L)
    check(identical(h$top_symbols, expected_top),
          "Top 100 are selected only from independently recomputed IS Sharpes")
    check(identical(dates(h$top_pnl), stock_dates),
          "Top-100 portfolio retains the correctly aligned calendar")
    close(h$top_pnl, rowMeans(independent_pnls[, expected_top, drop = FALSE], na.rm = TRUE),
          "Every top-100 portfolio PnL matches independent aligned arithmetic")

    # Recheck the optimized reference using slow median/mad loops for
    # multiple listing histories, including a selected and an unselected stock.
    selected_checks <- unique(c("NXPI", colnames(independent_pnls)[1],
                                 expected_top[1], tail(names(ranking), 1)))
    for (symbol in selected_checks) {
      column <- as.numeric(stock[, symbol])
      valid <- which(is.finite(column))
      direct <- independent_hampel(column[valid])
      close(independent_hampel_three(column[valid]), direct$pnl,
            paste("Transparent median/mad loop validates full history of", symbol))
    }
    close(h$correlation, cor(is_all[matched], os_all[matched]),
          "Reported IS/OOS correlation matches independent stock returns")
    independent_fit <- lm(os_all[matched] ~ is_all[matched])
    close(coef(h$regression), coef(independent_fit),
          "Scatter regression coefficients match independent IS/OOS calculations")
    independent_all <- rowMeans(independent_pnls, na.rm = TRUE)
    independent_top <- rowMeans(independent_pnls[, expected_top, drop = FALSE],
                                na.rm = TRUE)
    expected_portfolio_metrics <- t(vapply(seq_len(nrow(h$portfolio_metrics)), function(i) {
      row <- h$portfolio_metrics[i, ]
      x <- if (row$Portfolio == "All stocks") independent_all else independent_top
      keep <- switch(row$Period, Full = rep(TRUE, length(x)),
                      IS = insample, OOS = outsample)
      x <- x[keep]
      c(N = length(x), Sharpe = independent_sharpe(x),
        CumulativePnL = sum(x), AnnualizedVolatility = sqrt(252) * stats::sd(x))
    }, numeric(4)))
    close(as.matrix(h$portfolio_metrics[, c("N", "Sharpe", "CumulativePnL",
                                           "AnnualizedVolatility")]),
          expected_portfolio_metrics,
          "All portfolio-period sample sizes, Sharpes, cumulative PnLs and volatilities recompute")
    cat(sprintf("\nSUCCESS: %d independent checks passed.\n", checks))
    cat(sprintf("Verified %d SPY dates, %d stock dates, %d eligible stocks and %d matched IS/OOS stocks.\n",
                length(spy), length(stock_dates), ncol(independent_pnls), sum(matched)))
    cat("EMA causality checks exclude the package's two-observation startup; full-sample scaling is ex-post normalization.\n")
  }, error = function(e) {
    cat("\nVERIFICATION FAILED: ", conditionMessage(e), "\n", sep = "")
    stop(e)
  })
})
