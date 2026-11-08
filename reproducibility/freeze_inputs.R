# Run once from repository root to freeze the exact course inputs used.
suppressPackageStartupMessages(library(rutils))
dir.create("data", showWarnings = FALSE)
stock_env <- new.env()
stock_file <- "../../06_Data/R_Data/sp500_prices.RData"
stopifnot(file.exists(stock_file))
load(stock_file, envir = stock_env)
inputs <- list(
  spy_returns = na.omit(get("SPY", rutils::etfenv$returns)),
  stock_prices = stock_env$pricestock["2006/"],
  provenance = list(
    spy = "Installed rutils::etfenv$returns$SPY (na.omit)",
    stock = "Course 06_Data/R_Data/sp500_prices.RData: pricestock['2006/']",
    rutils_commit = utils::packageDescription("rutils")$RemoteSha,
    stock_file_sha256 = digest::digest(file = stock_file, algo = "sha256")
  )
)
saveRDS(inputs, "data/hw2_inputs.rds", compress = "xz")
print(lapply(inputs[1:2], function(x) c(dim(x), as.character(range(index(x))))))
