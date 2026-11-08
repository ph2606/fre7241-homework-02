# Homework 2 frozen inputs

`hw2_inputs.rds` contains the exact inputs used by the submitted analysis:

| Object | Source | Coverage |
| --- | --- | --- |
| `spy_returns` | Installed `rutils::etfenv$returns`, SPY, missing rows removed | 8,352 rows, 1993-02-01 through 2026-04-07 |
| `stock_prices` | Supplied `06_Data/R_Data/sp500_prices.RData`, `pricestock["2006/"]` | 5,042 dates x 717 columns, through 2026-01-16 |
| `provenance` | Source package commit and original stock-file SHA-256 | Recorded when freezing inputs |

These are the supplied historical course inputs, with no online refresh or
fabricated extension to match the template. The original stock file contains
earlier prices and a second object; only the requested 2006-onward matrix is
included here. Missing individual-stock histories remain missing.

The completed R file uses this snapshot when it is present. For a standalone
R submission in the organized course folder, it can instead load installed
`rutils` data and the original stock file. That fallback can yield different
numbers if the local data have changed. The instructor's original absolute
stock-file path is also supported.

`reproducibility/freeze_inputs.R` records the snapshot from the organized course
folder. It is a one-time provenance step, separate from the ordinary build;
rebuilding the report does not change the inputs. Hashes and package versions
are recorded in `reproducibility/`.
