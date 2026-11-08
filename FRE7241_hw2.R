#################################
### FRE7241 Homework #2 due at 6PM Tuesday November 10
#################################
# Max score 60pts

# Please write in this file the R code needed to perform the tasks below, 
# rename it to your_name_hw2.R
# and upload the file to Brightspace


############## Part I
# Summary: Simulate a volatility timing strategy
# using the inverse of the EMA volatility.

## Run the setup code below

library(rutils)

# The bid-ask spread for the most liquid ETFs
bidask <- 0.0001
# Calculate the SPY daily percentage returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
# Calculate the EMA volatility
volma <- HighFreq::run_var(retp, lambda=0.5)
volma <- sqrt(volma[, 2])

## End of setup code


# 1. (10pts)
# Calculate the position weights as the inverse of the EMA volatility.
# Lag the positions using the function rutils::lagit().

### Write your code here


# You should get the following outputs:
NROW(posv)
# [1] 8298
head(posv)
# [1] 0.0000 201.1293 304.0789 328.5427 473.3764 329.8407
tail(posv)
# [1] 215.9932 407.2572 292.0818 259.5863 214.6103 175.9076


# Calculate the PnLs minus the transaction costs.
# Scale the PnL volatility to that of SPY.

### Write your code here


# You should get the following outputs:
head(pnls)
#                     SPY
# 1993-02-01  0.000000e+00
# 1993-02-02  1.432596e-03
# 1993-02-03  1.101101e-02
# 1993-02-04  4.726830e-03
# 1993-02-05 -1.160927e-03
# 1993-02-08 -2.475144e-05
tail(pnls)
#                     SPY
# 2026-05-01  0.002001221
# 2026-05-04 -0.005187824
# 2026-05-05  0.008028929
# 2026-05-06  0.012352408
# 2026-05-07 -0.002280626
# 2026-05-08  0.004981521

# Calculate the Sharpe and Sortino ratios

### Write your code here


# You should get the following outputs:
#               SPY  Strategy
# Sharpe  0.5530825 0.6734175
# Sortino 0.6901985 0.8214143


# Plot dygraph of the strategy.
# Your plot should be similar to strat_voltiming_ema.png

### Write your code here


# 2. (10pts)
# Calculate the Sharpe ratios of the strategy for 
# the vector of lambda decay parameters lambdav.
# Use parallel computing with the package parallel.

library(parallel)  ##  Load package parallel

### Write your code here


# You should get the following outputs:
round(sharpev, 4)
# [1] 0.5842 0.6847 0.6975 0.6879 0.6734 0.6523 0.6345 0.6315 0.6496

# Calculate the largest Sharpe ratio and its lambda 
# decay parameter.
# You can use the functions max(), which.max().

### Write your code here


# You should get the following outputs:
# [1] 0.6975098
# [1] 0.3

# Plot the Sharpe ratios versus lambda decay parameters.
# Your plot should be similar to strat_voltiming_profile_ema.png

### Write your code here


############## Part II
# Summary: Simulate the Hampel strategy for single stocks.  

## Run the setup code below

library(rutils)

# Load the daily stock prices starting from 2006.
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
pricestock <- pricestock["2006/"]

## End of setup code


# 1. (10pts)
# Simulate the Hampel strategy for single stocks in pricestock.  
# Perform an lapply() loop over the columns of pricestock.
# Omit stocks with less than 2*lookb observations.

lookb <- 3

### Write your code here


# You should get outputs similar to:
class(pnls)
# [1] "list"
dim(pnls[[1]])
# [1] 2449    1

# Calculate the strategy Sharpe ratios and sort them in descending
# order.

### Write your code here


# You should get outputs similar to:
NROW(sharpev)
# [1] 1000
sharpev[1:10]
# KNX    CMCSA      SHW      DBX     AMCR      LIN     GTLB      VST     LYFT     TROW 
# 1.750951 1.728271 1.645614 1.332419 1.320659 1.285856 1.279433 1.261519 1.164916 1.161073 


# 2. (10pts)
# Calculate the mean PnLs for all the stocks.
# Omit the NA values in the rowMeans() function.

### Write your code here


# You should get outputs similar to:
class(pnlm)
# [1] "xts"
dim(pnlm)
# [1] 5119    1

# Plot dygraph of the mean PnLs for all the stocks.
# Your plot should be similar to strat_hampel_mean.png

### Write your code here


# Dygraphs plot the Hampel strategy for the stock NXPI.
# Your plot should be similar to strat_hampel_single.png

symboln <- "NXPI"

### Write your code here



# 3. (10pts)
# Calculate the in-sample Sharpe ratios for all stocks up to 2016.
# Calculate the out-of-sample Sharpe ratios after 2016.
# Some stocks don't have in-sample Sharpe ratios because they didn't
# exist before 2016.
# Select only the out-of-sample stocks that also have in-sample Sharpe
# ratios.

### Write your code here


# You should get outputs similar to:
NROW(sharpis)
# [1] 876
sharpis[1:10]
# MEDP         CSX         NBR        RGLD        TRIP        QCOM         JNJ         PWR         DDS 
# -0.63106754  0.77480438 -0.23822207  0.08738271 -0.60756204  1.07960436  0.40676496 -0.24269662 -0.36630075 
# MSI 
# 0.70949752 
NROW(sharpos)
# [1] 876
sharpos[1:10]
# MEDP         CSX         NBR        RGLD        TRIP        QCOM         JNJ         PWR         DDS 
# 0.43974996  0.81635127 -0.81614573  0.40935319 -0.31282879  0.35589633  0.28589710 -0.03399571  0.24651389 
# MSI 
# 0.01895394

# Draw a scatterplot of the in-sample versus out-of-sample Sharpe
# ratios and the regression line.
# Your plot should be similar to strat_hampel_scatter.png

### Write your code here


# Comment: 
# This shows that there is a persistence in the Sharpe ratios of the
# Hampel strategy for stocks.
# Stocks with large in-sample Sharpe ratios tend to have larger
# out-of-sample Sharpe ratios.


# 4. (10pts)
# Plot the mean PnLs for the 100 stocks with the largest in-sample
# Sharpe ratios.
# Your plot should be similar to strat_hampel_top.png

### Write your code here


# Comment: 
# This is an in-sample performance without accounting for transaction
# costs.
# Achieving such good performance in the out-of-sample period is not
# realistic.


