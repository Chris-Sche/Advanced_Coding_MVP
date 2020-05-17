# =========================== INFORMATION ===========================

# Authors:    Patrik Münch, patrik.muench@student.unisg.ch
#             Christoph Schenker, christoph.schenker@student.unisg.ch
# Date:       16.07.2020
# Class:      Programming with advanced computer languages, Spring Semester 2020
# Professor:  Dr. Mario Silic

# =========================== REFERENCES ============================

# http://eclr.humanities.manchester.ac.uk/index.php/R_GARCH
# https://www.youtube.com/watch?v=8VXmRl5gzEU

# =========================== PREPARATION ===========================

#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
# Install missing packages for the code
list_of_packages <- c("xts", "lubridate", "Quandl",
                      "ggplot2", "tidyr", "tidyverse", "scales", "tseries",
                      "reshape2", "fBasics", "gridExtra", "rstudioapi",
                      "formatR")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load and attach packages
library(xts)
library(lubridate)
library(Quandl)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(scales)
library(tseries)
library(reshape2)
library(fBasics)
library(gridExtra)
library(rstudioapi)
library(formatR)

#remove current variables in R environment
rm(list = ls())

#from https://rdrr.io/cran/rstudioapi/man/file-dialogs.html
#make user select the directory with the input csv document
WorkingDir <- NULL

while(is.null(WorkingDir)){
  WorkingDir <- selectDirectory(
    caption = "Please select the directory in which the input file is saved",
    label = "Here is the input folder",
    path = getActiveProject()
  )
}
setwd(WorkingDir)

dev.off()


# ==================== DOWNLOAD AND PREPARE DATA ====================

# Set parameters                                 ### << INPUT <<
reb_int      <- "weekly" 
source       <- "csv" 
export_plot  <- TRUE

# Obtain stock data
ticker_csv   <- c("SAMSUNG",
                  "SASOL",
                  "DYCOM",
                  "NESTLE",
                  "NOVARTIS",
                  "APPLE",
                  "ROYALD",
                  "NAVISTAR",
                  "UBS")

# Define function to source data
source_data <- function(source) {
  toDate <- function(x) as.Date(x, origin = "1986-03-86", format = "%d.%m.%y")
  Time_series_zoo <- read.zoo("RS_Time series_v3.csv", header = TRUE, sep = ","
                              , FUN = toDate)
  Time_series_xts <- as.xts(Time_series_zoo)
  
  # Convert daily to weekly data
  Time_series_weekly_xts <- to.weekly(Time_series_xts, OHLC = FALSE)
  
  # Set rebalancing interval and pick stocks
  stock_prices <- Time_series_weekly_xts[, ticker_csv]
  return(stock_prices)
}
stock_prices <- source_data(source)

# Define function for calculating log returns
calc_log_returns <- function(stock_prices) {
  log_returns <- stock_prices
  for (i in 1:ncol(stock_prices)) {
    log_returns[, i] <- diff(log(stock_prices[, i]))
  }
  return(log_returns)
}
# Calculate log returns
log_returns  <- calc_log_returns(stock_prices)
log_ret_cl   <- log_returns["1998-01-01/2018-12-31"]
nr_cols      <- ncol(log_returns)
ticker_sort <- sort(ticker_csv)

# Input for the dates                            ### << INPUT <<
start_date   <- "2012-01-01"
port_span    <- 7 # in years
cali_years   <- 10 # in years
wpy          <- 52
folder       <- paste("Output/", year(start_date), "-", year(start_date) + port_span - 1, 
                      "_", cali_years, "cali_", reb_int, sep = "")

# Define function for calculating dates: This function also checks that port_start and cali_start have the same length and cuts the longer if not
calc_dates <- function(stock_prices, start_date, port_span, cali_years, reb_int) {
  index <- as.character(index(stock_prices))
  port_start_date <- as.Date(index[year(index) == year(start_date)][1])
  port_start_row  <- which(index == as.character(port_start_date))
  port_end_row    <- port_start_row + port_span * wpy -1
  port_end_date   <- as.Date(index[port_end_row])
  port_start      <- as.Date(index[port_start_row:port_end_row])
  port_rows       <- match(as.character(port_start), index)
  cali_start_row  <- port_start_row - wpy * cali_years
  cali_start_date <- as.Date(index[cali_start_row])
  cali_end_row    <- cali_start_row + port_span * wpy - 1
  cali_end_date   <- as.Date(index[cali_end_row])
  cali_start      <- as.Date(index[cali_start_row:cali_end_row])
  cali_rows       <- match(as.character(cali_start), index)
  all_start       <- as.Date(index[cali_start_row:port_end_row])
  if (length(port_start) == length(cali_start)) {
    return(list(all_start, port_start, cali_start, port_rows, cali_rows))
  } else {
    v <- data.frame(cali_start = length(cali_start), port_start = 
                      length(port_start))
    l <- abs(length(cali_start) - length(port_start))
    max <- apply(v, MARGIN = 1, max)
    if (max == length(cali_start)) {
      cali_start <- cali_start[-((length(cali_start) - l +
                                    1):(length(cali_start)))]
    } else {
      port_start <- port_start[-((length(port_start) - l + 1
      ):(length(port_start)))]
    }
    return(list(all_start, port_start, cali_start, port_rows, cali_rows))
  }
}

# Calculate dates
# Calculate dates
dates <- calc_dates(stock_prices, start_date, port_span, cali_years, reb_int)
all_start    <- dates[[1]]
port_start   <- dates[[2]]
cali_start   <- dates[[3]]
port_rows    <- dates[[4]]
cali_rows    <- dates[[5]]
# Calculates data subsets according to parameters
port_data    <- log_returns[port_start]
cali_data    <- log_returns[cali_start]
all_data     <- log_returns[all_start]


# ==================== Mean Variance =====================

# Define function to calculate the covariance matrix (required as input for portfolio optimization)
calc_all_cov_forecast <- function (log_returns, port_start, cali_rows, port_rows) {
  all_forecast <- list()
  all_cors <- list()
  for (i in 1:length(port_start)) {
    dataset <- log_returns[cali_rows[i]:port_rows[i], ]
    all_forecast[[i]] <- cov(dataset)
    all_cors[[i]] <- cor(dataset)
  }
  names(all_forecast) <- port_start
  return(list(all_forecast, all_cors))
}
# Calc covariance matrices
meanvar_output        <- calc_all_cov_forecast(log_returns, port_start, cali_rows,
                                               port_rows)
all_forecast_meanvar  <- meanvar_output[[1]]
fore_cor_meanvar      <- meanvar_output[[2]]


# ============================ PORTFOLIO ============================

# Define functions to calculate (all) portfolio weights based on goal of mean-variance 

calc_pf_weights <- function(a_cov_mat) {
  ones <- c(rep(1, nrow(a_cov_mat)))
  weights <- (solve(a_cov_mat) %*% ones) 
  weights <- weights / sum(weights)
  return(weights)
}
calc_all_pf_weights <- function(all_cov_matrices) {
  all_pf_weights <- list()
  for (i in 1:length(all_cov_matrices)) {
    all_pf_weights[[i]] <- calc_pf_weights(all_cov_matrices[[i]])
  }
  names(all_pf_weights) <- port_start
  return(all_pf_weights)
}

# Define functions to "calculate" equal portfolio weights

calc_pf_weights_eql <- function(a_cov_mat) {
  ones <- c(rep(1, nrow(a_cov_mat)))
  weights <- ((1 / ncol(log_returns)) %*% ones)
  weights <- weights / sum(weights)
  return(weights)
}
calc_all_pf_weights_eql <- function(all_cov_matrices) {
  all_pf_weights_eql <- list()
  for (i in 1:length(all_cov_matrices)) {
    all_pf_weights_eql[[i]] <- calc_pf_weights_eql(all_cov_matrices[[i]])
  }
  names(all_pf_weights_eql) <- port_start
  return(all_pf_weights_eql)
}

# Calculate portfolio weights of both portfolios
pf_weights_meanvar  <- calc_all_pf_weights(all_forecast_meanvar)
pf_weights_eql      <- calc_all_pf_weights_eql(all_forecast_meanvar)

# Define function to calc portfolio returns and give the output of a return time series
calc_port_returns <- function (port_data, pf_weights, reb_int) {
  port_ret <- port_data[, -c(1:ncol(port_data))]
  for (i in 1:nrow(port_ret)) {
    weights <- unlist(pf_weights[[i]])
    port_ret[i] <- as.numeric(as.numeric(port_data[i, ]) %*% as.numeric(weights))
  }
  return(port_ret)
}

# Calculate all portfolio returns
port_ret_meanvar   <- calc_port_returns(port_data, pf_weights_meanvar, reb_int)
port_ret_eql       <- calc_port_returns(port_data, pf_weights_eql, reb_int)

# Define function to calc portfolio returns and give the output of a return time series

calc_port_value <- function (port_data, ret_series_eql, ret_series_meanvar, start_value) {
  port_all <- port_data[, -c(1:ncol(port_data))]
  port_all$EQL <- start_value
  port_all$MEANVAR <- start_value
  for (i in 2:nrow(port_all)) {
    port_all$EQL[i] <- as.numeric(port_all$EQL[i - 1]) * as.numeric(exp(ret_series_eql[i]))
    port_all$MEANVAR[i] <- as.numeric(port_all$MEANVAR[i - 1]) * as.numeric(exp(ret_series_meanvar[i]))
  }
  return (port_all)
}

# Calculate the portfolio value based on the returns
port_all           <- calc_port_value(port_data, port_ret_eql, port_ret_meanvar, 100)
port_all$DIFF      <- port_all$EQL - port_all$MEANVAR

# Table output
ann_fac <- wpy
m <- matrix(c(mean(port_ret_eql) * ann_fac, 
              mean(port_ret_meanvar) * ann_fac, 
              sd(port_ret_eql) * sqrt(ann_fac), 
              sd(port_ret_meanvar) * sqrt(ann_fac)), 
            nrow = 2, byrow = TRUE)
colnames(m)        <- c("EQL", "MEANVAR")
row.names(m)       <- c("Annualised mean", "Annualised standard deviation")
if(dir.exists(folder) == FALSE) (dir.create(file.path(folder), recursive = TRUE))
write.csv(m, file = paste(folder, "/stats.csv", sep = ""))

# ============================ PLOTTING =============================

# -------------- Plot: Portfolio development --------------
p <- data.frame(Date = index(port_all), EQL = port_all$EQL, 
                MEANVAR = port_all$MEANVAR, DIFF = port_all$DIFF)
if (export_plot == TRUE) {pdf(file = paste(folder, "/port.pdf", sep = ""))}
ggplot(p) + 
  geom_bar(aes(x = Date, y = DIFF * max(max(p$EQL), max(p$MEANVAR)) / 50), 
           stat = "identity", fill = "gray", alpha = 0.8) + 
  geom_line(aes(x = Date, y = EQL, color = "EQL"), size = 0.8) + 
  geom_line(aes(x = Date, y = MEANVAR, color = "MEANVAR"), size = 0.8) + 
  scale_y_continuous(name = "Portfolio value", 
                     sec.axis = sec_axis(~ . * 50 / max(max(p$EQL), max(p$MEANVAR)), 
                                         name = "Difference"),
                     limits = c(min(p$DIFF) * max(max(p$EQL), max(p$MEANVAR)) /  50,
                                max(max(p$EQL), max(p$MEANVAR)))) + 
  scale_colour_manual(name = '', values = c('MEANVAR' = 'black', 'EQL' = 'red')) +
  labs(title = paste("Performance from ", year(start_date), "-",
                     year(start_date) + port_span - 1, ", ", 
                     cali_years, " years calibration, ", reb_int, 
                     " rebalancing", sep = "")) +
  theme(plot.title = element_text(size = 12))
dev.off()
# --------------- Plot: Weight development ----------------

Date <- as.Date(names(all_forecast_meanvar))
if (export_plot == TRUE) {pdf(file = paste(folder, "/weights_meanvar.pdf", 
                                           sep = ""))}
weight_mat_meanvar <- data.frame(matrix(NA, nrow = length(pf_weights_meanvar),
                                        ncol = length(ticker_sort)))
for (i in 1:nrow(weight_mat_meanvar)) {
  weight_mat_meanvar[i, ] <- unlist(pf_weights_meanvar[[i]])
}
colnames(weight_mat_meanvar) <- ticker_sort
weight_mat_meanvar$Date <- Date
weight_mat_meanvar_long <- gather(weight_mat_meanvar, Company, Weights,
                                  ticker_sort[1]:ticker_sort[nr_cols], 
                                  factor_key = T)
ggplot(weight_mat_meanvar_long, aes(x = Date, y = Weights)) + 
  geom_area(aes(fill = Company), position = 'stack') + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = paste("Minimum variance weights from ", year(start_date), "-", 
                     year(start_date) + port_span - 1, ", ", 
                     cali_years, " years calibration, ", reb_int, 
                     " rebalancing", sep = "")) +
  theme(plot.title = element_text(size = 12))
dev.off()
if (export_plot == TRUE) {pdf(file = paste(folder, "/weights_meanvar.pdf", 
                                           sep = ""))}
weight_mat_meanvar <- data.frame(matrix(NA, nrow = length(pf_weights_meanvar), 
                                        ncol = length(ticker_sort)))
for (i in 1:nrow(weight_mat_meanvar)) {
  weight_mat_meanvar[i, ] <- unlist(pf_weights_meanvar[[i]])
}
colnames(weight_mat_meanvar) <- ticker_sort
weight_mat_meanvar$Date <- Date
weight_mat_meanvar_long <- gather(weight_mat_meanvar, Company, Weights,
                                  ticker_sort[1]:ticker_sort[nr_cols], 
                                  factor_key = T)
ggplot(weight_mat_meanvar_long, aes(x = Date, y = Weights)) + 
  geom_area(aes(fill = Company), position = 'stack') + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = paste("Minimum variance weights (", year(start_date), "-", 
                     year(start_date) + port_span - 1, "), ", 
                     cali_years, " years calibration, ", reb_int, 
                     " rebalancing", sep = "")) +
  theme(plot.title = element_text(size = 12))
dev.off()

# ========================== END OF SCRIPT ==========================



