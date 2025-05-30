################################################################################

#Install packages

install.packages('dccmidas')
library(dccmidas)
library(rumidas)
install.packages('xts')
library(xts)
install.packages('purrr')
library(purrr)
install.packages('rumidas')
library(rumidas)
library(tools)
library(readr)
library(dplyr)
library(lubridate)

install.packages('pbapply')
library(pbapply)


################################################################################
K < - 12

# VOLATILITY

gm_models_NASDAQ <- pblapply(midas_matrix_volatility, fit_NASDAQ)
gm_models_NASDAQ100 <- pblapply(midas_matrix_volatility, fit_NASDAQ100)
gm_models_SP1500 <- pblapply(midas_matrix_volatility, fit_SP1500)
gm_models_Canada <- pblapply(midas_matrix_volatility, fit_Canada)
gm_models_SP <- pblapply(midas_matrix_volatility, fit_SP)

volatility_results <- list(NASDAQ = gm_models_NASDAQ, 
                           NASDAQ100 = gm_models_NASDAQ100,
                           SP1500 = gm_models_SP1500,
                           Canada = gm_models_Canada,
                           SP500 = gm_models_SP)


# SENTIMENT

gm_models_NASDAQ_sent <- pblapply(midas_matrix_sentiment, fit_NASDAQ)
gm_models_NASDAQ100_sent <- pblapply(midas_matrix_sentiment, fit_NASDAQ100)
gm_models_SP1500_sent <- pblapply(midas_matrix_sentiment, fit_SP1500)
gm_models_Canada_sent <- pblapply(midas_matrix_sentiment, fit_Canada)
gm_models_SP_sent <- pblapply(midas_matrix_sentiment, fit_SP)

sentiment_results <- list(NASDAQ = gm_models_NASDAQ_sent, 
                          NASDAQ100 = gm_models_NASDAQ100_sent,
                          SP1500 = gm_models_SP1500_sent,
                          Canada = gm_models_Canada_sent,
                          SP500 = gm_models_SP_sent)

# INDUSTRIAL

gm_models_NASDAQ_ind <- pblapply(midas_matrix_industrial, fit_NASDAQ)
gm_models_NASDAQ100_ind <- pblapply(midas_matrix_industrial, fit_NASDAQ100)
gm_models_SP1500_ind <- pblapply(midas_matrix_industrial, fit_SP1500)
gm_models_Canada_ind <- pblapply(midas_matrix_industrial, fit_Canada)
gm_models_SP_ind <- pblapply(midas_matrix_industrial, fit_SP)

industrial_results <- list(NASDAQ = gm_models_NASDAQ_ind, 
                           NASDAQ100 = gm_models_NASDAQ100_ind,
                           SP1500 = gm_models_SP1500_ind,
                           Canada = gm_models_Canada_ind,
                           SP500 = gm_models_SP_ind)

# ECONOMICS
gm_models_NASDAQ_econ <- pblapply(midas_matrix_economic, fit_NASDAQ)
gm_models_NASDAQ100_econ <- pblapply(midas_matrix_economic, fit_NASDAQ100)
gm_models_SP1500_econ <- pblapply(midas_matrix_economic, fit_SP1500)
gm_models_Canada_econ <- pblapply(midas_matrix_economic, fit_Canada)
gm_models_SP_econ <- pblapply(midas_matrix_economic, fit_SP)

econ_results <- list(NASDAQ = gm_models_NASDAQ_econ, 
                           NASDAQ100 = gm_models_NASDAQ100_econ,
                           SP1500 = gm_models_SP1500_econ,
                           Canada = gm_models_Canada_econ,
                           SP500 = gm_models_SP_econ)


# SPREAD
gm_models_NASDAQ_spread <- pblapply(midas_matrix_spread, fit_NASDAQ)
gm_models_NASDAQ100_spread <- pblapply(midas_matrix_spread, fit_NASDAQ100)
gm_models_SP1500_spread <- pblapply(midas_matrix_spread, fit_SP1500)
gm_models_Canada_spread <- pblapply(midas_matrix_spread, fit_Canada)
gm_models_SP_spread <- pblapply(midas_matrix_spread, fit_SP)

spread_results <- list(NASDAQ = gm_models_NASDAQ_spread, 
                           NASDAQ100 = gm_models_NASDAQ100_spread,
                           SP1500 = gm_models_SP1500_spread,
                           Canada = gm_models_Canada_spread,
                           SP500 = gm_models_SP_spread)



################################################################################


fit_NASDAQ <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_NASDAQ,
      mat, 
      K = K
    ),
    
    error = identity
  )
}

fit_NASDAQ100 <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_NASDAQ100,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_Canada <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Canada,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_SP1500 <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_SP1500,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_SP <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_SP,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_SP100 <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_SP100,
      mat,
      K = K
    ),
    error = identity
  )
}

################################################################################


data <- data.frame()
datalist <- data.frame()
error_vars <- c()


n <- 5
# Vol : n = 6
# Econ : n = 5
# Industrial : n = 6
# Spread : n = 4
# Sentiment : n = 23

nm_var

# Change 2 things => names(...) and model <- ...[[name]]
for (name in names(econ_results)) {
  
  
  model <- econ_results[[name]]
  
  
  
  for (v in 1:n) {
    
    tryCatch(
    nm_var <- paste(name, names(model[v])), error = identity)
    
    tryCatch(variance_ratio <- var(log(model[[v]]$est_lr_in_s)) / var(log(model[[v]]$est_vol_in_s **2)) * 100, error = identity)
    tryCatch(AIC <- model[[v]]$inf_criteria[[1]], error = identity)
    tryCatch(BIC <- model[[v]]$inf_criteria[[2]], error = identity)
    tryCatch(MSE <- model[[v]]$loss_in_s[[1]], error = identity)
    tryCatch(QLike <- model[[v]]$loss_in_s[[2]], error = identity)
    tryCatch(loglik <- model[[v]]$loglik, error = identity)
    
    model_diagnostics <- c(variance_ratio,QLike,MSE, AIC, BIC,loglik)
    
    
    tryCatch(data <- data.frame(model[[v]]$rob_coef_mat, 
                       c(nm_var, nm_var,nm_var,nm_var, nm_var, nm_var), 
                       model_diagnostics), 
             
             error = function(e) {
                         # record that this_name failed
                         error_vars <<- c(error_vars, nm_var)
                       })
    

    
    tryCatch(datalist <- rbind(data, datalist), error = identity)
    
  }
}

write.csv(datalist, 'C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Results - Economic - US Indices v1.csv')


