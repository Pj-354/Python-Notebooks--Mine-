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

K <- 12

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

############################# SECTOR LOOP ######################################

# VOLATILITY

gm_models_ConsumerDisc <- pblapply(midas_matrix_volatility, fit_ConsumerDisc)
gm_models_Financial <- pblapply(midas_matrix_volatility, fit_Financial)
gm_models_Healthcare <- pblapply(midas_matrix_volatility, fit_Healthcare)
gm_models_Materials <- pblapply(midas_matrix_volatility, fit_Materials)
gm_models_Midcap <- pblapply(midas_matrix_volatility, fit_Midcap)
gm_models_SmallCap <- pblapply(midas_matrix_volatility, fit_SmallCap)
gm_models_RealEstate <- pblapply(midas_matrix_volatility, fit_RealEstate)
gm_models_Utilities <- pblapply(midas_matrix_volatility, fit_SmallCap)

volatility_results <- list(Consumer = gm_models_ConsumerDisc, 
                           Financial = gm_models_Financial,
                           Healthcare = gm_models_Healthcare,
                           Materials = gm_models_Materials,
                           MidCap = gm_models_Midcap,
                           SmallCap = gm_models_SmallCap,
                           RealEstate = gm_models_RealEstate,
                           Utilities = gm_models_Utilities)

# SENTIMENT SECTOR

gm_models_ConsumerDisc_sent <- pblapply(midas_matrix_sentiment, fit_ConsumerDisc)
gm_models_Financial_sent <- pblapply(midas_matrix_sentiment, fit_Financial)
gm_models_Healthcare_sent <- pblapply(midas_matrix_sentiment, fit_Healthcare)
gm_models_Materials_sent <- pblapply(midas_matrix_sentiment, fit_Materials)
gm_models_Midcap_sent <- pblapply(midas_matrix_sentiment, fit_Midcap)
gm_models_SmallCap_sent <- pblapply(midas_matrix_sentiment, fit_SmallCap)
gm_models_RealEstate_sent <- pblapply(midas_matrix_sentiment, fit_RealEstate)
gm_models_Utilities_sent <- pblapply(midas_matrix_sentiment, fit_SmallCap)

sentiment_results <- list(Consumer = gm_models_ConsumerDisc_sent, 
                          Financial = gm_models_Financial_sent,
                          Healthcare = gm_models_Healthcare_sent,
                          Materials = gm_models_Materials_sent,
                          MidCap = gm_models_Midcap_sent,
                          SmallCap = gm_models_SmallCap_sent,
                          RealEstate = gm_models_RealEstate_sent,
                          Utilities = gm_models_Utilities_sent)

# INDUSTRIAL SECTOR 

gm_models_Consumer_ind  <- pblapply(midas_matrix_industrial, fit_ConsumerDisc)
gm_models_Financial_ind <- pblapply(midas_matrix_industrial, fit_Financial)
gm_models_Healthcare_ind<- pblapply(midas_matrix_industrial, fit_Healthcare)
gm_models_Materials_ind <- pblapply(midas_matrix_industrial, fit_Materials)
gm_models_MidCap_ind    <- pblapply(midas_matrix_industrial, fit_Midcap)
gm_models_SmallCap_ind  <- pblapply(midas_matrix_industrial, fit_SmallCap)
gm_models_RealEstate_ind<- pblapply(midas_matrix_industrial, fit_RealEstate)
gm_models_Utilities_ind <- pblapply(midas_matrix_industrial, fit_Utilities)

industrial_results <- list(
  Consumer   = gm_models_Consumer_ind,
  Financial  = gm_models_Financial_ind,
  Healthcare = gm_models_Healthcare_ind,
  Materials  = gm_models_Materials_ind,
  MidCap     = gm_models_MidCap_ind,
  SmallCap   = gm_models_SmallCap_ind,
  RealEstate = gm_models_RealEstate_ind,
  Utilities  = gm_models_Utilities_ind
)

# ECON SECTOR

gm_models_Consumer_eco  <- pblapply(midas_matrix_economic, fit_ConsumerDisc)
gm_models_Financial_eco <- pblapply(midas_matrix_economic, fit_Financial)
gm_models_Healthcare_eco<- pblapply(midas_matrix_economic, fit_Healthcare)
gm_models_Materials_eco <- pblapply(midas_matrix_economic, fit_Materials)
gm_models_MidCap_eco    <- pblapply(midas_matrix_economic, fit_Midcap)
gm_models_SmallCap_eco  <- pblapply(midas_matrix_economic, fit_SmallCap)
gm_models_RealEstate_eco<- pblapply(midas_matrix_economic, fit_RealEstate)
gm_models_Utilities_eco <- pblapply(midas_matrix_economic, fit_Utilities)

econ_results <- list(
  Consumer   = gm_models_Consumer_eco,
  Financial  = gm_models_Financial_eco,
  Healthcare = gm_models_Healthcare_eco,
  Materials  = gm_models_Materials_eco,
  MidCap     = gm_models_MidCap_eco,
  SmallCap   = gm_models_SmallCap_eco,
  RealEstate = gm_models_RealEstate_eco,
  Utilities  = gm_models_Utilities_eco
)

# SPREAD SECTOR
gm_models_Consumer_spread  <- pblapply(midas_matrix_spread, fit_ConsumerDisc)
gm_models_Financial_spread <- pblapply(midas_matrix_spread, fit_Financial)
gm_models_Healthcare_spread<- pblapply(midas_matrix_spread, fit_Healthcare)
gm_models_Materials_spread <- pblapply(midas_matrix_spread, fit_Materials)
gm_models_MidCap_spread    <- pblapply(midas_matrix_spread, fit_Midcap)
gm_models_SmallCap_spread  <- pblapply(midas_matrix_spread, fit_SmallCap)
gm_models_RealEstate_spread<- pblapply(midas_matrix_spread, fit_RealEstate)
gm_models_Utilities_spread <- pblapply(midas_matrix_spread, fit_Utilities)

spread_results <- list(
  Consumer   = gm_models_Consumer_spread,
  Financial  = gm_models_Financial_spread,
  Healthcare = gm_models_Healthcare_spread,
  Materials  = gm_models_Materials_spread,
  MidCap     = gm_models_MidCap_spread,
  SmallCap   = gm_models_SmallCap_spread,
  RealEstate = gm_models_RealEstate_spread,
  Utilities  = gm_models_Utilities_spread
)

fit_ConsumerDisc <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_ConsumerDisc,
      mat, 
      K = K
    ),
    
    error = identity
  )
}

fit_Financial <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Financial,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_Materials <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Materials,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_Healthcare <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Healthcare,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_RealEstate <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_RealEstate,
      mat, 
      K = K
    ),
    error = identity
    
  )
}

fit_SmallCap <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_SmallCap600,
      mat,
      K = K
    ),
    error = identity
  )
}

fit_Utilities <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Utilities,
      mat,
      K = K
    ),
    error = identity
  )
}

fit_VentureComp <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_VentureComposite,
      mat,
      K = K
    ),
    error = identity
  )
}

fit_Midcap <- function(mat) {
  
  tryCatch(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_MidCap400,
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


n <- 23
# Vol : n = 6
# Econ : n = 5
# Industrial : n = 6
# Spread : n = 4
# Sentiment : n = 23



# Change 2 things => names(...) and model <- ...[[name]]
for (name in names(sentiment_results)) {
  
  
  model <- sentiment_results[[name]]
  
  
  
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

write.csv(datalist, '/Users/phillip/R Results B/Results - Sentiment - US Indice.csv')


