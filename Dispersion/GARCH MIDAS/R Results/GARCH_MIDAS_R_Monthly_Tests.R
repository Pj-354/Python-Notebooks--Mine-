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
#Read in data

# Returns 

US_indices_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - Major US Indices 2010-2025.csv')

US_indices_rets <- US_indices_rets %>% 
  mutate(Date = as.Date(Date))

US_sector_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - US Sector Indices 2010-2025.csv')

US_sector_rets <- US_sector_rets %>%
  mutate(Date = as.Date(Date))

EU_FinS_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - Financials.csv')


EU_RealEstate_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - RealEstate.csv')


# Indicators

Indicator_Volatility <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Volatility.csv')


Indicator_Volatility <- Indicator_Volatility %>%
  mutate(
    Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d")
  )

Indicator_Spread <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Spread.csv')

Indicator_Spread <- Indicator_Spread %>%
  mutate(
    Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d")
  )

Indicator_Sentiment <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Sentiment.csv')

Indicator_Sentiment <- Indicator_Sentiment %>%
  mutate(
    Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d")
  )

Indicator_Industrial <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Industrial.csv')

Indicator_Industrial <- Indicator_Industrial %>%
  mutate(
    Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d")
  )

Indicator_Economic <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Economic.csv')

Indicator_Economic <- Indicator_Economic %>%
  mutate(
    Date = as.Date(paste0(Date,'-01'), format = "%Y-%m-%d"))



#Convert to XTS
Indicator_Volatility <- xts(Indicator_Volatility, order.by = Indicator_Volatility$Date)
Indicator_Sentiment <- xts(Indicator_Sentiment, order.by = Indicator_Sentiment$Date)
Indicator_Economic <- xts(Indicator_Economic, order.by = Indicator_Economic$Date)
Indicator_Industrial <- xts(Indicator_Industrial, order.by = Indicator_Industrial$Date)
Indicator_Spread <- xts(Indicator_Spread, order.by = Indicator_Spread$Date)

US_indices_rets <- xts(US_indices_rets, order.by = US_indices_rets$Date)
US_sector_rets <- xts(US_sector_rets, order.by = US_sector_rets$Date)

# Combine Mixed Freq Data

rets_SP <- US_indices_rets$.SPX 
rets_DJ <- US_indices_rets$.DJI
rets_NASDAQ <- US_indices_rets$.IXIC
rets_NASDAQ100 <- US_indices_rets$.NDX
rets_SP1500 <- US_indices_rets$.SPSUPX
rets_Canada <- US_indices_rets$.GSPTSE

rets_SP100 <- sector_indices$.OEX 
rets_ConsumerDisc <- sector_indices$X.5SP25 
rets_Financial <- sector_indices$X.5SP40 #
rets_Healthcare <- sector_indices$X.5SP35
rets_Utilities <- sector_indices$X.5SP55
rets_RealEstate <- sector_indices$X.5SP60
rets_Materials <- sector_indices$X.5SP15
rets_CoreCommodities <- sector_indices$.TRCCBTR
rets_MidCap400 <- sector_indices$.MID
rets_SmallCap600 <- sector_indices$.SPCY
rets_VentureComposite <- sector_indices$.SPCDNX

#storage mode loop


rets_names <- ls(pattern = "^rets_")

for (name in rets_names) {
  tmp <- get(name, envir = .GlobalEnv)
  storage.mode(tmp) <- 'numeric'
  assign(name, tmp, envir = .GlobalEnv)
}

mv_names <- ls(pattern = "^midas")

for (name in mv_names) {
  tmp <- get(name, envir = .GlobalEnv)
  storage.mode(tmp) <- 'numeric'
  assign(name, tmp, envir = .GlobalEnv)
}

mv2_names <- ls(pattern = "^mv")

for (name in mv2_names) {
  tmp <- get(name, envir = .GlobalEnv)
  storage.mode(tmp) <- 'numeric'
  assign(name, tmp, envir = .GlobalEnv)
}

#Assign Macro Variables

#Volatility - Monthly
mv_vol_1 <- Indicator_Volatility$VIX
mv_vol_2 <- Indicator_Volatility$Skew
mv_vol_3 <- Indicator_Volatility$MOVE
mv_vol_4 <- Indicator_Volatility$Oil.Vol
mv_vol_5 <- Indicator_Volatility$VVIX
mv_vol_6 <- Indicator_Volatility$EqVol

mv_sent_1 <- Indicator_Sentiment$Overall.NFCI
mv_sent_2 <- Indicator_Sentiment$Leverage
mv_sent_3 <- Indicator_Sentiment$Risk
mv_sent_4 <- Indicator_Sentiment$Credit
mv_sent_5 <- Indicator_Sentiment$UMich
mv_sent_6 <- Indicator_Sentiment$BKK.Leading.Index
mv_sent_7 <- Indicator_Sentiment$CFNAI
mv_sent_8 <- Indicator_Sentiment$St.Louis.Stress.Index
mv_sent_9 <- Indicator_Sentiment$US.EPU
mv_sent_10 <- Indicator_Sentiment$Global.GPR
mv_sent_11 <- Indicator_Sentiment$USA.GPR
mv_sent_12 <- Indicator_Sentiment$Saudi.GPR

Indicator_Industrial <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Industrial.csv')

Indicator_Industrial <- Indicator_Industrial %>%
  mutate(
    Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d")
  )



mv_industrial_1 <- Indicator_Industrial$Global.Supply.Chain.Pressure.Index
mv_industrial_2 <- Indicator_Industrial$Industrial.Production
mv_industrial_3 <- Indicator_Industrial$New.Orders
mv_industrial_4 <- Indicator_Industrial$PPI
mv_industrial_5 <- Indicator_Industrial$Industrial.Production..Manufacturing.Only.
mv_industrial_6 <- Indicator_Industrial$Global.Price.of.Industrial.Materials

midas_matrix_industrial <- map(
  Industrial_Columns, 
  ~ mv_into_mat(
    x = rets_SP, 
    mv = .x, 
    K = 12,
    type = 'monthly')
)

Indicator_Economic <- xts(Indicator_Economic[ , setdiff(names(Indicator_Economic), "Date")],
                          order.by = Indicator_Economic$Date)

mv_economic_1 <- Indicator_Economic$value
mv_economic_2 <- Indicator_Economic$Unemployment.Rate
mv_economic_3 <- Indicator_Economic$Inflation.Rate
mv_economic_4 <- Indicator_Economic$Number.of.Car.Sales
mv_economic_5 <- Indicator_Economic$Electricity.Price.CPI.in.US.Cities

mv_spread_1 <- Indicator_Spread$T10.2.yield.curve..US.
mv_spread_2 <- Indicator_Spread$T10.3.yield.curve..US.
mv_spread_3 <- Indicator_Spread$Default.Spread..BofA.High.Yield.OAS.
mv_spread_4 <- Indicator_Spread$Defaut.Spread..BofA.EU.High.Yield.

midasv_vol_1 <- mv_into_mat(x = rets_SP, mv = mv_vol_1, K = 12, type = 'monthly')
midasv_vol_2 <- mv_into_mat(x = rets_SP, mv = mv_vol_2, K = 12, type = 'monthly')
midasv_vol_3 <- mv_into_mat(x = rets_SP, mv = mv_vol_3, K = 12, type = 'monthly')
midasv_vol_4 <- mv_into_mat(x = rets_SP, mv = mv_vol_4, K = 12, type = 'monthly')
midasv_vol_5 <- mv_into_mat(x = rets_SP, mv = mv_vol_5, K = 12, type = 'monthly')
midasv_vol_6 <- mv_into_mat(x = rets_SP, mv = mv_vol_6, K = 12, type = 'monthly')

midasv_sent_1 <- mv_into_mat(x = rets_SP, mv = mv_sent_1, K = 12, type = 'monthly')
midasv_sent_2 <- mv_into_mat(x = rets_SP, mv = mv_sent_2, K = 12, type = 'monthly')
midasv_sent_3 <- mv_into_mat(x = rets_SP, mv = mv_sent_3, K = 12, type = 'monthly')
midasv_sent_4 <- mv_into_mat(x = rets_SP, mv = mv_sent_4, K = 12, type = 'monthly')
midasv_sent_5 <- mv_into_mat(x = rets_SP, mv = mv_sent_5, K = 12, type = 'monthly')
midasv_sent_6 <- mv_into_mat(x = rets_SP, mv = mv_sent_6, K = 12, type = 'monthly')
midasv_sent_7 <- mv_into_mat(x = rets_SP, mv = mv_sent_7, K = 12, type = 'monthly')
midasv_sent_8 <- mv_into_mat(x = rets_SP, mv = mv_sent_8, K = 12, type = 'monthly')
midasv_sent_9 <- mv_into_mat(x = rets_SP, mv = mv_sent_9, K = 12, type = 'monthly')
midasv_sent_10 <- mv_into_mat(x = rets_SP, mv = mv_sent_10, K = 12, type = 'monthly')
midasv_sent_11 <- mv_into_mat(x = rets_SP, mv = mv_sent_11, K = 12, type = 'monthly')
midasv_sent_12 <- mv_into_mat(x = rets_SP, mv = mv_sent_12, K = 12, type = 'monthly')

Indicator_Volatility <- xts(Indicator_Volatility[ , setdiff(names(Indicator_Volatility), "Date")],
               order.by = Indicator_Volatility$Date)

Indicator_Sentiment <- xts(Indicator_Sentiment[ , setdiff(names(Indicator_Sentiment), "Date")],
                order.by = Indicator_Sentiment$Date)

Indicator_Industrial  <- xts(Indicator_Industrial[ , setdiff(names(Indicator_Industrial), "Date")],
                order.by = Indicator_Industrial$Date)



Indicator_Spread <- xts(Indicator_Spread[ , setdiff(names(Indicator_Spread), "Date")],
                order.by = Indicator_Spread$Date)



# Trying to use a for loop to create matrices for Ind, Economic and Spread Indicators

Volatility_Columns <- list(
  VIX    = mv_vol_1,
  Skew   = mv_vol_2,
  MOVE   = mv_vol_3,
  OilVol = mv_vol_4,
  VVIX   = mv_vol_5,
  EqVol  = mv_vol_6
)

# Sentiment indicators
Sentiment_Columns <- list(
  NFCI       = mv_sent_1,
  Leverage   = mv_sent_2,
  Risk       = mv_sent_3,
  Credit     = mv_sent_4,
  UMich      = mv_sent_5,
  BKKIndex   = mv_sent_6,
  CFNAI      = mv_sent_7,
  STLStress  = mv_sent_8,
  US_EPU     = mv_sent_9,
  GlobalGPR  = mv_sent_10,
  USA_GPR    = mv_sent_11,
  Saudi_GPR  = mv_sent_12
)

# Industrial indicators
Industrial_Columns <- list(
  GSCPI        = mv_industrial_1,  # Global Supply‐Chain Pressure Index
  IndProd      = mv_industrial_2,  # Industrial Production
  NewOrders    = mv_industrial_3,  # New Orders
  PPI          = mv_industrial_4,  # Producer Price Index
  IndProdMfg   = mv_industrial_5,  # Manufacturing‐only Prod.
  MatPrices    = mv_industrial_6   # Price of Industrial Materials
)

# Economic indicators
Economic_Columns <- list(
  EconValue    = mv_economic_1,  # whichever “value” represents
  UnempRate    = mv_economic_2,
  InflRate     = mv_economic_3,
  CarSales     = mv_economic_4,
  ElecPriceCPI = mv_economic_5   # Electricity Price CPI
)

# Spread indicators
Spread_Columns <- list(
  T10_2YC   = mv_spread_1,  # 10–2yr yield curve US
  T10_3YC   = mv_spread_2,  # 10–3yr yield curve US
  HY_OAS    = mv_spread_3,  # High‐yield OAS (BofA)
  EU_HY_OAS = mv_spread_4   # EU High‐yield OAS (BofA)
)

######### LOOP ##############

midas_matrix_volatility <- map(
  Volatility_Columns, 
  ~ mv_into_mat(
    x = rets_SP, 
    mv = .x, 
    K = 12,
    type = 'monthly')
)

midas_matrix_sentiment <- map(
  Sentiment_Columns, 
  ~ mv_into_mat(
    x = rets_SP, 
    mv = .x, 
    K = 12,
    type = 'monthly')
)

midas_matrix_economic <- map(
  Economic_Columns, 
  ~ mv_into_mat(
    x = rets_SP, 
    mv = .x, 
    K = 12,
    type = 'monthly')
)



midas_matrix_spread <- map(
  Spread_Columns, 
  ~ mv_into_mat(
    x = rets_SP, 
    mv = .x, 
    K = 12,
    type = 'monthly')
      )









###
mv2 <- US_monthly_variables$GEPU_current
mv3 <- US_monthly_variables$New.Orders
mv4 <- US_monthly_variables$UMich.Sentiment
mv5 <- US_monthly_variables$Unemployment.Rate
mv6 <- US_monthly_variables$Citi.Suprise
mv7 <- US_monthly_variables$VIX
mv8 <- US_monthly_variables$VXN.NASDAQ
mv9 <- US_monthly_variables$X2.10.Spread
mv10 <- US_monthly_variables$High.Yield.Spread
mv11 <- US_monthly_variables$BKK.Leading.Index
mv12 <- US_monthly_variables$National.Activity.Index
mv13 <- US_monthly_variables$Equity.Vol.Measure
mv14 <- US_monthly_variables$St.Louis.Stress.Index

#Assign Quarterly Macro Variables

mv_q1 <- Quarterly$Housing.Start
mv_q2 <- Quarterly$Industrial.Production.Index
mv_q3 <- Quarterly$Core.Inflation
mv_q4 <- Quarterly$PPI
mv_q5 <- Quarterly$GDP
mv_q6 <- Quarterly$UMich.Sentiment
mv_q7 <- Quarterly$St.Louis.Stress.Index
mv_q8 <- Quarterly$GEPU_current
mv_q9 <- Quarterly$GPR
mv_q10 <- Quarterly$BKK.Leading.Index
mv_q11 <- Quarterly$Unemployment.Rate
mv_q12 <- Quarterly$CFNAI
mv_q13 <- Quarterly$VIX
mv_q14 <- Quarterly$VXN.NASDAQ
mv_q15 <- Quarterly$Term.Spread
mv_q16 <- Quarterly$New.Orders
mv_q17 <- Quarterly$High.Yield.Spread
mv_q18 <- Quarterly$Equity.Vol.Measure
mv_q19 <- Quarterly$Recession.Spread
mv_q20 <- Quarterly$.CESIUSD
mv_q21 <- Quarterly$.MOVE

MV_list_Q <- list(HousingStart = midasq1,
                  IndProd = midasq2,
                  CoreInflation = midasq3,
                  PPI = midasq4,
                  GDP = midasq5,
                  UMich = midasq6,
                  StLouisStress = midasq7,
                  GEPU = midasq8,
                  GPR = midasq9,
                  BKK = midasq10,
                  Unemployment = midasq11,
                  CFNAI = midasq12,
                  VIX = midasq13,
                  VXN = midasq14,
                  TermSpread = midasq15,
                  NewOrders = midasq16,
                  DefaultSpread = midasq17,
                  EquityVolMeasure = midasq18,
                  RecessionSpread = midasq19,
                  CitiSurprise = midasq20,
                  MOVE = midasq21,
                  VXN = midasq14)

#Construct Midas Variable List
midasv1 <- mv_into_mat(x = SP, mv = mv1, K = 12, type = 'monthly')
midasv2 <- mv_into_mat(x = SP, mv = mv2, K = 12, type = 'monthly')
midasv3 <- mv_into_mat(x = SP, mv = mv3, K = 12, type = 'monthly')
midasv4 <- mv_into_mat(x = SP, mv = mv4, K = 12, type = 'monthly')
midasv5 <- mv_into_mat(x = SP, mv = mv5, K = 12, type = 'monthly')
midasv6 <- mv_into_mat(x = SP, mv = mv6, K = 12, type = 'monthly')
midasv7 <- mv_into_mat(x = SP, mv = mv7, K = 12, type = 'monthly')
midasv8 <- mv_into_mat(x = SP, mv = mv8, K = 12, type = 'monthly')
midasv9 <- mv_into_mat(x = SP, mv = mv9, K = 12, type = 'monthly')
midasv10 <- mv_into_mat(x = SP, mv = mv10, K = 12, type = 'monthly')
midasv11 <- mv_into_mat(x = SP, mv = mv11, K = 12, type = 'monthly')
midasv12 <- mv_into_mat(x = SP, mv = mv12, K = 12, type = 'monthly')
midasv13 <- mv_into_mat(x = SP, mv = mv13, K = 12, type = 'monthly')
midasv14 <- mv_into_mat(x = SP, mv = mv14, K = 12, type = 'monthly')

#For Quarterly


periodicity(mv_q1)
periodicity(SP)

midasq1 <- mv_into_mat(x = SP, mv = mv_q1, K = 4, type = 'quarterly')
midasq2 <- mv_into_mat(x = SP, mv = mv_q2, K = 4, type = 'quarterly')
midasq3 <- mv_into_mat(x = SP, mv = mv_q3, K = 4, type = 'quarterly')
midasq4 <- mv_into_mat(x = SP, mv = mv_q4, K = 4, type = 'quarterly')
midasq5 <- mv_into_mat(x = SP, mv = mv_q5, K = 4, type = 'quarterly')
midasq6 <- mv_into_mat(x = SP, mv = mv_q6, K = 4, type = 'quarterly')
midasq7 <- mv_into_mat(x = SP, mv = mv_q7, K = 4, type = 'quarterly')
midasq8 <- mv_into_mat(x = SP, mv = mv_q8, K = 4, type = 'quarterly')
midasq9 <- mv_into_mat(x = SP, mv = mv_q9, K = 4, type = 'quarterly')
midasq10 <- mv_into_mat(x = SP, mv = mv_q10, K = 4, type = 'quarterly')
midasq11 <- mv_into_mat(x = SP, mv = mv_q11, K = 4, type = 'quarterly')
midasq12 <- mv_into_mat(x = SP, mv = mv_q12, K = 4, type = 'quarterly')
midasq13 <- mv_into_mat(x = SP, mv = mv_q13, K = 4, type = 'quarterly')
midasq14 <- mv_into_mat(x = SP, mv = mv_q14, K = 4, type = 'quarterly')
midasq15 <- mv_into_mat(x = SP, mv = mv_q15, K = 4, type = 'quarterly')
midasq16 <- mv_into_mat(x = SP, mv = mv_q16, K = 4, type = 'quarterly')
midasq17 <- mv_into_mat(x = SP, mv = mv_q17, K = 4, type = 'quarterly')
midasq18 <- mv_into_mat(x = SP, mv = mv_q18, K = 4, type = 'quarterly')
midasq19 <- mv_into_mat(x = SP, mv = mv_q19, K = 4, type = 'quarterly')
midasq20 <- mv_into_mat(x = SP, mv = mv_q20, K = 4, type = 'quarterly')
midasq21 <- mv_into_mat(x = SP, mv = mv_q21, K = 4, type = 'quarterly')

# For Sector



################################################################################

#Put in a list

MV_list <- list(GPR = midasv1,
                GEP = midasv2,
                NewOrders = midasv3,
                UMich = midasv4,
                Unemployment = midasv5,
                CitiSuprise = midasv6,
                VIX = midasv7,
                VNX = midasv8,
                RecessionSpread = midasv9,
                HighYieldSpread = midasv10,
                BKK = midasv11,
                NAI = midasv12,
                EquityVol = midasv13,
                StLouisStress = midasv14)


MV_list_Q <- list(HousingStart = midasq1,
                  IndProd = midasq2,
                  CoreInflation = midasq3,
                  PPI = midasq4,
                  GDP = midasq5,
                  UMich = midasq6,
                  StLouisStress = midasq7,
                  GEPU = midasq8,
                  GPR = midasq9,
                  BKK = midasq10,
                  Unemployment = midasq11,
                  CFNAI = midasq12,
                  VIX = midasq13,
                  VXN = midasq14,
                  TermSpread = midasq15,
                  NewOrders = midasq16,
                  DefaultSpread = midasq17,
                  EquityVolMeasure = midasq18,
                  RecessionSpread = midasq19,
                  CitiSurprise = midasq20,
                  MOVE = midasq21
                  )


#Setting up the for loop 

K <- 12

fit_one <- function(mat) {
  
  try(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_NASDAQ,
      midasv_vol_1, 
      K = K
    ),
    silent = FALSE
  )
}

#Model List


################################################################################
K < - 12



gm_models_NASDAQ <- pblapply(Volatility_Indicator_, fit_NASDAQ)
gm_models_NASDAQ100 <- pblapply(Volatility_Indicator_, fit_NASDAQ100)
gm_models_SP1500 <- pblapply(Volatility_Indicator_, fit_SP1500)
gm_models_Canada <- pblapply(Volatility_Indicator_, fit_Canada)
gm_models_SP <- pblapply(Volatility_Indicator_, fit_SP)

gm_models_NASDAQ_sent <- pblapply(Sentiment_Indicator_, fit_NASDAQ)
gm_models_NASDAQ100_sent <- pblapply(Sentiment_Indicator_, fit_NASDAQ100)
gm_models_SP1500_sent <- pblapply(Sentiment_Indicator_, fit_SP1500)
gm_models_Canada_sent <- pblapply(Sentiment_Indicator_, fit_Canada)
gm_models_SP_sent <- pblapply(Sentiment_Indicator_, fit_SP)

sentiment_results <- list(NASDAQ = gm_models_NASDAQ_sent, 
                          NASDAQ100 = gm_models_NASDAQ100_sent,
                          SP1500 = gm_models_SP1500_sent,
                          Canada = gm_models_Canada_sent,
                          SP500 = gm_models_SP_sent
                          )

volatility_results <- list(NASDAQ = gm_models_NASDAQ, 
                          NASDAQ100 = gm_models_NASDAQ100,
                          SP1500 = gm_models_SP1500,
                          Canada = gm_models_Canada,
                          SP500 = gm_models_SP
                          )



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
n <- 6

for (name in names(volatility_results)) {
  
  
  model <- volatility_results[[name]]
  
  
  
  for (v in 1:n) {
    
    tryCatch(
    nm_var <- paste(name, names(Volatility_Indicator[v])), error = identity)
    
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

write.csv(datalist, 'C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Results - Volatility - US Indices v1.csv')


indices_list <- list(rets_NASDAQ, rets_NASDAQ100, rets_SP1500, rets_Canada)

sector_list <- list(rets_SP100, 
                    rets_ConsumerDisc, 
                    rets_Financial ,
                    rets_Healthcare,
                    rets_Utilities ,
                    rets_RealEstate,
                    rets_Materials ,
                    rets_CoreCommodities,
                    rets_MidCap400 ,
                    rets_SmallCap600, 
                    rets_VentureComposite)

Results <- list()

for (i in indices_list) {
  
  Results[[i]] <- pblapply(MV_list_Q,function(mat) {
    
    try(
      ugmfit(
        model = 'GM',
        skew = 'YES',
        distribution = 'norm',
        daily_ret = i,
        mat, 
        K = K
      ),
      silent = FALSE
    )
  
  })}
    
################################################################################

#Summary Lists

gm_sum <- lapply(gm_models_Q2, summary)
names(gm_sum) <- names(MV_list_Q2)

sent_results <- pblapply(sentiment_results, extract_items)

extract_items <- function(mod) {
  list(
    rob_coef_mat  = mod$rob_coef_mat,
    info_criteria = mod$inf_criteria,
    loss_in_s     = mod$loss_in_s,
    est_vol_in_s  = mod$est_vol_in_s,
    est_lr_in_s   = mod$est_lr_in_s,
    periods = mod$period,
    observations = mod$obs,
    loglikelihood = mod$loglik)
}

Sentiment_Indicator_
sentiment_results[1]
