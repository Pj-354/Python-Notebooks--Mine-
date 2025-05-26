################################################################################

#Install packages

install.packages('dccmidas')
library(dccmidas)
library(rumidas)
install.packages('xts')
library(xts)

install.packages('rumidas')
library(rumidas)
library(tools)
library(readr)
library(dplyr)
library(lubridate)

install.packages('pbapply')
library(pbapply)
#Read in data


US_indices_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/US_Indices_2006_2025.csv')

US_monthly_variables <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Monthly.csv')

sector_indices <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Sector Returns 2006.csv')

Quarterly <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Quarterly.csv')

Monthly <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Monthly Variables v1.csv')

                                 
#Ensure data classes are correct (date)

Quarterly <- Quarterly %>% 
  mutate(Date = as.Date(Date))

Monthly <- Monthly %>% 
  mutate(Date = as.Date(Date))

US_indices_rets <- US_indices_rets %>% 
  mutate(Date = as.Date(Date))

US_monthly_variables <- US_monthly_variables %>% 
  mutate(Date = as.Date(Date))

sector_indices <- sector_indices %>%
  mutate(Date = as.Date(Date))

#Convert to XTS
Quarterly <- xts(Quarterly, order.by = Quarterly$Date)
Monthly <- xts(Monthly, order.by = Monthly$Date)
US_indices_rets <- xts(US_indices_rets, order.by = US_indices_rets$Date)
sector_indices <- xts(sector_indices, order.by = sector_indices$Date)
US_monthly_variables <- xts(US_monthly_variables, order.by = US_monthly_variables$Date)

# Combine Mixed Freq Data

SP <- US_indices_rets$.SPX 
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

storage.mode(SP) <- 'numeric'

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
mv1 <- US_monthly_variables$GPR
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

K <- 4

fit_one <- function(mat) {
  
  try(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = rets_Healthcare,
      mat, 
      K = K
    ),
    silent = FALSE
  )
}

#Model List

gm_models_HC <- pblapply(MV_list_Q, fit_one)

names(gm_models_DJ) <- names(MV_list_Q)

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
    ,
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
      daily_ret = SP,
      mat, 
      K = K
    ),
    error = identity
    
  )
}


gm_models_NASDAQ <- pblapply(MV_list_Q, fit_NASDAQ)
gm_models_NASDAQ100 <- pblapply(MV_list_Q, fit_NASDAQ100)
gm_models_SP1500 <- pblapply(MV_list_Q, fit_SP1500)
gm_models_Canada <- pblapply(MV_list_Q, fit_Canada)
gm_models_SP <- pblapply(MV_list_Q, fit_SP)


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

extracts <- lapply(gm_models_DJ, extract_items)
names(extracts) <- names(gm_models_DJ)

saveRDS(extracts,file = 'DJ_Quarterly_MidasRegression.RDS')

xas <- readRDS('SP_Quarterly_MidasRegression_pt1.RDS')
xas <- readRDS('SP_Quarterly_MidasRegression_pt2.RDS')
