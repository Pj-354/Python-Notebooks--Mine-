#Install packages

install.packages('dccmidas')
library(dccmidas)

install.packages('xts')
library(xts)

install.packages('rumidas')
library(rumidas)
library(tools)
library(readr)
library(dplyr)
library(lubridate)

#Read in data


US_indices_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Indices Returns 2005.csv')

US_monthly_variables <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/Monthly.csv')
                                 
#Ensure data classes are correct (date)

US_indices_rets <- US_indices_rets %>% 
  mutate(Date = as.Date(Date))

US_monthly_variables <- US_monthly_variables %>% 
  mutate(Date = as.Date(Date))

storage.mode(US_indices_rets$.SPX) <- 'numeric'

US_indices_rets <- xts(US_indices_rets, order.by = US_indices_rets$Date)
US_monthly_variables <- xts(US_monthly_variables, order.by = US_monthly_variables$Date)

# Combine Mixed Freq Data

SP <- US_indices_rets$.SPX 
rets_DJ <- US_indices_rets$.DJI
rets_NASDAQ <- US_indices_rets$.IXIC
rets_NASDAQ100 <- US_indices_rets$.NDX
rets_SP1500 <- US_indices_rets$.SPSUPX
rets_Canada <- US_indices_rets$.GSPTSE

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

#Setting up the for loop 

Midas_Models <- list()

for (nm in names(MV_list)) {
  cat('Fitting', nm, '...\n')
  
  Midas_Models[[nm]] <- try(
    ugmfit(
      model = 'GM',
      skew = 'YES',
      distribution = 'norm',
      daily_ret = SP,
      MV = MV_list[[nm]],
      K = 12
    ),
    silent = TRUE
  )
}

get_info <- function(model) {
  if (inherits(mod, 'try-error')) return (c(LL = NA, BIC = NA))
  c(LL = mod$loglik, 
    BIC = mod$ic$BIC)
}

info_tab <- t(vapply(Midas_Models, get_info, c(LL = NA, BIC = NA)))
print(info_tab)

#Setting up the Univarate Model

model <- ugmfit(
  model="GM",
  skew="YES",
  distribution ="std",
  SP,
  midasv9,
  K = 12)

model9_summary = summary.rumidas(model)

#DCC Model

model3 <- dcc_fit(
  r_t = list(SP, DJ),
  univ_model = 'GM_noskew',
  distribution = 'norm',
  MV = list(midasv7, midasv7),
  K = 12,
  corr_model = 'DCCMIDAS',
  N_c = 144, K_c = 144)



model3


dcc_midas_ <- list(midasv7)
SP_dcc <- list(SP)

str(SP_dcc)
str(dcc_midas_)



# SP and Midas Variables must be numeric, no NAs
storage.mode(SP) <- 'numeric'
storage.mode(MV) <- 'numeric'
storage.mode(mv1, mv2) <- 'numeric'

#storage mode loop

rets_names <- ls(pattern = "^rets_")

for (name in rets_names) {
  tmp <- get(name, envir = .GlobalEnv)
  storage.mode(tmp) <- 'numeric'
  assign(name, tmp, envir = .GlobalEnv)
}

mv_names <- ls(pattern = "^midasv")

for (name in mv_names) {
  tmp <- get(name, envir = .GlobalEnv)
  storage.mode(tmp) <- 'numeric'
  assign(name, tmp, envir = .GlobalEnv)
}
