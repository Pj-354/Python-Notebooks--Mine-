
################################# PACKAGES #####################################

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


################################# VOLATILITY ###################################
Indicator_Volatility <- read.csv(
  '/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Volatility.csv'
)
Indicator_Volatility <- Indicator_Volatility %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
Indicator_Volatility <- xts(
  Indicator_Volatility[ , setdiff(names(Indicator_Volatility), "Date")],
  order.by = Indicator_Volatility$Date
)

mv_vol_1 <- Indicator_Volatility$VIX
mv_vol_2 <- Indicator_Volatility$Skew
mv_vol_3 <- Indicator_Volatility$MOVE
mv_vol_4 <- Indicator_Volatility$Oil.Vol
mv_vol_5 <- Indicator_Volatility$VVIX
mv_vol_6 <- Indicator_Volatility$EqVol

Volatility_Columns <- list(
  VIX    = mv_vol_1,
  Skew   = mv_vol_2,
  MOVE   = mv_vol_3,
  OilVol = mv_vol_4,
  VVIX   = mv_vol_5,
  EqVol  = mv_vol_6
)

midas_matrix_volatility <- map(
  Volatility_Columns,
  ~ mv_into_mat(x = rets_Financial, mv = .x, K = 12, type = 'monthly')
)

################################# SPREAD #######################################

Indicator_Spread <- read.csv(
  '/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Spread.csv'
)
Indicator_Spread <- Indicator_Spread %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
Indicator_Spread <- xts(
  Indicator_Spread[ , setdiff(names(Indicator_Spread), "Date")],
  order.by = Indicator_Spread$Date
)

mv_spread_1 <- Indicator_Spread$T10.2.yield.curve..US.
mv_spread_2 <- Indicator_Spread$T10.3.yield.curve..US.
mv_spread_3 <- Indicator_Spread$Default.Spread..BofA.High.Yield.OAS.
mv_spread_4 <- Indicator_Spread$Defaut.Spread..BofA.EU.High.Yield.

Spread_Columns <- list(
  T10_2YC   = mv_spread_1,
  T10_3YC   = mv_spread_2,
  HY_OAS    = mv_spread_3,
  EU_HY_OAS = mv_spread_4
)

midas_matrix_spread <- map(
  Spread_Columns,
  ~ mv_into_mat(x = rets_ConsumerDisc, mv = .x, K = 12, type = 'monthly')
)

################################# Sentiment ###################################

Indicator_Sentiment <- read.csv(
  '/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Sentiment.csv'
)
Indicator_Sentiment <- Indicator_Sentiment %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
Indicator_Sentiment <- xts(
  Indicator_Sentiment[ , setdiff(names(Indicator_Sentiment), "Date")],
  order.by = Indicator_Sentiment$Date
)

mv_sent_1  <- Indicator_Sentiment$Overall.NFCI
mv_sent_2  <- Indicator_Sentiment$Leverage
mv_sent_3  <- Indicator_Sentiment$Risk
mv_sent_4  <- Indicator_Sentiment$Credit
mv_sent_5  <- Indicator_Sentiment$UMich
mv_sent_6  <- Indicator_Sentiment$BKK.Leading.Index
mv_sent_7  <- Indicator_Sentiment$CFNAI
mv_sent_8  <- Indicator_Sentiment$St.Louis.Stress.Index
mv_sent_9  <- Indicator_Sentiment$US.EPU
mv_sent_10 <- Indicator_Sentiment$China.EPU
mv_sent_11 <- Indicator_Sentiment$Global.GPR
mv_sent_12 <- Indicator_Sentiment$China.GPR
mv_sent_13 <- Indicator_Sentiment$USA.GPR
mv_sent_14 <- Indicator_Sentiment$GER.GPR
mv_sent_15 <- Indicator_Sentiment$Saudi.GPR
mv_sent_16 <- Indicator_Sentiment$Global.Sustainability.Index
mv_sent_17 <- Indicator_Sentiment$US.Sustainability.Index
mv_sent_18 <- Indicator_Sentiment$GER.Sustainability.Index
mv_sent_19 <- Indicator_Sentiment$China.Sustainability.Index
mv_sent_20 <- Indicator_Sentiment$.CESICNY
mv_sent_21 <- Indicator_Sentiment$.CESIEUR
mv_sent_22 <- Indicator_Sentiment$.CESIGL



Sentiment_Columns <- list(
  NFCI         = mv_sent_1,   # Overall.NFCI
  Leverage     = mv_sent_2,   # Leverage
  Risk         = mv_sent_3,   # Risk
  Credit       = mv_sent_4,   # Credit
  UMich        = mv_sent_5,   # UMich
  BKKIndex     = mv_sent_6,   # BKK.Leading.Index
  CFNAI        = mv_sent_7,   # CFNAI
  STLStress    = mv_sent_8,   # St.Louis.Stress.Index
  US_EPU       = mv_sent_9,   # US.EPU
  China_EPU    = mv_sent_10,  # China.EPU
  Global_GPR   = mv_sent_11,  # Global.GPR
  China_GPR    = mv_sent_12,  # China.GPR
  USA_GPR      = mv_sent_13,  # USA.GPR
  GER_GPR      = mv_sent_14,  # GER.GPR
  Saudi_GPR    = mv_sent_15,  # Saudi.GPR
  Global_Sust  = mv_sent_16,  # Global.Sustainability.Index
  US_Sust      = mv_sent_17,  # US.Sustainability.Index
  GER_Sust     = mv_sent_18,  # GER.Sustainability.Index
  China_Sust   = mv_sent_19,  # China.Sustainability.Index
  CESICNY      = mv_sent_20,  # .CESICNY
  CESIEUR       = mv_sent_21,
  CESIGL  = mv_sent_22)  # .CESIGL

midas_matrix_sentiment <- map(
  Sentiment_Columns,
  ~ mv_into_mat(x = rets_SP, mv = .x, K = 12, type = 'monthly')
)

################################# INDUSTRIAL ###################################

Indicator_Industrial <- read.csv(
  '/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Industrial.csv'
)
Indicator_Industrial <- Indicator_Industrial %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
Indicator_Industrial <- xts(
  Indicator_Industrial[ , setdiff(names(Indicator_Industrial), "Date")],
  order.by = Indicator_Industrial$Date
)

mv_industrial_1 <- Indicator_Industrial$Global.Supply.Chain.Pressure.Index
mv_industrial_2 <- Indicator_Industrial$Industrial.Production
mv_industrial_3 <- Indicator_Industrial$New.Orders
mv_industrial_4 <- Indicator_Industrial$PPI
mv_industrial_5 <- Indicator_Industrial$Industrial.Production..Manufacturing.Only.
mv_industrial_6 <- Indicator_Industrial$Global.Price.of.Industrial.Materials

Industrial_Columns <- list(
  GSCPI      = mv_industrial_1,
  IndProd    = mv_industrial_2,
  NewOrders  = mv_industrial_3,
  PPI        = mv_industrial_4,
  IndProdMfg = mv_industrial_5,
  MatPrices  = mv_industrial_6
)

midas_matrix_industrial <- map(
  Industrial_Columns,
  ~ mv_into_mat(x = rets_ConsumerDisc, mv = .x, K = 12, type = 'monthly')
)

################################# ECONOMIC #####################################

Indicator_Economic <- read.csv(
  '/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Indicator - Economic.csv'
)
Indicator_Economic <- Indicator_Economic %>%
  mutate(Date = as.Date(paste0(Date, "-01"), format = "%Y-%m-%d"))
Indicator_Economic <- xts(
  Indicator_Economic[ , setdiff(names(Indicator_Economic), "Date")],
  order.by = Indicator_Economic$Date
)

mv_economic_1 <- Indicator_Economic$value
mv_economic_2 <- Indicator_Economic$Unemployment.Rate
mv_economic_3 <- Indicator_Economic$Inflation.Rate
mv_economic_4 <- Indicator_Economic$Number.of.Car.Sales
mv_economic_5 <- Indicator_Economic$Electricity.Price.CPI.in.US.Cities

Economic_Columns <- list(
  EconValue    = mv_economic_1,
  UnempRate    = mv_economic_2,
  InflRate     = mv_economic_3,
  CarSales     = mv_economic_4,
  ElecPriceCPI = mv_economic_5
)

midas_matrix_economic <- map(
  Economic_Columns,
  ~ mv_into_mat(x = rets_ConsumerDisc, mv = .x, K = 12, type = 'monthly')
)


################################# RETURNS ######################################

# SECTOR RETURNS

sector_indices <- read.csv('/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - US Sector Indices 2010-2025.csv')

# Run this if MIDAS on Realized Vol
Realized_Vol_US <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Realized Volatility - Major US Indices.csv')
sector_indices <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - RV - Major US Indices.csv')

sector_indices <- sector_indices %>%
  mutate(Date = as.Date(Date))

sector_indices  <- xts(sector_indices,  order.by = sector_indices$Date)

rets_ConsumerDisc     <- sector_indices$X.5SP25
rets_Financial        <- sector_indices$X.5SP40
rets_Healthcare       <- sector_indices$X.5SP35
rets_Utilities        <- sector_indices$X.5SP55
rets_RealEstate       <- sector_indices$X.5SP60
rets_Materials        <- sector_indices$X.5SP15
rets_CoreCommodities  <- sector_indices$.TRCCBTR
rets_MidCap400        <- sector_indices$.MID
rets_SmallCap600      <- sector_indices$.SPCY
rets_VentureComposite <- sector_indices$.SPCDNX

# US RETURNS
US_indices_rets <- read.csv('/Users/phillip/Python/Python-Notebooks--Mine--1/Python-Notebooks--Mine-/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - Major US Indices 2010-2025.csv')

# Run this if MIDAS on Realized Vol

Realized_Vol_Sector <- Realized_Vol_US <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Realized Volatility - Sector Indices.csv')
US_indices_rets <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/DCC MIDAS/Returns - Index - RV - US Sectors.csv')

US_indices_rets <- US_indices_rets %>% 
  mutate(Date = as.Date(Date))

US_indices_rets <- xts(US_indices_rets, order.by = US_indices_rets$Date)

rets_SP       <- US_indices_rets$.SPX
rets_DJ       <- US_indices_rets$.DJI
rets_NASDAQ   <- US_indices_rets$.IXIC
rets_NASDAQ100<- US_indices_rets$.NDX
rets_SP1500   <- US_indices_rets$.SPSUPX
rets_Canada   <- US_indices_rets$.GSPTSE



#––– Ensure numeric storage mode –––
rets_names  <- ls(pattern = "^rets_")
for (name in rets_names) {
  tmp <- get(name); storage.mode(tmp) <- 'numeric'; assign(name, tmp)
}

mv_names   <- ls(pattern = "^midasv_")
for (name in mv_names) {
  tmp <- get(name); storage.mode(tmp) <- 'numeric'; assign(name, tmp)
}

mv2_names  <- ls(pattern = "^mv_")
for (name in mv2_names) {
  tmp <- get(name); storage.mode(tmp) <- 'numeric'; assign(name, tmp)
}