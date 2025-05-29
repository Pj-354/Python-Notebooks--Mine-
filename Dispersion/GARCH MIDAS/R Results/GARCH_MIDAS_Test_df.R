library(mfGARCH)
library(tools)
library(readr)
library(dplyr)
library(lubridate)
EPU_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/Citi Suprise Economic Index 2005-2025.csv')
VIX_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/VIX_2005_2025.csv')
MOVE_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/MOVE_2005_2025.csv')
Skew_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/SkewX_2005_2025.csv')
VVIX_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/VVIX_2006_2025.csv')
Yields_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/Yields_2005-2025.csv')
OVX_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/OVX_2007_2025.csv')
GPU_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/Geopolitical Risk 1985-2025.csv')
GEP_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/Global Economic Policy Uncertainty Data.csv')
SP_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/GARCH Datasets/SP500_2005_2025.CSV')
big_df <- read.csv('C:/Users/Phillip/Desktop/Python/Python Notebooks (Mine)/Dispersion/GARCH MIDAS/SP_VIX_VVIX_SKEW_GPR_GEP_MOVE_CITI.csv')


x_GEP <- fit_mfgarch(data = big_df, y = 'Returns', x = 'GEP', low.freq = 'Year_Week', K = 52)
x_VIX <- fit_mfgarch(data = big_df, y = 'Returns', x = 'VIX', low.freq = 'date', K = 252)
x_MOVE <- fit_mfgarch(data = big_df, y = 'Returns', x = 'MOVE', low.freq = 'date', K = 20)
x_VVIX_VIX <- fit_mfgarch(data = big_df, y = 'Returns', x = 'VIX', low.freq = 'date', K = 252, x.two = 'VVIX', low.freq.two = 'date', K.two = 252, weighting.two = 'beta.restricted')
x_GEP_VVIX <- fit_mfgarch(data = big_df, y = 'Returns', x = 'GEP', low.freq = 'Year_Week', K = 52, x.two = 'VVIX', low.freq.two = 'date', K.two = 20, weighting.two ='beta.restricted')
x_RVOL_Citi <- fit_mfgarch(data = big_df, y = 'Returns',x = 'CESIUSD', low.freq = 'date', K = 12, x.two = 'RVOL', low.freq.two = 'date', weighting.two = 'beta.restricted', K.two = 12)


big_df <- big_df %>% 
      mutate(Year_Week = as.Date(Year_Week))
big_df <- big_df %>% 
  mutate(date = as.Date(date))


### Test Values
x_RVOL_Citi$variance.ratio
x_RVOL_Citi$broom.mgarch
x_GEP$broom.mgarch
x_GEP$variance.ratio
x_VIX$broom.mgarch
x_VIX$variance.ratio
x_MOVE$broom.mgarch
x_MOVE$variance.ratio
x_VVIX_VIX$broom.mgarch
x_VVIX_VIX$variance.ratio
x_GEP_VVIX$broom.mgarch
x_GEP_VVIX$variance.ratio
plot_weighting_scheme(x_RVOL_Citi)

## Test Values for Package Data

y$variance.ratio

y <- fit_mfgarch(data = df_mfgarch, y = "return", x = "nfci", low.freq = "year_week", K = 52,
                 x.two = "dindpro", K.two = 12, low.freq.two = "year_month", weighting.two = "beta.restricted")
y$broom.mgarch
