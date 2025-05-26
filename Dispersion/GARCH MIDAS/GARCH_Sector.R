###############################################################################
## 0.  LIBRARIES --------------------------------------------------------------
###############################################################################
library(xts)
library(rumidas)      # ugmfit()
library(pbapply)      # progress bars for nested lapply
library(dplyr)        # bind_rows & tidy ops



K <- 12                              # 12 lags => 13 rows per MIDAS matrix

returns_list <- list(S_P = SP,
                     Consumer = rets_ConsumerDisc,
                     Financial = rets_Financial,
                     Materials = rets_Materials,
                     MidCap = rets_MidCap400,
                     RealEstate = rets_RealEstate,
                     SmallCap = rets_SmallCap600,
                     SP100 = rets_SP100,
                     Utilities = rets_Utilities,
                     VentureComposite = rets_VentureComposite,
                     Dow = rets_DJ,
                     NASDAQ = rets_NASDAQ,
                     NASDAQ100 = rets_NASDAQ100,
                     SP1500 = rets_SP1500,
                     Canada = rets_Canada)


MV_list <- list(GPR            = midasv1,
                GEP            = midasv2,
                NewOrders      = midasv3,
                UMich          = midasv4,
                Unemployment   = midasv5,
                CitiSuprise    = midasv6,
                VIX            = midasv7,
                VNX            = midasv8,
                RecessionSpread= midasv9,
                HighYieldSpread= midasv10,
                BKK            = midasv11,
                NAI            = midasv12,
                EquityVol      = midasv13,
                StLouisStress  = midasv14)



###############################################################################

#Nested Fitting

all_models <-  pblapply(names(returns_list), function(asset) {
  

  inner <- lapply(names(MV_list), function(macro) {
    
    storage.mode(returns_list[[asset]]) <- 'numeric'
    
    tryCatch(
    
    ugmfit(model = 'GM',
           skew = 'YES',
           distribution = 'norm',
           returns_list[[asset]],
           mv_m = MV_list[[macro]],
           K = K),
    error = identity)
  }) 
  names <- names(MV_list)
  inner
  }) 


names(all_models) <- names(returns_list)


#Summary Tables

all_summary <- lapply(all_models, function(lst) lapply(list, summary))

################################################################################

coef_table <- bind_rows(
  lapply(names(all_summary), function(asset) {
    lapply(names(all_sum[[asset]]), function(macro) {
      all_sum[[asset]][[macro]]$par_table |>
        mutate(asset = asset, macro = macro, .before = 1)
    })
  })
)