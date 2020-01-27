
## This file performs the regressions of interest

rm(list = ls())

library(dplyr)
library(sandwich)
options(stringsAsFactors = FALSE)

setwd("C:/Users/ehorton/Documents/Princeton/wws538/final_project")

## Import data ---------------------------------------------------------------------------------------------
river.distances <- readr::read_csv("projectarea-parcels-riverdistance.csv") %>%
                     select(-X1) %>%
                     as.data.frame()

# Filter a bit, merge with river distances and merge
parcels <- readr::read_csv("parcels.csv") %>%
             select(-X, -X1) %>%
             as.data.frame() %>%
             left_join(river.distances) %>%
             filter(TotalValue != 0) %>%
             mutate(LogTotalValue = log(TotalValue),
                    TimeIndex = RollYear - 2006 + 1,
                    ParcelAge = RollYear - EffectiveYearBuilt) %>%
             arrange(RollYear, AIN)

# In case it's interesting
case.shiller <- read.csv("LXXRSA.csv") %>%
                  mutate(year = as.numeric(substr(DATE, nchar(DATE) - 3, nchar(DATE)))) %>%
                  filter(year >= 2006, year <= 2017) %>%
                  group_by(year) %>%
                  summarise(index = mean(LXXRSA)) %>%
                  mutate(index = index / index[1])
## TODO (maybe): FHFA HPI?

## Some summary statistics and charts -----------------------------------------------------------------
avg.value <- parcels %>%
               # Case-Shiller only tracks repeated sales of SFRs
               filter(sfr == 1) %>%
               group_by(RollYear) %>%
               summarise(AvgTotalValue = mean(TotalValue)) %>%
               mutate(AvgTotalValue = AvgTotalValue / AvgTotalValue[1])

plot(x = avg.value$RollYear, y = avg.value$AvgTotalValue, type = "l", col = "blue",
     ylim = c(min(case.shiller$index), max(avg.value$AvgTotalValue)))
lines(x = avg.value$RollYear, y = case.shiller$index, col = "red")

year <- 2017
parcels.sub <- filter(parcels, RollYear == year)

plot(x = parcels.sub$HubDist, y = parcels.sub$LogTotalValue, main = year)
hist(parcels.sub$TotalValue, main = year, breaks = "FD", xlim = c(0, 2e6))


## Regression analysis --------------------------------------------------------------------------------------
log.model <- formula(LogTotalValue ~ TimeIndex + ParcelAge + HubDist + SQFTmain + Bedrooms + Bathrooms + Units +
                     residential + commercial + industrial + institutional +
                     vacant + pool + parkinglot + sfr + multires)
log.model.notime <- formula(LogTotalValue ~ ParcelAge + HubDist + SQFTmain + Bedrooms + Bathrooms + Units +
                            residential + commercial + industrial + institutional +
                            vacant + pool + parkinglot + sfr + multires)

hedonic.reg <- lm(log.model, parcels)
#hedonic.reg.notime <- lm(log.model.notime, parcels)

summary(hedonic.reg)
plot(hedonic.reg$model$HubDist, hedonic.reg$residuals)


dist_beta_by_year <- function(model, variable, year, robust = FALSE){
  ols <- parcels %>%
           filter(RollYear == year) %>%
           lm(model, .)
  
  if (robust){
    nw <- lmtest::coeftest(ols, vcov = NeweyWest(ols))
    var.row <- nw[variable, ]
  }else{
    ols <- summary(ols)
    var.row <- t(as.matrix(ols$coefficients[variable, ]))
  }
  
  rownames(var.row) <- year
  
  return(var.row)
}


betas.by.year <- unique(parcels$RollYear) %>%
                   lapply(function(x) dist_beta_by_year(log.model.notime, "HubDist", x, robust = FALSE)) %>%
                   do.call(rbind, .)

## TODO
# 1) Stationarity?
# 2) Heteroskedasticity?
# 3) Overfitting OLS? Maybe too many observations
# 4) Residual diagnostics - white noise?
# 5) how to incorporate all of LA?

# Try Newey-West SEs
robust <- lmtest::coeftest(hedonic.reg, vcov = NeweyWest(hedonic.reg))
