
##Loading the needed library
library(data.table)
library(ggplot2)
library(tidyr)

filePath <- "C:/Users/user/documents/"
data <- fread(paste0(filePath,"QVI_data.csv"))

## Setting the themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


### Selecting control stores

## Adding a new month ID column in the data
data$YEARMONTH <- as.integer(format(data$DATE, "%Y%m"))

## Defining the measure calculations to use during the analysis
measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),  # Calculate total sales
  nCustomers = uniqueN(LYLTY_CARD_NBR),  # Count number of distinct customers
  nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),  # Calculate transactions per customer
  nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),  # Calculate chips per transaction
  avgPricePerUnit = mean(TOT_SALES / PROD_QTY)  # Calculate average price per unit
), by = .(STORE_NBR, YEARMONTH)][order(STORE_NBR, YEARMONTH)]

## Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]


### Create a function to calculate correlation for a measure and looping through each control store
calculateCorrelation <- function(inputTable, metricCol, storeComparison){
    calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(),
                               corr_measure = numeric())
    storeNumbers <- unique(inputTable[, STORE_NBR])
    for(i in storeNumbers){
      calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
                                     "corr_measure" = cor(inputTable[STORE_NBR == storeComparison,
                                                                     eval(metricCol)],
                                                          inputTable[STORE_NBR == i,
                                                                     eval(metricCol)]))
      calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
    }
    return(calcCorrTable)
  }
  
### Create a function to calculate a standardised magnitude distance for a measure and looping through each control store

  calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
    calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),
                               measure = numeric())
    storeNumbers <- unique(inputTable[, STORE_NBR])
    for (i in storeNumbers) {
      calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
                                     "YEARMONTH" = inputTable[STORE_NBR == storeComparison, 
                                                              YEARMONTH],
                                     "measure" = abs(inputTable[STORE_NBR == storeComparison,
                                                                eval(metricCol)] - inputTable[STORE_NBR == i,
                                                                                              eval(metricCol)]))
      calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    }
    
### Standardise the magnitude distance so that the measure ranges from 0 to 1  
    
    minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                                by = c("Store1", "YEARMONTH")]
    distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
    distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
    finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
    return(finalDistTable)
  }

### Use the functions for calculating correlation for store 77

trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),
                                    trial_store)[order(-corr_measure)]
corr_nSales
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),
                                        trial_store)[order(-corr_measure)]
corr_nCustomers

### Use the functions for calculating magnitude for store 77
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nSales
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)
magnitude_nCustomers


### Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales,
                      by = c("Store1","Store2"))[, scoreNSales := (corr_measure + mag_measure) / 2][
                        order(-scoreNSales)]
score_nSales
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers,
                          by = c("Store1", "Store2"))[, scoreNCust := (corr_measure + mag_measure) / 2][
                            order(-scoreNCust)]
score_nCustomers


#### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers,
                       by = c("Store1","Store2"))[, finalControlScore := scoreNSales *0.5 +
                                                    scoreNCust * 0.5][order(-finalControlScore)]
score_Control


#### Select control stores based on the highest matching store (closest to 1 but not the store itself, i.e. the second ranked highest store)
## Select control store for trial store 77
control_store <- score_Control[Store2 != trial_store][order(-finalControlScore)][1, Store2]
control_store



### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste0(YEARMONTH %/% 100, "-", YEARMONTH %% 100, "-01"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")



### Conduct visual checks on customer count trends by comparing the trial store to the control store and other stores.
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, nCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste0(YEARMONTH %/% 100, "-", YEARMONTH %% 100, "-01"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]

ggplot(pastCustomers, aes(TransactionMonth, nCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of customers", title = "Total number of customers by month")



### Scale pre-trial control sales to match pre-trial trial store sales 
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & 
                                                   YEARMONTH < 201902, sum(totSales)]/
  preTrialMeasures[STORE_NBR == control_store & 
                                                   YEARMONTH < 201902, sum(totSales)]


## Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ , 
                        controlSales := totSales * scalingFactorForControlSales]


### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales) /
                                            controlSales]
print(percentageDiff)


### As our null hypothesis is that the trial period is the same as the pre‐trial period, 
###let's take the standard deviation based on the scaled percentage difference in the pre‐trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
print(stdDev)



#### Note that there are 8 months in the pre‐trial period
#### hence 8 ‐ 1 = 7 degrees of freedom

degreesOfFreedom <- 7


#### We will test with a null hypothesis of there being 0 difference between trial
#### and control stores

percentageDiff[, tValue := percentageDiff / stdDev
][, TransactionMonth := as.Date(paste(
  YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]


#### Find the 95th percentile of the t distribution with the appropriate
#### degrees of freedom to compare against

qt(0.95, df = degreesOfFreedom)


#### Trial and control store total sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control",
                                                                "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100,
                                      YEARMONTH %% 100, 1,
                                      sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]


#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales* (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

### Plotting these in a graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth),
                ymin = 0, ymax = Inf, colour = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x ="Month of operation", y = "Total sales", title = "Total sales by month")


#### This would be a repeat of the steps before for total sales
#### Scale pre‐trial control customers to match pre‐trial trial store customers

scalingFactorForControlCusts <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,
                                                sum(nCustomers)] / preTrialMeasures[
                                                  STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][, controlCustomers := nCustomers *
    scalingFactorForControlCusts
][, Store_type := ifelse(STORE_NBR == trial_store,
                         "Trial",
                         ifelse(STORE_NBR ==
                                  control_store,
                                "Control",
                                "Other stores"))]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTimeCusts[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlCustomers - nCustomers) /
                                            controlCustomers]
print(percentageDiff)


#### As our null hypothesis is that the trial period is the same as the
###pre‐trial period, let's take the standard deviation based on the scaled
##percentage difference in the pre‐trial period

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
stdDev


# Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

# Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

# Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

# Plotting everything
ggplot(trialAssessment, aes(TransactionMonth, nCusts, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf,
                colour = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customer",
       title = "Total number of customers by month")



#### Trial store 86

measureOverTime <- data[, .(totSales = sum(TOT_SALES), nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = (uniqueN(TXN_ID)) / (uniqueN(LYLTY_CARD_NBR)),
                            nChipsPerTxn = (sum(PROD_QTY)) / (uniqueN(TXN_ID)),
                            avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY) ),
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]


#### Use the functions for calculating correlation
trial_store <- 86
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),  trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)


#### Create a combined score composed of correlation and magnitude
score_nSales <- merge(corr_nSales, magnitude_nSales,
                      by = c("Store1", "Store2"))[, scoreNSales := (corr_measure + mag_measure) / 2]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers,
                          by = c("Store1", "Store2"))[, scoreNCust := (corr_measure + mag_measure) / 2]

#### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers,
                       by = c("Store1", "Store2"))[, finalControlStore := scoreNSales * 0.5 +
                                                     scoreNCust * 0.5]


#### Select control stores based on the highest matching store
#### (closest to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 86
control_store <- score_Control[Store1 == trial_store, ][order(-finalControlStore)][2, Store2]
control_store


#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%100,
                                      YEARMONTH %% 100, 1,
                                      sep = "‐"), "%Y‐%m‐%d")
][YEARMONTH < 201903 , ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#### Visual checks on trends based on the drivers
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, numberCustomers := mean(nCustomers),
                                      by = c("YEARMONTH", "Store_type")][YEARMONTH < 201903, ]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, colour = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers",
       title = "Total number of customers by month")


#### Scale pre‐trial control sales to match pre‐trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,
                                                 sum(totSales)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]


#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store,
][, controlSales := totSales * scalingFactorForControlSales]


#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales) /
                                            controlSales]

#### As our null hypothesis is that the trial period is the same as the
#### pre‐trial period, let's take the standard deviation based on the scaled
#### percentage difference in the pre‐trial period

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

#### Trial and control store total sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100,
                                      YEARMONTH %% 100, 1,
                                      sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"),]

#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

#### Plotting them together
ggplot(trialAssessment, aes(TransactionMonth, totSales, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf,
                colour = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")



#### This would be a repeat of the steps before for total sales
#### Scale pre‐trial control customers to match pre‐trial trial store customers
scalingFactorForControlCusts <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,
                                                 sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR ==control_store,
][,controlCustomers := nCustomers *
    scalingFactorForControlCusts
][, Store_type := ifelse(
  STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))]


#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlCustomers - nCustomers) /
                                            controlCustomers]


#### As our null hypothesis is that the trial period is the same as the
#### pre‐trial period, let's take the standard deviation based on the scaled
#### percentage difference in the pre‐trial period

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)


#### Plotting them together
ggplot(trialAssessment, aes(TransactionMonth, nCusts, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf,
                colour = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers",
       title = "Total number of customers by month")


#### Trial store 88
measureOverTime <- data[, .(totSales = sum(TOT_SALES), nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)),
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the functions for calculating correlation
trial_store <- 88
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)


#### Create a combined score composed of correlation and magnitude
score_nSales <- merge(corr_nSales, magnitude_nSales,
                      by = c("Store1", "Store2"))[, scoreNSales := (corr_measure + mag_measure) / 2]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers,
                          by = c("Store1", "Store2"))[,scoreNCust := (corr_measure + mag_measure) / 2]

#### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers,
                       by = c("Store1", "Store2"))[, finalControlStore := scoreNSales * 0.5 +
                                                     scoreNCust * 0.5]


#### Select control stores based on the highest matching store
#### (closest to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 88

control_store <- score_Control[Store1 == trial_store, ][order(-finalControlStore)][2, Store2]
control_store


#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,"Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100,
                                      YEARMONTH %% 100, 1,
                                      sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales, aes(TransactionMonth, totSales, colour = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#### Visual checks on trends based on the drivers
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, numberCustomers := mean(nCustomers),
                                      by = c("YEARMONTH", "Store_type")
][YEARMONTH < 201903, ]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, colour = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers",
       title = "Total number of customers by month")


#### Scale pre‐trial control sales to match pre‐trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,
                                                 sum(totSales)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]


#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store,
][, controlSales := totSales * scalingFactorForControlSales]


#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store,
                                        c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales) /
                                            controlSales]

#### As our null hypothesis is that the trial period is the same as the
#### pre‐trial period, let's take the standard deviation based on the scaled
#### percentage difference in the pre‐trial period

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

#### Trial and control store total sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(
  YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"),]


#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]


#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)


#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf,
                colour = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")



#### This would be a repeat of the steps before for total sales
#### Scale pre‐trial control customers to match pre‐trial trial store customers
scalingFactorForControlCusts <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902,
                                                 sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]


#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR ==control_store,
][,controlCustomers := nCustomers *
    scalingFactorForControlCusts
][, Store_type := ifelse(
  STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))]


#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlCustomers - nCustomers) /
                                            controlCustomers]


#### As our null hypothesis is that the trial period is the same as the
#### pre‐trial period, let's take the standard deviation based on the scaled
#### percentage difference in the pre‐trial period

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7 

#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]


#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]


### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)


#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, nCusts, colour = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf,
                colour = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers",
       title = "Total number of customers by month")


