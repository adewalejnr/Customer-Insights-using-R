#### Data exploration is now starting!


### Load required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(dplyr)
library(stringr)

### Setting the filepath

filePath <- "/Users/User/Documents"
transactionData <- fread("/Users/User/Documents/QVI_transaction_data.csv")
customerData <- fread("/Users/User/Documents/QVI_purchase_behaviour.csv")



### Examining transaction data
head(transactionData)
str(transactionData)

## Convert DATE column to a date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
str(transactionData) ### confirming that the date format has changed


### Examining PROD_NAME column
transactionData[, .N, PROD_NAME]


## Examining the words in PROD_NAME to see if there are any incorrect entries such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

### Removing digits &  special characters
## Removing digits
productWords <- productWords[grepl("\\d", words) == FALSE, ]

## Removing special characters
productWords <- productWords[grepl("[:alpha:]", words), ]

### Let's look at the most common words by counting the number of times a word appears and
## sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]


## Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

## Summarizing the data to check for nulls and possible outliers
summary(transactionData)

## Investigating the prod. quantity further to understand why 200 chips was purchased in 1 transaction because that appears as an outlier
## Filtering the dataset to find the outlier
zScores <- scale(transactionData$PROD_QTY)
threshold <- 6
outliers <- transactionData$PROD_QTY[abs(zScores) > threshold]
print(outliers)

## Checking to see if the customer has had other transactions
targetPROD_QTY <- "200"
filteredData <- transactionData[PROD_QTY == targetPROD_QTY, ]
print(filteredData)

## Filtering out the customer based on the loyalty card number
targetLoyaltyCardNumber <- "226000"
filteredData <- transactionData[LYLTY_CARD_NBR != targetLoyaltyCardNumber, ]
print(filteredData)

## Re-examining the transaction data
summary(filteredData)

## Count the number of transactions by date
transactionData <- as.data.table(transactionData)
transactionCounts <- transactionData[, .N, by = DATE]
print(transactionCounts)

## Creating a sequence of dates and join this the count of transactions by date
dateSequence <- data.table(DATE = seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day"))
filledData <- merge(dateSequence, transactionCounts, by = "DATE", all.x = TRUE)
print(filledData)

## Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

## Plot transactions over time
ggplot(filledData, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## Filtering to December and looking at individual days
ggplot(filledData[format(DATE, "%m") == "12"], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time (December)") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


### Creating pack size
## I worked this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

## Confirming if the pack sizes look sensible
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]


transactionData

## Plotting a histogram showing the number of transactions by pack size
ggplot(transactionData1, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Pack Size", y = "Number of Transactions", title = "Distribution of Transactions by Pack Size")

### Creating Brand names
## I use the first word in PROD_NAME to work out the brand name
transactionData[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(pattern = ' ', PROD_NAME) - 1))]

## Checking the brand names
unique(transactionData$BRAND)

### Cleaning the brand names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]



## Checking the results to confirm if it looks reasonable. 
unique(transactionData$BRAND)



### Examining customer data
str(customerData)
summary(customerData)

## Examining the values of lifestage and premium_customer
customerData[, .N, by = LIFESTAGE][order(-N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]


## Merging transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

## checking if some customers were not matched on by checking for nulls.
null_customers <- data[is.null(data$PREMIUM_CUSTOMER), .N]
null_customers1 <- data[is.null(data$LIFESTAGE), .N]

print(null_customers)
print(null_customers1)

## Exporting the new dataset
fwrite(data, paste0("/Users/User/Documents/QVI_data.csv"))


#### Data exploration is now complete!


#### Data analysis on customer segments is starting


### Calculating the summary of sales by LIFESTAGE and PREMIUM CUSTOMER and creating a plot for it
sales_summary <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(total_sales = sum(TOT_SALES))

## Plot and label with proportion of sales
plot <- ggplot(data = sales_summary) +
  geom_mosaic(aes(weight = total_sales, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


plot + geom_text(data = ggplot_build(plot)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

                                                                                                                          

## Number of customers by LIFESTAGE and PREMIUM CUSTOMER

customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE,PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

## Plot and label with proportion of customers
p <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = 
          (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))



## Looking at the average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER

avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)),.(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

ggplot(avg_units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "LIFESTAGE", y = "Average Units", title = "Avg. Number of Units per Customer")
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



## Looking at the average price per unit by LIFESTAGE and PREMIUM_CUSTOMER

# Calculate the average sale price per unit
avg_price <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(avg_sale_price = mean(TOT_SALES))

# Plot the average sale price per unit
ggplot(data = avg_price, aes(weight = avg_sale_price, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



##  Performing a t-test to see if the difference is significant

pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")

##alternatively a chisq test can be performed

# Create a contingency table of the two categorical variables
cont_table <- table(data$LIFESTAGE, data$PREMIUM_CUSTOMER)

# Perform a chi-square test
chi2_result <- chisq.test(cont_table)

print(chi2_result)


## Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]

## Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]



### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

## Doing a deep dive into the brand that sell with the pack size
data[PACK_SIZE == 270, unique(PROD_NAME)]

