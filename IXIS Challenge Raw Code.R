#IXIS Data Science Challenge from home version
#Ian Fulton-Black
#8/30/2022


#Read in packages
library(tidyverse)
library(openxlsx)
library(readxl)


#Reading in data
setwd("C:/Users/ianfu/OneDrive/Desktop/R Stuff/Datasets")
carts<-read.csv("DataAnalyst_Ecom_data_addsToCart.csv", fileEncoding="UTF-8-BOM")
sessions<-read.csv("DataAnalyst_Ecom_data_sessionCounts.csv", fileEncoding="UTF-8-BOM")

#remove error, (not set), and turnaround from sessions$dim_browser
sessions2<-sessions %>% group_by(dim_browser) %>% summarise(nums=n())

cleansessions <- sessions %>% filter(dim_browser != 'error')
cleansessions <- cleansessions %>% filter(dim_browser != '(not set)')
cleansessions <- cleansessions %>% filter(dim_browser != 'turnaround')

#change cleansessions date to month
cleansessions$dim_date <- format(as.Date(cleansessions$dim_date, "%m/%d/%Y"), "%m")


#Creating the first aggregation plus ECR 
monthlydevice <- cleansessions %>% group_by(dim_date, dim_browser) %>%
  summarise(QTY = sum(QTY), sessions = sum(sessions), transactions = sum(transactions), ECR = transactions/sessions,
            .groups = 'drop')
colnames(monthlydevice)[which(names(monthlydevice) == "dim_date")] <- "Month"
monthlydevice$Month <- as.numeric(monthlydevice$Month)

#Creating second sheet (month over month comparison for 6/13 and 5/13)
carts2 <- carts %>% filter(dim_month == 5 | dim_month == 6)
monthbymonth <- monthlydevice %>% filter(Month == 5 | Month == 6)
colnames(carts2)[which(names(carts2) == "dim_month")] <- "Month"
total <- merge(monthbymonth,carts2,by='Month')

## all good to here



total <- total %>% group_by(Month) %>%
  summarise(QTY = sum(QTY), sessions = sum(sessions), transactions = sum(transactions), ECR = transactions/sessions,
            addsToCart = carts2$addsToCart,
            .groups = 'drop') 
total<-total[-c(2, 3), ]
QTY_diff <- diff(total$QTY)
total$QTY_diff <- QTY_diff
sessions_diff <- diff(total$sessions)
total$sessions_diff <- sessions_diff
transactions_diff <- diff(total$transactions)
total$transactions_diff <- transactions_diff
ECR_diff <- diff(total$ECR)
total$ECR_diff <- ECR_diff
addsToCart_diff <- diff(total$addsToCart)
total$addsToCart_diff <- addsToCart_diff
View(total)

#Creating a new file using openxlsx
workbook <- list("devicebymonth" = monthlydevice, "last2months" = total)
write.xlsx(workbook, file = "ixisworkbook.xlsx")


##STATS TIME 

##Exploratory notes
safariandchrome<-cleansessions %>% filter(dim_browser == "Safari" | dim_browser == "Chrome")
sum(safariandchrome$transactions)
sum(cleansessions$transactions)

safariandchrome<-cleansessions %>% filter(dim_browser == "Safari" | dim_browser == "Chrome")
sum(safariandchrome$sessions)
sum(cleansessions$sessions)

##Adds to Cart
carts<-carts %>% arrange(dim_year, dim_month)
carts$dim_month <- factor(carts$dim_month,                                    
                  levels = c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6))
ggplot(carts, aes(x= dim_month, y = addsToCart, fill = dim_year)) +
  geom_bar(stat = "identity") + guides(fill="none") 


mean(carts$addsToCart)

median(carts$addsToCart)

carts %>% group_by(dim_month) %>% summarise(min(addsToCart), max(addsToCart), median(addsToCart),
                                          mean(addsToCart), sd(addsToCart))

cleansessions %>% group_by(dim_date) %>% summarise(min(transactions), max(transactions), median(transactions),
                                            mean(transactions), sd(transactions))
sum(cleansessions$transactions)

##scatterplot sessions by transactions

plussessions<-monthlydevice %>% filter(sessions > 100)
gg <- ggplot(plussessions, aes(x= sessions, y=transactions)) + 
  geom_point(aes(col=dim_browser, size=QTY)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 500000)) + 
  ylim(c(0, 15000)) + 
  labs(subtitle="Sessions Vs Transactions", 
       y="Transactions", 
       x="Sessions", 
       title="Scatterplot", 
       caption = "Source: Google Analytics")

plot(gg)


#Most Recent
six <- monthbymonth %>% filter(Month == 6) 
ss<-ggplot(six, aes(x=transactions, y=QTY)) + 
  geom_point() + geom_abline()+
  xlim(c(0, 500)) + 
  ylim(c(0, 750)) + 
  labs(subtitle="06/13: Quantity Vs Transactions", 
       y="Quantity", 
       x="Transactions", 
       caption = "Source: Google Analytics")

five <- monthbymonth %>% filter(Month == 5) 
rr<-ggplot(five, aes(x=transactions, y=QTY)) + 
  geom_point() + geom_abline()+
  xlim(c(0, 500)) + 
  ylim(c(0, 750)) + 
  labs(subtitle="05/13: Quantity Vs Transactions", 
       y="Quantity", 
       x="Transactions", 
       caption = "Source: Google Analytics")

library(patchwork)

rr+ss

