#################################
### Retail Gaint Sales Forecasting
### CASE STUDY - Time Series
### Team Name - DSWarriors
#################################

# kindly ignore the warning messages till the end
# Those warnings can be ignored.

#---------------------Install the required packages---------------
 
# install.packages('forecast')
# install.packages('tseries')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('graphics')
# install.packages('stats')
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("data.table")

#---------------------Business Understanding----------------------

# The online super store gaint "Global Mart" operate worldwide and takes  orders and delivers across the globe
# It deals with all the major product categories - consumer, corporate & home office.
# The sales/operations team, want to finalise the plan for the next 6 months.  
# So, we want to forecast the sales and the demand for the next 6 months, 
# that would help us manage the revenue and inventory accordingly.

# Goals of this Case Study
#1. Subset the data from the transactional database to form the 21 Market Buckets [7 Global Market Regions x 3 Consumer Segments].
#2. Identify the top 2 Most Profitable and Consistently Profitable Market Buckets.
#3. Build a Time Series Model to forecast the Demand and Sales for these two Market Buckets using 1. Classical Decomposition and 2.Auto Arima
#4. Perform Model Evaluation using MAPE and Accuracy. Use the final models to predict the Demand and Sales of the next 6 months for the 2 Market Buckets.

#---------------------importing the reqired libraries--------------

library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(graphics)
library(lubridate)
library(stats)
library(tidyr)
library(data.table)
 
#---------------------Data Understanding & Preparation---------------

# Set working directory

 setwd("C:/PGDDS/Time Series Assignment")

# Let's import the dataset into the R environment

global_superstore_db <- read.csv("Global Superstore.csv",stringsAsFactors = F)

View(global_superstore_db)
#51290 - records
#24 - attributes

str(global_superstore_db)
#we see that we have 17 character variable and 7 continuous variable

#Data Dictionary to understand the attribute significance:
#Order ID:	Unique ID of the transaction 
#Order Date:	Date on which the order was placed
#Ship Date:	Date on which the shipment was made
#Ship Mode:	The mode of shipment (category)
#Customer ID:	The unique ID of the customer
#Customer Name:	Name of the customer
#Segment:	The market segment to which the product belongs
#City:	City of the delivery address
#State:	State of the delivery address
#Country:	Country of the delivery address
#Postal Code:	Postal code of the delivery address
#Market:	Market segment to which the customer belongs
#Region: Geographical region of the customer
#Product ID:	Unique ID of the product
#Category:	Category of the product
#Sub-Category:	Sub-category of the product
#Product Name:	Name of the product
#Sales:	Total sales value of the transaction
#Quantity:	Quantity of the product ordered
#Discount:	Discount percentage offered on the product
#Profit:	Profit made on the transaction
#Shipping Cost:	Shipping cost incured on the transaction
#Order Priority:	Priority assigned to the order

#Row.ID is synonymous to serial number and is not going to be of significance to us. Therefore we will eliminate it.
global_superstore_db<- global_superstore_db[,-1]

# Checking missing value
sapply(global_superstore_db, function(x) sum(is.na(x))/nrow(global_superstore_db)) # No missing values
#Removing postal code column
global_superstore_db$Postal.Code<-NULL

#Checking for Duplicates in the dataset
length(which(duplicated(global_superstore_db)==TRUE))
#Therefore there are no duplicated rows or identical records in the dataset

#Checking if Order.ID is the primary key of this dataset
length(unique(global_superstore_db$Order.ID))
length(unique(global_superstore_db$Customer.ID))
length(unique(global_superstore_db$Product.ID))

#Extracting Order month from the order date
global_superstore_db$Order.Date<-as.Date(global_superstore_db$Order.Date,"%d-%m-%Y")
str(global_superstore_db)
global_superstore_db$order_month<-month(global_superstore_db$Order.Date)
global_superstore_db$order_month<-month.abb[global_superstore_db$order_month]

unique(global_superstore_db$Segment)
#There are 3 unique Customer Segments [1]"Consumer" [2]"Corporate" [3]"Home Office"

unique(global_superstore_db$Market)
#There are 7 unique Market Segments as follows:
#1. Africa - African Continent
#2. APAC - Asia Pacific/Asia Central
#3. Canada - Canada
#4. EMEA - Europe, Middle East and Africa
#5. EU - European Union
#6. LATAM - Latin America
#7. US - United States of America

unique(global_superstore_db$Category)
#There are 3 unique product categories sold by GLOBAL MART [1]"Technology" [2]"Furniture" [3]"Office Supplies"

#Listing 21 different buckets
buckets<-unique(global_superstore_db[,c("Market","Segment")])
buckets

#Extracting Order month from the order date
global_superstore_db$Order.Date<-as.Date(global_superstore_db$Order.Date,"%d-%m-%Y")
str(global_superstore_db)
global_superstore_db$order_month<-month(global_superstore_db$Order.Date)
global_superstore_db$order_month<-month.abb[global_superstore_db$order_month]

#Identifying the Earliest Order.Date
min(global_superstore_db$Order.Date)
#Identifying the Latest Order.Date
max(global_superstore_db$Order.Date)

#Aggregating profit on the basis of 21 buckets
aggr_profit_value<-global_superstore_db %>% select(Market,Segment,Profit,order_month) %>% group_by(Market,Segment,order_month) %>% mutate(monthly_profit= sum(Profit))
aggr_profit_value<-aggr_profit_value %>% select(Market,Segment,order_month,monthly_profit)
aggr_profit_value<-unique(aggr_profit_value)

#Plot of monthly profit on the basis of market and segment
aggr_profit_value$order_month<-factor(aggr_profit_value$order_month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

plot_profit<-ggplot(aggr_profit_value,aes(x=order_month,y=monthly_profit,group=1))
plot_profit+geom_point()+geom_line()+facet_grid(Market~Segment)

aggr_profit_buckets<-aggr_profit_value %>% group_by(Market,Segment) %>% summarise(mean_cff=mean(monthly_profit),sd_cff=sd(monthly_profit))
aggr_profit_buckets$coeff_var<-aggr_profit_buckets$sd_cff/aggr_profit_buckets$mean_cff
aggr_profit_buckets<-aggr_profit_buckets %>% arrange(coeff_var)

#GS added code starts for EDA

aggr_profit_buckets$Mark_Seg<-NULL
aggr_profit_buckets$Mark_Seg<-paste(aggr_profit_buckets$Market,aggr_profit_buckets$Segment,sep=" ")


plot2<-ggplot(aggr_profit_buckets,aes(x=aggr_profit_buckets$Mark_Seg,y=aggr_profit_buckets$coeff_var,label = round(aggr_profit_buckets$coeff_var,digits=2)))
plot2+coord_flip()+geom_bar(stat="identity",position="dodge")+xlab("Market Segment")+ylab("Coeff. of variance of profit")+geom_text(size = 3, position = position_stack(vjust = 0.5))+ggtitle("Coefficient of variance in  profit Vs. Market Segment")

#GS added code ends for EDA

#What are the 2 most profitable segments ?
#The most profitable buckets are the ones which has the lowest coefficient of variation
aggr_profit_buckets[1:2,]

#Hence we will choose the following segments for further Analysis
buckets_analyze<-data.frame(Market=aggr_profit_buckets[1:2,"Market"],Segment=aggr_profit_buckets[1:2,"Segment"])
buckets_analyze
# The most profitable and consistent segments are
# Market  Segment
# 1   EU Consumer
# 2   APAC Consumer

#------------------------ Profit Aggregation Ends ----------------------------

#------------------------ Aggregation of Sales ------------------------------------

# Now we will find the aggregated Sales for each month for each market and each segment
aggr_bucket_value<-global_superstore_db %>%select(Market,Segment,Sales,order_month) %>%group_by(Market,Segment,order_month) %>% mutate(monthly_sales=sum(Sales))
aggr_bucket_value<-aggr_bucket_value%>% select(Market,Segment,order_month,monthly_sales)
aggr_bucket_value<-unique(aggr_bucket_value)
nrow(aggr_bucket_value)
aggr_bucket_value

# Yearly Sales Aggregated - To identify yearly trends 
plot_sales<-ggplot(aggr_bucket_value,aes(x=order_month,y=monthly_sales,group=1))
plot_sales+geom_point()+geom_line()+facet_grid(Market~Segment)

# Let us get the aggregated Quantity for each month for each market and each segment
aggr_quantity_value<-global_superstore_db %>%select(Market,Segment,Quantity,order_month) %>%group_by(Market,Segment,order_month) %>% mutate(monthly_quantity=sum(Quantity))
aggr_quantity_value<-aggr_quantity_value%>% select(Market,Segment,order_month,monthly_quantity)
aggr_quantity_value<-unique(aggr_quantity_value)
nrow(aggr_quantity_value)
class(aggr_quantity_value)
ggplot(aggr_quantity_value,aes(x=order_month,y=monthly_quantity,group=1))+geom_point()+geom_line()+facet_grid(Market~Segment)

# Now we will merge aggregated Profit,Sales and Quantity and plot the time series
aggr_data_merge<-merge(aggr_profit_value,aggr_bucket_value,by=c("Market","Segment","order_month"),all=T)
aggr_data_merge<-merge(aggr_data_merge,aggr_quantity_value,by=c("Market","Segment","order_month"),all=T)
aggr_data_merge

# Normalisation of Monthly Sales,Profit and Quantity
aggr_data_merge$monthly_profit<-scale(aggr_data_merge$monthly_profit)
aggr_data_merge$monthly_sales<-scale(aggr_data_merge$monthly_sales)
aggr_data_merge$monthly_quantity<-scale(aggr_data_merge$monthly_quantity)

plot_all<-ggplot(aggr_data_merge,aes(order_month,group=1))+geom_point(aes(y=monthly_profit))+geom_line(aes(y=monthly_profit,colour="monthly_profit"))
plot_all<-plot_all+geom_point(aes(y=monthly_sales))+geom_line(aes(y=monthly_sales,colour="monthly_sales"))+facet_grid(Market~Segment)
plot_all<-plot_all+geom_point(aes(y=monthly_quantity))+geom_line(aes(y=monthly_quantity,colour="monthly_quantity"))
plot_all<-plot_all+scale_color_manual(values=c("red", "blue", "green"))
plot_all

# Now we will select data for the 2 buckets identified from the data
bucket_subset_analyse<-filter(global_superstore_db,Market %in% buckets_analyze$Market & Segment %in% buckets_analyze$Segment)
bucket_subset_analyse<-bucket_subset_analyse %>% select(Market,Segment,Order.Date,order_month,Sales,Quantity)
str(bucket_subset_analyse)

#---------------------- End of Data Cleaning and Preparation ----------------------------

# From Oder Date we will now extract Year
bucket_subset_analyse$order_year<-year(bucket_subset_analyse$Order.Date)

# Lets get the monthly sales for each of the 2 segments for each month of each year
aggregated_subset_sales<-bucket_subset_analyse %>%select(Market,Segment,Sales,order_month,order_year,Order.Date) %>%group_by(Market,Segment,order_month,order_year) %>% mutate(monthly_sales=sum(Sales))
aggregated_subset_sales

aggregated_subset_quantity<-bucket_subset_analyse %>%select(Market,Segment,Quantity,order_month,order_year,Order.Date) %>%group_by(Market,Segment,order_month,order_year) %>% mutate(monthly_demand=sum(Quantity))
aggregated_subset_quantity

#Now we will apply time series for each segment

#--------------TIME SERIES ANALYSIS MODELLING-----------------------------------------------------

smoothen<-function(timeser,w=3){
  
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  smoothedseries
}

plotExponential<-function(timeser,alphas,title=""){
  plot(timeser,main=title)
  cols<-c('red', 'blue', 'green', 'black')
  
  labels <- c(paste('alpha =', alphas), 'Original')
  for(i in seq(1 , length(alphas))){
    smoothedseries<-HoltWinters(timeser,alpha = alphas[i],beta = F,gamma = F)
    lines(fitted(smoothedseries)[,1],col=cols[i],lwd=2)
    
  }
  legend("topleft", labels,col=cols, lwd=2,pt.cex = 1,cex=0.5)
}

#--------------------- EU - European Union-Consumer Segment Time Series Analysis---------------------------------------------

#--------------------- Creation of time series data---------------------------------------------
eu_consmr_sales<-aggregated_subset_sales %>% filter(Market=="EU" & Segment=="Consumer") %>%  select(Order.Date,monthly_sales)  
eu_consmr_sales$Month<-as.Date(cut(eu_consmr_sales$Order.Date,breaks="month"))
eu_consmr_sales<-eu_consmr_sales[,c("Month","monthly_sales")]
eu_consmr_sales<- unique(eu_consmr_sales)
eu_consmr_sales<-arrange(eu_consmr_sales,Month)
month=as.numeric(row.names(eu_consmr_sales))
eu_consmr_sales$Month<-NULL
eu_consmr_sales$Month<-month

eu_consmr_demand<-aggregated_subset_quantity %>% filter(Market=="EU" & Segment=="Consumer") %>%  select(Order.Date,monthly_demand)  
eu_consmr_demand$Month<-as.Date(cut(eu_consmr_demand$Order.Date,breaks="month"))
eu_consmr_demand<-eu_consmr_demand[,c("Month","monthly_demand")]
eu_consmr_demand<- unique(eu_consmr_demand)
eu_consmr_demand<-arrange(eu_consmr_demand,Month)
month=as.numeric(row.names(eu_consmr_demand))
eu_consmr_demand$Month<-NULL
eu_consmr_demand$Month<-month

apac_consumer_sales<-aggregated_subset_sales %>% filter(Market=="APAC" & Segment=="Consumer") %>%  select(Order.Date,monthly_sales)  
apac_consumer_sales$Month<-as.Date(cut(apac_consumer_sales$Order.Date,breaks="month"))
apac_consumer_sales<-apac_consumer_sales[,c("Month","monthly_sales")]
apac_consumer_sales<- unique(apac_consumer_sales)
apac_consumer_sales<-arrange(apac_consumer_sales,Month)
month=as.numeric(row.names(apac_consumer_sales))
apac_consumer_sales$Month<-NULL
apac_consumer_sales$Month<-month

apac_consumer_demand<-aggregated_subset_quantity %>% filter(Market=="APAC" & Segment=="Consumer") %>%  select(Order.Date,monthly_demand)  
apac_consumer_demand$Month<-as.Date(cut(apac_consumer_demand$Order.Date,breaks="month"))
apac_consumer_demand<-apac_consumer_demand[,c("Month","monthly_demand")]
apac_consumer_demand<- unique(apac_consumer_demand)
apac_consumer_demand<-arrange(apac_consumer_demand,Month)
month=as.numeric(row.names(apac_consumer_demand))
apac_consumer_demand$Month<-NULL
apac_consumer_demand$Month<-month
#------------------------------ End Creation of time series data -----------------------------------------------

#------------------------------ Time Series for forecasting EU - European Union-Consumer sales data -----------------------
eu_consumer_sales_timeser_total<-ts(eu_consmr_sales$monthly_sales)

plot(eu_consumer_sales_timeser_total,col="red",main="EU Consumer Sales Time Series")
eu_consumer_sales_42 <- eu_consmr_sales[1:42,]
timeser_1 <- ts(eu_consumer_sales_42$monthly_sales)
plot(timeser_1)

#GS added code  to decompose
plot(decompose(ts(eu_consumer_sales_42$monthly_sales,frequency=4)))

kpss.test(timeser_1)### not stationary
timeser_1_smoothedseries<-smoothen(timeser = timeser_1,w=1)

eu_consumer_sales_42_in <- eu_consumer_sales_42$Month
lines(timeser_1_smoothedseries, col="blue", lwd=2)

smootheddf1 <- as.data.frame(cbind(eu_consumer_sales_42_in, as.vector(timeser_1_smoothedseries)))
colnames(smootheddf1) <- c('Month', 'Sales')

lmfit_1 <- lm(Sales ~ sin(Month) * poly(Month,3), data=smootheddf1)
global_pred_1 <- predict(lmfit_1, Month=eu_consumer_sales_42_in)
summary(global_pred_1)
lines(eu_consumer_sales_42_in, global_pred_1, col='red', lwd=2)

# Look at the locally predictable series and model it as an ARMA series
arma_local_pred_1 <- timeser_1-global_pred_1
plot(arma_local_pred_1, col='red', type = "l")
acf(arma_local_pred_1)
acf(arma_local_pred_1, type="partial")
armafit_1 <- auto.arima(arma_local_pred_1)
armafit_1

#We'll check if the residual series is white noise
resi_1 <- arma_local_pred_1-fitted(armafit_1)
plot(resi_1)
kpss.test(resi_1)
#Both testshows that the series is now pure stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_1 <- eu_consmr_sales[43:48,]
outdata_1<-data.frame(outdata_1)
class(outdata_1)
timevals_out_1 <- outdata_1$Month

global_pred_out_1 <- predict(lmfit_1,data.frame(Month =timevals_out_1))

fcast_1 <- global_pred_out_1
class(fcast_1)
# We will now compare our prediction with the actual values, using MAPE
mape_class_dec_1 <- accuracy(fcast_1,outdata_1[,1])[5]
mape_class_dec_1 ##28.04 Good low Mape value

class_dec_pred_1 <- c(ts(global_pred_1),ts(global_pred_out_1))
plot(eu_consumer_sales_timeser_total, col = "black")
lines(class_dec_pred_1, col = "red")

# Code added by GS
# Auto Arima
#-------------
autoarima1 <- auto.arima(timeser_1)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

resi_auto_arima1 <- timeser_1 - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary")
kpss.test(resi_auto_arima1)

# Evaluate the model using MAPE
fcast_auto_arima1 <- predict(autoarima1, n.ahead = 6)

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$pred,outdata_1[,1])[5]
MAPE_auto_arima1 ## 28.9226

# We will now plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$pred))
plot(eu_consumer_sales_timeser_total, col = "black")
lines(auto_arima_pred1, col = "red")

#------------------------ Time Series for forecasting EU - European Union-Consumer demand data ---------------

eu_consumer_demand_timeser_total<-ts(eu_consmr_demand$monthly_demand)
plot(eu_consumer_demand_timeser_total,col="red",main="EU Consumer Demand Time Series")
EU_Consumer_Demand_42 <- eu_consmr_demand[1:42,]
timeser_2 <- ts(EU_Consumer_Demand_42$monthly_demand)
plot(timeser_2)

# GS added code  to decompose
plot(decompose(ts(EU_Consumer_Demand_42$monthly_demand,frequency=4)))

kpss.test(timeser_2)### not stationary
timeser_2_smoothedseries<-smoothen(timeser = timeser_2,w=1)

eu_consumer_demand_42_in <- EU_Consumer_Demand_42$Month
lines(timeser_2_smoothedseries, col="blue", lwd=2)

smootheddf2 <- as.data.frame(cbind(eu_consumer_demand_42_in, as.vector(timeser_2_smoothedseries)))
colnames(smootheddf2) <- c('Month', 'Demand')

# Fit a multiplicative model with trend and seasonality to the data, which is modeled using a sinusoid function
lmfit_2 <- lm(Demand ~ sin(Month) * poly(Month,3), data=smootheddf2)
global_pred_2 <- predict(lmfit_2, Month=eu_consumer_demand_42_in)
summary(global_pred_2)
lines(eu_consumer_demand_42_in, global_pred_2, col='red', lwd=2)

# Look at the locally predictable series and model it as an ARMA series
arma_local_pred_2 <- timeser_2-global_pred_2
plot(arma_local_pred_2, col='red', type = "l")
acf(arma_local_pred_2)
acf(arma_local_pred_2, type="partial")
armafit_2 <- auto.arima(arma_local_pred_2)

tsdiag(armafit_2)
armafit_2

#We'll check if the residual series is white noise
resi_2 <- arma_local_pred_2-fitted(armafit_2)
plot(resi_2)
kpss.test(resi_2)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_2 <- eu_consmr_demand[43:48,]
outdata_2<-data.frame(outdata_2)
timevals_out_2 <- outdata_2$Month

global_pred_out_2 <- predict(lmfit_2,data.frame(Month =timevals_out_2))
fcast_2 <- global_pred_out_2

# Compare our prediction with the actual values, using MAPE
mape_class_dec_2 <- accuracy(fcast_2,outdata_2[,1])[5]
mape_class_dec_2 ##35.31684

#Plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_2 <- c(ts(global_pred_2),ts(global_pred_out_2))
plot(eu_consumer_demand_timeser_total, col = "black")
lines(class_dec_pred_2, col = "red")

# Code added by GS
# Auto Arima
#-------------
autoarima2 <- auto.arima(timeser_2)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,outdata_2[,1])[5]
MAPE_auto_arima2 # 30.13319

# Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(eu_consumer_demand_timeser_total, col = "black")
lines(auto_arima_pred2, col = "red")


#------------------Building time series for forecasting APAC consumer sales data-------------------
apac_consumer_sale_timeser_total<-ts(apac_consumer_sales$monthly_sales)

plot(apac_consumer_sale_timeser_total,col="red",main="APAC Consumer Sales Time Series")
apac_consumer_sales_42 <- apac_consumer_sales[1:42,]
timeser_3 <- ts(apac_consumer_sales_42$monthly_sales)

# GS added code  to decompose
plot(decompose(ts(apac_consumer_sales_42$monthly_sales,frequency=4)))


plot(timeser_3)
kpss.test(timeser_3)### not stationary
timeser_3_smoothedseries<-smoothen(timeser = timeser_3,w=1)

APAC_Consumer_Sales_42_in <- apac_consumer_sales_42$Month
lines(timeser_3_smoothedseries, col="blue", lwd=2)


smootheddf3 <- as.data.frame(cbind(APAC_Consumer_Sales_42_in, as.vector(timeser_3_smoothedseries)))
colnames(smootheddf3) <- c('Month', 'Sales')

# Fit a multiplicative model with trend and seasonality to the data, which is modeled using a sinusoid function
lmfit_3 <- lm(Sales ~ sin(Month) * poly(Month,3), data=smootheddf3)
global_pred_3 <- predict(lmfit_3, Month=APAC_Consumer_Sales_42_in)
summary(global_pred_3)
lines(APAC_Consumer_Sales_42_in, global_pred_3, col='red', lwd=2)

# Look at the locally predictable series and model it as an ARMA series
arma_local_pred_3 <- timeser_3-global_pred_3
plot(arma_local_pred_3, col='red', type = "l")
acf(arma_local_pred_3)
acf(arma_local_pred_3, type="partial")
armafit_3 <- auto.arima(arma_local_pred_3)

tsdiag(armafit_3)
armafit_3

# We'll check if the residual series is white noise
resi_3 <- arma_local_pred_3-fitted(armafit_3)
plot(resi_3)
kpss.test(resi_3)

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months

outdata_3 <- apac_consumer_sales[43:48,]
outdata_3<-data.frame(outdata_3)
timevals_out_3 <- outdata_3$Month

global_pred_out_3 <- predict(lmfit_3,data.frame(Month =timevals_out_3))

fcast_3 <- global_pred_out_3
#Now, let's compare our prediction with the actual values, using MAPE
mape_class_dec_3 <- accuracy(fcast_3,outdata_3[,1])[5]
mape_class_dec_3 ##31.15703

# Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_3 <- c(ts(global_pred_3),ts(global_pred_out_3))
plot(apac_consumer_sale_timeser_total, col = "black")
lines(class_dec_pred_3, col = "red")

# Code added by GS
# Auto Arima
#-------------
autoarima3 <- auto.arima(timeser_3)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

# Let's evaluate the model using MAPE
fcast_auto_arima3 <- predict(autoarima3, n.ahead = 6)
MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$pred,outdata_3[,1])[5]
MAPE_auto_arima3 #27.68952

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$pred))
plot(apac_consumer_sale_timeser_total, col = "black")
lines(auto_arima_pred3, col = "red")

#----------------------Building time series for forecasting APAC consumer demand data-----------------

apac_consmr_demand_timeser_total<-ts(apac_consumer_demand$monthly_demand)
plot(apac_consmr_demand_timeser_total,col="red",main="Apac Consumer Demand Time Series")
apac_consumer_demand_42 <- apac_consumer_demand[1:42,]
timeser_4 <- ts(apac_consumer_demand_42$monthly_demand)
plot(timeser_4)

#GS added code  to decompose
plot(decompose(ts(apac_consumer_demand_42$monthly_demand,frequency=4)))

kpss.test(timeser_4)### not stationary
timeser_4_smoothedseries<-smoothen(timeser = timeser_4,w=1)

apac_consumer_demand_42_in <- apac_consumer_demand_42$Month
lines(timeser_4_smoothedseries, col="blue", lwd=2)

smootheddf4 <- as.data.frame(cbind(apac_consumer_demand_42_in, as.vector(timeser_4_smoothedseries)))
colnames(smootheddf4) <- c('Month', 'Demand')

#Now, let's fit a multiplicative model with trend and seasonality, which is modeled using a sinusoid function
lmfit_4 <- lm(Demand ~ sin(Month) * poly(Month,3), data=smootheddf4)
global_pred_4 <- predict(lmfit_4, Month=apac_consumer_demand_42_in)
summary(global_pred_4)
lines(apac_consumer_demand_42_in, global_pred_4, col='red', lwd=2)

#Now, let's look at the locally predictable series and model it as an ARMA series
arma_local_pred_4 <- timeser_4-global_pred_4
plot(arma_local_pred_4, col='red', type = "l")
acf(arma_local_pred_4)
acf(arma_local_pred_4, type="partial")
armafit_4 <- auto.arima(arma_local_pred_4)

tsdiag(armafit_4)
armafit_4

#We'll check if the residual series is white noise
resi_4 <- arma_local_pred_4-fitted(armafit_4)
plot(resi_4)
kpss.test(resi_4)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_4 <- apac_consumer_demand[43:48,]
outdata_4<-data.frame(outdata_4)
timevals_out_4 <- outdata_4$Month

global_pred_out_4 <- predict(lmfit_4,data.frame(Month =timevals_out_4))

fcast_4 <- global_pred_out_4
#Now, let's compare our prediction with the actual values, using MAPE
mape_class_dec_4 <- accuracy(fcast_4,outdata_4[,1])[5]
mape_class_dec_4 ##41.49113

# Plot the predictions along with original values, to get a visual feel of the fit

class_dec_pred_4 <- c(ts(global_pred_4),ts(global_pred_out_4))
plot(apac_consmr_demand_timeser_total, col = "black")
lines(class_dec_pred_4, col = "red")

# Code added by GS
# Auto Arima
#-------------
autoarima4 <- auto.arima(timeser_4)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black")
lines(fitted(autoarima4), col="red")

# Also, let's evaluate the model using MAPE
fcast_auto_arima4 <- predict(autoarima4, n.ahead = 6)

MAPE_auto_arima4 <- accuracy(fcast_auto_arima4$pred,outdata_4[,1])[5]
MAPE_auto_arima4 #26.24458

# Plot the predictions along with original values, to get a visual feel of the fit

auto_arima_pred4 <- c(fitted(autoarima4),ts(fcast_auto_arima4$pred))
plot(apac_consmr_demand_timeser_total, col = "black")
lines(auto_arima_pred4, col = "red")

#----------------------------- End Notes -----------------------------------------#
#We have sucessfully forecasted the Demand and Sales Forecasting Model for the target market buckets:
#[1] APAC-CONSUMER
#[2] EU-CONSUMER
#Modelling is done using Classical Decomposition & ARIMA methods. Results are as follows

#1.EU_Consumer_Sales Classical MAPE:28.04386    ARIMA Mape  :   28.9226
#2.EU_Consumer_Demand Classical MAPE:35.31684    ARIMA Mape  :  30.13319
#3.APAC_Consumer_Sales Classical MAPE: 31.15703   ARIMA Mape  :  27.68952
#4.APAC_Consumer_Demand Classical MAPE: 41.49113   ARIMA Mape  : 26.24458 
# ARIMA is found to be better  for modelling
#----------------------------- Thanks ---------------------------------------------#