# setwd("E:/DSC/Statistic/R")
#===============================================================================
# Term Project: AirBNB House Price Prediction in New York
# Suchada Manowon 640632023
#===============================================================================

options(max.print = 10000) # set global option

#===========================================
# import module and read dataset
#===========================================

# --- import libraries
library(tidyverse) # for data viualization
library(knitr)
library(dplyr) # for group by
library(corrplot) # for plot corrlation
library(Hmisc)
library(GGally) # for plot correlation
library(fastDummies) # for create dummy var
library(Metrics) # for check RMSE
library(caret) # for check model

# --- import data
data = read.csv('AB_NYC_2019.csv' ,stringsAsFactors = T) # set var string to factor

#===========================================
# 1 Data preprocessing
#===========================================

# NOTE: checking missing value and drop some column
# - neighbourhood_group: if not group will more better
# - neighbourhood:so many group and same mean of neighbourhood_group ** many be not use
# - room_type: ok
# - price: ok
# - last_review and reviews_per_month missing value 10052 => 25% and corr with price is -0.03060835 drop it 
#   delete some columns that not want to use

data = subset(data, select = -c(id, name,host_id,host_name, neighbourhood))
#data = subset(data, select = -c(id, name,host_id,host_name))
str(data) # view structure
describe(data)

# check missing value
# data <- data %>% summarise_all(~(sum(is.na(.))/n()))
# data <- gather(missing_airbnb, key = "variables", value = "percent_missing")
# data <- missing_airbnb[missing_airbnb$percent_missing > 0.0, ] 


#===========================================
# 2 Exploratory Data Analysis
#===========================================

# Dependent variable distribution
hist(data$price,
     main="Histogram of Price",
     xlab="price",
     xlim=c(0,1000),
     freq=,
     breaks=150
)

# correlation of variable x
# ------------------------------------------
# Plotting the correlation matrix
par(mar=c(1,1,1,1)) # set Plots outline

# linear relationship from the scatter plots 
pairs(data[,-8],main="scatterplot matrix")

airbnb_cor <- data[, sapply(data, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")

# NOTE: from the ggcorr, we see low correlation between all variables


# Boxplots for Numerical Variables
boxplot(data$price, ylab="Price")
boxplot(data$minimum_nights, ylab="minimum_nights")
boxplot(data$calculated_host_listings_count, ylab="calculated_host_listings_count")
# NOTE: have many outline

ggplot(data, aes(x = fct_infreq(data$room_type), fill = data$room_type)) +
  geom_bar() +
  theme(legend.position = "bottom")

ggplot(data, aes(x = fct_infreq(data$neighbourhood_group), fill = data$neighbourhood_group)) +
  geom_bar() +
  theme(legend.position = "bottom")

ggplot(data, aes(x = room_type, y = price)) +
  geom_violin() +
  scale_y_log10()

ggplot(data, aes(x = neighbourhood_group, y = price)) +
  geom_violin() +
  scale_y_log10()


#===========================================
# 3 Multiple regression model
#===========================================

# Factor variable, Dummy variable: change 
#-------------------------------------------
data.dum = dummy_cols(data, select_columns = c('neighbourhood_group','room_type'))
names(data.dum) %<>% stringr::str_replace_all("\\s","_") %>% tolower
# NOTE: NOT need to change to dummy var, in lm will change suto when they are factors.

# Variable selection
# add and remove using AIC and t-test, reduced mullticollity
# ------------------------------------------
# null_mod =lm(price~1, data=data.dum) # not have independence var
# full_mod =lm(price ~., data=data.dum) # use all independence var
# library(MASS)
# step.model =  stepAIC(full_mod, direction = "backward",
#                       trace = FALSE)
# summary(step.model)


# Split data: trainset:testset -> 90:10
# ------------------------------------------
# sampling techinque separate test and trains data

samplesize = 0.8*nrow(data.dum)
set.seed(252)
index = sample(seq_len(nrow(data.dum)), size=samplesize)

dataTrain = data.dum[index,]
dataTest = data.dum[-index,]


# Building Multiple Regression Model
#-------------------------------------------
model = lm(price ~ neighbourhood_group_brooklyn+neighbourhood_group_manhattan+neighbourhood_group_staten_island
           +latitude+longitude+room_type_private_room+room_type_shared_room
           +reviews_per_month
           # +neighbourhood
           +number_of_reviews+calculated_host_listings_count+availability_365,
           data=dataTrain)
summary(model)
AIC(model) # 669843.5 -> 605449.9 -> 599611.3 -> 363560.6
# predModel1 = predict(model, newdata = dataTest) # not need to predict, model is so bad
# predModel1= exp(predModel1)
#plot(model)

# Transformation: Log-Linear Regression Model
#-------------------------------------------
# NOTE: change some numeric variables to log1p + use minimum_nights, better result
# clean data again drop outlier price
dataTrainLog =  dataTrain %>% filter(price < quantile(dataTrain$price, 0.9) & price > quantile(dataTrain$price, 0.1)) %>% tidyr::drop_na()
modelLog = lm(log1p(price) ~ neighbourhood_group_brooklyn+neighbourhood_group_manhattan+neighbourhood_group_staten_island
              +latitude+longitude+room_type_private_room+room_type_shared_room
              # +neighbourhood
              +log1p(reviews_per_month)+log1p(number_of_reviews)+log1p(calculated_host_listings_count)+log1p(availability_365),
              data=dataTrainLog)
summary(modelLog) 
AIC(modelLog) # 669843.5 -> 605449.9 -> 11990.43 (better)
#plot(modelLog)
# NOTE: SSE of log model increasing(not good), but AIC and r-square are better. 

# # use log model to test
dataTest = dataTest %>% filter(price <= quantile(dataTest$price, 0.8) & price >= quantile(dataTest$price, 0.2)) %>% tidyr::drop_na()
regressionPred = predict(modelLog, newdata = dataTest)
regressionPred = exp(regressionPred)


#===========================================
# 4. Model checking
#===========================================

summary(model)$r.squared
summary(modelLog)$r.squared # better than model 

d = dataTest$price - regressionPred
mse = mean((d)**2)
mae = mean(abs(d))
RMSE = sqrt(mean( (d)**2 ))
SSE = sum((d)**2)
SSR = sum((regressionPred - mean(dataTest$price)) ** 2)
R2 = 1 - SSE/(SSE + SSR)

cat("MSE:", mse, "\n","MAE:", mae, "\n", "SSE:", SSE, "\n",
    "RMSE:", RMSE, "\n", "R-squared:", R2)


#errors/residuals must follow normal distribution, so we need to check first
resd_MRM = summary(model)$res
resd_LogMRM = summary(modelLog)$res

# hist(resd, breaks = 20)
hist(resd_MRM)
qqnorm(resd_MRM, pch = 19, frame = FALSE)
qqline(resd_MRM, col = "blue", lwd = 2)

# hist(resd, breaks = 20)
hist(resd_LogMRM)
qqnorm(resd_LogMRM, pch = 19, frame = FALSE)
qqline(resd_LogMRM, col = "blue", lwd = 2)


# Plot to show error
# --------------------
par(mar=c(1,1,1,1))
# par(mfrow = c(2, 2))
plot(model)
plot(modelLog)
#install.packages("ggResidpanel")
library(ggResidpanel)
resid_panel(model)
resid_xpanel(model)

#install.packages("ggfortify")
library(ggfortify)
fit.f <- fortify(modelLog)
ggplot(fit.f, aes(x = .fitted, y = .resid)) + geom_point()