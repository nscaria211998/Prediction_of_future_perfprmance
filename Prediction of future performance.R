# loading the data set in r
install.packages("readxl")
install.packages("neuralnet")
install.packages("tidyverse")
install.packages("leaps")
install.packages("corrplot")
install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("mltools")
install.packages("repr")
library(repr)
library(readxl)
library(neuralnet)
library(tidyverse)
library(leaps)
library(corrplot)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(mltools)
library(repr)

# Function to scrape season skater statistics from Hockey-reference.com
# The scraping function was developed by a group member in a different class
# and the appropriate citations can be found in the references of the report. To
# test the scraping function, please un comment the function, run the the function and
# call it using the season to extract from the website. e.g. to get data for season
# 2008, use scrapeSkaters(2008). The group pre-scraped the data and saved it as an excel
#file for grading convenience
# scrapeSkaters <- function(S) {
# # The function takes parameter S which is a string and represents the season (YYYY)
# # Returns: data frame
#
# require(XML)
# require(httr)
#
# # Define certicificate file, needed since website is HTTPS
# cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
#
# cafile <- "/etc/ssl/cert.pem"
#
#
# # Read secure page
# ## create the URL to scrape data from
# URL <- paste("https://www.hockey-reference.com/leagues/NHL_",S, "_skaters.html", sep="")
# page <- GET(URL,config(cainfo=cafile))
#
# # Use regex to extract the desired table from the page
# x <- text_content(page) #will give a deprecation warning, but that is OK
# tab <- sub('.*(<table class="sortable stats_table".*?>.*</table>).*', '\\1', x)
#
# ## grab the data from the page
# tables <- readHTMLTable(tab)
# ds.skaters <- tables$stats
#
# ds.skaters <- ds.skaters[which(ds.skaters$Rk!="Rk"),]
#
# ## Convert to lower case character data (otherwise will be treated as factors)
# for(i in 1:ncol(ds.skaters)) {
# ds.skaters[,i] <- as.character(ds.skaters[,i])
# names(ds.skaters) <- tolower(colnames(ds.skaters))
# }
#
# ## finally fix the columns - NAs forced by coercion warnings
# for(i in c(1, 3, 6:19)) {
# ds.skaters[,i] <- as.numeric(ds.skaters[, i])
# }
#
# cn <- colnames(ds.skaters)
# ds.skaters <- cbind(ds.skaters,ppp=rowSums(ds.skaters[,which(cn=="pp")]))
# ds.skaters <- cbind(ds.skaters,shp=rowSums(ds.skaters[,which(cn=="sh")]))
# cn <- colnames(ds.skaters)
#
# ## fix a couple of the column names
# #colnames(ds.skaters)
# names(ds.skaters)[11] <- "pim"
# names(ds.skaters)[18] <- "ppa"
# names(ds.skaters)[14] <- "ppg"
# names(ds.skaters)[15] <- "shg"
# names(ds.skaters)[19] <- "sha"
# names(ds.skaters)[10] <- "PlusMinus"
#
#
# ## remove the header and totals row
# ds.skaters <- ds.skaters[!is.na(ds.skaters$rk), ]
#
# ## add the year too
# ds.skaters$season <- S
#
# ## remove any ' from players names (will case parsing issues later otherwise)
# ds.skaters$player <- gsub("'","",ds.skaters[,"player"])
#
# ## return the dataframe of subset of all categories
# return(ds.skaters[,c(2:11,14,18,29,15,19,30,20,25,24)])
# #ds.skaters
# }
data <- read_excel("~/Downloads/data_stat-1.xlsx")
str(data)
df <- data
df <- df[,-1]
#plot(df)
#preliminary analysis
mean_df <- colMeans(df[,2:ncol(df)])
median_df <- apply(df[2:ncol(df)], 2, median)
var_df <- apply(df[2:ncol(df)], 2, var)
std_df <- apply(df[2:ncol(df)], 2, sd)
ggplot(data=df, aes(g_ns)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()
#Model selection
#full model
m1 <- lm(g_ns ~., data = df)
summary(m1)
summary(m1)$coefficient
#model 2 dropped variables -
m2 <- lm(g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + s + hit + blk, data = df)
summary(m2)
summary(m2)$coefficient
#model 3 stepwise selection -
m3 <- lm(g_ns ~ . , data = df)
selectedMod <- step(m3)
summary(selectedMod)
#model 4 Best subsets
m4 <- regsubsets(g_ns ~. , data = df, nvmax = 17)
summary(m4)
m4 <- regsubsets(g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + ppg + s + sha + shg + hit + blk + year, data = df, nvmax = 14)
summary(m4)
res.sum <- summary(m4)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
#k folds cross validations -
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- caret::train(model.formula, data = data, method = "lm",
                     trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:14
cv.errors <- map(model.ids, get_model_formula, m4, "g_ns") %>%
  map(get_cv_error, data = df) %>%
  unlist()
cv.errors
which.min(cv.errors)
m4_selected <- lm(g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + s + hit + blk +
                    year, data = df)
summary(m4_selected)
plot(m4_selected)
#transformations and linearity assumptions plotsdf2 <- df[,- c(5, 8, 10:13)]
plot(df2)
dft <- data.frame(log(df2$age), df2$gp, sqrt(df2$g), df2$a, df2$PlusMinus, df2$pim, df2$ppa, df2$s, df2$hit, df2$blk, df2$year,
                  df2$g_ns)
plot(dft)
colnames(dft) <- c("age", "gp", "g", "a", "PlusMinus", "pim", "ppa", "s", "hit", "blk", "year", "g_ns")
m5 <- lm(g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + s + hit + blk +
           year, data = dft)
summary(m5)
summary(m5)$coefficient
confint(m5)
par(mfrow = c(2, 2))
plot(m5)
# anova table for selected model
anova(m5)
# predictions
#machine learning
#scaling the data
set.seed(100)
df_<- dft[,-c(11)]
scaled <- as.data.frame(df_[,1:ncol(df_)])
maxs <- as.data.frame(apply(df_, 2, function(x){ max(x)})) %>% t()
mins <- as.data.frame(apply(df_, 2, function(x){ min(x)})) %>% t()
scaled <- as.data.frame(scale(scaled, center = mins, scale = maxs - mins))
# Train-test random splitting
index <- sample(1:nrow(df_),round(0.5*nrow(df_)))
train_ <- scaled[index,]
test_ <- scaled[-index,]
st_g <- g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + s + hit + blk
#training the neural networks.
#Please note that the training takes around 20-25 seconds
nn <- neuralnet(st_g, data=train_ , hidden = c(8),stepmax = 1e+40,learningrate = 10,act.fct = "tanh", linear.output=T)
pr <- neuralnet::compute(nn,test_)
pr_nn <- (pr$net.result*(max(data$g_ns)-min(data$g_ns)))+min(data$g_ns)
test.nn <- ((test_$g_ns)*(max(data$g_ns)-min(data$g_ns)))+min(data$g_ns)
MSE_nn <- sum((test.nn - pr_nn)^2)/nrow(test_)
print(MSE_nn)
# Step 2 - predicting and evaluating the model on train data
predictions = predict(m5, newdata = dft)
mse(predictions, dft$g_ns)
#plots
par(mfrow=c(1,2))
plot(dft$g_ns, predictions, xlab = "goals next season", ylab = "predicted goals", col = "maroon", main = "Actual vs predicted goals
MLR")
plot(test.nn, pr_nn, xlab = "goals next season", ylab = "predicted goals", col = "red", main = "Actual vs predicted goals ANN")
confint(m5)
par(mfrow = c(2, 2))
plot(m5)
# anova table for selected model
anova(m5)
# predictions
#machiene learning
#scaling the data
set.seed(100)
df_<- dft[,-c(11)]
scaled <- as.data.frame(df_[,1:ncol(df_)])
maxs <- as.data.frame(apply(df_, 2, function(x){ max(x)})) %>% t()
mins <- as.data.frame(apply(df_, 2, function(x){ min(x)})) %>% t()
scaled <- as.data.frame(scale(scaled, center = mins, scale = maxs - mins))
# Train-test random splitting
index <- sample(1:nrow(df_),round(0.5*nrow(df_)))
train_ <- scaled[index,]
test_ <- scaled[-index,]
st_g <- g_ns ~ age + gp + g + a + PlusMinus + pim + ppa + s + hit + blk
#training the neural networks.
#Please note that the training takes around 20-25 seconds
nn <- neuralnet(st_g, data=train_ , hidden = c(8),stepmax = 1e+40,learningrate = 10,act.fct = "tanh", linear.output=T)
pr <- neuralnet::compute(nn,test_)
pr_nn <- (pr$net.result*(max(data$g_ns)-min(data$g_ns)))+min(data$g_ns)
test.nn <- ((test_$g_ns)*(max(data$g_ns)-min(data$g_ns)))+min(data$g_ns)
MSE_nn <- sum((test.nn - pr_nn)^2)/nrow(test_)
print(MSE_nn)
# Step 2 - predicting and evaluating the model on train data
predictions = predict(m5, newdata = dft)
mse(predictions, dft$g_ns)
#plots
par(mfrow=c(1,2))
plot(dft$g_ns, predictions, xlab = "goals next season", ylab = "predicted goals", col = "maroon", main = "Actual vs predicted goals
MLR")
plot(test.nn, pr_nn, xlab = "goals next season", ylab = "predicted goals", col = "red", main = "Actual vs predicted goals ANN")