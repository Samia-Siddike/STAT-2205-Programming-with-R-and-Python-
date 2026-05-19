#setting home directory
getwd()
setwd("F:/Programming with R and Python")
list.files()
.libPaths()

#read data
library(readxl)
airline<-read.csv("flight_data.csv",1)
airline

#random Sampling using student ID
set.seed(12339)
#random sampling
install.packages("dplyr")
library(dplyr)
airline_rs<-sample_n(airline,12339)
airline_rs


###question 1
#What do coach ticket price look like ?
library(ggplot2)
ggplot(airline_rs, aes(x = coach_price)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  ggtitle("Distribution of Coach Ticket Prices")

#high and low values, averege
max(airline_rs$coach_price)
min(airline_rs$coach_price)
mean(airline_rs$coach_price)


###question 2
#What are the prices for 8-hour flights?
flight_8hr<-subset(airline_rs,hours==8)
max(flight_8hr$coach_price)
min(flight_8hr$coach_price)
mean(flight_8hr$coach_price)


###question 3
class(airline_rs$delay)
median(airline$delay)
library(ggplot2)
ggplot(airline_rs, aes(x = delay)) +
  geom_histogram(fill = "orange", bins = 30) +
  ggtitle("Distribution of Flight Delays")

summary(airline$delay)


###question 4
#multiple correlation
correlation <- cor(airline_rs[,c('coach_price','miles','passengers',
                              'delay','hours')])["coach_price",]
print(correlation)


###question 5
#relationship between coach and first_class prices
#scatter plot
library(ggplot2)
ggplot(airline_rs, aes(x = coach_price, y = firstclass_price)) +
  geom_point(color = "blue") +
  ggtitle("Coach Price vs First-Class Price")

cor(airline$coach_price, airline$firstclass_price)
model<-lm(firstclass_price~coach_price,data=airline_rs)
print(model)
summary(model)

###question_6
#multiple regression
Coach_price_lm=lm(coach_price ~ inflight_meal+inflight_entertainment
                 +inflight_wifi, data=airline_rs)
print(Coach_price_lm)
summary(Coach_price_lm)

###question 7
#linear regression
cor(airline_rs$hours,airline_rs$passengers)
model_passenger<-lm(hours~passengers,data=airline_rs)
summary(model_passenger)
#visualization-scatter plot
plot(airline$hours,airline$passengers,
     main="Flight hours Vs passenger",
     xlab="Flight duration (Hours)",
     ylab="Number of passengers",
     col=4, las=1)


###question 8
weekend_data<-airline_rs[airline_rs$weekend==1,]
weekday_data<-airline_rs[airline_rs$weekend==0,]
print(weekday_data)
print(weekend_data)
library(ggplot2)
ggplot(airline_rs, aes(x = coach_price,
                       y = firstclass_price,
                       colour = factor(weekend))) +
                       geom_point() +
  ggtitle("weekend vs weekday:Coach Price vs First-Class Price")

#regression
model1<-lm(firstclass_price~coach_price*weekend,data=airline_rs)
summary(model1)
model2<-lm(firstclass_price~coach_price*day_of_week,data=airline_rs)
summary(model2)


###question 9
#boxplot
library(ggplot2)
ggplot(airline_rs,aes(x=day_of_week,
                      y=coach_price,
                      fill=factor(redeye)))+geom_boxplot()+
  ggtitle("Redeye vs non-redeye:coach price by day of week")
#mean comparision
aggregate(coach_price~day_of_week+redeye,
          data=airline_rs,mean)


###question 10
#summary statistics
numeric_vars <- airline_rs[, c(
  "miles",
  "passengers",
  "delay",
  "coach_price",
  "firstclass_price",
  "hours")]

sapply(numeric_vars, mean, na.rm = TRUE)

sapply(numeric_vars, median, na.rm = TRUE)

sapply(numeric_vars, sd, na.rm = TRUE)

#visualization using histogram, boxplots and bar chart
#Histogram
library(ggplot2)
ggplot(airline_rs,
       aes(x = coach_price)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Coach Price",
    x = "Coach Price",
    y = "Frequency"
  )

#Boxplot
library(ggplot2)
ggplot(airline_rs,
       aes(y = coach_price)) +
  geom_boxplot(
    fill = "turquoise3",
    color = "black"
  ) +
  labs(
    title = "Boxplot of Coach Price",
    y = "Coach Price"
  )
#bar chart
ggplot(airline_rs,
       aes(x = day_of_week)) +
  geom_bar(
    fill = "coral",
    color = "black"
  ) +
  labs(
    title = "Flights by Day of Week",
    x = "Day of Week",
    y = "Count"
  )

#weekend vs weekday bar chart
library(ggplot2)
ggplot(airline_rs,
       aes(x = factor(weekend),
           fill = factor(weekend))) +
  geom_bar() +
  scale_fill_manual(
    values = c("steelblue", "skyblue")
  ) +
  labs(
    title = "Weekend vs Weekday Flights",
    x = "Weekend",
    y = "Count"
  )

#testing hypothesis
#t-test(weekend affect on coach price)
t.test(coach_price ~ weekend, data = airline_rs)

#z-test
install.packages("BSDA")
library(BSDA)
z.test(
  airline_rs$coach_price,
  mu = 100,
  sigma.x = sd(airline_rs$coach_price))

#chi-square test (weekend vs redeye)
chisq.test(table(airline_rs$weekend, airline_rs$redeye))

#Independent t-test
t.test(coach_price ~ weekend, data = airline_rs)

#price difference (Weekend vs Weekday)
aggregate(coach_price ~ weekend,
          data = airline_rs,
          mean)
#correlation analysis
cor(airline_rs[, c("coach_price",
                   "firstclass_price",
                   "miles","hours",
                   "passengers","delay")],
    use = "complete.obs")

#Regression Analysis
model1 <- lm(coach_price ~ inflight_meal +
               inflight_entertainment +
               inflight_wifi +
               miles +
               passengers,
             data = airline_rs)

summary(model1)

#linear regression(price prediction)
model2 <- lm(firstclass_price ~ coach_price + miles + passengers,
             data = airline_rs)

summary(model2)

#Logistic Regression (Redeye prediction)
table(airline_rs$redeye)
airline_rs$redeye <- ifelse(
  airline_rs$redeye == "Yes", 1,0)
log_model <- glm(redeye ~ coach_price + miles + passengers + delay,
                 data = airline_rs,
                 family = binomial)

summary(log_model)





