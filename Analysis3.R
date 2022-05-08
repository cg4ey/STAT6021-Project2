getwd()
setwd("C:/Users/ddd/Documents/UVA/MSDS/Spring 2022/STAT6021 Project2")

library(ggplot2)
library(faraway)
library(leaps)
library(MASS)

df<-read.csv("kc_house_data.csv",header=T)
##don't need the Unique ID of the house
df<-subset(df,select=-c(id))
##Since there is a zipcode variable, let's remove long/lat
df<-subset(df,select=-c(lat,long))
##was able to map zipcodes to cities, so also remove the zipcode (will be easier to look at in the analysis)
# data found: https://www.zip-codes.com/county/wa-king.asp
df<-subset(df,select=-c(zipcode))

sapply(df,class)
##make city, date, view, and waterfront categorical
df$date<-factor(df$date)
df$city<-factor(df$city)
df$view<-factor(df$view)
df$waterfront<-factor(df$waterfront)
##also make yr_renovated into binary variable
df$yr_renovated <- ifelse(df$yr_renovated>0,1,0)
df$yr_renovated<-factor(df$yr_renovated)

sapply(df,class)
dim(df)

##remove zero bedroom properties because want to look at homes not land
df<-df%>%
  filter(df$bedrooms>0)

#Since pricing generally weighed at low end, log transform price
df$price<-log(df$price)

##check for any outliers, remove outliers
ggplot(df, aes(y = price)) +
  geom_boxplot() +
  theme(plot.title=element_text(hjust = .5)) +
  labs(y = "Price", title="Market Prices")
outliers<-boxplot.stats(df$price)$out #1.13M
out_up<-min(outliers[outliers>14])
out_low<-max(outliers[outliers<12])
df<-df%>%
  filter(price<out_up & price>out_low)

##check if any linear relationships or collinearity problems
pairs(subset(df,select = -c(date,yr_renovated,city,view,waterfront)),lower.panel = NULL)
#most reasonable relationships are sqft_living, grade, sqft_above, sqft_living15 with price
#collinearity maybe with: sqft_living and sqft_above, sqft_basement, sqft_living15
# sqft_above and sqft_basement, sqft_living15
# bedrooms and sqft_living
# bathrooms and sqft_above, grade, sqft_above
# grade and sqft_above, sqft_living15

## -------------------------------------------------
## Model Creation 
## -------------------------------------------------

##stepwise models
regnull<-lm(price~1,data=df)
regall<-lm(price~.,data=df)
step(regnull,scope=list(lower=regnull,upper=regall),direction="forward") 
step(regall,scope=list(lower=regnull,upper=regall),direction="backward") 

result1<-lm(formula = price ~ grade + city + sqft_living + yr_built + 
              view + floors + condition + sqft_living15 + bathrooms + sqft_lot + 
              waterfront + yr_renovated + bedrooms + sqft_above + sqft_lot15, 
            data = df)
result2<-lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
              floors + waterfront + view + condition + grade + sqft_above + 
              yr_built + yr_renovated + sqft_living15 + sqft_lot15 + city, 
            data = df)

summary(result1)
result1
summary(result2)
result2
#interesting that all of them are significant except for sqft_lot15.
#Let's choose to use model 2 (random choice) since they both have the same adj. R^2

##-------------------------------------------------
## Partial regression checks
##-------------------------------------------------

##sqft_lot15
yresult_lot15<-lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                    floors + waterfront + view + condition + grade + sqft_above + 
                    yr_built + yr_renovated + sqft_living15 + city, 
                  data = df)
xresult_lot15<-lm(formula = sqft_lot15 ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                    floors + waterfront + view + condition + grade + sqft_above + 
                    yr_built + yr_renovated + sqft_living15 + city, 
                  data = df)
yres_lot15<-yresult_lot15$residuals
xres_lot15<-xresult_lot15$residuals
plot(xres_lot15, yres_lot15, main="Partial Regression for sqft_lot15")
##Can get rid of this since there is no linear relationship being shown. 

result2<-lm(formula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
              floors + waterfront + view + condition + grade + sqft_above + 
              yr_built + yr_renovated + sqft_living15 + city, 
            data = df)
summary(result2)
result2
#everything is significant now, adj R^2 .7563 has not changed, so can assume this fit is just as good. 

##------------------------------------------------
## Check linearity assumptions
##------------------------------------------------

yhat<-result2$fitted.values
res<-result2$residuals
df<-data.frame(df,yhat,res)
ggplot(df, aes(x=yhat,y=res))+
  geom_point(alpha=.3)+
  geom_hline(yintercept = 0,color="red")+
  theme(plot.title=element_text(hjust=.5))+
  labs(x="Predicted Price",y="Residuals",title="Model 2")

acf(res)
qqnorm(res)
qqline(res)

##Seems to be pretty good for most of the range, perhaps model not accurate past 14

##--------------------------------------------------
## Create simple model to compare to
##--------------------------------------------------

##typically, housing market goes by sqft_living, bedrooms, bathrooms, sqft_lot, waterfront, city
result0<-lm(price~sqft_living + sqft_lot + bedrooms + bathrooms + waterfront + city, data=df)
summary(result0)
result0
##this adj R^2 less than the model created by computer, so NOT as good fit. .6752

##--------------------------------------------------
## Outliers and leverage/influential points
##--------------------------------------------------

#outliers
ext.stud.res<-rstudent(result2)
n<-dim(df)[1] #number of observations
p<-14 #number of predictors in model
crit<-qt(1-.05/(2*n),n-p-1)
ext.stud.res[abs(ext.stud.res)>crit]
## five homes here

df[3957,]
df[7198,]
df[12535,]
df[18047,]
df[20716,]


#leverage
lev<-lm.influence(result2)$hat
lev[lev>2*p/n]
#lots of these

#influential observations
cooks<-cooks.distance(result2)
cooks[cooks>qf(0.5,p,n-p)]
#zero

DFFITS<-dffits(result2)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]
#lots here

DFBETAS<-dfbetas(result2)
abs(DFBETAS)>2/sqrt(n)
#looks like 0-2 in each category.

##------------------------------------------------
## Prediction Intervals
##-------------------------------------------------

##Let's use a couple example homes and see how accurate we are
#price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
#             floors + waterfront + view + condition + grade + sqft_above + 
#               yr_built + yr_renovated + sqft_living15 + sqft_lot15 + city, 

###here, assume sqft_living15 is same as home itself; assume view=1 for apartment, 2 for tiny yard, 3 for large yard, 
##  4 for waterfront; condition = 4 good; grade = 10 high end of average home

##https://www.zillow.com/homedetails/8929-SW-184th-St-Vashon-WA-98070/48852331_zpid/
home1<-data.frame(bedrooms=3, bathrooms=1.5, sqft_living=1950, sqft_living15= 1950, sqft_above=1950, sqft_lot=43124, 
                  floors=2, waterfront="0", yr_built=1915, yr_renovated="1", city="Vashon", view="3", condition=4, grade=10)
predict(result2, home1, level=.95, interval="prediction")
#13.69766 13.20987 14.18545 (fit lower upper)
price1<-log(915000) #13.7266
## PRETTY CLOSE!

##interesting listing... yacht
##https://www.zillow.com/homedetails/1700-Westlake-Ave-N-40-Seattle-WA-98109/2064232670_zpid/
home2<-data.frame(bedrooms=4, bathrooms=2, sqft_living=683, sqft_living15= 683, sqft_above=683, sqft_lot=897.34, 
                  floors=1, waterfront="1", yr_built=2003, yr_renovated="0", city="Seattle", view="4", condition=4, grade=10)
predict(result2, home2, level=.95, interval="prediction")
price2<-log(490000) #13.1021
#13.55552 13.06849 14.04255 (fit lower upper)



