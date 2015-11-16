
###################################################################################
install.packages("mgcv")
library(mgcv)

install.packages("ggplot2")
library(ggplot2)

install.packages("plotrix")
library(plotrix)

install.packages("sm")
library(sm)

install.packages("vioplot")
library(vioplot)

install.packages("car")
library(car)

#complete needed packages

yelp <- read.csv("/Users/Yifan/Documents/CUSP/Semester 1/Applied Data Science/ADS Project/newdata.csv", na.strings = ".")
yelp[yelp=='']=NA
yelp[yelp=='Others'] = NA
yelp <- na.omit(yelp)

par(mfrow = c(1,1))

#pie plot of categories
pie3D(as.array(summary(yelp$categories)), border = NA, explode = 0.1)

#boxplot of price range
#boxplot(yelp$stars, yelp$priceRange, main = "priceRange")

#show the distribution of categories

x1 <- yelp$stars[yelp$categories=="American"]
x2 <- yelp$stars[yelp$categories=="Burgers"]
x3 <- yelp$stars[yelp$categories=="Chinese"]
x4 <- yelp$stars[yelp$categories=="Deli"]
x5 <- yelp$stars[yelp$categories=="Italian"]
x6 <- yelp$stars[yelp$categories=="Japanese"]
x7 <- yelp$stars[yelp$categories=="Mexican"]
x8 <- yelp$stars[yelp$categories=="Middle Eastern"]
x9 <- yelp$stars[yelp$categories=="Pizza"]
x10 <- yelp$stars[yelp$categories=="Sandwiches"]
x11 <- yelp$stars[yelp$categories=="Seafood"]
x12 <- yelp$stars[yelp$categories=="Steakhouses"]
x13 <- yelp$stars[yelp$categories=="Thai"]

vioplot(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,
        names = c("American","Burgers","Chinese","Deli",
                  "Italian","Japanese","Mexican","Mid-East","Pizza","Sandwich",
                  "Seafood","Steak","Thai"), col ="gold")
#recode all factor into 0,1,2,...
yelp$delivery<- recode (yelp$delivery,"'False' = 0; 'True' = 1")

yelp$categories<-recode(yelp$categories, "'American' = 0;'Burgers' = 1;'Chinese' = 2; 'Deli' = 3 ;
                 'Italian' = 4; 'Japanese' = 5; 'Mexican'=6; 'Middle Eastern' = 7; 'Pizza' = 8; 'Sandwiches' = 9;
                 'Seafood' = 10; 'Steakhouses' = 11; 'Thai' = 12")

yelp$noiselevel<-recode(yelp$noiselevel,"'quiet' =0 ;'average'= 1; 'loud' =2 ; 'very_loud' =3")


#distribution of review count
hist(yelp$review_count,xlab = "# Reviews", main = "Histogram", col= rainbow(9), border = FALSE,breaks = 10)

#find the didtribution of review count less than 100
review_count_new <- yelp$review_count[yelp$review_count <= 100]
hist(review_count_new,xlab = "# Reviews <= 100", main = "Histogram", col= rainbow(20), border = FALSE,breaks =20)

#build the model with all possible variables

model.try <- gam(stars ~ Competitorcount + factor(categories)+ factor(opentime) + factor(closetime)  + 
               factor(priceRange) + factor(noiselevel) + 
               factor(delivery)  + avg_Income + population  , data = yelp)

#summary(model)
anova(model.try)

# build a revise model by selecting variables whose p-values less than 0.05
model.revise <- gam(stars ~ Competitorcount + factor(categories)  + 
               factor(priceRange) + factor(noiselevel) + 
               factor(delivery)  + population, data = yelp)

anova(model.revise)

# all p-values are less than 0.05
# then view the summary of this model

summary(model.revise)


model.final <- gam(stars ~ Competitorcount + factor(categories)  + factor(noiselevel) + 
                      factor(delivery) + population, data = yelp)

anova(model.final)
summary(model.final)

#par(mfrow = c(1,1))

## Check for normality

fitted <- model.final$fitted.values
res <- residuals(model.final)

par(mfrow=c(1,2))

hist(res)
qqnorm(res)

##########################################################################################

