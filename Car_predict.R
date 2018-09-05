################ Assignment- Linear Regression#################

#Loading the required library 
library(MASS)
library(car)
library(ggplot2)

#Load the Dataset
car <- read.csv("CarPrice_Assignment.csv",header = TRUE)


##############.......................Data Cleansing............######################

#Check duplicates
sum(duplicated(car))

#Checking for null 
sum(is.na(car))
colSums(is.na(car))

summary(car)


#Creating CarComapny as a derived variable.
car$CarName <- as.character(car$CarName)
car$carCompany <- sapply(strsplit(car$CarName," "), `[`, 1)

#Data cleaning for Company Name
car$carCompany<- gsub('maxda',"mazda",car$carCompany)
car$carCompany<- gsub('Nissan',"nissan",car$carCompany)
car$carCompany<- gsub('porcshce',"porsche",car$carCompany)
car$carCompany<- gsub('vokswagen',"volkswagen",car$carCompany)
car$carCompany<- gsub('vw',"volkswagen",car$carCompany)
car$carCompany<- gsub('toyouta',"toyota",car$carCompany)


#Removing Car name column as using car ID instead of it: 
car<-car[, !(colnames(car) %in% c("CarName"))]



#drivewheel data cleaning : changing 4wd to fwd
car$drivewheel<- gsub('4',"f",car$drivewheel)



#Checking for Outliers
ggplot(car,aes(x=car$car_ID,y=car$price)) + geom_boxplot();
quantile(car$price)       
# Reason for not changing the outliers : They have better specifications / attributes so if Geely Auto wants to predict the 
# car price with such specs we will need this data.



####################### converting factors with 2 levels to numerical variables ################################

#Fule Type  0 : gas        1 : diseal 
levels(car$fueltype )<-c(1,0)
car$fueltype<- as.numeric(levels(car$fueltype))[car$fueltype]

#aspiration  1: std      0 : turbo 
levels(car$aspiration )<-c(1,0)
car$aspiration<- as.numeric(levels(car$aspiration))[car$aspiration]

#Door Number 0 : two     1 : Four     
levels(car$doornumber )<-c(1,0)
car$doornumber<- as.numeric(levels(car$doornumber))[car$doornumber]

#drive Wheel  0:rdw   1:fwd
car$drivewheel <- factor(car$drivewheel)
levels(car$drivewheel )<-c(1,0)
car$drivewheel<- as.numeric(levels(car$drivewheel))[car$drivewheel]

# Engine location 1: front    0: rear
levels(car$enginelocation )<-c(1,0)
car$enginelocation<- as.numeric(levels(car$enginelocation))[car$enginelocation]


# Create the dummy variable for carbody variable
dummy_1 <- data.frame(model.matrix( ~carbody, data = car))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_1
car_1 <- cbind(car[,-27], dummy_1)


# Create the dummy variable for enginetype variable
dummy_1 <- data.frame(model.matrix( ~enginetype, data = car_1))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_2
car_2 <- cbind(car_1[,-31], dummy_1)


# Create the dummy variable for cylindernumber variable
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = car_2))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_3
car_3 <- cbind(car_2[,-37], dummy_1)


# Create the dummy variable for fuelsystem variable
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = car_3))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_4
car_4 <- cbind(car_3[,-43], dummy_1)


# Create the dummy variable for carCompany variable
dummy_1 <- data.frame(model.matrix( ~carCompany, data = car_4))
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_4
car_5 <- cbind(car_4[,-50], dummy_1)

#removing the main columns of dummy variable : 
car_5<-car_5[, !(colnames(car_5) %in% c("carCompany","fuelsystem","cylindernumber","enginetype","carbody"))]

#Checking for null in car_5
sum(is.na(car_5))

# Car data set for second model where outliers are removed 
# Price above 20000 is in outliers ,so computiing it with the highest value 
carOutlier_5 <- subset(car_5,car_5$price < 29000)


################### Model Building #############

###### Model 1 :

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_5), 0.7*nrow(car_5))
train = car_5[trainindices,]
test = car_5[-trainindices,]


# Build model 1 containing all variables
model_1 <- lm(price~.,data=train)
summary(model_1)

#Using StepAIC
step <- stepAIC(model_1, direction="both")
step


#model_ 2
model_2 <- lm(formula = price ~ car_ID + fueltype + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)

summary(model_2)

# Removing fuelsystemmpfi from model_ 
model_3 <- lm(formula = price ~ car_ID + fueltype + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)

summary(model_3)

# Removing fueltype from model_
model_4 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_4)


# Removing compressionratio from model_
model_5 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_5)


# Removing carbodyhardtop from model_
model_6 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_6)

# Removing carbodyhatchback from model_
model_7 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + 
                carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_7)

# Removing carbodysedan from model_
model_8 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg  + carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_8)

# Removing carbodywagon from model_
model_9 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg  +  enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                carCompanyvolvo, data = train)
summary(model_9)

# Removing citympg from model_
model_10 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 peakrpm +  enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 fuelsystem2bbl +  carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault + carCompanysaab + carCompanytoyota + carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_10)

# In model_ 10 all the variables have P value less that 0.05

# Removing fuelsystem2bbl from model_ as it has high p value
model_11 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 peakrpm +  enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault + carCompanysaab +  carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_11)

# Removing carCompanysaab from model_
model_12 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 peakrpm +  enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault +   carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_12)

# Removing peakrpm from model_
model_13 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault +   carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_13)

# Removing cylindernumberfive from model_
model_14 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanyisuzu + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault +   carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_14)

# Removing carCompanyisuzu from model_
model_15 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanyhonda + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault +   carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_15)

# Removing carCompanyhonda from model_
model_16 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyrenault +   carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_16)

# Removing carCompanyrenault from model_
model_17 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanyplymouth + 
                 carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_17)

# Removing carCompanyplymouth from model_
model_18 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + 
                 carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_18)

# Removing enginetypedohcv from model_
model_19 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + 
                 carCompanyvolkswagen + 
                 carCompanyvolvo, data = train)
summary(model_19)

# Removing carCompanyvolvo from model_
model_20 <- lm(formula = price ~ car_ID + aspiration + drivewheel + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + 
                 carCompanyvolkswagen, data = train)
summary(model_20)

# Removing drivewheel from model_
model_21 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + 
                 carCompanyvolkswagen, data = train)
summary(model_21)

# Removing carCompanynissan from model_
model_22 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + 
                 carCompanyvolkswagen, data = train)
summary(model_22)


# Removing carCompanymercury from model_
model_23 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + curbweight + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + 
                 carCompanymitsubishi + 
                 carCompanyvolkswagen, data = train)
summary(model_23)


vif(model_23)

# curbweight is dependent on other variables so removing it

model_24 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + 
                 carCompanymitsubishi + 
                 carCompanyvolkswagen, data = train)
summary(model_24)

vif(model_24)


#Removing carCompanyvolkswagen as its least significant 
model_25 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + carCompanydodge + 
                 carCompanymazda + 
                 carCompanymitsubishi, data = train)
summary(model_25)
vif(model_25)

#As number of variables are high Removing carCompanydodge as its least significant 
model_26 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw + 
                 carCompanymazda + 
                 carCompanymitsubishi, data = train)
summary(model_26)

#As number of variables are high Removing carCompanymazda as its least significant 
model_27 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw +carCompanymitsubishi, data = train)
summary(model_27)

#As number of variables are high Removing carCompanymitsubishi as its least significant 
model_28 <- lm(formula = price ~ car_ID + aspiration + 
                 enginelocation + carwidth + enginesize + stroke + 
                 enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 carCompanybmw , data = train)
summary(model_28)

vif(model_28)


### Testing data on final model_

Predict_with_outliers <- predict(model_28,test[,-25])
test$test_price <- Predict_with_outliers
r_with_outliers<- cor(test$price,test$test_price)
rsquared_with_outliers <- cor(test$price,test$test_price)^2
rsquared_with_outliers






########### Model 2 : Removing car Company name vatiable and removing outliers and checking the model. ############


carOutlier_5<-car_4[, !(colnames(car_4) %in% c("carCompany","fuelsystem","cylindernumber","enginetype","carbody"))]
carOutlier_5$price[which(car_4$price >20000)] <- 28000

################### Model Building #############

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carOutlier_5), 0.7*nrow(carOutlier_5))
train = carOutlier_5[trainindices,]
test = carOutlier_5[-trainindices,]


# Build model 1 containing all variables
model1 <- lm(price~.,data=train)
summary(model1)


#Using StepAIC
step <- stepAIC(model1, direction="both")
step



####

model2 <- lm(formula = price ~ car_ID + fueltype + aspiration + drivewheel + 
               enginelocation + wheelbase + carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + citympg + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               cylindernumberthree + fuelsystemmpfi, data = train)


summary(model2)

# removing cylindernumberthree
model3 <- lm(formula = price ~ car_ID + fueltype + aspiration + drivewheel + 
               enginelocation + wheelbase + carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + citympg + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
                fuelsystemmpfi, data = train)


summary(model3)

# removing drivewheel
model4 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation + wheelbase + carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + citympg + highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model4)

# removing drivewheel
model5 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation + wheelbase + carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm +  highwaympg + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model5)

# removing highwaympg
model6 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation + wheelbase + carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model6)


# removing wheelbase
model7 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation +  carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model7)


# removing carbodyhardtop
model8 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation +  carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm +  carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model8)


# removing carbodysedan
model9 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation +  carwidth + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm +  carbodyhatchback + 
               carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model9)


# removing carwidth
model10 <- lm(formula = price ~ car_ID + fueltype + aspiration + 
               enginelocation + carheight + curbweight + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm +
               carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = train)


summary(model10)
vif(model10)

# removing fueltype
model11 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + carheight + curbweight + 
                enginesize + boreratio + stroke + compressionratio + horsepower + 
                peakrpm +
                carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + cylindernumbersix + 
                fuelsystemmpfi, data = train)


summary(model11)


# removing compressionratio
model12 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + carheight + curbweight + 
                enginesize + boreratio + stroke + horsepower + 
                peakrpm +
                carbodywagon + enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + cylindernumbersix + 
                fuelsystemmpfi, data = train)


summary(model12)


vif(model12)


# removing carbodywagon
model13 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                enginesize + boreratio + stroke + horsepower + 
                peakrpm + enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + cylindernumbersix + 
                fuelsystemmpfi, data = train)


summary(model13)


# removing peakrpm
model14 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                enginesize + boreratio + stroke + horsepower + 
                enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + cylindernumbersix + 
                fuelsystemmpfi, data = train)


summary(model14)

# removing cylindernumbersix
model15 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                enginesize + boreratio + stroke + horsepower + 
                enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + 
                fuelsystemmpfi, data = train)


summary(model15)
vif(model15)

# removing enginesize as it has highest p value right now and 2nd highest VIF value
model16 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                boreratio + stroke + horsepower + 
                enginetypedohcv + enginetypel + 
                enginetypeohcf + cylindernumberfour + 
                fuelsystemmpfi, data = train)


summary(model16)

# removing enginetypedohcv
model17 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                boreratio + stroke + horsepower + 
                 enginetypel + 
                enginetypeohcf + cylindernumberfour + 
                fuelsystemmpfi, data = train)


summary(model17)

# removing horsepower
model18 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                boreratio + stroke + 
                enginetypel + 
                enginetypeohcf + cylindernumberfour + 
                fuelsystemmpfi, data = train)


summary(model18)


# removing horsepower
model19 <- lm(formula = price ~ car_ID +  aspiration + 
                enginelocation + curbweight + 
                boreratio + stroke + 
                enginetypel + 
                enginetypeohcf + cylindernumberfour , data = train)


summary(model19)

### Testing 2nd model 

Predict_without_outliers <- predict(model19,test[,-25])
test$test_price <- Predict_without_outliers
r_without_outliers <- cor(test$price,test$test_price)
rsquared_without_outliers <- cor(test$price,test$test_price)^2
rsquared_without_outliers




########## Comparison Between the 2 models

rsquared_with_outliers

rsquared_without_outliers


# Conclusion : Model 1 has given better results.

