#################################################################################################
#### Code for taking cleaned 2017/2018 data and creating Random Forest Model Estimates ##########
#################################################################################################

#Clear Workspace
rm(list=ls())

#Load required packages - Random Forest
#install.packages(c("randomForest","rpart", "rpart.plot", "ggplot2"))

library(rpart) 
library(randomForest)
library(rpart.plot)
library(ggplot2)


# Get the cleaned, daily aggregated data
Dat <- read.csv("file:///C:/Users/natel/OneDrive/Backup 03082017/Pothole Data/2017_2018 Daily_Random_Forest_Data.csv",header=TRUE)

#Importing "forgets" the date format, reapply
Dat$Date <- as.Date(Dat$Date)

#Take a look
View(Dat)


#Just a quick ggplot
gg <- ggplot()
gg <- gg + geom_point(data = Dat, aes(x = Date, y = N2Oflux_ug_N2ON_m2_hr), color="darkorchid4", alpha=.3, size=1.5)
gg <- gg + theme_bw()
gg <- gg + geom_vline(xintercept=as.numeric(as.Date("2017-4-24")), linetype=4, size=1) 

gg


# Breiman et al. 1984 - Classification and Regression Trees - R Package "rpart"/"rpart.plot"

#Create a few testing datasets(using 63.25% of the original dataset-the default for 
# "randomForest")

samp1 <- Dat[sample(nrow(Dat), 1898), ]

samp2 <- Dat[sample(nrow(Dat), 1898), ]

samp3 <- Dat[sample(nrow(Dat), 1898), ]

# regression tree from first sample
regtree_N2O1 <- rpart(
  formula = N2Oflux_ug_N2ON_m2_hr ~ .- Date,
  data    = samp1,
  method  = "anova")

#Again!
regtree_N2O2 <- rpart(
  formula = N2Oflux_ug_N2ON_m2_hr ~ .- Date,
  data    = samp2,
  method  = "anova")

#Again!
regtree_N2O3 <- rpart(
  formula = N2Oflux_ug_N2ON_m2_hr ~ .- Date,
  data    = samp3,
  method  = "anova")

#Plot them
rpart.plot(regtree_N2O1)

rpart.plot(regtree_N2O2)

rpart.plot(regtree_N2O3)


#Trees are highly variable and sensitive to small changes in input data


#################################################################################################################
#####################     Generate Random Forest model     ###################################################### 

#Less variable results by "averaging" over many trees
#Less overfitting because uncommon features are canceled out


#Save two plots/8 for testing

Dattest<- Dat[Dat$pH== 6.78|Dat$pH== 6.41,]
Dattrain<- Dat[Dat$pH != 6.78,]
Dattrain<- Dat[Dat$pH != 6.41,]


#OOB Random Forest
rf_N2O <- randomForest(
  formula = N2Oflux_ug_N2ON_m2_hr ~ .- Date,
  data    = Dattrain)




#Summary
rf_N2O

#How's our tuning
#Number of trees?
plot(rf_N2O)

#Sample Size?

#Number of variables at each split?

# number of trees with lowest MSE
#which.min(rf_N2O$mse)

# RMSE of this random forest
sqrt(rf_N2O$mse[which.min(rf_N2O$mse)])

#How big is this?
mean(Dat$N2Oflux_ug_N2ON_m2_hr)





#Let's try a prediction on the test dataset - Probably not a good training/testing dataset!
pred_randomForest <- predict(rf_N2O, Dattest)
Dattest$predict <- pred_randomForest


#Calculate our own RMSE because we don't trust the black box

Dattest$MSE <- (abs((Dattest$N2Oflux_ug_N2ON_m2_hr-Dattest$predict)^2))

sqrt(mean(Dattest$MSE))

 


#Plot it just for fun
gg <- ggplot()
gg <- gg + geom_point(data = Dattest, aes(x = Date, y = N2Oflux_ug_N2ON_m2_hr), color="darkorchid4", alpha=.3, size=1.5)
gg <- gg + theme_bw()
gg <- gg + geom_vline(xintercept=as.numeric(as.Date("2017-4-24")), linetype=4, size=1) 
gg <- gg + geom_point(data = Dattest, aes(x = Date, y = predict), color="darkgoldenrod1", alpha=.3, size=1.5)

#Looks pretty good!
gg

#On closer inspection it doesn't look as good...
gg2<- gg + ylim(-100,2000)

gg2







#variable importance plots 
varImpPlot(rf_N2O) 



#Look at Partial Dependency Plots - Now we're really getting out on a limb, no pun intented

#For some reason, I have to run a new random forest
Y = Dat$N2Oflux_ug_N2ON_m2_hr
X =  Dat[c(3:11)]


rf2_N2O = randomForest(y = Y,x = X)


par(mfrow=c(6,3),mar=c(4,2.2,0,0))

for(i in 1: ncol(X)){pplot<-partialPlot(rf2_N2O,X,x.var = names(X)[i],xlab=names(X)[i],main="")}


#Temperature looks weird, why might that be?

#Autocorrelation?

Datlow<- Dat[Dat$N2Oflux_ug_N2ON_m2_hr <= 1800,]

huh<- ggplot ()
huh<- huh + geom_point(data = Datlow, aes(x = Plot.Temp, y = Plot.VWC, color=N2Oflux_ug_N2ON_m2_hr))


