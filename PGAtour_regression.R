library(readr)
library(leaps)
library(MASS)
pgatour2006 <- read_csv("INSERT DATA HERE")


summary(pgatour2006)            #lets take a brief look at the variables we are working with
pgatour2006 <- pgatour2006[,-1] #right from the start we are gonna get rid of the Names variable
pgatour2006$TigerWoods <- as.character(pgatour2006$TigerWoods)       #they made a variable to keep track if a player was Tiger Woods or not lol

corrplot(cor(pgatour2006[,-1])) #there is some colinearity between a handful of variables. We will expect to have to eliminate variables and reduce complexity
plot(pgatour2006[,-1])          #no discernible patterns between response and explanatory. However, response variable does have a high range of values

m1 <- lm(PrizeMoney ~., data = pgatour2006)  #creation of full model, using every variable as a predictor
summary(m1)                                  #we see a lot of insignificant predictors. Full model is definitely overly complex
plot(m1)                                     #conditions for heteroscedascity and normality are grossly violated. Some outliers may exist.

bestsubset <- regsubsets(PrizeMoney ~., data = pgatour2006, nvmax = 10)
bestsubset_summary <- summary(bestsubset)    #because our dataset is small and our rpedictors not too many we can run a best subset algorithm                                  
bestsubset_summary$outmat                    #if our data was much larger this would be computationally expensive and a terrible idea

                                              #we are gonna check our models using different criteria
which.min(bestsubset_summary$bic)             #BIC is typically the most conservative of measurements, with high penalties for numerous predictors - 4
which.max(bestsubset_summary$adjr2)           #AdjR2 tries to take into account number of predictors but mainly measures how clsoe our regression follows its own line. This is most of the time an overfit - 8
which.min(bestsubset_summary$cp)              #Mallows Cp, kind of a middle ground between bix and adjr2 - 5

#we are now gonna compare our 3 measurement criteria
m2a <- lm(PrizeMoney ~ TigerWoods + GIR + BirdieConversion + Scrambling, data = pgatour2006)
m2b <- lm(PrizeMoney ~ TigerWoods + GIR + BirdieConversion + SandSaves + Scrambling, data = pgatour2006)
m2c <- lm(PrizeMoney ~ TigerWoods + DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + BounceBack, data = pgatour2006)

#all three of our models produce similar results but we are gonna go with the simplest model to prevent overfitting.
#in general we want to follow to rules of parsimony as closely as possible so we go with m2a
summary(m2a)
summary(m2b)
summary(m2c)
plot(m2a)   #we have chose our predictors, but we sitll see our conditions not met. Mainly, normality and heteroscedasity. 

#we are going to apply a transformation. I personally, chose to use a box-cox to figure out a transformation on our response variable to make interpretation of the model easy.
boxcox(PrizeMoney ~ TigerWoods + GIR + BirdieConversion + Scrambling, data = pgatour2006)   #box-cox MLE maximing lambda is most definitely close to 0. This usually translates into a log transformation

#Also we are gonna remove the TigerWoods variable. It's hilarious how he himself is considered a variable but in terms of model understanding it doesn't make much sense.
m3 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, data = pgatour2006)
summary(m3)
plot(m3) #remove observation 40, outlier with high leverage (typically skews our coefficients)

pgatour2006 <- pgatour2006[-40, ]
m3 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, data = pgatour2006)
plot(m3)  #remove observation 167. Again outlier with high leverage

pgatour2006 <- pgatour2006[-167, ]
m3 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, data = pgatour2006)
plot(m3)  #I'm pretty happy with the outliers that have lower leverage in this model. Thus this will be our final model. 
summary(m3)

#The best predictors of prize money for the 2006 PGA tour attendees ended up being GIR, BirdieConversion, and Scrambling.
#Side note: Technically being Tiger Woods is statistically considered a good predictor of your prize money earnings but practicality wise is not.
