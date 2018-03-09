library(AER)
library(mgcv)
library(car)
library(sandwich)
library(lmtest)

data(HousePrices)

###### Regression for Table II #####
table2.reg <- lm(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                 + garage + prefer + log(lotsize) + log(bedrooms) + log(bathrooms)
                 + log(stories), data = HousePrices)
summary(table2.reg)

###### Regression for Table III #####
table3.reg <- lm(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                 + garage + prefer + log(lotsize) + bedrooms + bathrooms
                 + stories, data = HousePrices)
summary(table3.reg)

###### Create exploration, validation, and test sets #####
data.copy <- HousePrices
data.copy <- data.copy[sample(nrow(data.copy)),]
exploration.data <-  data.copy[1:109,]
validation.data <- data.copy[110:437,]
test.data <- data.copy[438:546,]

###### Histograms #####
par(mfrow=c(3,2)) 

hist(exploration.data$price,
     breaks = "FD",
     xlab = "House Price",
     main = "")

hist(log(exploration.data$price),
     breaks = "FD",
     xlab = "Log House Price",
     main = "")

hist(exploration.data$lotsize,
     breaks = "FD",
     xlab = "Lot Size",
     main = "")

hist(exploration.data$bedrooms,
     breaks = "FD",
     xlab = "Number of Bedrooms",
     main = "")

hist(exploration.data$bathrooms,
     breaks = "FD",
     xlab = "Number of Bathrooms",
     main = "")

hist(exploration.data$stories,
     breaks = "FD",
     xlab = "Number of Stories",
     main = "")

###### Tables #####
table(exploration.data$driveway)
print("Driveway")

table(exploration.data$recreation)
print("Recreational Room")

table(exploration.data$fullbase)
print("Full Basement")

table(exploration.data$gasheat)
print("Gas for Hot Water")

table(exploration.data$aircon)
print("Central Air Conditioning")

table(exploration.data$prefer)
print("Preferred Neighborhood")

###### jackknife residuals versus fitted values #####
table2.reg2 <- lm(price ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                  + garage + prefer + log(lotsize) + log(bedrooms) + log(bathrooms)
                  + log(stories), data = exploration.data)

jr.fv.plot <- plot(fitted(table2.reg2), rstudent(table2.reg2),
                   ylab = "Jackknife Residuals",
                   xlab = "Fitted Values",
                   pch  = 19,
                   col  = rgb(0, 0, 0, .1))
jr.fv.plot

###### Box-Cox transformation #####
bc <- boxCox(price ~ 1 + driveway + recreation + fullbase + gasheat + aircon
             + garage + prefer + log(lotsize) + log(bedrooms) + log(bathrooms)
             + log(stories), plotit = TRUE, data = exploration.data)
bc$x[bc$y == max(bc$y)]

###### Component-residual plot #####
table2.reg3 <- lm(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                  + garage + prefer + lotsize + log(bedrooms) + log(bathrooms)
                  + log(stories), data = exploration.data)

cr.plot <- crPlots(table2.reg3, terms = ~ lotsize, data = exploration.data)
cr.plot 

###### Box-Tidwell transformation #####
boxTidwell(log(price) ~ lotsize, data = exploration.data)

##### Semi-Parametric GAM Model #####
gam1 <- gam(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
            + garage + prefer + s(lotsize) + log(bedrooms) + log(bathrooms)
            + log(stories), data = exploration.data)

pr.plot <- plot(gam1,
                residuals = TRUE,  # Include the partial residuals
                shade     = TRUE,  # Include shaded confidence bands
                pages     = 1,
                scale     = 0, 
                cex       = 3) 

##### Cross-Validation #####
nfolds <- 5
case.folds <- rep(1:nfolds, length.out = nrow(HousePrices))
case.folds <- sample(case.folds) 

# Create empty vectors to store results
new.model <- c()  
gam.model <- c()

for (fold in 1:nfolds) {
  
  # Create training and test cases
  train <- validation.data[case.folds != fold, ]
  test <- validation.data[case.folds == fold, ]
  
  # Run the models
  new.train <- lm(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                  + garage + prefer + I(lotsize^-0.1763991) + log(bedrooms) + log(bathrooms)
                  + log(stories), data = validation.data)  
  gam.train <- gam(log(price) ~ 1 + driveway + recreation + fullbase + gasheat + aircon
                   + garage + prefer + s(lotsize) + log(bedrooms) + log(bathrooms)
                   + log(stories), data = validation.data)
  
  # Generate test SSEs, predict function automatically finds the correct col
  new.test <- (log(test$price) - predict(new.train, test))^2
  gam.test <- (log(test$price) - predict(gam.train, test))^2
  
  # Calculate rMSEs
  rMSEtest1 <- sqrt(sum(new.test)/length(new.test))
  rMSEtest2 <- sqrt(sum(gam.test)/length(gam.test))
  
  # Append the rMSE from this iteration to vectors
  new.model <- c(new.model, rMSEtest1)  
  gam.model <- c(new.model, rMSEtest2)   
}

# Average the MSEs
new.avg <- mean(new.model)
gam.avg <- mean(gam.model)

#t.test(new.avg, gam.avg)

##### Comparing Robust Standard Errors #####
coeftest(table2.reg, 
         vcov = vcovHC(table2.reg, type = "HC0"),
         df = df.residual(table2.reg))  # residual df is n - number of parameters  

##### Statistical Story #####
vcov = vcovHC(table2.reg, type = "HC0")
vars <- diag(vcov)
beta.hat <- coefficients(table2.reg)
lower <- beta.hat - 1.96*sqrt(vars)
upper <- beta.hat + 1.96*sqrt(vars)