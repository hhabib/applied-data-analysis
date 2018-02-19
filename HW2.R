install.packages("AER")
library(AER)
data(CPS1988)

##### Reproduce Table 1.A ######################################################
rwg <- CPS1988$wage
ed <- CPS1988$education
exper <- CPS1988$experience
race <- CPS1988$ethnicity
lm1A <- lm(log(rwg) ~ 1 + ed + exper + I(exper^2) + race)
summary(lm1A)

##### Reproduce Table 2.A ######################################################
lm2A <- lm(log(rwg) ~ 1 + ed + exper + I(exper^2) + I(exper^3) + I(exper^4) 
           + race)
summary(lm2A)

##### Divide data into 20/60/20 Sets ###########################################
data.copy <- CPS1988
data.copy <- data.copy[sample(nrow(data.copy)),]
exploration.data <-  data.copy[1:5631,]
validation.data <- data.copy[5632:22524,]
test.data <- data.copy[22525:28155,]

##### Make Histograpms for wages, log wages, education, and experience variable 
png(filename  = "histograms.png", 
    width  = 3000,  # adjust width of .png file, in pixels
    height = 3000,  # adjust height of .png file, in pixels
    res    = 300)  # set resolution in pixels per inch

par(mfrow=c(2,2)) 

hist(exploration.data$wage,
     breaks = "FD",
     xlab = "Wage",
     main = "")

hist(log(exploration.data$wage),
     breaks = "FD",
     xlab = "Log Wage",
     main = "")

hist(exploration.data$education,
     breaks = "FD",
     xlab = "Education",
     main = "")

hist(exploration.data$experience,
     breaks = "FD",
     xlab = "Experience",
     main = "")

dev.off()

##### Make scatterplots for wages, log wages against education and experience ## 
png(filename  = "scatterplots.png",
     width  = 3000,  # adjust width of .png file, in pixels
     height = 3000,  # adjust height of .png file, in pixels
     res    = 300)  # set resolution in pixels per inch

par(mfrow=c(2,2))

plot(exploration.data$education,
     exploration.data$wage,
     xlab = "Education", 
     ylab = "Wage")
abline(lm(exploration.data$wage ~ exploration.data$education), col="red")  

plot(exploration.data$education,
     log(exploration.data$wage), 
     xlab = "Education", 
     ylab = "Log Wage")
abline(lm(log(exploration.data$wage) ~ exploration.data$education), col="red") 

plot(exploration.data$experience,
     exploration.data$wage, 
     xlab = "Experience", 
     ylab = "Wage")
abline(lm(exploration.data$wage ~ exploration.data$experience), col="red")   


plot(exploration.data$experience, 
     log(exploration.data$wage),
     xlab = "Experience", 
     ylab = "Log Wage")
abline(lm(log(exploration.data$wage) ~ exploration.data$experience), col="red")   

dev.off()

##### Compare wages and log wages to a normal distribution ##################### 
png(filename  = "distributions.png",
    width  = 6000,  # adjust width of .png file, in pixels
    height = 3000,  # adjust height of .png file, in pixels
    res    = 300)  # set resolution in pixels per inch

par(mfrow=c(1,2))

h <- hist(exploration.data$wage,
     breaks = "FD",
     xlab = "Wage",
     main = "")
xfit <- seq(min(exploration.data$wage), 
            max(exploration.data$wage), 
            length=100)
yfit <- dnorm(xfit, 
              mean = mean(exploration.data$wage), 
              sd = sd(exploration.data$wage))
yfit <- yfit*diff(h$mids[1:2])*length(exploration.data$wage)
lines(xfit, yfit, col="blue", lwd=2) 

h <- hist(log(exploration.data$wage),
          breaks = "FD",
          xlab = "Log Wage",
          main = "")
xfit <- seq(min(log(exploration.data$wage)), 
            max(log(exploration.data$wage)), 
            length=100)
yfit <- dnorm(xfit, 
              mean = mean(log(exploration.data$wage)), 
              sd = sd(log(exploration.data$wage)))
yfit <- yfit*diff(h$mids[1:2])*length(log(exploration.data$wage))
lines(xfit, yfit, col="blue", lwd=2) 

dev.off()

##### Plot regression residuals for Table 1.A and 2.A ##########################
png(filename  = "residuals.png",
    width  = 3000,  # adjust width of .png file, in pixels
    height = 3000,  # adjust height of .png file, in pixels
    res    = 300)  # set resolution in pixels per inch

par(mfrow=c(2,1))

qqPlot(lm1A, 
       xlab = "Table 1.A", 
       ylab = "Studentized Residuals", 
       id.n = 3,
       pch  = 19,
       col  = rgb(0, 0, 0, .5))

qqPlot(lm2A, 
       xlab = "Table 2.A", 
       ylab = "Studentized Residuals", 
       id.n = 3,
       pch  = 19,
       col  = rgb(0, 0, 0, .5))

dev.off()

##### Evaluate the backcasting story of the Mincer type model ##################
exp.rwg <- exploration.data$wage
exp.ed <- exploration.data$education
exp.exper <- exploration.data$experience
exp.race <- exploration.data$ethnicity
lm.explore <- lm(log(exp.rwg) ~ 1 + exp.ed + exp.exper + I(exp.exper^2) + exp.race)

png(filename  = "backcasting.png",
    width  = 3000,  # adjust width of .png file, in pixels
    height = 3000,  # adjust height of .png file, in pixels
    res    = 300)  # set resolution in pixels per inch
plot(predict(lm.explore, exploration.data), 
     log(exploration.data$wage),
     xlab = "Predicted",
     ylab = "Actual",
     pch = 19,
     col = rgb(0, 0, 0, .3))
lines(log(exploration.data$wage), log(exploration.data$wage), col="blue", lwd=2)
dev.off()

##### 5-fold cross-validation ##################################################
# Borrowed from week4.R
nfolds <- 5
case.folds <- rep(1:nfolds, length.out = nrow(validation.data))
case.folds <- sample(case.folds) 

# Create empty vectors to store results
mincer <- c()  
simple <- c()

for (fold in 1:nfolds) {
  
  # Create training and test cases
  train <- validation.data[case.folds != fold, ]
  test <- validation.data[case.folds == fold, ]
  
  # Run the simple and complex models
  simple.train <- lm(log(wage) ~ 1 + 
                       education + experience + ethnicity, 
                     data = train)  
  mincer.train <- lm(log(wage) ~ 1 + 
                      education + experience + I(experience^2) + ethnicity, 
                    data = train)
  
  # Generate test SSEs, predict function automatically finds the correct col
  simple.test <- (test$wage - predict(simple.train, test))^2
  mincer.test <- (test$wage - predict(mincer.train, test))^2
  
  # Calculate rMSEs
  rMSEtest1 <- sqrt(sum(simple.test)/length(simple.test))
  rMSEtest2 <- sqrt(sum(mincer.test)/length(mincer.test))
  
  # Append the rMSE from this iteration to vectors
  simple <- c(simple, rMSEtest1)  
  mincer <- c(mincer, rMSEtest2)   
}

# Average the MSEs
simple.avg <- mean(simple)
mincer.avg <- mean(mincer)

t.test(simple, mincer)

##### Find the error of the model on the test set ##############################
mincer.test <- (test.data$wage - predict(mincer.train, test.data))^2
rMSEtest <- sqrt(sum(mincer.test)/length(mincer.test))