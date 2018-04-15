library(mlmRev)
library(arm)
library(car)
library(HLMdiag)
library(multiwayvcov)
library(lmtest)
data("Exam", package = "mlmRev")

##### References: Some of this code is borrowed from week12.R, week13.R and the lecture notes #####

##### Divide data into 20/60/20 Sets #####
data.copy <- Exam
data.copy <- data.copy[sample(nrow(data.copy)),]

exploration.data <-  data.copy[1:812,]
validation.data <- data.copy[813:3247,]
test.data <- data.copy[3248:4059,]

##### 1. Histograms and Tables #####
par(mfrow=c(2,2)) 

hist(as.numeric(exploration.data$school),
     breaks = 64,
     xlab = "School",
     main = "")

hist(exploration.data$normexam,
     breaks = "FD",
     xlab = "GCSE exam scores",
     main = "")

hist(exploration.data$schavg,
     breaks = "FD",
     xlab = "Average London Reading Test intake scores",
     main = "")

hist(exploration.data$standLRT,
     breaks = "FD",
     xlab = "Standardized London Reading Test intake scores",
     main = "")

table(Exam$schgend)
print("School Gender Types")

table(exploration.data$schgend)
print("School Gender Types")

table(exploration.data$vr)
print("Verbal Reasoning Subscores at Intake")

table(exploration.data$intake)
print("Overall London Reading Test Score at Intake")

table(exploration.data$sex)
print("Student's Gender")

table(exploration.data$type)
print("Type of School")

##### 2.  Complete, no, and partial pooling regressions using the intercept #####
cp.reg1 <- lm(normexam ~ 1, data = exploration.data)
np.reg1 <- lm(normexam ~ factor(school) - 1, data = exploration.data)
pp.reg1 <- lmer(normexam ~ 1 + (1|school), data = exploration.data)
as.matrix(coef(np.reg1))[1]
as.matrix(coef(pp.reg1)$school)[1]

##### 4. Add the student’s standardized intake score on the London Reading Test as a predictor #####
cp.reg2 <- lm(normexam ~ standLRT, data = exploration.data)
np.reg2 <- lm(normexam ~ standLRT + factor(school) - 1, data = exploration.data)
pp.reg2 <- lmer(normexam ~ standLRT + (1|school), data = exploration.data)

##### 5. Add the average London Reading Test intake score for the school as a group-level predictor #####
pp.reg3 <- lmer(normexam ~ standLRT + schavg + (1|school), data = exploration.data)

rownames <- rownames(ranef(pp.reg3)$school)
schavg <- c()
for(i in rownames){
  schavg[i] <- mean(exploration.data$schavg[exploration.data$school == i])
}
p.pool <- data.frame(int = rep(NA,65),
                     std.error = rep(NA,65),
                     school = rownames,
                     schavg = schavg)
p.pool$int <-fixef(pp.reg3)[1] + unlist(ranef(pp.reg3)) +   fixef(pp.reg3)[3]*p.pool$schavg
plot(p.pool$int,
     p.pool$schavg,
     pch  = 20,
     col  = rgb(0, 0, 0, .5),
     ylab = "Avg London Reading Test Score",
     xlab = "Partially Pooled Intercept")
abline(fixef(pp.reg3)[1], fixef(pp.reg3)[3])

np.coef <- coef(np.reg2)
np.coef <- np.coef[2:66]
n.pool <- data.frame(int = np.coef,
                     std.error = rep(NA,65),
                     school = rownames,
                     schavg = schavg)

plot(n.pool$int,
     n.pool$schavg,
     pch  = 20,
     col  = rgb(0, 0, 0, .5),
     ylab = "Avg London Reading Test Score",
     xlab = "No Pooling Intercept")

abline(np.reg2)

##### 6.Extend the previous model by allowing the standardized intake score to vary at the school level #####
pp.reg4 <- lmer(normexam ~ standLRT + schavg + (1 + standLRT|school), data = exploration.data)

rownames <- rownames(ranef(pp.reg4)$school)
standLRT <- c()
for(i in rownames){
  standLRT[i] <- mean(exploration.data$standLRT[exploration.data$school == i])
}
p.pool2 <- data.frame(int = rep(NA,65),
                      std.error = rep(NA,65),
                      school = rownames,
                      schavg = schavg,
                      standLRT = standLRT)
rand.effects <- unlist(ranef(pp.reg4))
rand.int <- rand.effects[1:65]
rand.standLRT <- rand.effects[66:130]
p.pool2$int <- fixef(pp.reg4)[1] + rand.int + rand.standLRT + fixef(pp.reg4)[2]*p.pool2$standLRT + fixef(pp.reg4)[3]*p.pool2$schavg

plot(p.pool2$int,
     p.pool2$standLRT,
     pch  = 20,
     col  = rgb(0, 0, 0, .5),
     ylab = "Standardized Intake Score",
     xlab = "Partially Pooled Intercept")
abline(fixef(pp.reg4)[1]+fixef(pp.reg4)[2]*p.pool2$standLRT[1] + fixef(pp.reg4)[3]*p.pool2$schavg[1], rand.int[1]+rand.standLRT[1]*p.pool2$standLRT[1], lty = 3, lwd = 2,col = "red")

##### 7. Use level 1 diagnostics to discover any problems with the first level of the previous model. #####
LS.resids1 <- lm(normexam ~ standLRT + schavg, data = exploration.data)
par(cex = 1.3, mar = c(5, 4, 2, 1), mfrow = c(2, 2))
plot(fitted(LS.resids1), rstudent(LS.resids1),
     xlab = "Fitted Values",
     ylab = "Jackknife LS Residuals",
     pch = 19,
     col = rgb(0, 0, 0, .3))
# Add lowess smooth
lines(lowess(fitted(LS.resids1), rstudent(LS.resids1)),
      col = "green", lwd = 2)
abline(h = 0, lty = 2, col = "red", lwd = 2)
plot(exploration.data$standLRT, rstudent(LS.resids1),
     xlab = "Standardized LRT",
     ylab = "Jackknife LS Residuals",
     pch  = 19,
     col  = rgb(0, 0, 0, .3))
plot(exploration.data$schavg, rstudent(LS.resids1),
     xlab = "School Average LRT",
     ylab = "Jackknife LS Residuals",
     pch  = 19,
     col  = rgb(0, 0, 0, .3))
lines(lowess(exploration.data$schavg, rstudent(LS.resids1)),
      col = "green", lwd = 2)
abline(h = 0, lty = 2, col = "red", lwd = 2)
qqPlot(LS.resids1, id.n = 3, distribution = "t", df = df.residual(LS.resids1),
       ylab = "Jackknife LS Residuals")

par(mfrow=c(1,2)) 
boxCox(lm(normexam ~ 1, data = exploration.data), family = "yjPower")
hist((exploration.data$standLRT)^(1/3),
     breaks = "FD",
     xlab = "Standardized London Reading Test intake scores",
     main = "")

##### 8. Use level 2 diagnostics to discover problems with the second level of the previous model #####
pp.reg5 <- lmer(normexam ~ standLRT + schavg + standLRT*schavg + (1 + standLRT|school), data = exploration.data)

EB.resids <- HLMresid(pp.reg5, level = "school", type = "EB")
par(cex = 1.3, mar = c(5, 4, 2, 1), mfrow = c(2, 2))
schavg.values <- unique(exploration.data$schavg)
plot(schavg.values, EB.resids[, 1],
     pch  = 19,
     col  = rgb(0, 0, 0, .5),
     ylim = c(-.1, .1),
     xlab = "School Avg LRT",
     ylab = "Empirical Bayes Intercept Residuals")
abline(h = 0, col = "red", lty = 2)
lines(lowess(schavg.values, EB.resids[, 1]),
      col = "green", lwd = 2)
plot(schavg.values, EB.resids[, 2],
     pch  = 19,
     col  = rgb(0, 0, 0, .5),
     xlab = "School Avg LRT",
     ylab = "Empirical Bayes Slope Residuals")
abline(h = 0, col = "red", lty = 2)
lines(lowess(schavg.values, EB.resids[, 2]),
      col = "green", lwd = 2)
qqPlot(EB.resids[, 1], id.n = 3,
       ylab = "Empirical Bayes Intercept Residuals",
       xlab = "Normal Quantiles")
qqPlot(EB.resids[, 2], id.n = 3,
       ylab = "Empirical Bayes Slope Residuals",
       xlab = "Normal Quantiles")

cooks.d <- cooks.distance(pp.reg5, group = "school")
par(cex = 1.3, mar = c(5, 4, 2, 1))
dotplot_diag(x = cooks.d,
             cutoff = "internal",
             name = "cooks.distance",
             ylab = "Cook’s Distance",
             xlab = "School")
##### 9. Compare a no pooling dummy variable version of your revised model with a partial pooling version. #####
np.reg3 <- lm(normexam ~ standLRT + I(standLRT^3) + factor(school) - 1, data = exploration.data)
pp.reg6 <- lmer(normexam ~ standLRT + I(standLRT^3) + (1 + standLRT|school), data = exploration.data)

vcovCL <- cluster.vcov(np.reg3, exploration.data$school, df_correction = FALSE)
coeftest(np.reg3, vcovCL)

##### 10.  Compare your model to a simpler version using level 1 and level 2 leave-one-out-cross-validation. #####
simple <- c()
complex <- c()

for(i in 1:nrow(validation.data)){
  # Train complex model dropping the ith observation
  lmer.complex <- lmer(normexam ~ standLRT +  I(standLRT^3) + schavg + standLRT*schavg + (1 + standLRT|school), data = validation.data[-i, ])
  
  # Find the school from the dropped observation
  school <- validation.data$school[i]
  print(i)
  
  
  # Let's break down the prediction into each of its parts
  # There's the fixed + random intercept
  rand.int <- coef(lmer.complex)$school[school, 1] 
  # Plus the school-level LRT prediction of the intercept
  avgLRT.int <- coef(lmer.complex)$school[1, 4]*validation.data$schavg[i]
  # Plus the fixed + random slope
  rand.slope <- coef(lmer.complex)$school[school, 2]*validation.data$standLRT[i] + coef(lmer.complex)$school[school, 3]*(validation.data$standLRT[i])^3
  # Plus the school-level LRT prediction of the slope
  avgLRT.slope <- prod(coef(lmer.complex)$school[1, 4],
                       validation.data$standLRT[i],
                       validation.data$schavg[i]) 
  # The prediction is the sum of these four components
  pred.comp <- rand.int + avgLRT.int + rand.slope + avgLRT.slope
  # Calculate the squared residual
  comp.sq.resid <- (validation.data$normexam[i] - pred.comp)^2
  # Add the squared residual to the cv vector
  complex <- append(complex, comp.sq.resid)
  
  # Train simple model dropping the ith observation
  lmer.simple <- lmer(normexam ~ standLRT + schavg + standLRT*schavg + (1 + standLRT|school), data  = validation.data[-i, ])
  # There's the random intercept
  rand.int <- coef(lmer.simple)$school[school, 1] 
  # Plus the school LRT prediction of the intercept
  avgLRT.int <- coef(lmer.simple)$school[1, 3]*validation.data$schavg[i]
  # Plus the standLRT of measurement effect
  slope <- coef(lmer.simple)$school[school, 2]*validation.data$standLRT[i]
  # The prediction is the sum of these three components
  pred.simp <- rand.int + avgLRT.int + slope
  # Calculate the squared residual
  simp.sq.resid <- (validation.data$normexam[i] - pred.simp)^2
  # Add the squared residual to the cv vector
  simple <- c(simple, simp.sq.resid)
}

comp.rMSE <- sqrt(sum(complex)/length(complex))
simp.rMSE <- sqrt(sum(simple)/length(simple))
comp.rMSE
simp.rMSE

school.standLRT <- tapply(validation.data$standLRT, 
                          validation.data$school,  
                          mean)

school.schavg <- tapply(validation.data$schavg, 
                        validation.data$school, 
                        mean)

school.normexam <- tapply(validation.data$normexam, 
                          validation.data$school, 
                          mean)

# Put it all together in school-level data
school.data <- data.frame(standLRT = school.standLRT, 
                          schavg = school.schavg,
                          normexam = school.normexam)
complex <- c()
simple <- c()
for(i in 1:length(unique(validation.data$school))){
  school.drop <- levels(as.factor(validation.data$school))[i]
  print(i)
  # Include all counties except the dropped county
  train.data <- validation.data[!(validation.data$school == school.drop), ]
  # Fit the complex model on the training data
  lmer.complex <- lmer(normexam ~ standLRT +  I(standLRT^3) + schavg + standLRT*schavg + (1 + standLRT|school), data = train.data)
  
  # Get the overall intercept
  int <- fixef(lmer.complex)[1]
  # Multiply standLRT fixed effect times the 
  slope <- fixef(lmer.complex)[2]*school.data$standLRT[i]
  
  # Multiply standLRT^3 fixed effect times the 
  slope3 <- fixef(lmer.complex)[3]*(school.data$standLRT[i])^3
  
  # Multiply by the school-level LRT
  lrt <- fixef(lmer.complex)[4]*school.data$schavg[i]
  # Get the interaction
  lrt.standLRT <- prod(fixef(lmer.complex)[5],
                       school.data$standLRT[i],
                       school.data$schavg[i])
  pred.comp <- int + slope + slope3 + lrt + lrt.standLRT
  # Calculate the squared residual
  comp.sq.resid <- (school.data$normexam[i] - pred.comp)^2
  # Add the squared residual to the cv vector
  complex <- c(complex, comp.sq.resid)
  
  lmer.simple <- lmer(normexam ~ standLRT + schavg + standLRT*schavg + (1 + standLRT|school), data  = train.data)
  
  # Get the overall intercept
  int <- fixef(lmer.simple)[1]
  # Multiply standLRT fixed effect times the 
  slope <- fixef(lmer.simple)[2]*school.data$standLRT[i]
  # Multiply by the school-level LRT
  lrt <- fixef(lmer.simple)[3]*school.data$schavg[i]
  # Get the interaction
  lrt.standLRT <- prod(fixef(lmer.simple)[4],
                       school.data$standLRT[i],
                       school.data$schavg[i])
  pred.comp <- int + slope + lrt + lrt.standLRT
  # Calculate the squared residual
  simp.sq.resid <- (school.data$normexam[i] - pred.comp)^2
  # Add the squared residual to the cv vector
  simple <- c(complex, simp.sq.resid)
  
}

lv2.comp.rMSE <- sqrt(sum(complex)/length(complex))
lv2.simp.rMSE <- sqrt(sum(simple)/length(simple))
