# read and extract data from chds_births.csv
child_birth <- read.csv("chds_births.csv", header = T) # open and extract data from "chds_births.csv"

# helper function ti calculate mode of a data set
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# code for necessary constants
num_var <- ncol(child_birth) # number of variables (include response variable wt)
num_col <- nrow(child_birth) # number of collected data of each variable (include missing data)
num_obs <- c() # number of observed data of each variable (not include missing data)

# perform imputation (replace suitable data for missing values,NA)
for (x in 1:num_var) {
  if (colnames(child_birth)[x] == "wt" || colnames(child_birth)[x] == "gestation" || colnames(child_birth)[x] == "parity" || colnames(child_birth)[x] == "mage" || colnames(child_birth)[x] == "mht" || colnames(child_birth)[x] == "mwt" || colnames(child_birth)[x] == "fage" || colnames(child_birth)[x] == "fht" || colnames(child_birth)[x] == "fwt") {
    # use mean value to replace missing data for non-catagorical variables
    sum <- 0
    count <- 0
    for (y in 1:num_col){
      if (!is.na(child_birth[y, x])) {
        sum <- sum + child_birth[y, x]
        count <- count + 1
      }
    }
    mean <- sum / count
    num_obs <- c(num_obs, count)
    for (y in 1:num_col){
      if (is.na(child_birth[y, x])) {
        child_birth[y, x] <- mean
      }
    }
  } else {
    # use hot-deck method, select mode value from current data set and use it to replace missing and bad data for catagorical variables
    if (colnames(child_birth)[x] == "marital") {
      pool <- c()
      count <- 0
      for (y in 1:num_col){
        if ((!is.na(child_birth[y, x])) &&  child_birth[y, x] != 0) {
          pool <- c(pool, child_birth[y, x])
          count <- count + 1
        }
      }
      num_obs <- c(num_obs, count)
      for (y in 1:num_col){
        if (is.na(child_birth[y, x]) || child_birth[y, x] == 0) {
          child_birth[y, x] <- Mode(pool)
        }
      }
    } else if (colnames(child_birth)[x] == "number") {
      pool <- c()
      count <- 0
      for (y in 1:num_col){
        if ((!is.na(child_birth[y, x])) &&  child_birth[y, x] != 8) {
          pool <- c(pool, child_birth[y, x])
          count <- count + 1
        }
      }
      num_obs <- c(num_obs, count)
      for (y in 1:num_col){
        if (is.na(child_birth[y, x]) || child_birth[y, x] == 8) {
          child_birth[y, x] <- Mode(pool)
        }
      }
    } else {
      pool <- c()
      count <- 0
      for (y in 1:num_col){
        if (!is.na(child_birth[y, x])) {
          pool <- c(pool, child_birth[y, x])
          count <- count + 1
        }
      }
      num_obs <- c(num_obs, count)
      for (y in 1:num_col){
        if (is.na(child_birth[y, x])) {
          child_birth[y, x] <- Mode(pool)
        }
      }
    }
  }
}

# label categories
child_birth$meth <- cut(child_birth$meth, breaks = c(0, 5, 6, 7, 8, 9, 10), labels = c("Caucasian", "Mexican", "African-American", "Asian", "Mixed", "Other"), include.lowest = TRUE)
child_birth$feth <- cut(child_birth$feth, breaks = c(0, 5, 6, 7, 8, 9, 10), labels = c("Caucasian", "Mexican", "African-American", "Asian", "Mixed", "Other"), include.lowest = TRUE)
child_birth$med <- cut(child_birth$med, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), labels = c("elementary school", "middle school", "high school", "high school + trade school", "high school + some college", "college graduate", "trade school", "high school unclear"))
child_birth$fed <- cut(child_birth$fed, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), labels = c("elementary school", "middle school", "high school", "high school + trade school", "high school + some college", "college graduate", "trade school", "high school unclear"))
child_birth$income <- cut(child_birth$income, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("under 2500", "2500-4999", "5000-7499", "7500-9999", "10000-12499", "12500-14999", "15000-17499", "17500-19999", "20000-22499", "over 22500"))
child_birth$marital <- cut(child_birth$marital, breaks = c(0, 1, 2, 3, 4, 5), labels = c("married", "legally seperated", "divorced", "windowed", "never married"))
child_birth$smoke <- cut(child_birth$smoke, breaks = c(-1, 0, 1, 2, 3), labels = c("never", "smoke now", "until pregnancy", "used to, not anymore"))
child_birth$time <- cut(child_birth$time, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("never smoked", "still smokes", "during pregnancy", "less than a year", "1-2 years", "2-3 years", "3-4 years", "5-9 years", "more than 10 years", "quit but don't know when"))
child_birth$number <- cut(child_birth$number, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("never smoked", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-60", "more than 60", "smoke but don't know how much"))

# code for pair plots
pairs(~ wt + gestation + parity + marital + income + smoke + time + number, cex = 0.1, data = child_birth) # create pair plot between wt and other variables
pairs(~ wt + meth + mage + med + mht + mwt, cex = 0.1, data = child_birth) # create pair plot between wt and variables relate to mother's information
pairs(~ wt + feth + fage + fed + fht + fwt, cex = 0.1, data = child_birth) # create pair plot between wt and variables relate to father's information

# pre-fitting models
MPre1 <- lm(wt ~ gestation + mage + mht + mwt, data = child_birth)
MPre2 <- lm(wt ~ gestation + fage + fht + fwt, data = child_birth)
MPre3 <- lm(wt ~ gestation + mage + mht + mwt + fage + fht + fwt, data = child_birth)
MPre4 <- lm(wt ~ gestation + smoke + time + number, data = child_birth)

# automated selection
# Forward Selection
M0 <- lm(wt ~ 1, data = child_birth)
MFmax <- lm(wt ~ (.), data = child_birth)
M0Fwd <- lm(wt ~ 1, data = child_birth)
MFwd <- step(object = M0Fwd, scope = list(lower = M0, upper = MFmax), direction = "forward", trace = FALSE)
#Backward Selection
M0 <- lm(wt ~ 1, data = child_birth)
MBmax <- lm(wt ~ (.), data = child_birth)
M0Bck <- lm(wt ~  (.), data = child_birth)
MBck <- step(object = M0Bck, scope = list(lower = M0, upper = MBmax), direction = "backward", trace = FALSE)

# code for determing two canidate models
# R2
candidate_r2 <- c(summary(MPre1)$r.squared, summary(MPre2)$r.squared, summary(MPre3)$r.squared, summary(MPre4)$r.squared, summary(MFwd)$r.squared, summary(MBck)$r.squared)#, summary(MStp)$r.squared)
names(candidate_r2) <- c("MPre1", "MPre2", "MPre3", "MPre4", "MFwd", "MBck")#, "MStp")
max_r2 <- max(candidate_r2)
sec_max_r2 <- max(candidate_r2[candidate_r2 != max(candidate_r2)])
max_r2_model <- "None"
sec_max_r2_model <- "None"
for (key in names(candidate_r2)) {
  if (candidate_r2[[key]] == max_r2) {
    max_r2_model <- key
  } else if (candidate_r2[[key]] == sec_max_r2) {
    sec_max_r2_model <- key
  }
}
candidate_r2
max_r2_model
sec_max_r2_model
# Adjust R2
canadiate_adj_r2 <- c(summary(MPre1)$adj.r.squared, summary(MPre2)$adj.r.squared, summary(MPre3)$adj.r.squared, summary(MPre4)$adj.r.squared, summary(MFwd)$adj.r.squared, summary(MBck)$adj.r.squared)#, summary(MStp)$adj.r.squared)
names(canadiate_adj_r2) <- c("MPre1", "MPre2", "MPre3", "MPre4", "MFwd", "MBck")#, "MStp")
max_adj_r2 <- max(canadiate_adj_r2)
sec_max_adj_r2 <- max(canadiate_adj_r2[canadiate_adj_r2 != max(canadiate_adj_r2)])
max_adj_r2_model <- "None"
sec_max_adj_r2_model <- "None"
for (key in names(canadiate_adj_r2)) {
  if (canadiate_adj_r2[[key]] == max_adj_r2) {
    max_adj_r2_model <- key
  } else if (canadiate_adj_r2[[key]] == sec_max_adj_r2) {
    sec_max_adj_r2_model <- key
  }
}
canadiate_adj_r2
max_adj_r2_model
sec_max_adj_r2_model
# AIC
candidate_AIC <- c(AIC(MPre1), AIC(MPre2), AIC(MPre3), AIC(MPre4), AIC(MFwd), AIC(MBck))#, AIC(MStp))
names(candidate_AIC) <- c("MPre1", "MPre2", "MPre3", "MPre4", "MFwd", "MBck")#, "MStp")
min_AIC <- min(candidate_AIC)
sec_min_AIC <- min(candidate_AIC[candidate_AIC != min(candidate_AIC)])
min_AIC_model <- "None"
sec_min_AIC_model <- "None"
for (key in names(candidate_AIC)) {
  if (candidate_AIC[[key]] == min_AIC) {
    min_AIC_model <- key
  } else if (candidate_AIC[[key]] == sec_min_AIC) {
    sec_min_AIC_model <- key
  }
}
candidate_AIC
min_AIC_model
sec_min_AIC_model
# PRESS
candidate_PRESS <- c(sum((resid(MPre1)/(1-hatvalues(MPre1))) ^ 2), sum((resid(MPre2)/(1-hatvalues(MPre2))) ^ 2), sum((resid(MPre3)/(1-hatvalues(MPre3))) ^ 2), sum((resid(MPre4)/(1-hatvalues(MPre4))) ^ 2), sum((resid(MFwd)/(1-hatvalues(MFwd))) ^ 2), sum((resid(MBck)/(1-hatvalues(MBck))) ^ 2))
names(candidate_PRESS) <- c("MPre1", "MPre2", "MPre3", "MPre4", "MFwd", "MBck")#, "MStp")
min_PRESS <- min(candidate_PRESS)
sec_min_PRESS <- min(candidate_PRESS[candidate_PRESS != min(candidate_PRESS)])
min_PRESS_model <- "None"
sec_min_PRESS_model <- "None"
for (key in names(candidate_PRESS)) {
  if (candidate_PRESS[[key]] == min_PRESS) {
    min_PRESS_model <- key
  } else if (candidate_PRESS[[key]] == sec_min_PRESS) {
    sec_min_PRESS_model <- key
  }
}
candidate_PRESS
min_PRESS_model
sec_min_PRESS_model

# Plot Residual vs Fitted Plot, QQ Plot and other plots
plot(MBck, ask = FALSE)
plot(MFwd, ask = FALSE)

#residuals vs preidicted values for MFwd
lchdb <- child_birth
MFwdsum <- summary(MFwd)
yFwd.hat <- predict(MFwd)

sigmaFwd.hat <- MFwdsum$sigma
resFwd <- resid(MFwd)
stanFwd.res <- resFwd/sigmaFwd.hat

#compute leverages for MFwd
XFwd <- model.matrix(MFwd)
HFwd <- XFwd %*% solve(crossprod(XFwd), t(XFwd))
head(diag(HFwd))
hFwd <- hatvalues(MFwd)
range(hFwd - diag(HFwd))

#studentized residuals for MFwd
studFwd.res <- stanFwd.res/sqrt(1-hFwd)

#PRESS residuals for MFwd
pressFwd <- resFwd/(1-hFwd)

#DFFITS residuals for MFwd
dftsFwd <- dffits(MFwd)

#standardize each of these such that they are identical at thge average leverage value for MFwd
pFwd <- length(coef(MFwd))
nFwd <- nobs(MFwd)
hbarFwd <- pFwd/nFwd # average leverage
studFwd.res <- studFwd.res*sqrt(1-hbarFwd) # at h = hbar, stud.res = stan.res
pressFwd <- pressFwd*(1-hbarFwd)/sigmaFwd.hat # at h = hbar, press = stan.res
dftsFwd <- dftsFwd*(1-hbarFwd)/sqrt(hbarFwd) # at h = hbar, dfts = stan.res

#plot all residuals for MFwd
par(mfrow = c(1,2), mar = c(2,2,.3,.3))
# against predicted values
cex <- .8
plot(yFwd.hat, rep(0, length(yFwd.hat)), type = "n", # empty plot to get the axis range
     ylim = range(stanFwd.res, studFwd.res, pressFwd, dftsFwd), cex.axis = cex,
     xlab = "Predicted Values", ylab = "Residuals")
# dotted line connecting each observations residuals for better visibility
segments(x0 = yFwd.hat,
         y0 = pmin(stanFwd.res, studFwd.res, pressFwd, dftsFwd),
         y1 = pmax(stanFwd.res, studFwd.res, pressFwd, dftsFwd),
         lty = 2)
points(yFwd.hat, stanFwd.res, pch = 21, bg = "black", cex = cex)
points(yFwd.hat, studFwd.res, pch = 21, bg = "blue", cex = cex)
points(yFwd.hat, pressFwd, pch = 21, bg = "red", cex = cex)
points(yFwd.hat, dftsFwd, pch = 21, bg = "orange", cex = cex)
# against leverages
plot(hFwd, rep(0, length(yFwd.hat)), type = "n", cex.axis = cex,
     ylim = range(stanFwd.res, studFwd.res, pressFwd, dftsFwd),
     xlab = "Leverages", ylab = "Residuals")
segments(x0 = hFwd,
         y0 = pmin(stanFwd.res, studFwd.res, pressFwd, dftsFwd),
         y1 = pmax(stanFwd.res, studFwd.res, pressFwd, dftsFwd),
         lty = 2)
points(hFwd, stanFwd.res, pch = 21, bg = "black", cex = cex)
points(hFwd, studFwd.res, pch = 21, bg = "blue", cex = cex)
points(hFwd, pressFwd, pch = 21, bg = "red", cex = cex)
points(hFwd, dftsFwd, pch = 21, bg = "orange", cex = cex)
abline(v = hbarFwd, col = "grey60", lty = 2)
legend("topright", legend = c("Standardized", "Studentized", "PRESS", "DFFITS"),
       pch = 21, pt.bg = c("black", "blue", "red", "orange"), title = "Residual Type:",
       cex = cex, pt.cex = cex)
#cook's distance vs leverage for MFwd
DFwd <- cooks.distance(MFwd)
#flag some of the points for MFwd
inflFwd.ind <- which.max(DFwd)
levFwd.ind <- hFwd > 2*hbarFwd # leverage more than 2x the average
clrsFwd <- rep("black", len = nFwd)
clrsFwd[levFwd.ind] <- "blue"
clrsFwd[inflFwd.ind] <- "red"
par(mfrow = c(1,1), mar = c(4,4,1,1))
cex <- .8
plot(hFwd, DFwd, xlab = "Leverage", ylab = "Cookâ..s Influence Measure",
     pch = 21, bg = clrsFwd, cex = cex, cex.axis = cex)
pFwd <- length(coef(MFwd))
nFwd <- nrow(lchdb)
hbarFwd <- pFwd/nFwd # average leverage
abline(v = 2*hbarFwd, col = "grey60", lty = 2) # 2x average leverage
legend("topleft", legend = c("High Leverage", "High Influence"), pch = 21,
       pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)

#residuals vs preidicted values for MBck
lchdb <- child_birth
MBcksum <- summary(MBck)
yBck.hat <- predict(MBck)

sigmaBck.hat <- MBcksum$sigma
resBck <- resid(MBck)
stanBck.res <- resBck/sigmaBck.hat

#compute leverages for MBck
XBck <- model.matrix(MBck)
HBck <- XBck %*% solve(crossprod(XBck), t(XBck))
head(diag(HBck))
hBck <- hatvalues(MBck)
range(hBck - diag(HBck))

#studentized residuals for MBck
studBck.res <- stanBck.res/sqrt(1-hBck)

#PRESS residuals for MBck
pressBck <- resBck/(1-hBck)

#DFFITS residuals for MBck
dftsBck <- dffits(MBck)

#standardize each of these such that they are identical at thge average leverage value for MBck
pBck <- length(coef(MBck))
nBck <- nobs(MBck)
hbarBck <- pBck/nBck # average leverage
studBck.res <- studBck.res*sqrt(1-hbarBck) # at h = hbar, stud.res = stan.res
pressBck <- pressBck*(1-hbarBck)/sigmaBck.hat # at h = hbar, press = stan.res
dftsBck <- dftsBck*(1-hbarBck)/sqrt(hbarBck) # at h = hbar, dfts = stan.res

#plot all residuals for MBck
par(mfrow = c(1,2), mar = c(2,2,.3,.3))
# against predicted values
cex <- .8
plot(yBck.hat, rep(0, length(yFwd.hat)), type = "n", # empty plot to get the axis range
     ylim = range(stanBck.res, studBck.res, pressBck, dftsBck), cex.axis = cex,
     xlab = "Predicted Values", ylab = "Residuals")
# dotted line connecting each observations residuals for better visibility
segments(x0 = yBck.hat,
         y0 = pmin(stanBck.res, studBck.res, pressBck, dftsBck),
         y1 = pmax(stanBck.res, studBck.res, pressBck, dftsBck),
         lty = 2)
points(yBck.hat, stanBck.res, pch = 21, bg = "black", cex = cex)
points(yBck.hat, studBck.res, pch = 21, bg = "blue", cex = cex)
points(yBck.hat, pressBck, pch = 21, bg = "red", cex = cex)
points(yBck.hat, dftsBck, pch = 21, bg = "orange", cex = cex)
# against leverages
plot(hBck, rep(0, length(yBck.hat)), type = "n", cex.axis = cex,
     ylim = range(stanBck.res, studBck.res, pressBck, dftsBck),
     xlab = "Leverages", ylab = "Residuals")
segments(x0 = hBck,
         y0 = pmin(stanBck.res, studBck.res, pressBck, dftsBck),
         y1 = pmax(stanBck.res, studBck.res, pressBck, dftsBck),
         lty = 2)
points(hBck, stanBck.res, pch = 21, bg = "black", cex = cex)
points(hBck, studBck.res, pch = 21, bg = "blue", cex = cex)
points(hBck, pressBck, pch = 21, bg = "red", cex = cex)
points(hBck, dftsBck, pch = 21, bg = "orange", cex = cex)
abline(v = hbarBck, col = "grey60", lty = 2)
legend("topright", legend = c("Standardized", "Studentized", "PRESS", "DFFITS"),
       pch = 21, pt.bg = c("black", "blue", "red", "orange"), title = "Residual Type:",
       cex = cex, pt.cex = cex)
#cook's distance vs leverage for MBck
DBck <- cooks.distance(MBck)
#flag some of the points for MBck
inflBck.ind <- which.max(DBck)
levBck.ind <- hBck > 2*hbarBck # leverage more than 2x the average
clrsBck <- rep("black", len = nBck)
clrsBck[levBck.ind] <- "blue"
clrsBck[inflBck.ind] <- "red"
par(mfrow = c(1,1), mar = c(4,4,1,1))
cex <- .8
plot(hBck, DBck, xlab = "Leverage", ylab = "Cookâ..s Influence Measure",
     pch = 21, bg = clrsBck, cex = cex, cex.axis = cex)
pBck <- length(coef(MBck))
nBck <- nrow(lchdb)
hbarBck <- pBck/nBck # average leverage
abline(v = 2*hbarBck, col = "grey60", lty = 2) # 2x average leverage
legend("topleft", legend = c("High Leverage", "High Influence"), pch = 21,
       pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)


#cross-validation step
nreps <- 2e3
ntrain <- round(0.8 * num_col)
ntest <- num_col - ntrain
mspe1 <- rep(NA, nreps)
mspe2 <- rep(NA, nreps)
logLambda <- rep(NA, nreps)

for (ii in 1:nreps) {
  if (ii %% 400 == 0) message("ii = ", ii)
  #randomly select training observations
  train.ind <- sample(num_col, ntrain) #training observations
  M1.cv <- update(MFwd, subset = train.ind)
  M2.cv <- update(MBck, subset = train.ind)
  #out-of-sample residuals for bothe models
  #that is, testing data - predictions with training parameters
  a <- predict(M1.cv, newdata = child_birth[-train.ind,])
  b <- predict(M2.cv, newdata = child_birth[-train.ind,])
  M1.res <- child_birth$wt[-train.ind] - a
  M2.res <- child_birth$wt[-train.ind] - b
  #mean-square prediction errors
  mspe1[ii] <- mean(M1.res^2)
  mspe2[ii] <- mean(M2.res^2)
  #out-of-sample likelihood ratio
  M1.sigma <- sqrt(sum(resid(M1.cv)^2)/ntrain)
  M2.sigma <- sqrt(sum(resid(M2.cv)^2)/ntrain)
  #since res = y-pred, dnorm(y, pred, sd) = dnorm(res, 0, sd)
  logLambda[ii] <- sum(dnorm(M1.res, mean = 0, sd = M1.sigma, log=TRUE))
  logLambda[ii] <- logLambda[ii] - sum(dnorm(M2.res, mean = 0, sd=M2.sigma, log=TRUE))
}

par(mfrow = c(1,2))
par(mar = c(2.5, 2.5, .1, .1))
#plot rMSPE boxplot
boxplot(x = list(sqrt(mspe1), sqrt(mspe2)), names=expression(M[FWD], M[BCK]), cex=.7, ylab = expression(sqrt(MSPE)), col=c("yellow", "orange"))
#plot out-of-sample log(Lambda)
hist(logLambda, breaks=50, freq=FALSE, xlab=expression(Lambda^{test}), main="", cex=.7)
abline(v=mean(logLambda),col="red") #average value


