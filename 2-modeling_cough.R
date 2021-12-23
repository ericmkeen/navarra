################################################################################
################################################################################
# Modeling a cough rate distribution based on Navarra users
################################################################################
################################################################################
# Eric Keen, December 2021

library(devtools)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(MASS)
library(mgcv)

################################################################################
################################################################################
# Get cough rate parameters (mean, variance) for each user

# We provide the result of this routine, so that it is easy to pass to statisticians,
# but you can also reproduce the result yourself

if('data-modeling.rds' %in% dir()){
  fits100 <- readRDS('data-modeling.rds')
}else{

  # The raw data here is hourly monitoring duration and coughs for each consenting user
  # with more than 100 hours of monitoring. Only keeps hours in which there is at least 0.5 hours of monitoring.
  hc <- readRDS('data-hours.rds')

  # Explore
  ggplot(hc, aes(x=coughs)) +
    scale_x_continuous(trans='log', breaks=c(0.1,1,2,5,10,25,50,100,200)) +
    xlab('Hourly cough rate') +
    geom_histogram()

  # Loop through each user, get fit parameters for a neg. binom.
  uids <- unique(hc$uid)
  i=1
  fits<- data.frame()
  for(i in 1:length(uids)){
    uid_i <- uids[i] ; uid_i

    # Filter hourly record to this user
    hc_i <- hc %>% filter(uid == uid_i)
    observations <- hc_i$coughs

    # Determine fit parameters from sample
    mean_cough_rate <- mean(observations,na.rm=TRUE)
    var_cough_rate <- var(observations, na.rm=TRUE)
    nb_size <- (mean_cough_rate^2) / (var_cough_rate - mean_cough_rate)

    # Store result
    fit_i <- data.frame(uid=uid_i,
                        hours=length(observations),
                        coughs=sum(observations,na.rm=TRUE),
                        mu= mean_cough_rate,
                        var= var_cough_rate,
                        size=nb_size)
    fits <- rbind(fits, fit_i)
  }

  nrow(fits)

  # Check out result, for users with at least 100 hours
  fits100 <- fits %>% filter(hours >= 100)

  nrow(fits100)

  # Exclude users 1 through 11
  saveRDS(fits100,'data-modeling.rds')
}

################################################################################
################################################################################
# Fit model to relationship between mean and variance across users

# Check out data
head(fits100)
nrow(fits100)
nrow(fits100)
nrow(fits100[fits100$cough > 0,])
nrow(fits100[fits100$mu > 0,])
nrow(fits100[fits100$size > 0,])

# Plot process
par(mfrow=c(2,1))
par(mar=c(4.2,4.2,.5,.5))
plot(log(var) ~ log(mu), data=fits100,
     xlab='log Mean Hourly Cough Rate',
     ylab='log Variance of Cough Rate',
     pch=16,col=adjustcolor('black',alpha.f=.3))
var_mu_fit <- lm( log(var) ~ log(mu), data=fits100[fits100$mu > 0,])
abline(var_mu_fit, col='firebrick')


summary(var_mu_fit)
graphics::text(1,-4,label="p < 0.0001")
graphics::text(1,-5,expression(R^2 == 0.94))
b1  <- var_mu_fit$coefficients[2] ; b1
intercept  <- var_mu_fit$coefficients[1] ; intercept


mu <- seq(0.01,10,length=1000)
y <- exp(b1*log(mu) + intercept)
plot(var ~ mu, data=fits100,
     xlab='Mean Hourly Cough Rate',
     ylab='Variance of Cough Rate',
     pch=16,col=adjustcolor('black',alpha.f=.3))
lines(y~mu, col='firebrick')
#graphics::text(6.5,20,expression(y = e^(1.339*ln(mu) + 1.907)))
text(7.5,20,expression(Variance = 6.33*Mean^1.339))
text(5.2,20,'Variance = ')


################################################################################
################################################################################
# Result of model: a function that predicts variance based upon mean

var_function <- function(mu,b1 = 1.339539, intercept = 1.90719){
  var_return <- exp(b1*log(mu) + intercept)
  #plot(var_function(mu) ~ mu, type='l')
  return(var_return)
}

