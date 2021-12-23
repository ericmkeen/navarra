################################################################################
################################################################################
# Cough simulator
################################################################################
################################################################################
# This code creates a function for simulating a cough time series,
# based on empirical relationships discerned in the Navarra cohort

# This function is useful as a demo (see bottom code within the FALSE if statement)
# And it will be sourced by subsequent files. Hence, the only thing not wrapped
# in a FALSE condition is the function's pieces.

################################################################################
################################################################################

# Bring over variance function from 'monitoring-3-modeling-cough-rate.R'

var_function <- function(mu,b1 = 1.339539, intercept = 1.90719){
  var_return <- exp(b1*log(mu) + intercept)
  #plot(var_function(mu) ~ mu, type='l')
  return(var_return)
}

################################################################################
################################################################################
# Create cough simulation function

simulate_cough_timeseries <- function(mean_cough_rate,
                                      var_function,
                                      hours=24,
                                      verbose= FALSE,
                                      toplot=FALSE){
  if(FALSE){
    # Dummy data ignored by function call
    # Use to debug / play with function
    mean_cough_rate = 4
    var_function <- function(mu,b1 = 1.353115, intercept = 1.986585){
      var_return <- exp(b1*log(mu) + intercept)
      return(var_return)
    }
    hours = 24*14
    toplot = TRUE
    verbose = FALSE
  }

  # Calculate variance & nb_size based on the mean cough rate provided
  mu <- mean_cough_rate
  cough_var <- var_function(mu)
  nb_size <- (mean_cough_rate^2) / (cough_var - mean_cough_rate)

  # Simulate coughs
  coughs <- rnbinom(n=hours, size = nb_size, mu = mean_cough_rate)

  # Summarize daily cough rate
  max_hours <- max(c(hours,24)) ; max_hours
  days <- floor(max_hours / 24) ; days
  daily <- c()
  if(days==1){
    daily = sum(coughs)
  }else{
    for(i in 1:(days-1)){
      hr_start <- i*24 - 23
      hr_end <- hr_start + 23
      coughi <- sum(coughs[hr_start:hr_end])
      daily <- c(daily,coughi)
    }
  }
  daily

  # Compute error curve
  rate_estimate <- c()
  for(i in 1:hours){rate_estimate[i] <- mean(coughs[1:i],na.rm=TRUE)}
  rate_estimate
  mre <- data.frame(hours=1:hours,
                    estimate=rate_estimate,
                    nb_mean=mean_cough_rate,
                    nb_var=cough_var,
                    nb_size)
  mre$error <- mre$estimate - mre$nb_mean
  mre$prop_error <- ((mre$nb_mean + abs(mre$error))/mre$nb_mean) - 1
  head(mre)

  if(toplot){  #================================================================
    par(mfrow=c(3,1))
    par(mar=c(4.2,4.2,3,.5))
    hist(coughs,breaks=seq(0,max(coughs)+ 1,by=1),
         xlab='Hourly cough counts',
         main=paste0('Realized hourly cough rate = ',round(mean(coughs,na.rm=TRUE),3)))
    abline(v=mean(coughs,na.rm=TRUE),col='firebrick')

    plot(daily,type='o',pch=16,ylim=c(0,(max(daily)*1.1)), xlab='Daily time series',
         main=paste0('Realized daily cough rate = ',round(mean(daily,na.rm=TRUE),3)))
    abline(h=mean(daily),col='firebrick')

    plot(mre$estimate ~ mre$hours,ylim=c(0,(max(mre$estimate)*1.1)),
         pch=16,type='o',cex=.5,
         xlab='Hours of monitoring', ylab='Estimate of true cough rate')
    abline(h=mre$nb_mean[1],col='firebrick')
    abline(h=0.9*mre$nb_mean[1],col='red',lty=3)
    abline(h=1.1*mre$nb_mean[1],col='red',lty=3)
  }

  results <- list(hourly_coughs=coughs,
                  daily_coughs=daily,
                  error=mre)

  return(results)
}

################################################################################
################################################################################
# Demonstrate theory

if(FALSE){

  par(mfrow=c(1,1))
  par(mar=c(4.2,4.2,.5,.5))
  plot(1,type='n',xlim=c(0,10),axes=FALSE,ann=FALSE, ylim=c(0,14))
  axis(1,at=0:10)
  axis(2,las=2)
  title(xlab='Days of monitoring', ylab='Estimate of cough rate')

  for(i in 1:100){
    simi <- simulate_cough_timeseries(mean_cough_rate = 4,
                                      var_functio=var_function,
                                      hours = 24*10,
                                      toplot=FALSE)
    err <- simi$error
    err$days <- err$hours / 24
    lines(err$estimate ~ err$days,col=adjustcolor('black',alpha.f=.1))

    abline(h=4,lty=1,col='firebrick')
    abline(h=1.1*4,lty=3,col='red')
    abline(h=.9*4,lty=3,col='red')
  }

}

