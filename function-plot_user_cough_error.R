#' Calculate cough rate error
#'
#' @param ho Hyfe user Object
#' @param hrs Hours of day (0 - 24) to subset the dataset to.
#' @param iterations Number of iterations to estimate mean error at each subsample interval
#' @param verbose If `TRUE`, status updates will be printed to console.
#' @param toplot Toggle plots on or off.
#' @return Plot & list
#'
plot_user_cough_error <- function(ho,
                                  hrs=0:24,
                                  iterations=100,
                                  verbose=FALSE,
                                  toplot=TRUE){

  # Parameter values for debugging:
  #iterations = 100
  #hrs = 0:24
  #mr <- ho
  #verbose=TRUE
  #hours <- sim_ho$hours

  # Get 'true' cough rate
  hours <- ho$hours
  nrow(hours)
  grand_rate <- sum(hours$coughs,na.rm=TRUE) / sum(hours$rec,na.rm=TRUE)
  grand_rate # coughs per hour

  # Prepare time series
  if(verbose){message("Preparing second-by-second time series for this user ...")}
  head(ho$peaks)
  head(ho$hours)
  ts_range <- range(ho$hours$timestamp) ; ts_range

  ts_start <- ts_range[1]
  ts_stop <- ts_range[2]
  ts <- ts_start : ts_stop ; length(ts)
  df_ts <- data.frame(ts,rec=0,cough=0)

  sessi <- ho$sessions
  sessi <- sessi[sessi$session_time > 0,]

  #if(nrow(sessi)>0){
    # Simplify session data
    sessi <- sessi %>% transmute(ts_start = unclass(start_time),
                                 ts_stop = unclass(stop_time))
    head(sessi)

    # Collapse sessions into a vector of every timestamp second monitored
    ts_sessions <- apply(sessi,1,function(x){x[1]:x[2]})
    ts_sessions <- unlist(ts_sessions,use.names=FALSE)

    # Get second-by-second time series of when recording was happening
    matchi <- which(df_ts$ts %in% ts_sessions) ; length(matchi)
    if(length(matchi)>0){
      df_ts$rec[matchi] <- 1
    }

    # Get coughs
    peaki <- ho$coughs ; nrow(peaki) ; head(peaki)
    matchi <- which(df_ts$ts %in% peaki$ts) ; length(matchi)
    if(length(matchi)>0){
      df_ts$cough[matchi] <- 1
    }

    # Get rolling sum for each second, looking an hour ahead
    if(verbose){message("Rolling sum of recording and coughs ...")}
    df_ts <- df_ts %>% mutate(rec_roll = unlist(slider::slide(rec, sum, .after=3599)))
    df_ts <- df_ts %>% mutate(cough_roll = unlist(slider::slide(cough, sum, .after=3599)))
    nrow(df_ts)
    unique(df_ts$cough_roll)

    # Remove timestamps for which there is not a half hour of recording in the next hour
    df_ts <- df_ts %>% filter(rec_roll >= (3599/2))
    nrow(df_ts)

    # Prepare random sampling function that ensures there is no overlap in the hours sampled
    rand_pick_min <- function(ar, min.dist = 3599, n.picks){
      stopifnot(is.numeric(min.dist),
                is.numeric(n.picks), n.picks%%1 == 0)
      if(length(ar)/n.picks < min.dist){
        stop('The number of picks exceeds the maximum number of divisions that the array allows which is: ',
             floor(length(ar)/min.dist))}
      picked <- array(NA, n.picks)
      copy <- ar
      for (i in 1:n.picks) {
        if(length(copy)>0){
          #stopifnot(length(copy) > 0)
          picked[i] <- sample(copy, 1)
          copy <- copy[ abs(copy - picked[i]) >= min.dist ]
        }else{
          #picked[i] <- NA
          picked <- NA
        }
      }
      return(picked)
    }

    #min.dist = 3599
    #ar = df_ts$ts ; head(ar)
    #n.picks = 1
    #rand_pick_min(ar = df_ts$ts, min.dist = 3599, n.picks = 2)

    errors <- data.frame()
    j=1
    for(j in 1:iterations){
      if(verbose){message("Iteration ",j," out of ",iterations," ...")}

      i=12
      for(i in c(12,seq(24,240,by=24))){
        subsamples <- rand_pick_min(ar = df_ts$ts, min.dist = 3599, n.picks = i)
        rates_i <- NA
        if(!is.na(subsamples[1])){
          df_sub <- df_ts[df_ts$ts %in% subsamples,] ; df_sub
          rates_i <- sum(df_sub$cough_roll) / (sum(df_sub$rec_roll)/3600) ; rates_i
        }
        dfji <- data.frame(uid=paste(ho$uid,collapse=','),
                          hours=i,
                          actual_rate=grand_rate,
                          estimate=rates_i)
        print(dfji)
        errors <- rbind(errors,dfji)
      }
    }

    error_sum <- errors %>%
      dplyr::mutate(error = abs(estimate - actual_rate)) %>%
      dplyr::group_by(hours) %>%
      dplyr::summarize(uid=unique(uid),
                       iterations = iterations,
                       mean_estimate = mean(estimate[is.finite(estimate)],na.rm=TRUE),
                       sd_estimate = sd(estimate[is.finite(estimate)],na.rm=TRUE),
                       mean_error = mean(error[is.finite(error)],na.rm=TRUE),
                       sd_error = sd(error[is.finite(error)],na.rm=TRUE)) %>%
      dplyr::mutate(mean_prop_error = abs(((grand_rate + abs(mean_error)) / grand_rate) - 1),
                    sd_prop_error = abs(((grand_rate + abs(sd_error)) / grand_rate) - 1))

    error_sum

    # Add error delta
    df_error <- data.frame(hours = error_sum$hours[1:(nrow(error_sum)-1)],
                           e1 = error_sum$mean_prop_error[1:(nrow(error_sum)-1)],
                           e2 = error_sum$mean_prop_error[2:nrow(error_sum)])
    df_error$delta <- abs(df_error$e1 - df_error$e2)
    error_sum <- dplyr::left_join(error_sum,df_error,by='hours')

    if(toplot){
      par(mar=c(4.2,4.2,.5,.5))
      par(mfrow=c(2,2))
      plot(estimate ~ hours,
           data=errors,
           pch=16,cex=.5,
           col=adjustcolor('black',alpha.f=.2),
           log='x',
           ylim=c(0,10*grand_rate),
           xlab='Hours used to estimate cough rate',
           ylab='Cough rate estimate (coughs per hour)')
      abline(h=grand_rate,lty=1,col='red')

      plot(mean_error ~ hours,
           data=error_sum,
           type='o', pch=16,cex=.5,
           xlab = 'Hours used to estimate cough rate',
           ylab = 'Mean cough rate error',
           log='x',
           ylim=c(0,5*grand_rate))
      lines(x=error_sum$hours,
            y=(error_sum$mean_error + error_sum$sd_error),
            lty=3,col='grey70')
      lines(x=error_sum$hours,
            y=(error_sum$mean_error - error_sum$sd_error),
            lty=3,col='grey70')

      plot(mean_prop_error ~ hours,
           data=error_sum,
           type='o', pch=16,cex=.5,
           log='x',
           xlab='Hours used to estimate cough rate',
           ylab='Mean proportional cough rate error',
           ylim=c(0,5))
      lines(x=error_sum$hours,
            y=(error_sum$mean_prop_error + error_sum$sd_prop_error),
            lty=3,col='grey70')
      lines(x=error_sum$hours,
            y=(error_sum$mean_prop_error - error_sum$sd_prop_error),
            lty=3,col='grey70')
      abline(h=0.1,lty=1,col='red')

      plot(delta ~ hours,
           data=error_sum,
           type='l',
           log='x',
           xlab='Hours used to estimate cough rate',
           ylab='Change in prop. cough rate error',
           ylim=c(0,3))
      abline(h=0.01,lty=1,col='red')

      par(mfrow=c(1,1))
    }

    ho_return <- list(raw = errors,
                      summary = error_sum,
                      actual_rate = grand_rate,
                      tot_rec = sum(hours$rec,na.rm=TRUE),
                      hrs_used=hrs,
                      iterations=iterations)
    return(ho_return)
  }
