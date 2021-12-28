################################################################################
################################################################################
# Navarra -- efficacy vs effect size
################################################################################
################################################################################
# Eric Keen, December 2021

# You need the function from this file
source('function-cough_simulator.R')

################################################################################
################################################################################
# VitaloJak simulator function

# Slightly different function than the previous
# Simulate a cough series PRIOR to treatment
# Simulate AFTER
# Determine effect detected by longitudinal monitoring
# Determine effect detected by VitaloJak

vitalojak_power <- function(before, after, n=14){

  if(FALSE){
    n=14
    before = 4
    after = 0.9*4
  }

  # True expected effect
  true_effect <- before - after

  # Longitudinal  ==============================================================

  rates1 <- simulate_cough_timeseries(mean_cough_rate = before, var_functio=var_function, hours = 24*n)$hourly_coughs
  rates2 <- simulate_cough_timeseries(mean_cough_rate = after, var_functio=var_function, hours = 24*n)$hourly_coughs

  rates1
  rates2

  # p value
  wt <- wilcox.test(rates1, rates2,alternative='greater')
  wt$p.value

  # Means
  realized_1 <- sum(rates1) / length(rates1) ; realized_1
  realized_2 <- sum(rates2) / length(rates2) ; realized_2
  effect_lon <- realized_1 - realized_2
  error_lon <- true_effect - effect_lon
  error_prop_lon <- ((true_effect + abs(error_lon))/true_effect) -  1

  # Bootstrap CI
  diffi <- c()
  for(i in 1:10000){
    simpre <- sample(rates1,size=length(rates1),replace=TRUE)
    simpos <- sample(rates2,size=length(rates2),replace=TRUE)
    diffi <- c(diffi,(mean(simpre) - mean(simpos)))
  }
  #hist(diffi)
  diff_uci <- quantile(diffi,0.975) ; diff_uci
  diff_lci <- quantile(diffi,0.025) ; diff_lci

  # 24-hr estimates   ==========================================================
  # Randomization

  # Get random starting points for 24-hour monitoring in the before and after period
  jax <- data.frame(pre_starts = sample(1:(length(rates1)-24),10000,replace=TRUE),
                    pos = sample(1:(length(rates2)-24),10000,replace=TRUE))
  head(jax)

  diffs <- apply(jax,1,function(x){
    m1 <- mean(rates1[x[1]:(x[1] + 23)])
    m2 <- mean(rates2[x[2]:(x[2] + 23)])
    d1 <- m1 - m2
    return(d1)
  })
  #hist(diffs)

  # Calculate differences & errors
  jax_errors <- effect_lon - diffs
  #hist(jax_errors)
  error_24 <- mean(abs(jax_errors)) ; error_24
  prop_error_24 <- ((effect_lon + error_24) / effect_lon) - 1 ; prop_error_24

  # P value and failure rates
  pval_24 <- length(which(diffs <= 0)) / length(diffs) ; pval_24 # p-value
  failure_24 <- (length(which(diffs > diff_uci | diffs < diff_lci)) / nrow(jax)) ; failure_24 # p-value

  # Compile
  dfi <- data.frame(before,
                    after,
                    effect = true_effect,
                    effect_factor = (1 - (mean(after)/ mean(before))),
                    n=n*24,
                    before_lon = realized_1,
                    after_lon = realized_2,
                    effect_lon,
                    error_lon,
                    error_prop_lon,
                    lci_lon = diff_lci,
                    uci_lon = diff_uci,
                    pval_lon = wt$p.value,
                    pval_24,
                    failure_24,
                    error_24,
                    prop_error_24)
  dfi
  return(dfi)
}

################################################################################
################################################################################
# Iterative analysis

# We provide the results of this analysis, but you can reproduce it using the `else` portion of this loop

if('data-power.rds' %in% dir()){
  df <- readRDS('data-power.rds')
}else{

  # Apply function in iterative simulation  ======================================

  df <- data.frame()
  i = 0.1
  for(i in c(0.01,seq(0.05,0.95,by=0.05))){
    before <- 4
    after <- before*(1-i) ; after
    for(j in 1:100){
      dfi <- vitalojak_power(before,after,14)
      print(dfi)
      df <- rbind(df,dfi)
    }
  }

  saveRDS(df,'navarra-power.rds')
}

################################################################################
################################################################################
# Plots

head(df)
nrow(df)

# Failure to detect trend
ggplot(df,aes(x=effect_factor, y= pval_24)) +
  geom_point(color='cadetblue4',alpha=.1) +
  ylab('Rate of failure to detect change') +
  xlab('Fraction reduction in original cough rate') +
  scale_y_continuous(breaks=seq(0,1,by=0.10)) +
  scale_x_continuous(breaks=c(0.01,seq(0.05,0.95,by=0.10))) +
  #labs(title='Detecting an effect with 24-hr monitoring depends on effect size') +
  stat_summary(fun='mean',col='firebrick',alpha=.4,geom='line',lwd=1.5) +
  theme_light()


# Confidence intervals  ========================================================

head(df)

df %>%
  dplyr::group_by(effect_factor) %>%
  dplyr::summarize(pos_mean = base::mean(pval_24),
                   pos_lci = stats::quantile(pval_24,0.025),
                   pos_uci = stats::quantile(pval_24,0.975),
                   ci_mean = base::mean(failure_24),
                   ci_lci = stats::quantile(failure_24,0.025),
                   ci_uci = stats::quantile(failure_24,0.975))



