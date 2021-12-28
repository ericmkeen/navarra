################################################################################
################################################################################
# Navarra -- simulating a VitaloJak for the 3 archetypes
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(fitdistrplus)
library(DescTools)
library(ggpubr)

################################################################################
################################################################################
# Setup function
# This function simulates a 24-hour Vitalojak monitoring period during
# a 'before' period (14 days) and
# a 'after' period (14 days)
# then does iterative sampling to ask how often, if at all, an effect between
# the two periods would be detected by a vitalojak

vitalojak_simulator <- function(ho, # Hyfe user object
                                pre_d1, # Start day-of-season (days$dos) for 'pre' period
                                pos_d1, # Start dos for 'pos' period
                                range_width=14, # days in each window
                                verbose=FALSE){

  if(FALSE){
    # Debugging
    range_width = 14
    pre_d1 = 1
    pos_d1 = 25
    verbose=TRUE
  }

  # Work with the days dataframe in the Hyfe object
  mr <- ho$days
  head(mr)

  # Determine pre- and post- time windows
  rpre <- pre_d1:(pre_d1 + (range_width-1)) ; rpre # pre range
  rpos <- pos_d1:(pos_d1 + (range_width-1)) ; rpos # pre range

  # Get pre- and post- data
  mrpre <- mr %>% filter(dos %in% rpre)  ; mrpre$rec
  mrpos <- mr %>% filter(dos %in% rpos)  ; mrpos$rec

  # Keep only days with > 12 hours of recording
  mrpre <- mrpre %>% filter(rec >= 12) ; nrow(mrpre)
  mrpos <- mrpos %>% filter(rec >= 12) ; nrow(mrpos)
  if(verbose){message('n_pre = ',nrow(mrpre),'   |   n_pos = ',nrow(mrpos))}

  # Get pre- and post- mean cough rates
  meanpre <- mean(mrpre$rate[is.finite(mrpre$rate)]) ; meanpre
  meanpos <- mean(mrpos$rate[is.finite(mrpos$rate)]) ; meanpos
  cipre <- paste0(round(DescTools::BootCI(mrpre$rate[is.finite(mrpre$rate)], FUN=mean)[2:3],3), collapse='-')
  cipos <- paste0(round(DescTools::BootCI(mrpos$rate[is.finite(mrpos$rate)], FUN=mean)[2:3],3), collapse='-')
  if(verbose){message('mean_pre = ',round(meanpre,3),'   |   mean_pos = ', round(meanpos,3))}

  diffi <- c()
  for(i in 1:10000){
    simpre <- sample(mrpre$rate,size=nrow(mrpre),replace=TRUE)
    simpos <- sample(mrpos$rate,size=nrow(mrpos),replace=TRUE)
    diffi <- c(diffi,(mean(simpos) - mean(simpre)))
  }
  diff_mean <- mean(diffi) ; diff_mean
  diff_uci <- quantile(diffi,0.975) ; diff_uci
  diff_lci <- quantile(diffi,0.025) ; diff_lci

  # Are cough rates during these two periods significantly different?
  wt <- wilcox.test(mrpre$rate,mrpos$rate,alternative='greater')
  if(verbose){message('Hyfe p-value (one-tail Mann-Whitney U test)= ',round(wt$p.value,7))}

  # Simulate VitaloJak:
  # A single day of recording in each period
  # Randomly sample the two days, compare cough rates, store difference
  # Iterate 100,000 times

  jax <- data.frame(pre = sample(mrpre$rate,10000,replace=TRUE),
                    pos = sample(mrpos$rate,10000,replace=TRUE)) %>%
    mutate(diff = pos - pre)

  # Plot distribution of differences
  hist(jax$diff,breaks=seq(min(jax$diff),max(jax$diff),length=30),
       #main = 'Simulated 24-hr VitaloJak',
       main=NULL,
       xlab = 'Pos rate - Pre rate',
       border='grey70',col='grey50')
  abline(v=0,lty=3,col='steelblue3')
  abline(v=diff_mean,lty=1,col='red')
  abline(v=diff_uci,lty=3,col='red')
  abline(v=diff_lci,lty=3,col='red')

  # Calculate p value
  pval_pos <- length(which(jax$diff >= 0)) / nrow(jax) ; pval_pos # p-value
  pval_ci <- (length(which(jax$diff > diff_uci | jax$diff < diff_lci)) / nrow(jax)) ; pval_ci # p-value
  if(verbose){message('VitaloJack p-value (randomization) = ',pval_pos)}

  rlist <- list(details = list(results=jax$diff,
                               pre = mrpre,
                               pos = mrpos,
                               inputs = data.frame(range_width, pre_d1, pos_d1)),
                               summary = list(n_pre = nrow(mrpre),
                               n_pos = nrow(mrpos),
                               means = data.frame(meanpre,
                                                  cipre,
                                                  meanpos,
                                                  cipos,
                                                  diff_mean = round(diff_mean,3),
                                                  diff_lci = round(diff_lci,3),
                                                  diff_uci = round(diff_uci,3)),
                               p_hyfe = wt$p.value,
                               vitalojak = data.frame(failure_pos = pval_pos,
                                                      failure_ci = pval_ci)))

  return(rlist)
}

################################################################################
################################################################################
# Smoker

hos <- readRDS('data-archetype_smoker.rds')

# Plot
p_smoker <-
  hos$days %>%
  mutate(hourly_rate = cough_rate / 24) %>%
  ggplot(aes(x=date,
           y=hourly_rate)) +
  geom_point() +
  theme_light() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab(NULL) +
  ylab('Coughs per hour') +
  scale_y_continuous(breaks = 0:7,limits=c(0,6.5)) +

  geom_segment(aes(x = hos$days$date[74], xend = hos$days$date[74], y=0, yend=6), lty=2, size=.25) +
  geom_segment(aes(x = hos$days$date[135], xend = hos$days$date[135], y=0, yend=6), lty=2, size=.25) +
  geom_segment(aes(x = hos$days$date[177], xend = hos$days$date[177], y=0, yend=6), lty=2, size=.25) +

  annotate("text", x = hos$days$date[74], y = 6.4, label = "Stopped\nsmoking", size=2.7) +
  annotate("text", x = hos$days$date[135], y = 6.4, label = "Resumed\nsmoking", size=2.7) +
  annotate("text", x = hos$days$date[177], y = 6.4, label = "Stopped\nsmoking", size=2.7) +

  annotate('rect',
           xmin = hos$days$date[39], xmax = hos$days$date[69],
           ymin = 0, ymax = 6, alpha = .2, fill='firebrick') +
  annotate('rect',
           xmin = hos$days$date[79], xmax = hos$days$date[109],
           ymin = 0, ymax = 6, alpha = .2, fill='firebrick') +

  annotate('rect',
           xmin = hos$days$date[111], xmax = hos$days$date[125],
           ymin = 0, ymax = 6, alpha = .2, fill='cadetblue4') +
  annotate('rect',
           xmin = hos$days$date[145], xmax = hos$days$date[159],
           ymin = 0, ymax = 6, alpha = .2, fill='cadetblue4') +

  annotate('rect',
           xmin = hos$days$date[(177 - 10 - 14)], xmax = hos$days$date[(177 - 10)],
           ymin = 0, ymax = 6, alpha = .2, fill='darkorchid4') +
  annotate('rect',
           xmin = hos$days$date[(177 + 10)], xmax = hos$days$date[(177 + 10 + 14)],
           ymin = 0, ymax = 6, alpha = .2, fill='darkorchid4')

p_smoker

# Quit smoking on Jan 18
# Compare month prior to month after, with 5-day buffer on each side of 1/18
vitalojak_simulator(hos,pre_d1 = 39, pos_d1 = 79, range_width = 30)$summary

# Relapsed smoking by March 20
# Compare 2 wk prior to 2wks after, with 10-day buffer on each side of 3/20
results <- vitalojak_simulator(hos,pre_d1 = 111, pos_d1 = 145, range_width = 14)$summary
results
(1 - results$vitalojak$failure_pos)
(1 - results$vitalojak$failure_ci)

# Quit smoking again by first week of May (~ day 177)
# Compare 2 wk prior to 2wks after, with 10-day buffer on each side of 5/1
vitalojak_simulator(hos,pre_d1 = (177 - 10 - 14), pos_d1 = (177 + 10), range_width = 14)$summary


################################################################################
################################################################################
# Chronic cougher

ho <- readRDS('data-archetype_chronic.rds')

# Plot
p_chronic <-
  ho$days %>%
  mutate(hourly_rate = cough_rate / 24) %>%
  ggplot(aes(x=date,
             y=hourly_rate)) +
  geom_point() +
  theme_light() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y",
               limits=(c(ho$days$date[1],ho$days$date[188]))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab(NULL) +
  ylab('Coughs per hour') +
  scale_y_continuous(breaks = seq(0,30,by=3),limits=c(0,30)) +

  geom_segment(aes(x = ho$days$date[20], xend = ho$days$date[20], y=0, yend=28), lty=2, size=.25) +
  geom_segment(aes(x = ho$days$date[51], xend = ho$days$date[51], y=0, yend=28), lty=2, size=.25) +
  geom_segment(aes(x = ho$days$date[120], xend = ho$days$date[120], y=0, yend=28), lty=2, size=.25) +

  annotate("text", x = ho$days$date[20], y = 29, label = "Gabapentin", size=2.7) +
  annotate("text", x = ho$days$date[51], y = 29, label = "Max. dose", size=2.7) +
  annotate("text", x = ho$days$date[120], y = 29, label = "Omeprazole", size=2.7) +

  annotate('rect',
           xmin = ho$days$date[1], xmax = ho$days$date[14],
           ymin = 0, ymax = 28, alpha = .2, fill='firebrick') +
  annotate('rect',
           xmin = ho$days$date[25], xmax = ho$days$date[39],
           ymin = 0, ymax = 28, alpha = .2, fill='firebrick') +

  annotate('rect',
           xmin = ho$days$date[101], xmax = ho$days$date[114],
           ymin = 0, ymax = 28, alpha = .2, fill='cadetblue4') +
  annotate('rect',
           xmin = ho$days$date[125], xmax = ho$days$date[139],
           ymin = 0, ymax = 28, alpha = .2, fill='cadetblue4')

p_chronic

# Gabapentin starts on Feb 17 (day 20)
# Compare 2 wks prior to 2 wks after, with 5-day buffer on each side of 2/17
vitalojak_simulator(ho,pre_d1 = 1, pos_d1 = 25, range_width = 14)$summary

# Omeprazole added - May 28 (day 120)
# Compare 2 wks prior to 2 wks after, with 5-day buffer on each side of 2/17
vitalojak_simulator(ho,pre_d1 = 101, pos_d1 = 125, range_width = 14)$summary


################################################################################
# Compile plot

p_chronic
p_smoker

ggarrange(p_chronic + labs(title='A. Case 1'),
          p_smoker + labs(title='B. Case 2'),
          nrow=2)

