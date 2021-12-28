################################################################################
################################################################################
# Navarra - Minimum monitoring times
################################################################################
################################################################################
# Eric Keen, December 2021

library(dplyr)
library(ggplot2)
library(gridExtra)

################################################################################
################################################################################

# Read in data
ERROR <- readRDS('data-proportional_error.rds')
raw_errors <- readRDS('data-raw_errors.rds')

# This analysis uses the datasets produced using the looped code below.
# We include the code we used to derive that dataset from the raw data,
# but we do not include the raw data, since it includes data for users who did not provided informed consent.

# How those datasets were produced (do not run):
if(FALSE){

  # Objects representing raw datasets:
  # hd = raw Hyfe data for all usres
  # valid_uids = list of users who meet inclusion criteria (consent + min. monitoring threshold)

  # Source the function used to caculate cough error
  source('function-plot_user_cough_error.R')

  # Subset to coughs only
  nrow(hd$peaks)
  hd$peaks <- hd$peaks %>% dplyr::filter(prediction_score >= 0.7)
  nrow(hd$peaks)

  # Subset to users with 10 days of monitoring or more
  valid_uids <- valid_uids[valid_uids$hours_tot >= 240,]
  nrow(valid_uids)

  # Subset to users with cough rate of at least 0.5 coughs per hour
  # Extremely low cough rates inflate error rates and tend not to be the users
  # that are targeted in clinical trials.
  valid_uids$rate <- valid_uids$coughs / valid_uids$hours_tot
  valid_uids
  valid_uids <- valid_uids[which(valid_uids$rate > 0.5),]
  nrow(valid_uids)
  range(valid_uids$hours_tot)
  range(valid_uids$rate)
  valid_uids$uid

  # Get info for these users
  head(valid_uids)
  id_key <- get_hyfe_data(id='Navarra',id_type='cohort_id', get_peaks=FALSE)$id_key
  head(id_key)
  ids <- id_key %>% filter(uid %in% valid_uids$uid)
  ids
  #data.frame(hyfe_id = ids$hyfe_id)

  # THE FORTHCOMING LOOP IS SLOW -- takes approx 3 days to run

  # Calculate error ~ monitoring curves for each user
  valid_uids
  ERROR <- raw_errors <- data.frame()
  for(UIDI in 1:nrow(valid_uids)){
    #UIDI = 1
    uidi <- valid_uids$uid[UIDI]
    message(UIDI,':: ',uidi)
    #uidi <- 'yVo9vX5fipZ6ZpxrNWifIfyAWn22'

    # Format user data
    ho <- format_user_data(hd=hd,
                           id=uidi,
                           id_type='uid',
                           verbose=FALSE)

    # Calculate cough rate error as a function of monitoring time
    errors <- plot_user_cough_error(ho,iterations=100,verbose=TRUE)

    # Store result
    raw_errors <- rbind(raw_errors, errors$raw)
    ERROR <- rbind(ERROR,errors$summary)

    saveRDS(raw_errors,file='data-raw_errors.rds')
    saveRDS(ERROR,file='data-proportional_error.rds')
  }
}


# Summarize and plot result ==========================================

# Rename to simplify
mr <- raw_errors
mrs <- ERROR

length(unique(mr$uid))

# Add error fields to raw results
mr <- mr %>%
  mutate(days = hours / 24,
         error = estimate - actual_rate) %>%
  mutate(abs_error = abs(error)) %>%
  mutate(prop_error = ((estimate + abs_error)/estimate) - 1)

# Format summary results
mrs <- mrs %>%
  mutate(error24 = mean_error*24,
         days= hours / 24)

# Create summary dataframe
error_summ <- mrs %>%
  group_by(days) %>%
  summarize(#mean = mean(error24,na.rm=TRUE),
            prop = mean(mean_prop_error,na.rm=TRUE),
            q5 = quantile(mean_prop_error,0.05),
            q95 = quantile(mean_prop_error,0.95))

error_summ

# Plot it
ggplot() +
  #geom_jitter(data=mr, aes(x=days, y=prop_error),alpha=.02,height=0,width=.1,color='grey30') +
  geom_line(data=mrs, aes(x=days, y=mean_prop_error, group = uid),alpha=.3,color='cadetblue4') +
  geom_line(data=error_summ, aes(x=days, y=prop),alpha=.5,lwd=1.5,color='firebrick') +
  scale_y_continuous(breaks=c(0,.1,.25,.5,.75,1,1.25),limits=c(0,1.25)) +
  scale_x_continuous(breaks=c(0.5,1:10)) +
  ylab('Proportional error') +
  xlab('Days of monitoring') +
  theme_light()


length(which(mr$prop_error > 1.50)) / nrow(mr)

