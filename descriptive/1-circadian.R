################################################################################
################################################################################
# Basic exploration of circadian pattern
################################################################################
################################################################################
# Eric Keen, December 2021

library(egg)
library(ggplot2)
library(dplyr)

################################################################################
################################################################################
# Get 24-hr cough metrics for each user

# This analysis uses the dataset `data-circadian.rds`.
# We include the code we used to derive that dataset from the raw data,
# but we do not include the raw data, since it includes data for users who did not provided informed consent.

if('data-circadian.rds' %in% dir()){
  diels <- readRDS('data-circadian.rds')
}else{

  # This is the code used to generate that dataset

  # Objects representing raw datasets:
  # hd = raw Hyfe data for all usres
  # users = list of users
  # valid_uids = list of users who meet inclusion criteria (consent + min. monitoring threshold)

  diels <- hourly_counts <- data.frame()
  for(UIDI in 1:nrow(valid_uids)){

    uidi <- valid_uids$uid[UIDI]
    message('UID ',UIDI,' out of ',nrow(valid_uids),' :: ', uidi,' :: diel rates ...')

    # Format user data (a Hyfe function)
    ho <- format_user_data(hd=hd,
                           id=uidi,
                           id_type='uid',
                           verbose=FALSE)

    # Only keep user hours with > 30 minutes recording
    hours <- ho$hours %>% filter(rec > 0.5)
    nrow(hours)
    head(hours)

    # Add hours list to growing dataframe
    hours$uid <- uidi
    hourly_counts <- rbind(hourly_counts, hours)

    # Get hourly cough rate
    dieli <- hours %>%
      group_by(hr) %>%
      summarize(rec=sum(rec,na.rm=TRUE),
                coughs=sum(coughs,na.rm=TRUE)) %>%
      mutate(rate = coughs / rec,
             uid=uidi)

    # Store result
    dieli
    diels <- rbind(diels,dieli)
  }

  # These results are used to generate the following datasets:
  #saveRDS(hourly_counts, 'data-hours.rds')
  #saveRDS(diels, 'data-circadian.rds')

}

################################################################################
################################################################################
# Plot circadian cough rate

# checkout data
head(diels)

# reduce to one uid entry per hour of day
diels2 <- diels %>%
  group_by(uid, hr) %>%
  summarize(coughs = sum(coughs),
            rec = sum(rec)) %>%
  mutate(rate = coughs / rec)
head(diels2)
#length(unique(diels2$uid))

# Sample size
diels2 %>% filter(rec >=10) %>% summarize(uids=length(unique(uid))) %>% nrow()

# reduce to one row for each hour of day
diels3 <- diels2 %>%
  filter(rec >= 10) %>%
  group_by(hr) %>%
  summarize(n = n(),
            rate_mean = mean(rate[is.finite(rate)]),
            rate_sd = sd(rate[is.finite(rate)])) %>%
  mutate(rate_se = rate_sd / sqrt(n))
diels3

# Get 24-hour mean
diels_mean <- mean(diels3$rate_mean)
diels_mean

# Add zero-centered values
diels2 <- diels2 %>%
  mutate(scaled_rate = rate - diels_mean)

diels4 <- diels3 %>%
  mutate(zeroed_mean = rate_mean - diels_mean,
         scaled_mean = rate_mean / diels_mean)
diels4


# Plot result

p1 <- ggplot() +
  scale_y_continuous(breaks=seq(-2,2,by=.5),limits=c(-2,2)) +
  geom_hline(yintercept=0,lty=2) +
  ylab('Relative cough rate') +
  xlab('Hour of day') +
  geom_point(data=diels4,aes(x=hr,y=zeroed_mean),cex=3,colour = "firebrick", alpha=.8) +
  geom_segment(data=diels4,aes(x=hr,y=(zeroed_mean- rate_se), xend=hr, yend=(zeroed_mean + rate_se)),col='firebrick') +
  theme_light()

p1



