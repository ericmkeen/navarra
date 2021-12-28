# Navarra Clinica -- performance analysis of 49 tests with ~20 solicited coughs each
# This file takes pre-formatted datasets and carries out performance evaluation
# then plots results

# Setup dependencies
library(devtools)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)

# Source performance evaluation function
source('hyfe_performance.R')

################################################################################
# Bring in datasets

labels <- read.csv('labels.csv') # analyst labels
detections <- read.csv('detections.csv') # hyfe detections
offsets <- read.csv('offsets_emk.csv') # offsets for each phone-test

################################################################################
# Analyze performance

results <-
  hyfe_performance(detections,
                   labels,
                   offsets,
                   offset_cutoff = 2,
                   #n3_cutoff = 10,
                   n3_cutoff = 10,
                   #quality_cutoff = .05, # Strict test quality
                   #quality_cutoff = .5, # Lax test quality
                   quality_cutoff = 0, # No test quality
                   #remove_tests = c(2, 17, 38, 48),
                   prediction_threshold = 0.85,
                   toplot = FALSE,
                   verbose=TRUE)

################################################################################
# Review

# Overall performance metrics
results$punchline

# Summary for tests used in performance metric
results$summary
results$summary %>% nrow
results$summary$test

# Summaries for each valid test
results$tests
results$tests %>% nrow
results$tests$events[results$tests$alias == 'navarrac+002@hyfe.ai'] %>% sum # number of labeled cough-seconds in the tests used for performance metrics

results$tests %>% as.data.frame %>% head(15)
results$tests$fpr_3

################################################################################
# Explore details / sample sizes, etc

df <- results$details  # each row is a labeled cough-second on each device
df %>% nrow

# Labels sample sizes
dfl <- df[df$alias == 'navarrac+002@hyfe.ai' & df$label < 4 & df$label > 0,]
nrow(dfl) # number of labeled cough-seconds

head(dfl)

nrow(dfl)

dfl$test %>% table
dfl$test %>% table %>% mean

dfl$label %>% table
dfl$label %>% table / nrow(dfl)

dfl$valid %>% table
nrow(dfl[dfl$valid,]) / nrow(dfl)

dfl %>% filter(valid == TRUE) %>% nrow

################################################################################
# Plots

dfsum <- results$tests # each row is the result for an alias-test
dfsum$phone <- 'Phone 1'
dfsum$phone[dfsum$alias == 'navarrac+003@hyfe.ai'] <- 'Phone 2'

# Summary of each test (horizontal column plot)
p_sens <- ggplot(dfsum,aes(y=factor(test),x=sens_3)) + geom_col() +
  ylab('Test') + xlab('Sensitivity') + facet_wrap(~phone) + theme_light()
p_spec <- ggplot(dfsum,aes(y=factor(test),x=spec_3)) + geom_col() +
  ylab('Test') + xlab('Specificity') + facet_wrap(~phone) + theme_light()
p_tests <- ggarrange(p_sens,p_spec,nrow=2)
p_tests

# Summary for each device (violin plot)
p_viol_sens <-
  ggplot(dfsum,aes(y=sens_3, x=phone)) +
  geom_violin(fill='dodgerblue4', alpha=.5, draw_quantiles = c(0.25,.5,.75)) +
  scale_y_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  ylab('Sensitivity') + xlab(NULL) + theme_light()
p_viol_spec <-
  ggplot(dfsum,aes(y=spec_3, x=phone)) +
  geom_violin(fill='dodgerblue4', alpha=.5, draw_quantiles = c(0.25,.5,.75)) +
  scale_y_continuous(breaks=seq(0,1,by=.1),limits=c(0,1)) +
  ylab('Specificity') + xlab(NULL) + theme_light()
p_overall <- ggarrange(p_viol_sens, p_viol_spec,ncol=2)
p_overall


################################################################################
# Confusion matrix

df <- results$tests %>% filter(alias == 'navarrac+002@hyfe.ai')

df %>% head
names(df)

df %>% select(events, n_3, tp_3, fn_3)

sum(df$events) - sum(df$n_0)
sum(df$n_3)

matrix(data=c(sum(df$tp_3),
              sum(df$fn_3),
              sum(df$fp_3),
              (sum(df$n_0) - sum(df$fp_3))),
              #round(((sum(df$duration) - sum(df$n_3)) - sum(df$fp_3)))),
       nrow = 2, ncol = 2,
       byrow = FALSE,
       dimnames = list(c('Cough-seconds detected',
                         'Non-cough-seconds detected'),
                       c('Cough-seconds labeled',
                         'Non-cough-seconds labeled')))

