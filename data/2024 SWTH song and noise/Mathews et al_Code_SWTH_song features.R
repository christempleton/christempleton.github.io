#### NOTES ####

## Sometimes the below code involves frequency 5 and 95% not just PFC.Max and Min. For the manuscript only the PFC variables were used. 


## Final results table for experiment analysis was created by replacing the estimates in the original table with the emmeans values from the EMM code. Standard error was also replaced with the EMM output values. 


## As of 5/8/26 I am using the original models code that does not include TOD or exemplar track. I also am switching to frequency 5% and 95% for my min and max frequency variables instead of PFC ones. This will extend to calculating bandwidth from the frequency 95% - frequency 5%. 

#### SETUP ####

## PACKAGES
install.packages("stringr")
install.packages("gridExtra")
install.packages("writexl")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("car")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("broom")
install.packages("janitor")
install.packages("purrr")
install.packages("lme4")
install.packages("viridis")
install.packages("MASS")
install.packages("lmerTest")
install.packages("broom.mixed")
install.packages("openxlsx")
install.packages("ggeffects")
install.packages("insight") 
install.packages("gt")
install.packages("emmeans")
install_latest("tidyr")

library(emmeans) # Use for EMMs
library(gt) # Use for creating custom tables
library(insight) # Needed for the ggeffects package.
library(ggeffects)  # To calculate predicted values for plotting results
library(openxlsx) #For write.xlsx function
library(broom.mixed) # For extracting model summaries and for creating tables
library(lmerTest) #obtain p-values for LMMs
library(MASS) # rlm() function. Reduces the influence of outliers
library(viridis) # color blind friendly palette
library(lme4) # LMMs
library(broom) # RM-ANOVA function
library(janitor) # RM-ANOVA function
library(purrr) # RM-ANOVA function
library(tidyverse) # data maniuplation
library(ggpubr) # publication ready figures, and shapiro_test()
library(rstatix) # pipe friendly R function for stats, get_anova_table() and anova_test()
library(car) #mauchly.test, and powerTransf
library(lmtest)
library(ggplot2)
library(writexl) #for creating excel files
library(gridExtra) #for arranging ggplots
library(dplyr) #for piping
library(stringr) #for str_trim()
library(tidyr) #for drop_na

## SET WORKING DIRECTORY TO WHERE YOU STORED THE DATA

## READ IN DATA

birds=read.csv("NoiseVocal4.csv", header=T) # READ IN MASTER DATA
birds=birds[1:2615,1:43] # make sure no extra columns or rows

## DATA WRANGLING 
str(birds)
birds$Species=as.factor(birds$Species)
birds$Vocal_type=as.factor(birds$Vocal_type)
birds$Trial_type=as.factor(birds$Trial_type)
birds$Exemplar.track=as.factor(birds$Exemplar.track)
birds$Time.of.day=as.numeric(birds$Time.of.day)
birds$SWTHsong_type=as.factor(birds$SWTHsong_type)
birds$BirdID=as.factor(birds$BirdID)
birds$Begin.File=as.factor(birds$Begin.File)
birds$IndividualID=as.factor(birds$IndividualID)
levels(SWTH_experiment_full$Trial_type)


## Trial_type has 8 levels for some reason. Need to combine the two durings, posts, pres, and surveys

#Merge the different levels into 4
birds <- birds %>%
  mutate(Trial_type = str_trim(Trial_type),   # Deleting any whitespace around words
         Trial_type = case_when(
           Trial_type == "pre" ~ "Pre",       # Convert 'pre' to 'Pre'
           Trial_type == "post" ~ "Post",     # Convert 'post' to 'Post'
           Trial_type == "survey" ~ "Survey", # Convert 'survey' to 'Survey'
           Trial_type == "during" ~ "During",
           TRUE ~ Trial_type                  # Keep other values as they are
         ))

## Vocal_type has 5 levels for some reason, one of them is blank. Also need to merge song and Song 

birds$Vocal_type <- droplevels(birds$Vocal_type) # removes unused factor levels

birds <- birds %>%
  mutate(Vocal_type = str_trim(Vocal_type), # Deleting any whitespace around words
         Vocal_type = case_when(
           Vocal_type == "song" ~ "Song",     
           TRUE ~ Vocal_type                  # Keep other values as they are
         ))

## Species has 4 levels for some reason, one of them is blank. 

birds$Species <- droplevels(birds$Species) # removes unused factor levels


# CONVERT back to factor
birds$Trial_type=as.factor(birds$Trial_type)
birds$Species=as.factor(birds$Species)
birds$Vocal_type=as.factor(birds$Vocal_type)

#### SUBSETTING DATA SETS ####

## Make the many subsets needed

SWTH <- birds %>% filter(Species == "SWTH") # subset for SWTH
SWTH_survey <- SWTH %>% filter(Trial_type %in% c("Pre", "Survey")) # subset for SWTH surveys
SWTH_survey_song <- SWTH_survey %>% filter(Vocal_type == "Song") # subset for SWTH surveys songs
SWTH_survey_full <- SWTH_survey_song %>% filter(SWTHsong_type == "Full") # subset for SWTH surveys full songs
SWTH_survey_intro <- SWTH_survey_song %>% filter(SWTHsong_type == "Intro") # subset for SWTH surveys intro notes
SWTH_experiment <- SWTH %>% filter(Trial_type %in% c("Pre", "During","Post")) # subset for SWTH experiment
SWTH_experiment_song <- SWTH_experiment %>% filter(Vocal_type == "Song") # subset for SWTH experiment songs
SWTH_experiment_full <- SWTH_experiment_song %>% filter(SWTHsong_type == "Full") # subset for SWTH experiment full songs
SWTH_experiment_intro <- SWTH_experiment_song %>% filter(SWTHsong_type == "Intro") # subset for SWTH experiment intro notes
SWTH_experiment_intro_pre <-SWTH_experiment_intro %>% filter(Trial_type == "Pre") # subset for SWTH pre experiment intro song
SWTH_experiment_intro_during <-SWTH_experiment_intro %>% filter(Trial_type == "During") # subset for SWTH during experiment intro song
SWTH_experiment_intro_post <-SWTH_experiment_intro %>% filter(Trial_type == "Post") # subset for SWTH post experiment intro song
SWTH_experiment_full_pre <-SWTH_experiment_full %>% filter(Trial_type == "Pre") # subset for SWTH pre experiment full song
SWTH_experiment_full_during <-SWTH_experiment_full %>% filter(Trial_type == "During") # subset for SWTH during experiment full song
SWTH_experiment_full_post <-SWTH_experiment_full %>% filter(Trial_type == "Post") # subset for SWTH post experiment full song

#### RECREATE EXPERIMENT DATASETS, BUT CLEANED ####

## Combine the three phases of an experiment back together for each species and vocal type

# 1= pre, 2= during, 3=post

SWTH_experiment_intro= bind_rows(SWTH_experiment_intro_pre, SWTH_experiment_intro_during, SWTH_experiment_intro_post, .id="source")
SWTH_experiment_intro$source=as.factor(SWTH_experiment_intro$source)

SWTH_experiment_full= bind_rows(SWTH_experiment_full_pre, SWTH_experiment_full_during, SWTH_experiment_full_post, .id="source")
SWTH_experiment_full$source=as.factor(SWTH_experiment_full$source)


## Dropping unused levels for each data frame (those dataframes I am continuing with)
SWTH_experiment_full$Begin.File <- droplevels(SWTH_experiment_full$Begin.File)
SWTH_experiment_intro$Begin.File <- droplevels(SWTH_experiment_intro$Begin.File)

## Dropping unused levels from trial_type
# Droplevels for trial_type
SWTH_experiment_full$Trial_type <- droplevels(SWTH_experiment_full$Trial_type)
SWTH_experiment_intro$Trial_type <- droplevels(SWTH_experiment_intro$Trial_type)

## Reorganize the trial_type levels to be pre, during, post

# Specify the new level order as a vector
new_level_order <- c("Pre", "During", "Post")  

# Reorder levels for Trial_type in each dataset
SWTH_experiment_full$Trial_type <- factor(SWTH_experiment_full$Trial_type, levels = new_level_order)
SWTH_experiment_intro$Trial_type <- factor(SWTH_experiment_intro$Trial_type, levels = new_level_order)


#### CHANGE DURING TO NOISE IN EXPERIMENT SWTH DATASETS (5/2) ####

## Change for full songs

SWTH_experiment_full <- SWTH_experiment_full %>%
  mutate(Trial_type = recode_factor(Trial_type, "During" = "Noise"))

## Reorder for full songs

SWTH_experiment_full$Trial_type <- factor(SWTH_experiment_full$Trial_type,
                                          levels = c("Pre", "Noise", "Post"))

## Change for intro notes

SWTH_experiment_intro <- SWTH_experiment_intro %>%
  mutate(Trial_type = recode_factor(Trial_type, "During" = "Noise"))

## Reorder for full songs

SWTH_experiment_intro$Trial_type <- factor(SWTH_experiment_intro$Trial_type,
                                           levels = c("Pre", "Noise", "Post"))


#### CREATE NA FREE DATASETS (9/13)####

### After having added exemplar track and TOD to our data I had to introduce NAs. The below code truncates our data to avoid files with NAs. This code should NOT BE RUN if you want to use all the datapoints. Only needed for the survey data.

SWTH_survey_full=  SWTH_survey_full %>%
  drop_na(Time.of.day)

SWTH_survey_intro=  SWTH_survey_intro %>%
  drop_na(Time.of.day)

#### FORMAT EXEMPLAR TRACK IN EXPERIMENT DATASETS (9/16) ####

## Exemplar.track has 6 levels for some reason, one of them is blank. Also need to merge song and Song 

# Remove from full song dataset
SWTH_experiment_full$Exemplar.track <- droplevels(SWTH_experiment_full$Exemplar.track) # removes unused factor levels
levels(SWTH_experiment_full$Exemplar.track)

# Remove from intro note dataset
SWTH_experiment_intro$Exemplar.track <- droplevels(SWTH_experiment_intro$Exemplar.track) # removes unused factor levels
levels(SWTH_experiment_intro$Exemplar.track)

#### CALCULATE THE AVERAGE, MIN, AND MAX SAMPLE SIZE FOR SURVEYS (5/30) ####

# Count the number of datapoints per IndividualID
individual_counts <- SWTH_survey_full %>%
  count(IndividualID, name = "datapoints")

# Calculate summary statistics
summary_stats <- individual_counts %>%
  summarise(
    average_datapoints = mean(datapoints),
    min_datapoints = min(datapoints),
    max_datapoints = max(datapoints)
  )

# Print the summary statistics
print(summary_stats)



#### CREATE TABLE FOR SAMPLE SIZE PER SPECIES, TRIAL TYPE, VOCAL TYPE, CALL TYPE, AND SWTH SONG TYPE ####

## Usually you will need to sort out the datasets that you do not care about. 

# Get all objects in the environment
dataframelist <- ls()

# Filter only data frames
column1 <- dataframelist[sapply(dataframelist, function(x) is.data.frame(get(x)))] 

# Function to get the number of rows in each data frame
column2 <- sapply(column1, function(x) nrow(get(x)))

# Create a data frame with the two columns
table1 <- data.frame(
  DataFrame = column1,
  NumObservations = column2
)

# Print the table
print(table1)

# Export table1 to an Excel file
write_xlsx(table1, "raw_datasets.xlsx")



#### MULTIPLE COMPARISIONS CORRECTION FUNCTION (added 5/8/26)####

# This function performs multiple comparison correction on p-values of multiple
# I found it online at https://stats.stackexchange.com/questions/554529/correcting-for-multiple-linear-mixed-models
# linear mixed models (lmms) created with lmerTest(). The inputs are: 
#     * lmms (list)          : a list of all the lmms to be corrected
#     * method (string)      : which correction method should be applied, see 
#                              ?p.adjust. Default is "fdr"
#     * ignore (list)        : list of strings with the names of regressors of 
#                              no interest for which no correction should be 
#                              performed. Default is empty list.
#     * ignore_pattern (list): list of strings patterns where regressors which 
#                              contain these should not be corrected for. 
#                              Default is empty list.
#     * sep (logical)        : logical determining if multiple comparison 
#                              correction should be confirmed per predictor or 
#                              for all predictors together. Default is FALSE.
#
# The function has one output: 
#     * lmms.cor (list)      : a list containing all settings and a data frame
#                              df.cor with the columns outcome, predictor, 
#                              pvalues, p.sig, pvalues.adjusted and pad.sig
#
# (c) 10maxgold@gmail.com
#

p.adjust_lmm = function(lmms,method="fdr",ignore=list(),sep=T,ignore_pattern=list()) {
  
  library(stringr) # str_sub
  
  namedList <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)),deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
  }
  
  lmms.cor = namedList(method,ignore,sep)
  
  df.cor = data.frame(outcome=c(),predictor=c(),pvalue=c())
  
  for (i in 1:length(lmms)) {
    
    # Extract model summary coefficient table, dropping the intercept (row 1)
    sumtab <- coef(summary(lmms[[i]]))
    
    # Safety check: if only intercept exists, skip this model
    if (nrow(sumtab) < 2) {
      warning(paste("Model", i, "has no predictors beyond the intercept. Skipping."))
      next
    }
    
    sumtab <- sumtab[-1, , drop = FALSE]  # drop intercept row
    
    outcome   <- as.character(formula(lmms[[i]])[2])  # cleaner formula parsing
    predictor <- rownames(sumtab)
    pvalue    <- unname(sumtab[, 5])                   # column 5 = p-value in lmerTest
    
    dat   <- data.frame(outcome, predictor, pvalue)
    df.cor <- rbind(df.cor, dat)
  }
  
  if (length(ignore_pattern) != 0) {
    for (x in ignore_pattern) {
      df.cor = df.cor[!grepl(x, df.cor$predictor),]
    }
  }
  
  if (length(ignore) != 0) {
    for (x in ignore) {
      df.cor = df.cor[df.cor$predictor != x,]
    }
  }
  
  df.cor$p.sig = " "
  df.cor$p.sig[df.cor$pvalue < 0.1]   = "."
  df.cor$p.sig[df.cor$pvalue < 0.05]  = "*"
  df.cor$p.sig[df.cor$pvalue < 0.01]  = "**"
  df.cor$p.sig[df.cor$pvalue < 0.001] = "***"
  
  if (sep) {
    df.cor$pvalue.adjusted = NaN
    preds = unique(df.cor$predictor)
    for (p in preds) {
      df.cor$pvalue.adjusted[df.cor$predictor == p] = p.adjust(df.cor$pvalue[df.cor$predictor == p],method=method)
    }
  } else {
    df.cor$pvalue.adjusted = p.adjust(df.cor$pvalue,method=method)
  }
  
  df.cor$p.ad.sig = " "
  df.cor$p.ad.sig[df.cor$pvalue.adjusted < 0.1]   = "."
  df.cor$p.ad.sig[df.cor$pvalue.adjusted < 0.05]  = "*"
  df.cor$p.ad.sig[df.cor$pvalue.adjusted < 0.01]  = "**"
  df.cor$p.ad.sig[df.cor$pvalue.adjusted < 0.001] = "***"
  
  df.cor = df.cor[order(df.cor$predictor),]
  
  lmms.cor$df.cor = df.cor
  
  return(lmms.cor)
  
}

#### SURVEY DATA ANALYSIS, LMMS ASSUMPTIONS ####

## automate assumption checking by creating plots and tables with test results

# Set working directory to where you want the figures to go
setwd()

# Function
check_assumptions <- function(data, dataset_name, response_vars, combined_results) {
  # Create main output directory if it doesn't exist
  dir.create("Raw_Survey_LMM_Assumptions5", showWarnings = FALSE)
  
  # Create a subdirectory for the current dataset
  dataset_dir <- file.path("Raw_Survey_LMM_Assumptions5", dataset_name)
  dir.create(dataset_dir, showWarnings = FALSE)
  
  # Loop over each response variable
  for (response in response_vars) {
    # Define the model formula for the survey models
    formula <- as.formula(paste(response, "~ LAeq..dB..AVG + (1 | IndividualID)"))
    
    # Fit the model
    model <- lmer(formula, data = data)
    
    # Extract residuals and fitted values
    residuals <- resid(model)
    fitted_values <- fitted(model)
    
    # Perform Levene's Test for Homogeneity of Variance
    levene <- leveneTest(residuals ~ factor(data$IndividualID)) # assuming you want to check within individual variation
    levene_p <- levene$`Pr(>F)`[1]
    
    # Perform Shapiro-Wilk Test for Normality of Residuals
    shapiro <- shapiro.test(residuals)
    shapiro_p <- shapiro$p.value
    
    # Extract random effects
    random_effects <- ranef(model)$IndividualID[[1]]
    
    # Store results in the combined results data frame
    combined_results <- rbind(combined_results, data.frame(
      Dataset = dataset_name,
      Response_Variable = response,
      Levene_p_value = levene_p,
      Shapiro_Wilk_p_value = shapiro_p
    ))
    
    # Create plots for the current response variable
    plot_path <- file.path(dataset_dir, paste0(response, "_assumptions_plot.png"))
    png(plot_path, width = 1200, height = 400)
    par(mfrow = c(1, 4)) # Four plots side by side
    
    # Residuals vs Fitted Plot
    plot(fitted_values, residuals, main = "Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    # QQ Plot for Normality of Residuals
    qqnorm(residuals, main = "Q-Q Plot of Residuals")
    qqline(residuals, col = "red")
    
    # Q-Q Plot for Normality of Random Effects
    qqnorm(random_effects, main = "Q-Q Plot of Random Effects")
    qqline(random_effects, col = "red")
    
    # Autocorrelation Plot for Independence of Errors
    acf(residuals, main = "ACF of Residuals", lag.max = 20)
    
    dev.off() # Save the plot
  }
  
  return(combined_results)
}

# List of datasets
datasets <- list(
  SWTH_survey_full = SWTH_survey_full,
  SWTH_survey_intro = SWTH_survey_intro
)

# Response variables
response_vars <- c("Dur.90...s.", "Bandwidth", "PFC.Max.Freq..Hz.", "PFC.Min.Freq..Hz.", "Peak.Freq..Hz.")

# Initialize a data frame to store combined results
combined_results <- data.frame(Dataset = character(),
                               Response_Variable = character(),
                               Levene_p_value = numeric(),
                               Shapiro_Wilk_p_value = numeric(),
                               stringsAsFactors = FALSE)

# Run the function for each dataset
for (dataset_name in names(datasets)) {
  combined_results <- check_assumptions(datasets[[dataset_name]], dataset_name, response_vars, combined_results)
}

# Export the combined results table to an Excel file
write_xlsx(combined_results, path = file.path("Raw_Survey_LMM_Assumptions5", "Combined_Assumptions_Test_Results.xlsx"))


#### SURVEY DATA ANALYSIS, LMMS BY HAND , INCLUDES TOD AND TRACK####


## SWTH_survey_full

# Duration 90%
lmm_SWTH_survey_full_Dur.90 = lmer(Dur.90...s. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Dur.90)

# Bandwidth
lmm_SWTH_survey_full_BW = lmer(Bandwidth ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_BW)

# PFC Max Frequency
lmm_SWTH_survey_full_PFC.Max.Freq = lmer(PFC.Max.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_PFC.Max.Freq)

# PFC Min Frequency
lmm_SWTH_survey_full_PFC.Min.Freq = lmer(PFC.Min.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_PFC.Min.Freq)

# Peak Frequency
lmm_SWTH_survey_full_Peak.Freq = lmer(Peak.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Peak.Freq)


## SWTH_survey_intro

# Duration 90%
lmm_SWTH_survey_intro_Dur.90 = lmer(Dur.90...s. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Dur.90)

# Bandwidth
lmm_SWTH_survey_intro_BW = lmer(Bandwidth ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_BW)

# PFC Max Frequency
lmm_SWTH_survey_intro_PFC.Max.Freq = lmer(PFC.Max.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_PFC.Max.Freq)

# PFC Min Frequency
lmm_SWTH_survey_intro_PFC.Min.Freq = lmer(PFC.Min.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_PFC.Min.Freq)

# Peak Frequency
lmm_SWTH_survey_intro_Peak.Freq = lmer(Peak.Freq..Hz. ~ LAeq..dB..AVG + Time.of.day + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Peak.Freq)



#### ORIGINAL SURVEY DATA ANALYSIS, LMMS BY HAND (Using for 5/8/26) ####


## SWTH_survey_full

# Frequency 5%
lmm_SWTH_survey_full_Freq.5 = lmer(Freq.5...Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Freq.5)

# Frequency 95%
lmm_SWTH_survey_full_Freq.95 = lmer(Freq.95...Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Freq.95)

# Duration 90%
lmm_SWTH_survey_full_Dur.90 = lmer(Dur.90...s. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Dur.90)

# Bandwidth
lmm_SWTH_survey_full_BW = lmer(Bandwidth ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_BW)

# PFC Max Frequency
lmm_SWTH_survey_full_PFC.Max.Freq = lmer(PFC.Max.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_PFC.Max.Freq)

# PFC Min Frequency
lmm_SWTH_survey_full_PFC.Min.Freq = lmer(PFC.Min.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_PFC.Min.Freq)

# Peak Frequency
lmm_SWTH_survey_full_Peak.Freq = lmer(Peak.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_full)
summary(lmm_SWTH_survey_full_Peak.Freq)


## SWTH_survey_intro

# Frequency 5%
lmm_SWTH_survey_intro_Freq.5 = lmer(Freq.5...Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Freq.5)

# Frequency 95%
lmm_SWTH_survey_intro_Freq.95 = lmer(Freq.95...Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Freq.95)

# Duration 90%
lmm_SWTH_survey_intro_Dur.90 = lmer(Dur.90...s. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Dur.90)

# Bandwidth
lmm_SWTH_survey_intro_BW = lmer(Bandwidth ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_BW)

# PFC Max Frequency
lmm_SWTH_survey_intro_PFC.Max.Freq = lmer(PFC.Max.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_PFC.Max.Freq)

# PFC Min Frequency
lmm_SWTH_survey_intro_PFC.Min.Freq = lmer(PFC.Min.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_PFC.Min.Freq)

# Peak Frequency
lmm_SWTH_survey_intro_Peak.Freq = lmer(Peak.Freq..Hz. ~ LAeq..dB..AVG + (1|IndividualID), data=SWTH_survey_intro)
summary(lmm_SWTH_survey_intro_Peak.Freq)


#### CALCULATE ADJUSTED p-values FOR SURVEY LMMS (added 5/8/26)####

# Using the p.adjust_lmm function that was created earlier in the code. 

## Create model list
SWTH_survey_full_models_list <- list(
  lmm_SWTH_survey_full_BW = lmm_SWTH_survey_full_BW, 
  lmm_SWTH_survey_full_Dur.90 = lmm_SWTH_survey_full_Dur.90, 
  lmm_SWTH_survey_full_Freq.5 = lmm_SWTH_survey_full_Freq.5, 
  lmm_SWTH_survey_full_Freq.95 = lmm_SWTH_survey_full_Freq.95, 
  lmm_SWTH_survey_full_Peak.Freq = lmm_SWTH_survey_full_Peak.Freq,
)


SWTH_survey_intro_models_list <- list( 
lmm_SWTH_survey_intro_BW = lmm_SWTH_survey_intro_BW, 
lmm_SWTH_survey_intro_Dur.90 = lmm_SWTH_survey_intro_Dur.90, 
lmm_SWTH_survey_intro_Freq.5 = lmm_SWTH_survey_intro_Freq.5, 
lmm_SWTH_survey_intro_Freq.95 = lmm_SWTH_survey_intro_Freq.95, 
lmm_SWTH_survey_intro_Peak.Freq = lmm_SWTH_survey_intro_Peak.Freq
)


## Calculate adjusted p-values with Benjamini-Hochberg (1995; fdr) correction
results <- p.adjust_lmm(
  #lmms           = SWTH_survey_full_models_list,
  lmms           = SWTH_survey_intro_models_list,
  method         = "fdr",     # Benjamini-Hochberg; "fdr" and "BH" are equivalent in p.adjust()
  ignore         = list("(Intercept)"),  # explicitly ignore intercept 
  sep            = TRUE,      # adjust per-predictor across outcomes
  ignore_pattern = list()
)

# Define the desired outcome order from your model list
outcome_order <- c(
  "Freq.5...Hz.", 
  "Freq.95...Hz.", 
  "Bandwidth", 
  "Peak.Freq..Hz.", 
  "Dur.90...s."
)

# Reorder the results table to match
results$df.cor <- results$df.cor[order(match(results$df.cor$outcome, outcome_order)), ]

# View the results table
results$df.cor

# Save results table
write_xlsx(results$df.cor, "survey_full_table.xlsx")
write_xlsx(results$df.cor, "survey_intro_table.xlsx")




## Calculate adjusted p-values with p.adjust()
# Extract p-values from each model manually as a double check
# coef(summary(model)) gives the coefficient table; 
# [2, 5] grabs row 2 (your predictor, skipping intercept), column 5 (p-value)

pvalues <- c(
  coef(summary(lmm_SWTH_survey_full_BW))[2, 5],
  coef(summary(lmm_SWTH_survey_full_Dur.90))[2, 5],
  coef(summary(lmm_SWTH_survey_full_Freq.5))[2, 5],
  coef(summary(lmm_SWTH_survey_full_Freq.95))[2, 5],
  coef(summary(lmm_SWTH_survey_full_Peak.Freq))[2, 5]
)

# Give them names so the output is readable
names(pvalues) <- c("BW", "Dur.90", "Freq.5", "Freq.95", "Peak.Freq")

# Apply BH correction to the whole vector at once
p.adjust(pvalues, method = "BH")



#### OLD SURVEY RESULTS TABLE CREATION ####


# Set working directory to where you want the figures to go
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/WWU/Dr. Templeton's Lab/PostGrad_research/NoiseVocal_experiment/Outputs")

# Function to extract and save results

extract_lmm_results <- function(models_list, model_names, output_file) {
  result_list <- list()
  
  for (model_name in model_names) {
    model <- models_list[[model_name]]
    
    # Extract random effects variance
    rand_effects <- VarCorr(model)
    random_effect_sd <- sqrt(as.numeric(rand_effects$IndividualID[1]))
    
    # Extract fixed effects
    fixed_effects <- summary(model)$coefficients
    
    # Create base model results
    model_results <- data.frame(
      Model = model_name,
      Response_Variable = as.character(formula(model)[[2]]),
      Random_effect_Std.Dev = random_effect_sd,
      Fixed_Intercept_Estimate = fixed_effects["(Intercept)", "Estimate"],
      Fixed_Intercept_Std.Error = fixed_effects["(Intercept)", "Std. Error"],
      Fixed_Intercept_t.value = fixed_effects["(Intercept)", "t value"],
      Fixed_Intercept_P.Value = fixed_effects["(Intercept)", "Pr(>|t|)"],
      stringsAsFactors = FALSE
    )
    
    # Check if Trial_type is present and add its values
    if ("LAeq..dB..AVG" %in% rownames(fixed_effects)) {
      model_results$LAeq..dB..AVG_Estimate <- fixed_effects["LAeq..dB..AVG", "Estimate"]
      model_results$LAeq..dB..AVG_Std.Error <- fixed_effects["LAeq..dB..AVG", "Std. Error"]
      model_results$LAeq..dB..AVG_t.value <- fixed_effects["LAeq..dB..AVG", "t value"]
      model_results$LAeq..dB..AVG_P.Value <- fixed_effects["LAeq..dB..AVG", "Pr(>|t|)"]
    }
    # Calculate R-squared values using performance package
    if (requireNamespace("performance", quietly = TRUE)) {
      r2_values <- performance::r2(model)
      model_results$R2_conditional <- r2_values$R2_conditional
      model_results$R2_marginal <- r2_values$R2_marginal
    }
    
    # Append to result list
    result_list[[model_name]] <- model_results
  }
  
  # Combine all results
  all_results <- do.call(rbind, result_list)
  rownames(all_results) <- NULL
  
  # Write to Excel file
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(all_results, output_file, rowNames = FALSE)
  } else {
    write.csv(all_results, gsub("\\.xlsx$", ".csv", output_file), row.names = FALSE)
  }
  
  return(all_results)
}

# Assuming you have a list of models, e.g., `models_list`
models_list <- list(
  lmm_SWTH_survey_full_BW = lmm_SWTH_survey_full_BW, 
  lmm_SWTH_survey_full_Dur.90 = lmm_SWTH_survey_full_Dur.90, 
  lmm_SWTH_survey_full_PFC.Max.Freq = lmm_SWTH_survey_full_PFC.Max.Freq, 
  lmm_SWTH_survey_full_PFC.Min.Freq = lmm_SWTH_survey_full_PFC.Min.Freq, 
  lmm_SWTH_survey_full_Freq.5 = lmm_SWTH_survey_full_Freq.5,
  lmm_SWTH_survey_full_Freq.95 = lmm_SWTH_survey_full_Freq.95,
  lmm_SWTH_survey_full_Peak.Freq = lmm_SWTH_survey_full_Peak.Freq, 
  lmm_SWTH_survey_intro_BW = lmm_SWTH_survey_intro_BW, 
  lmm_SWTH_survey_intro_Dur.90 = lmm_SWTH_survey_intro_Dur.90, 
  lmm_SWTH_survey_intro_PFC.Max.Freq = lmm_SWTH_survey_intro_PFC.Max.Freq, 
  lmm_SWTH_survey_intro_PFC.Min.Freq = lmm_SWTH_survey_intro_PFC.Min.Freq, 
  lmm_SWTH_survey_intro_Freq.5 = lmm_SWTH_survey_intro_Freq.5,
  lmm_SWTH_survey_intro_Freq.95 = lmm_SWTH_survey_intro_Freq.95,
  lmm_SWTH_survey_intro_Peak.Freq = lmm_SWTH_survey_intro_Peak.Freq
)

# Replace with your actual model names
model_names <- names(models_list)

# Create output file name
output_file <- "Raw_Survey_LMM_Full_Results_RoyalSocRevision1.0.xlsx"

# Extract and save results
extract_lmm_results(models_list, model_names, output_file)






#### EXPERIMENT DATA ANALYSIS, LMMS ASSUMPTIONS ####

## automate assumption checking by creating plots and tables with test results

# Set working directory to where you want the figures to go
setwd()

# Define the function
check_assumptions <- function(data, dataset_name, response_vars, combined_results) {
  # Create main output directory if it doesn't exist
  dir.create("Raw_Experiment_LMM_Assumptions9", showWarnings = FALSE)
  
  # Create a subdirectory for the current dataset
  dataset_dir <- file.path("Raw_Experiment_LMM_Assumptions9", dataset_name)
  dir.create(dataset_dir, showWarnings = FALSE)
  
  # Loop over each response variable
  for (response in response_vars) {
    # Define the model formula
    formula <- as.formula(paste(response, "~ Trial_type + (1 | IndividualID)"))
    
    # Fit the model
    model <- lmer(formula, data = data)
    
    # Extract residuals and fitted values
    residuals <- resid(model)
    fitted_values <- fitted(model)
    
    # Perform Levene's Test for Homogeneity of Variance
    levene <- leveneTest(residuals ~ factor(data$IndividualID))
    levene_p <- levene$`Pr(>F)`[1]
    
    # Perform Shapiro-Wilk Test for Normality of Residuals
    shapiro <- shapiro.test(residuals)
    shapiro_p <- shapiro$p.value
    
    # Extract random effects
    random_effects <- ranef(model)$IndividualID[[1]]
    
    # Store results in the combined results data frame
    combined_results <- rbind(combined_results, data.frame(
      Dataset = dataset_name,
      Response_Variable = response,
      Levene_p_value = levene_p,
      Shapiro_Wilk_p_value = shapiro_p
    ))
    
    # Create plots for the current response variable
    plot_path <- file.path(dataset_dir, paste0(response, "_assumptions_plot.png"))
    png(plot_path, width = 1200, height = 400)
    par(mfrow = c(1, 4)) # Four plots side by side
    
    # Residuals vs Fitted Plot
    plot(fitted_values, residuals, main = "Residuals vs Fitted",
         xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    # QQ Plot for Normality of Residuals
    qqnorm(residuals, main = "Q-Q Plot of Residuals")
    qqline(residuals, col = "red")
    
    # Q-Q Plot for Normality of Random Effects
    qqnorm(random_effects, main = "Q-Q Plot of Random Effects")
    qqline(random_effects, col = "red")
    
    # Autocorrelation Plot for Independence of Errors
    acf(residuals, main = "ACF of Residuals", lag.max = 20)
    
    dev.off() # Save the plot
  }
  
  return(combined_results)
}


# List of datasets
datasets <- list(
  SWTH_experiment_full = SWTH_experiment_full,
  SWTH_experiment_intro = SWTH_experiment_intro
)

# Response variables
response_vars <- c("Dur.90...s.", "Bandwidth", "PFC.Max.Freq..Hz.", "PFC.Min.Freq..Hz.", "Peak.Freq..Hz.")

# Initialize a data frame to store combined results
combined_results <- data.frame(Dataset = character(),
                               Response_Variable = character(),
                               Levene_p_value = numeric(),
                               Shapiro_Wilk_p_value = numeric(),
                               stringsAsFactors = FALSE)

# Run the function for each dataset
for (dataset_name in names(datasets)) {
  combined_results <- check_assumptions(datasets[[dataset_name]], dataset_name, response_vars, combined_results)
}

# Export the combined results table to an Excel file
write_xlsx(combined_results, path = file.path("Raw_Experiment_LMM_Assumptions9", "Combined_Assumptions_Test_Results.xlsx"))



#### EXPERIMENT DATA ANALYSIS, LMMS BY HAND, INCLUDES TOD AND TRACK ####

## For SWTH_experiment_full

# Bandwidth
lmm_SWTH_experiment_full_BW = lmer(Bandwidth ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_BW)

# Duration 90%
lmm_SWTH_experiment_full_Dur.90 = lmer(Dur.90...s. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Dur.90)

# PFC Max
lmm_SWTH_experiment_full_PFC.Max = lmer(PFC.Max.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_PFC.Max)

# PFC Min
lmm_SWTH_experiment_full_PFC.Min = lmer(PFC.Min.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_PFC.Min)

# Peak frequency
lmm_SWTH_experiment_full_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Peak.Freq)


## For SWTH_experiment_intro

# Bandwidth
lmm_SWTH_experiment_intro_BW = lmer(Bandwidth ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_BW)

# Duration 90%
lmm_SWTH_experiment_intro_Dur.90 = lmer(Dur.90...s. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Dur.90)

# PFC Max
lmm_SWTH_experiment_intro_PFC.Max = lmer(PFC.Max.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_PFC.Max)

# PFC Min
lmm_SWTH_experiment_intro_PFC.Min = lmer(PFC.Min.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_PFC.Min)

# Peak frequency
lmm_SWTH_experiment_intro_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + Time.of.day + Exemplar.track + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Peak.Freq)





#### ORIGINAL EXPERIMENT DATA ANALYSIS, LMMS BY HAND (Using for 5/8/26) ####

## For SWTH_experiment_full

# Frequency 5%
lmm_SWTH_experiment_full_Freq.5 = lmer(Freq.5...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Freq.5)

# Frequency 95%
lmm_SWTH_experiment_full_Freq.95 = lmer(Freq.95...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Freq.95)

# Bandwidth
lmm_SWTH_experiment_full_BW = lmer(Bandwidth ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_BW)

# Duration 90%
lmm_SWTH_experiment_full_Dur.90 = lmer(Dur.90...s. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Dur.90)

# PFC Max
lmm_SWTH_experiment_full_PFC.Max = lmer(PFC.Max.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_PFC.Max)

# PFC Min
lmm_SWTH_experiment_full_PFC.Min = lmer(PFC.Min.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_PFC.Min)

# Peak frequency
lmm_SWTH_experiment_full_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Peak.Freq)

residuals <- resid(lmm_SWTH_experiment_full_Peak.Freq)
shapiro.test(residuals)
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")


## For SWTH_experiment_intro

# Frequency 5%
lmm_SWTH_experiment_intro_Freq.5 = lmer(Freq.5...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Freq.5)

# Frequency 95%
lmm_SWTH_experiment_intro_Freq.95 = lmer(Freq.95...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Freq.95)

residuals <- resid(lmm_SWTH_experiment_intro_Freq.95)
shapiro.test(residuals)
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")


# Bandwidth
lmm_SWTH_experiment_intro_BW = lmer(Bandwidth ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_BW)

# Duration 90%
lmm_SWTH_experiment_intro_Dur.90 = lmer(Dur.90...s. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Dur.90)

# PFC Max
lmm_SWTH_experiment_intro_PFC.Max = lmer(PFC.Max.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_PFC.Max)

# PFC Min
lmm_SWTH_experiment_intro_PFC.Min = lmer(PFC.Min.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_PFC.Min)

# Peak frequency
lmm_SWTH_experiment_intro_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Peak.Freq)


#### CALCULATE ADJUSTED p-values FOR EXPERIMENT LMMS (added 5/8/26) ####

# Using the p.adjust_lmm function that was created earlier in the code. 

## Create model lists
SWTH_experiment_full_models_list <- list(
    lmm_SWTH_experiment_full_Freq.5 = lmm_SWTH_experiment_full_Freq.5, 
    lmm_SWTH_experiment_full_Freq.95 = lmm_SWTH_experiment_full_Freq.95,
    lmm_SWTH_experiment_full_BW = lmm_SWTH_experiment_full_BW, 
    lmm_SWTH_experiment_full_Peak.Freq = lmm_SWTH_experiment_full_Peak.Freq,
    lmm_SWTH_experiment_full_Dur.90 = lmm_SWTH_experiment_full_Dur.90, 
    lmm_SWTH_rate_Song = lmm_SWTH_rate_Song
)


SWTH_experiment_intro_models_list <- list( 
  lmm_SWTH_experiment_intro_BW = lmm_SWTH_experiment_intro_BW, 
  lmm_SWTH_experiment_intro_Dur.90 = lmm_SWTH_experiment_intro_Dur.90,
  lmm_SWTH_experiment_intro_Freq.95 = lmm_SWTH_experiment_intro_Freq.95, 
  lmm_SWTH_experiment_intro_Freq.5 = lmm_SWTH_experiment_intro_Freq.5,
  lmm_SWTH_experiment_intro_Peak.Freq = lmm_SWTH_experiment_intro_Peak.Freq
)


## Calculate adjusted p-values with Benjamini-Hochberg (1995; fdr) correction
results <- p.adjust_lmm(
  #lmms           = SWTH_experiment_full_models_list,
  lmms           = SWTH_experiment_intro_models_list,
  method         = "fdr",     # Benjamini-Hochberg; "fdr" and "BH" are equivalent in p.adjust()
  ignore         = list("(Intercept)"),  # explicitly ignore intercept 
  sep            = FALSE,      # FALSE pools all p-values together
  ignore_pattern = list()
)

# Define the desired outcome order from your model list
outcome_order <- c(
  "Freq.5...Hz.", 
  "Freq.95...Hz.", 
  "Bandwidth", 
  "Peak.Freq..Hz.", 
  "Dur.90...s.", 
  "Rate"
)

# Reorder the results table to match
results$df.cor <- results$df.cor[order(match(results$df.cor$outcome, outcome_order)), ]

# View the results table
results$df.cor

# Save results table
write_xlsx(results$df.cor, "experiment_full_table.xlsx")
write_xlsx(results$df.cor, "experiment_intro_table.xlsx")

## Calculate adjusted p-values with p.adjust()
# Extract p-values from each model manually as a double check
# coef(summary(model)) gives the coefficient table; 
# [2, 5] grabs row 2 (your predictor, skipping intercept), column 5 (p-value)

pvalues <- c(
  coef(summary(lmm_SWTH_experiment_full_BW))[2, 5],
  coef(summary(lmm_SWTH_experiment_full_Dur.90))[2, 5],
  coef(summary(lmm_SWTH_experiment_full_Freq.5))[2, 5],
  coef(summary(lmm_SWTH_experiment_full_Freq.95))[2, 5],
  coef(summary(lmm_SWTH_experiment_full_Peak.Freq))[2, 5]
)

# Give them names so the output is readable
names(pvalues) <- c("BW", "Dur.90", "Freq.5", "Freq.95", "Peak.Freq")

# Apply BH correction to the whole vector at once
p.adjust(pvalues, method = "BH")



#### OLD EXPERIMENT RESULTS TABLE CREATION ####
extract_lmm_results <- function(models_list, model_names, output_file) {
  result_list <- list()
  
  for (model_name in model_names) {
    model <- models_list[[model_name]]
    
    # Extract random effects variance
    rand_effects <- VarCorr(model)
    random_effect_sd <- sqrt(as.numeric(rand_effects$IndividualID[1]))
    
    # Extract fixed effects
    fixed_effects <- summary(model)$coefficients
    
    # Create base model results
    model_results <- data.frame(
      Model = model_name,
      Response_Variable = as.character(formula(model)[[2]]),
      Random_effect_Std.Dev = random_effect_sd,
      Fixed_Intercept_Estimate = fixed_effects["(Intercept)", "Estimate"],
      Fixed_Intercept_Std.Error = fixed_effects["(Intercept)", "Std. Error"],
      Fixed_Intercept_t.value = fixed_effects["(Intercept)", "t value"],
      Fixed_Intercept_P.Value = fixed_effects["(Intercept)", "Pr(>|t|)"],
      During_Estimate = NA,
      During_Std.Error = NA,
      During_t.value = NA,
      During_P.Value = NA,
      Post_Estimate = NA,
      Post_Std.Error = NA,
      Post_t.value = NA,
      Post_P.Value = NA,
      stringsAsFactors = FALSE
    )
    
    # Check if Trial_type is present and add its values
    if ("Trial_typeNoise" %in% rownames(fixed_effects)) {
      model_results$During_Estimate <- fixed_effects["Trial_typeNoise", "Estimate"]
      model_results$During_Std.Error <- fixed_effects["Trial_typeNoise", "Std. Error"]
      model_results$During_t.value <- fixed_effects["Trial_typeNoise", "t value"]
      model_results$During_P.Value <- fixed_effects["Trial_typeNoise", "Pr(>|t|)"]
    }
    
    if ("Trial_typePost" %in% rownames(fixed_effects)) {
      model_results$Post_Estimate <- fixed_effects["Trial_typePost", "Estimate"]
      model_results$Post_Std.Error <- fixed_effects["Trial_typePost", "Std. Error"]
      model_results$Post_t.value <- fixed_effects["Trial_typePost", "t value"]
      model_results$Post_P.Value <- fixed_effects["Trial_typePost", "Pr(>|t|)"]
    }
    
    # Calculate R-squared values using performance package
    if (requireNamespace("performance", quietly = TRUE)) {
      r2_values <- performance::r2(model)
      model_results$R2_conditional <- r2_values$R2_conditional
      model_results$R2_marginal <- r2_values$R2_marginal
    }
    
    # Append to result list
    result_list[[model_name]] <- model_results
  }
  
  # Combine all results
  all_results <- do.call(rbind, result_list)
  rownames(all_results) <- NULL
  
  # Write to Excel file
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(all_results, output_file, rowNames = FALSE)
  } else {
    write.csv(all_results, gsub("\\.xlsx$", ".csv", output_file), row.names = FALSE)
  }
  
  return(all_results)
}

# Assuming you have a list of models, e.g., `models_list`
#models_list <- list(
  #lmm_SWTH_experiment_full_BW = lmm_SWTH_experiment_full_BW, 
  #lmm_SWTH_experiment_full_Dur.90 = lmm_SWTH_experiment_full_Dur.90, 
  #lmm_SWTH_experiment_full_PFC.Max = lmm_SWTH_experiment_full_PFC.Max,
  #lmm_SWTH_experiment_full_PFC.Min = lmm_SWTH_experiment_full_PFC.Min, 
  #lmm_SWTH_experiment_full_Peak.Freq = lmm_SWTH_experiment_full_Peak.Freq,
  #lmm_SWTH_rate_Song = lmm_SWTH_rate_Song, 
  #lmm_SWTH_experiment_intro_BW = lmm_SWTH_experiment_intro_BW, 
  #lmm_SWTH_experiment_intro_Dur.90 = lmm_SWTH_experiment_intro_Dur.90,
  #lmm_SWTH_experiment_intro_PFC.Max = lmm_SWTH_experiment_intro_PFC.Max, 
  #lmm_SWTH_experiment_intro_PFC.Min = lmm_SWTH_experiment_intro_PFC.Min,
  #lmm_SWTH_experiment_intro_Peak.Freq = lmm_SWTH_experiment_intro_Peak.Freq
#)


models_list <- list( #t his list has freq 5 and 95
  lmm_SWTH_experiment_full_BW = lmm_SWTH_experiment_full_BW, 
  lmm_SWTH_experiment_full_Dur.90 = lmm_SWTH_experiment_full_Dur.90, 
  lmm_SWTH_experiment_full_Freq.5 = lmm_SWTH_experiment_full_Freq.5,
  lmm_SWTH_experiment_full_Freq.95 = lmm_SWTH_experiment_full_Freq.95,
  lmm_SWTH_experiment_full_Peak.Freq = lmm_SWTH_experiment_full_Peak.Freq, 
  lmm_SWTH_rate_Song = lmm_SWTH_rate_Song, 
  lmm_SWTH_experiment_intro_BW = lmm_SWTH_experiment_intro_BW, 
  lmm_SWTH_experiment_intro_Dur.90 = lmm_SWTH_experiment_intro_Dur.90, 
  lmm_SWTH_experiment_intro_Freq.5 = lmm_SWTH_experiment_intro_Freq.5,
  lmm_SWTH_experiment_intro_Freq.95 = lmm_SWTH_experiment_intro_Freq.95,
  lmm_SWTH_experiment_intro_Peak.Freq = lmm_SWTH_experiment_intro_Peak.Freq
)

# Replace with your actual model names
model_names <- names(models_list)

# Create output file name
output_file <- "Raw_Experiment_LMM_Full_Results_RoyalSocRevision1.0.xlsx"

# Extract and save results
extract_lmm_results(models_list, model_names, output_file)


#### SURVEY FINAL PLOTS FOR SWTH ####

### SWTH_survey_full

## Frequency 5%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_Freq.5, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_Freq.5_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = Freq.5...Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Min Frequency (Hz)") +
  annotate("text", x=30, y=2700, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Frequency 95%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_Freq.95, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_Freq.95_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = Freq.95...Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Max Frequency (Hz)") +
  annotate("text", x=29, y=6500, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Bandwidth

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_BW, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_BW_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = Bandwidth), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Bandwidth (Hz)") +
  annotate("text", x=29, y=5000, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Duration 90%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_Dur.90, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_Dur.90_plot= ggplot() +
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = Dur.90...s.), color= "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Duration (s)") +
  annotate("text", x=30, y=2.0, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Max

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_PFC.Max.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_PFCMax_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = PFC.Max.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Max Frequency (Hz)") +
  annotate("text", x=29, y=8500, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Min

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_PFC.Min.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_PFCMin_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = PFC.Min.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Min Frequency (Hz)") +
  annotate("text", x=30, y=2400, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Peak Freq

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_full_Peak.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_full_Peak.Freq_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_full, aes(x = LAeq..dB..AVG, y = Peak.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Peak Frequency (Hz)") +
  annotate("text", x=30, y=5400, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

# Arrange the above 7 plots
grid.arrange(SWTH_survey_full_Freq.5_plot, SWTH_survey_full_Freq.95_plot, SWTH_survey_full_BW_plot, SWTH_survey_full_Peak.Freq_plot, SWTH_survey_full_Dur.90_plot, ncol = 3, nrow = 2)




### SWTH_survey_intro

## Frequency 5%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_Freq.5, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect
min(SWTH_survey_full$Freq.5...Hz.)
# Plot
SWTH_survey_intro_Freq.5_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = Freq.5...Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Min Frequency (Hz)") +
  annotate("text", x=30, y=2500, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Frequency 95%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_Freq.95, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_Freq.95_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = Freq.95...Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Max Frequency (Hz)") +
  annotate("text", x=30, y=3300, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )
## Bandwidth

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_BW, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_BW_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = Bandwidth), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Bandwidth (Hz)") +
  annotate("text", x=30, y=1300, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Duration 90%

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_Dur.90, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_Dur.90_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = Dur.90...s.), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Duration (s)") +
  annotate("text", x=30, y=0.3, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Max

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_PFC.Max.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_PFCMax_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = PFC.Max.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Max Frequency (Hz)") +
  annotate("text", x=30, y=3500, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Min

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_PFC.Min.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_PFCMin_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = PFC.Min.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("") +
  ylab("Min Frequency (Hz)") +
  annotate("text", x=30, y=2500, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Peak Freq

# Create Best Fit Line
predicted_effects <- ggpredict(lmm_SWTH_survey_intro_Peak.Freq, terms = "LAeq..dB..AVG") # Extract predicted values for the fixed effect

# Plot
SWTH_survey_intro_Peak.Freq_plot= ggplot() +
  # Add the predicted effects as a line with a confidence ribbon
  geom_line(data = predicted_effects, aes(x = x, y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(data = predicted_effects, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  # Add raw data points
  geom_point(data = SWTH_survey_intro, aes(x = LAeq..dB..AVG, y = Peak.Freq..Hz.), color = "black", alpha = 1) +
  # Add labels
  xlab("Ambient noise level (dB)") +
  ylab("Peak Frequency (Hz)") +
  annotate("text", x=30, y=3300, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

# Arrange the above 7 plots
grid.arrange(SWTH_survey_intro_Freq.5_plot, SWTH_survey_intro_Freq.95_plot, SWTH_survey_intro_BW_plot, SWTH_survey_intro_Peak.Freq_plot, SWTH_survey_intro_Dur.90_plot, ncol = 3, nrow = 2)


#### EXPERIMENT FINAL LINE PLOTS FOR SWTH (WITH SE), TEMPORALLY CONNECTED POINTS ####

### SWTH_experiment_full

## Freq 95%

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Freq.95...Hz.),
    se = sd(Freq.95...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_Freq.95_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=5500, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Freq 5%

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Freq.5...Hz.),
    se = sd(Freq.5...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_Freq.5_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=2500, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")


## Bandwidth

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Bandwidth),
    se = sd(Bandwidth) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_BW_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2, alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed", alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), alpha = 1,width = 0.1) +  # Use SE instead of CI
  labs(x = "", y = "Bandwidth (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=3500, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Duration 90%

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Dur.90...s.),
    se = sd(Dur.90...s.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_Dur.90_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Duration (s)", color = "bird ID") +
  annotate("text", x=.7, y=1.7, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## PFC Max

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Max.Freq..Hz.),
    se = sd(PFC.Max.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_PFCMax_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=7200, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## PFC Min

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Min.Freq..Hz.),
    se = sd(PFC.Min.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_PFCMin_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=2300, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Peak Freq

# Calculate means and SE
summary_data <- SWTH_experiment_full %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Peak.Freq..Hz.),
    se = sd(Peak.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_full_Peak.freq_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Peak Frequency (Hz)", color = "Bird ID") +
  annotate("text", x=.7, y=3750, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

# Arrange the above 5 plots
grid.arrange(SWTH_experiment_full_Freq.5_plot, SWTH_experiment_full_Freq.95_plot, SWTH_experiment_full_BW_plot, SWTH_experiment_full_Peak.freq_plot, SWTH_experiment_full_Dur.90_plot, SWTH_song_rate_plot, ncol = 3, nrow = 2) #added song rate plot too


### SWTH_experiment_intro


## Freq 95%

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Freq.95...Hz.),
    se = sd(Freq.95...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_Freq.95_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=3000, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Freq 5%

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Freq.5...Hz.),
    se = sd(Freq.5...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_Freq.5_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=2400, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")


## Bandwidth

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Bandwidth),
    se = sd(Bandwidth) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_BW_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Bandwidth (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=750, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Duration 90%

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Dur.90...s.),
    se = sd(Dur.90...s.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_Dur.90_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Duration (s)", color = "bird ID") +
  annotate("text", x=.7, y=0.165, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## PFC Max

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Max.Freq..Hz.),
    se = sd(PFC.Max.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_PFCMax_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=2950, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## PFC Min

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Min.Freq..Hz.),
    se = sd(PFC.Min.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_PFCMin_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)", color = "bird ID") +
  annotate("text", x=.7, y=2300, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

## Peak Freq

# Calculate means and SE
summary_data <- SWTH_experiment_intro %>%
  group_by(IndividualID, Trial_type) %>%
  summarise(
    mean_value = mean(Peak.Freq..Hz.),
    se = sd(Peak.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "During" phase
has_during <- summary_data %>%
  filter(Trial_type == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_experiment_intro_Peak.freq_plot = ggplot(summary_data, aes(x = Trial_type, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Peak Frequency (Hz)", color = "Bird ID") +
  annotate("text", x=.7, y=2800, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

# Arrange the above 5 plots
grid.arrange(SWTH_experiment_intro_Freq.5_plot, SWTH_experiment_intro_Freq.95_plot, SWTH_experiment_intro_BW_plot, SWTH_experiment_intro_Peak.freq_plot, SWTH_experiment_intro_Dur.90_plot, ncol = 3, nrow = 2) #added song rate plot too


#### EXPERIMENT FINAL BAR PLOTS FOR SWTH (WITH SE) ####

### SWTH_experiment_full

## Frequency 5%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Freq.5...Hz.),
    se = sd(Freq.5...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_full_Freq.5_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightcoral", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)") +
  coord_cartesian(ylim = c(1500, 2100)) +  # Keeps all data but zooms in
  annotate("text", x=.7, y=2100, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Frequency 95%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Freq.95...Hz.),
    se = sd(Freq.95...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_full_Freq.95_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "purple", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)") +
  coord_cartesian(ylim = c(3900, 4500)) +
  annotate("text", x=.7, y=4500, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Bandwidth

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Bandwidth),
    se = sd(Bandwidth) / sqrt(n()),  # Standard Error (SE)
    n = n() # Sample size
  )

# Create the plot
SWTH_experiment_full_BW_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "orange", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Bandwidth (Hz)") +
  coord_cartesian(ylim = c(1500, 2500)) +
  annotate("text", x=.7, y=2500, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Duration 90%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Dur.90...s.),
    se = sd(Dur.90...s.) / sqrt(n()),  # Standard Error (SE)
    n = n() # Sample size
  )

# Create the plot
SWTH_experiment_full_Dur.90_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "pink", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Duration (s)") +
  coord_cartesian(ylim = c(0.80, 1.40)) +
  annotate("text", x=.7, y=1.4, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Max

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Max.Freq..Hz.),
    se = sd(PFC.Max.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_full_PFCMax_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "purple", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)") +
  coord_cartesian(ylim = c(5100, 6100)) +
  annotate("text", x=.7, y=6100, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Min

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Min.Freq..Hz.),
    se = sd(PFC.Min.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_full_PFCMin_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightcoral", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)") +
  coord_cartesian(ylim = c(1300, 2000)) +
  annotate("text", x=.7, y=2000, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Peak Freq

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_full %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Peak.Freq..Hz.),
    se = sd(Peak.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_full_Peak.freq_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Peak Frequency (Hz)") +
  coord_cartesian(ylim = c(2500, 3100)) +
  annotate("text", x=.7, y=3100, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

# Arrange the above 7 plots
grid.arrange(SWTH_experiment_full_Freq.5_barplot, SWTH_experiment_full_Freq.95_barplot, SWTH_experiment_full_BW_barplot, SWTH_experiment_full_Peak.freq_barplot, SWTH_experiment_full_Dur.90_barplot, SWTH_song_rate_bar, ncol = 3, nrow = 2)



### SWTH_experiment_intro


## Frequency 5%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Freq.5...Hz.),
    se = sd(Freq.5...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_Freq.5_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightcoral", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)") +
  coord_cartesian(ylim = c(1300, 2100)) +  # Keeps all data but zooms in
  annotate("text", x=.7, y=2100, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Frequency 95%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Freq.95...Hz.),
    se = sd(Freq.95...Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_Freq.95_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "purple", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)") +
  coord_cartesian(ylim = c(1700, 2600)) +
  annotate("text", x=.7, y=2600, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )


## Bandwidth

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Bandwidth),
    se = sd(Bandwidth) / sqrt(n()),  # Standard Error (SE)
    n = n() # Sample size
  )

# Create the plot
SWTH_experiment_intro_BW_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "orange", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Bandwidth (Hz)") +
  coord_cartesian(ylim = c(200, 500)) +
  annotate("text", x=.7, y=500, label= "C", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Duration 90%

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Dur.90...s.),
    se = sd(Dur.90...s.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_Dur.90_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "pink", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Duration (s)") +
  coord_cartesian(ylim = c(0.06, 0.12)) +
  annotate("text", x=.7, y=0.12, label= "E", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Max

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Max.Freq..Hz.),
    se = sd(PFC.Max.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_PFCMax_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "purple", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Max Frequency (Hz)") +
  coord_cartesian(ylim = c(1800, 2600)) +
  annotate("text", x=.7, y=2600, label= "B", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## PFC Min

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(PFC.Min.Freq..Hz.),
    se = sd(PFC.Min.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_PFCMin_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightcoral", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "", y = "Min Frequency (Hz)") +
  coord_cartesian(ylim = c(1100, 2100)) +
  annotate("text", x=.7, y=2100, label= "A", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

## Peak Freq

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_experiment_intro %>%
  group_by(Trial_type) %>%
  summarise(
    mean_value = mean(Peak.Freq..Hz.),
    se = sd(Peak.Freq..Hz.) / sqrt(n())  # Standard Error (SE)
  )

# Create the plot
SWTH_experiment_intro_Peak.freq_barplot = ggplot(summary_data, aes(x = Trial_type, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Peak Frequency (Hz)") +
  coord_cartesian(ylim = c(1500, 2500)) +
  annotate("text", x=.7, y=2500, label= "D", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

# Arrange the above 5 plots
grid.arrange(SWTH_experiment_intro_Freq.5_barplot, SWTH_experiment_intro_Freq.95_barplot, SWTH_experiment_intro_BW_barplot, SWTH_experiment_intro_Peak.freq_barplot, SWTH_experiment_intro_Dur.90_barplot, ncol = 3, nrow = 2)






#### EXPERIMENT FINAL RESULTS TABLE FOR SWTH (have to manually add EMMs) ####

# Set working directory to where you want the table to go
setwd()

# Define model names and corresponding response variable labels
model_list <- list(
  lmm_SWTH_experiment_full_Freq.5, lmm_SWTH_experiment_full_Freq.95, 
  lmm_SWTH_experiment_full_BW, lmm_SWTH_experiment_full_Peak.Freq,
  lmm_SWTH_experiment_full_Dur.90, lmm_SWTH_rate_Song, 
  lmm_SWTH_experiment_intro_Freq.5, lmm_SWTH_experiment_intro_Freq.95,
  lmm_SWTH_experiment_intro_BW, lmm_SWTH_experiment_intro_Peak.Freq,
  lmm_SWTH_experiment_intro_Dur.90
)

response_labels <- c(
  "Full song minimum frequency (Hz)", "Full song maximum frequency (Hz)",  "Full song bandwidth (Hz)", "Full song peak frequency (Hz)", "Full song duration (s)", "Full song rate (songs/minute)",
  "Intro note minimum frequency (Hz)", "Intro note maximum frequency (Hz)","Intro note bandwidth (Hz)", "Intro note peak frequency (Hz)", "Intro note duration (s)"
)

# Function to extract results and format them
extract_results <- function(model, response_label) {
  tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Pre (intercept)",
        term == "Trial_typeDuring" ~ "During",
        term == "Trial_typePost" ~ "Post",
        term == "Playback_phaseDuring" ~ "During",
        term == "Playback_phasePost" ~ "Post", 
        TRUE ~ term
      ),
      Response_Variable = response_label
    ) %>%
    mutate(Response_Variable = ifelse(row_number() == 1, Response_Variable, ""))
}

# Apply function to all models
results_list <- mapply(extract_results, model_list, response_labels, SIMPLIFY = FALSE)

# Combine all results into one data frame
final_results <- bind_rows(results_list) %>%
  select(Response_Variable, term, estimate, std.error, df, statistic, p.value) %>%
  rename(
    `Response variable` = Response_Variable,
    `Parameter` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `df` = df,
    `t value` = statistic,
    `P value` = p.value
  ) %>%
  mutate(
    `P value` = ifelse(`P value` < 0.001, "<0.001", sprintf("%.3f", `P value`))
  )

# Create and format table using gt
gt_table <- final_results %>%
  gt() %>%
  cols_label(
    `Response variable` = "Response Variable",
    `Parameter` = "Parameter",
    `Estimate` = "Estimate",
    `Std. Error` = "Std. Error",
    `df` = "df",
    `t value` = "t value",
    `P value` = "P value"
  ) %>%
  fmt_number(columns = c("df"), decimals = 0) %>%
  fmt_number(columns = c("Estimate", "Std. Error", "t value"), decimals = 2) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.weight = "bold"
  ) %>%
  # Apply bold styling to rows where p-value < 0.05
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = (`P value` < 0.05 | `P value` == "<0.001") & Parameter != "Pre (intercept)"
    )
  )

# Save the table as a HTML
gtsave(gt_table, "Experiment_results_RoyalSoc_revisions1.0.html")

# Save the table as a Word document
gtsave(gt_table, "Experiment_results_RoyalSoc_revisions1.0.docx")

#### SURVEY FINAL RESULTS TABLE FOR SWTH ####

# Set working directory to where you want the table to go
setwd()

# Define model names and corresponding response variable labels
model_list <- list(
  lmm_SWTH_survey_full_Freq.5, lmm_SWTH_survey_full_Freq.95, lmm_SWTH_survey_full_BW,    lmm_SWTH_survey_full_Peak.Freq, lmm_SWTH_survey_full_Dur.90, 
  lmm_SWTH_survey_intro_Freq.5, lmm_SWTH_survey_intro_Freq.95,
   lmm_SWTH_survey_intro_BW, lmm_SWTH_survey_intro_Peak.Freq,
  lmm_SWTH_survey_intro_Dur.90
)

response_labels <- c(
  "Full song minimum frequency (Hz)", "Full song maximum frequency (Hz)",  "Full song bandwidth (Hz)", 
  "Full song peak frequency (Hz)", "Full song duration (s)", 
  "Intro note minimum frequency (Hz)", "Intro note maximum frequency (Hz)","Intro note bandwidth (Hz)", "Intro note peak frequency (Hz)", "Intro note duration (s)"
)

# Function to extract results and format them
extract_results <- function(model, response_label) {
  tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "LAeq..dB..AVG" ~ "Ambient noise (dB)",
        TRUE ~ term
      ),
      Response_Variable = response_label
    ) %>%
    mutate(Response_Variable = ifelse(row_number() == 1, Response_Variable, ""))
}

# Apply function to all models
results_list <- mapply(extract_results, model_list, response_labels, SIMPLIFY = FALSE)

# Combine all results into one data frame
final_results <- bind_rows(results_list) %>%
  select(Response_Variable, term, estimate, std.error, df, statistic, p.value) %>%
  rename(
    `Response variable` = Response_Variable,
    `Parameter` = term,
    `Estimate` = estimate,
    `Std. Error` = std.error,
    `df` = df,
    `t value` = statistic,
    `P value` = p.value
  ) %>%
  mutate(
    `P value` = ifelse(`P value` < 0.001, "<0.001", sprintf("%.3f", `P value`))
  )

# Create and format table using gt
gt_table <- final_results %>%
  gt() %>%
  cols_label(
    `Response variable` = "Response Variable",
    `Parameter` = "Parameter",
    `Estimate` = "Estimate",
    `Std. Error` = "Std. Error",
    `df` = "df",
    `t value` = "t value",
    `P value` = "P value"
  ) %>%
  fmt_number(columns = c("df"), decimals = 0) %>%
  fmt_number(columns = c("Estimate", "Std. Error", "t value"), decimals = 2) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.weight = "bold"
  ) %>%
  # Apply bold styling to rows where p-value < 0.05
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = (`P value` < 0.05 | `P value` == "<0.001") & Parameter != "Intercept"
    )
  )

# Save the table as a HTML
gtsave(gt_table, "Survey_results_RoyalSoc_revisions1.0.html")

# Save the table as a Word document
gtsave(gt_table, "Survey_results_RoyalSoc_revisions1.0.docx")

#### CALCULATING ESTIMATED MARGINAL MEANS FOR SWTH EXPERIMENT MODELS ####

## For SWTH_experiment_full

# Bandwidth
lmm_SWTH_experiment_full_BW = lmer(Bandwidth ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_BW)

EMM_SWTH_experiment_full_BW = emmeans(lmm_SWTH_experiment_full_BW, ~ Trial_type, lmer.df = "satterthwaite")


# Duration 90%
lmm_SWTH_experiment_full_Dur.90 = lmer(Dur.90...s. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Dur.90)

EMM_SWTH_experiment_full_Dur.90 = emmeans(lmm_SWTH_experiment_full_Dur.90, ~ Trial_type, lmer.df = "satterthwaite")

# Freq 95
lmm_SWTH_experiment_full_Freq.95 = lmer(Freq.95...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Freq.95)

EMM_SWTH_experiment_full_Freq.95 = emmeans(lmm_SWTH_experiment_full_Freq.95, ~ Trial_type, lmer.df = "satterthwaite")


# Freq 5
lmm_SWTH_experiment_full_Freq.5 = lmer(Freq.5...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Freq.5)

EMM_SWTH_experiment_full_Freq.5 = emmeans(lmm_SWTH_experiment_full_Freq.5, ~ Trial_type, lmer.df = "satterthwaite")

# Peak frequency
lmm_SWTH_experiment_full_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_full)
summary(lmm_SWTH_experiment_full_Peak.Freq)

EMM_SWTH_experiment_full_Peak.Freq= emmeans(lmm_SWTH_experiment_full_Peak.Freq, ~ Trial_type, lmer.df = "satterthwaite")

## For full song rate

EMM_SWTH_rate_Song = emmeans(lmm_SWTH_rate_Song, ~ Playback_phase, lmer.df = "satterthwaite")

## For SWTH_experiment_intro

# Bandwidth
lmm_SWTH_experiment_intro_BW = lmer(Bandwidth ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_BW)

EMM_SWTH_experiment_intro_BW = emmeans(lmm_SWTH_experiment_intro_BW, ~ Trial_type, lmer.df = "satterthwaite")

# Duration 90%
lmm_SWTH_experiment_intro_Dur.90 = lmer(Dur.90...s. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Dur.90)

EMM_SWTH_experiment_intro_Dur.90 = emmeans(lmm_SWTH_experiment_intro_Dur.90, ~ Trial_type, lmer.df = "satterthwaite")

# Freq 95
lmm_SWTH_experiment_intro_Freq.95 = lmer(Freq.95...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Freq.95)

EMM_SWTH_experiment_intro_Freq.95 = emmeans(lmm_SWTH_experiment_intro_Freq.95, ~ Trial_type, lmer.df = "satterthwaite")

# Freq 5
lmm_SWTH_experiment_intro_Freq.5 = lmer(Freq.5...Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Freq.5)

EMM_SWTH_experiment_intro_Freq.5 = emmeans(lmm_SWTH_experiment_intro_Freq.5, ~ Trial_type, lmer.df = "satterthwaite")

# Peak frequency
lmm_SWTH_experiment_intro_Peak.Freq = lmer(Peak.Freq..Hz. ~ Trial_type + (1 | IndividualID), data = SWTH_experiment_intro)
summary(lmm_SWTH_experiment_intro_Peak.Freq)

EMM_SWTH_experiment_intro_Peak.Freq = emmeans(lmm_SWTH_experiment_intro_Peak.Freq, ~ Trial_type, lmer.df = "satterthwaite")



### Organize emm results into one table

# Convert each EMM object to a data frame and tag with metadata
emm_list <- list(
  # SWTH_experiment_full
  list(emm = EMM_SWTH_experiment_full_Freq.5,     dataset = "full",  response = "Freq.5"),
  list(emm = EMM_SWTH_experiment_full_Freq.95,     dataset = "full",  response = "Freq.95"),
  list(emm = EMM_SWTH_experiment_full_BW,         dataset = "full",  response = "Bandwidth"),
  list(emm = EMM_SWTH_experiment_full_Peak.Freq,   dataset = "full",  response = "Peak.Freq"),
  list(emm = EMM_SWTH_experiment_full_Dur.90,      dataset = "full",  response = "Dur.90"),
  
  # SWTH_rate_Song (different contrast variable: Playback_phase)
  list(emm = EMM_SWTH_rate_Song,                   dataset = "rate",  response = "Song_rate"),
  
  # SWTH_experiment_intro
  list(emm = EMM_SWTH_experiment_intro_Freq.5,    dataset = "intro", response = "Freq.5"),
  list(emm = EMM_SWTH_experiment_intro_Freq.95,    dataset = "intro", response = "Freq.95"),
  list(emm = EMM_SWTH_experiment_intro_BW,         dataset = "intro", response = "Bandwidth"),
  list(emm = EMM_SWTH_experiment_intro_Peak.Freq,  dataset = "intro", response = "Peak.Freq"),
  list(emm = EMM_SWTH_experiment_intro_Dur.90,     dataset = "intro", response = "Dur.90")
)

# Collapse into one table
emm_table <- bind_rows(lapply(emm_list, function(x) {
  df <- as.data.frame(x$emm)
  # Rename whichever grouping column is present to a common name
  names(df)[names(df) %in% c("Trial_type", "Playback_phase")] <- "Contrast_level"
  df$Dataset  <- x$dataset
  df$Response <- x$response
  df
})) |>
  select(Dataset, Response, Contrast_level, emmean, SE, df, lower.CL, upper.CL)

print(emm_table)

# Optional: export to CSV
write_xlsx(emm_table, "EMM_results_combined.xlsx")
