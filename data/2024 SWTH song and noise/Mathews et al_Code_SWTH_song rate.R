#### NOTES ####

## This code works stand alone from the Final_SWTH_analysis code. That said, when run in conjection the figures and tables all combine. 

#### SETUP ####

## PACKAGES
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

library(openxlsx) #For write.xlsx function
library(broom.mixed) # For extracting model summaries
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




### READ IN

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/WWU/Dr. Templeton's Lab/PostGrad_research/NoiseVocal_experiment/Data")

birdrate=read.csv("VocalRate_data2.0.csv", header=T) # READ IN RATE MASTER DATA

### INITIAL DATA WRANGLING 
str(birdrate)
birdrate$Species = as.factor(birdrate$Species)
birdrate$IndividualID = as.factor(birdrate$IndividualID)
birdrate$Vocal_type = as.factor(birdrate$Vocal_type)
birdrate$Playback_phase = as.factor(birdrate$Playback_phase)
birdrate$Rate = as.numeric(birdrate$Rate)
levels(birdrate$Species)

## Species has 4 levels for some reason, one of them is BRCR . 

birdrate <- birdrate %>%
  mutate(Species = str_trim(Species),   # Deleting any whitespace around words
         Species = case_when(
           Species == "BRCR " ~ "BRCR",       # Convert 'BRCR ' to 'BRCR'
           TRUE ~ Species                  # Keep other values as they are
         ))

birdrate$Species = as.factor(birdrate$Species) # convert back to factor

## Reorganize the Playback_phase levels to be pre, during, post

# Specify the new level order as a vector
new_level_order <- c("Pre", "During", "Post")  

# Reorder levels for Trial_type in each dataset
birdrate$Playback_phase <- factor(birdrate$Playback_phase, levels = new_level_order)


### Make the subsets needed

SWTHrate <- birdrate %>% filter(Species == "SWTH") # subset for SWTH
SWTH_rate_Song <- SWTHrate %>% filter(Vocal_type == "Song") # subset for SWTH Songs

length(unique(SWTH_rate_Song$IndividualID)) # 15 individuals

### SECONDARY DATA WRANGLING 
str(SWTH_rate_Song)
SWTH_rate_Song$Exemplar.Track=as.factor(SWTH_rate_Song$Exemplar.Track)
SWTH_rate_Song$Time.of.day=as.numeric(SWTH_rate_Song$Time.of.day)
levels(SWTH_rate_Song$Exemplar.Track)



#### CHANGE DURING TO NOISE (5/2) ####

## Change for song rate

SWTH_rate_Song <- SWTH_rate_Song %>%
  mutate(Playback_phase = recode_factor(Playback_phase, "During" = "Noise"))

## Reorder for song rate

SWTH_rate_Song$Playback_phase <- factor(SWTH_rate_Song$Playback_phase,
                                        levels = c("Pre", "Noise", "Post"))

#### TEST LMM ASSUMPTIONS ####

# Set working directory to where you want the figures to go
setwd()

# Define the function
check_assumptions <- function(data, dataset_name, response_vars, combined_results) {
  # Create main output directory if it doesn't exist
  dir.create("rate_LMM_Assumptions2", showWarnings = FALSE)
  
  # Create a subdirectory for the current dataset
  dataset_dir <- file.path("rate_LMM_Assumptions2", dataset_name)
  dir.create(dataset_dir, showWarnings = FALSE)
  
  # Loop over each response variable
  for (response in response_vars) {
    # Define the model formula
    formula <- as.formula(paste(response, "~ Playback_phase + (1 | IndividualID)"))
    
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
  SWTH_rate_Song = SWTH_rate_Song
)

# Response variables
response_vars <- c("Rate")

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
write_xlsx(combined_results, path = file.path("rate_LMM_Assumptions2", "Combined_Assumptions_Test_Results.xlsx"))

#### LMMS BY HAND ####

## SWTH_rate_Song with TOD and track

lmm_SWTH_rate_Song = lmer(Rate ~ Playback_phase + Time.of.day + Exemplar.Track + (1 | IndividualID), data=SWTH_rate_Song)
summary(lmm_SWTH_rate_Song)

## SWTH_rate_Song without TOD and track (using for 5/8/26)
lmm_SWTH_rate_Song = lmer(Rate ~ Playback_phase + (1 | IndividualID), data=SWTH_rate_Song)
summary(lmm_SWTH_rate_Song)

#### FINAL LINE PLOTS FOR SWTH SONG RATE ####

# Calculate means and SE
summary_data <- SWTH_rate_Song %>%
  group_by(IndividualID, Playback_phase) %>%
  summarise(
    mean_value = mean(Rate),
    se = sd(Rate) / sqrt(n())  # Standard Error (SE)
  )

# Identify individuals who have a data point in the "Noise" phase
has_during <- summary_data %>%
  filter(Playback_phase == "Noise") %>%
  pull(IndividualID) %>%
  unique()

# Create the plot
SWTH_song_rate_plot = ggplot(summary_data, aes(x = Playback_phase, y = mean_value, color = as.factor(IndividualID), group = IndividualID)) +
  geom_point(size = 2,alpha = 1) +
  geom_line(data = summary_data %>% filter(IndividualID %in% has_during), 
            linetype = "dashed",alpha = 0.5) +  # Only connect individuals that have a "During" phase
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.1,alpha = 1) +  # Use SE instead of CI
  labs(x = "Playback phase", y = "Song Rate (songs/minute)", color = "bird ID") +
  annotate("text", x=.7, y=9, label= "F", size=10) +
  theme_classic(base_size = 20) +  # Set a base size for all text elements
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  theme(legend.position = "none")

#### FINAL BAR PLOTS FOR SWTH SONG RATE ####

# Calculate means and SE across IndividualID for each Trial_type
summary_data <- SWTH_rate_Song %>%
  group_by(Playback_phase) %>%
  summarise(
    mean_value = mean(Rate),
    se = sd(Rate) / sqrt(n()),  # Standard Error (SE)
    n = n() # Sample size
  )

# Create the plot
SWTH_song_rate_bar = ggplot(summary_data, aes(x = Playback_phase, y = mean_value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightgreen", color = "black") +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +
  labs(x = "Playback phase", y = "Song Rate (songs/minute)") +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  # sets baseline at 0, no padding
  annotate("text", x = .7, y = 4.8, label = "F", size = 10) +
  theme_classic(base_size = 20) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

