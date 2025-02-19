# Panel Data Analysis Project in R

# 1. Install and Load Necessary Packages
install.packages("tidyverse")
install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

# 2. Load and Prepare Your Data
# Ensure your data is in "long format" (one row per observation per time period).
library(readxl)
d <- read_csv("d.csv")
View(d)

# ID variable (I = Airline) and a time variable (T).
pd <- d %>% rename(id = I, time = T) # Example: Renaming "I" to "id" and "T" to "time"

# Check the structure of your data
str(pd)
head(pd)

# Ensure 'time' is a numeric or integer variable for most panel data functions
pd$time <- as.numeric(pd$time)  # or as.integer()

# 3. Exploratory Data Analysis (EDA)
# Summary statistics
summary(pd)

# Visualize your data (examples)
ggplot(pd, aes(x = time, y = dependent_variable, group = id)) +
  geom_line() +
  facet_wrap(~id) +  # Creates separate plots for each ID
  labs(title = "Time Series of Dependent Variable by ID", x = "Time", y = "Dependent Variable")

# Boxplots to see distribution across IDs
ggplot(d, aes(x = factor(id), y = dependent_variable)) + # factor converts ID to categorical
  geom_boxplot() +
  labs(title = "Distribution of Dependent Variable Across IDs", x = "ID", y = "Dependent Variable")


# 4. Panel Data Models

# a) Pooled OLS (Ignoring panel structure)
pooled_model <- lm(dependent_variable ~ independent_variable1 + independent_variable2, d = data)
summary(pooled_model)

# b) Fixed Effects Model (Within Transformation)
fixed_effects_model <- plm(dependent_variable ~ independent_variable1 + independent_variable2, data = data, index = c("id", "time"), model = "within")
summary(fixed_effects_model)

# c) Random Effects Model
random_effects_model <- plm(dependent_variable ~ independent_variable1 + independent_variable2, data = data, index = c("id", "time"), model = "random")
summary(random_effects_model)

# 5. Model Selection (Choosing between Fixed and Random Effects)
# Hausman Test
phtest(fixed_effects_model, random_effects_model) # Null: Random Effects is consistent

# If p-value is significant (e.g., < 0.05), reject the null, suggesting Fixed Effects is more appropriate.

# 6. Hypothesis Testing and Interpretation
# Examine the coefficients of your chosen model (fixed or random effects).
# Interpret the coefficients in the context of your research question.
# For example: A coefficient of 0.5 for independent_variable1 means a one-unit increase in independent_variable1 is associated with a 0.5-unit increase in dependent_variable, *holding other variables constant*.
 
# 7. Diagnostic Tests
# a) Serial Correlation (in residuals)
pbgtest(fixed_effects_model)  # Breusch-Godfrey test for panel data

# b) Heteroscedasticity
bptest(fixed_effects_model) # Breusch-Pagan test

# 8. Robust Standard Errors (if serial correlation or heteroscedasticity are present)
# Use the `vcovHC` function from the `sandwich` package for robust standard errors.
robust_se <- vcovHC(fixed_effects_model, type = "HC1") # Example: White's heteroscedasticity-consistent SEs
coeftest(fixed_effects_model, vcov = robust_se)

# 9. Further Analysis (if needed)
# Dynamic panel data models (using `plm` or other packages like `systemfit`) if you have lagged dependent variables.
# Instrumental variable estimation if you have endogeneity issues.
# Other advanced panel data techniques as required by your research question.

# 10. Save Results
# You can save your model output and visualizations.
# For example:
# write.csv(data, "cleaned_data.csv", row.names = FALSE)
# ggsave("my_plot.png", my_plot)  # Save a ggplot

# End of Code