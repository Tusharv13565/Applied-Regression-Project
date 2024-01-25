# Load necessary libraries
library(readxl)
library(dplyr)

# Read the Excel file into 'car'
car <- read_excel("C:/Users/tusha/Downloads/23-testcar-2023-10-25 (1).xlsx")

# Assuming 'car' is your dataset
columns_to_keep <- c("Test Veh Displacement (L)", 
                     "Rated Horsepower", 
                     "# of Cylinders and Rotors", 
                     "# of Gears", 
                     "Equivalent Test Weight (lbs.)",
                     "Axle Ratio",
                     "N/V Ratio",
                     "Test Fuel Type Cd",
                     "THC (g/mi)",
                     "CO (g/mi)",
                     "CO2 (g/mi)",
                     "RND_ADJ_FE",
                     "Drive System Description")

# Filter 'car' dataset for 'Tier 2 Cert Gasoline' and 'All Wheel Drive', selecting numeric columns
filtered_data <- car %>%
  filter(`Test Fuel Type Cd` == "61" & `Drive System Description` == "All Wheel Drive") %>%
  select(all_of(columns_to_keep))

# Filter numeric columns
numeric_data <- filtered_data %>%
  select_if(is.numeric)

# Create scatterplot matrix
pairs(numeric_data)
head(numeric_data)
summary(numeric_data)
str(numeric_data)

# comparision plots 

plot(
  numeric_data$`Rated Horsepower`,
  numeric_data$`Test Veh Displacement (L)`,
  main = 'Displacement vs Horsepower',
  xlab = 'Horsepower',
  ylab = 'Displacement'
)


plot(
  numeric_data$`Rated Horsepower`,
  numeric_data$RND_ADJ_FE,
  main = 'Rated Horsepower vs RND_ADJ_FE',
  xlab = 'Rated Horsepower',
  ylab = 'RND_ADJ_FE'
)

# histograms

hist(numeric_data$`Rated Horsepower`, main = 'Histogram of Rated Horsepower', xlab = 'Rated Horsepower')

hist(numeric_data$`# of Cylinders and Rotors`, main = 'Histogram of # of Cylinders and Rotors', xlab = '# of Cylinders and Rotors')

# Creating a histogram for "# of Gears"
hist(numeric_data$`# of Gears`, main = 'Histogram of # of Gears', xlab = '# of Gears')

hist(numeric_data$`Test Veh Displacement (L)`, main = 'Histogram of Test Veh Displacement (L)', xlab = 'Test Veh Displacement (L)')

hist(numeric_data$`CO2 (g/mi)`, main = 'Histogram of CO2 (g/mi)', xlab = 'CO2 (g/mi)')

mydata <- numeric_data

f_model <- lm(RND_ADJ_FE ~ `Test Veh Displacement (L)` + `# of Cylinders and Rotors` + `# of Gears` + `Rated Horsepower` + `Equivalent Test Weight (lbs.)` + `Axle Ratio` + `THC (g/mi)` + `CO (g/mi)`, data = mydata)

residuals <- residuals(f_model)
fitted_values <- fitted(f_model)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(f_model)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(f_model), residuals(f_model), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(f_model))), fitted(f_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")
summary(f_model)

# with 4 predictors
full_model <- lm(RND_ADJ_FE ~ `THC (g/mi)` + `Rated Horsepower` + `Test Veh Displacement (L)`  + `CO (g/mi)` + 'CO2 (g/mi)', data = mydata)

residuals <- residuals(full_model)
fitted_values <- fitted(full_model)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(full_model)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(full_model), residuals(full_model), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(full_model))), fitted(full_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")

#without (Test Veh Displacement)
reduced_model_1 <- lm(RND_ADJ_FE ~ `Rated Horsepower` + `CO (g/mi)` + `THC (g/mi)`, data = mydata)


residuals <- residuals(reduced_model_1)
fitted_values <- fitted(reduced_model_1)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(reduced_model_1)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_1), residuals(reduced_model_1), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_1))), fitted(reduced_model_1),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")


f_model <- lm(RND_ADJ_FE ~ `Test Veh Displacement (L)` + `# of Cylinders and Rotors` + `# of Gears` + `Rated Horsepower` + `Equivalent Test Weight (lbs.)` + `Axle Ratio` + `THC (g/mi)` + `CO (g/mi)`, data = mydata)

residuals <- residuals(f_model)
fitted_values <- fitted(f_model)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(f_model)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(f_model), residuals(f_model), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(f_model))), fitted(f_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")
summary(f_model)

# with 4 predictors
full_model <- lm(RND_ADJ_FE ~ `Rated Horsepower` + `Test Veh Displacement (L)`  + `CO (g/mi)` + `CO (g/mi)`, data = mydata)

residuals <- residuals(full_model)
fitted_values <- fitted(full_model)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(full_model)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(full_model), residuals(full_model), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(full_model))), fitted(full_model),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")

#without (Test Veh Displacement)
reduced_model_1 <- lm(RND_ADJ_FE ~ `Rated Horsepower` + `CO (g/mi)`, data = mydata)


residuals <- residuals(reduced_model_1)
fitted_values <- fitted(reduced_model_1)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(reduced_model_1)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_1), residuals(reduced_model_1), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_1))), fitted(reduced_model_1),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")



#without (Rated Horsepower)
reduced_model_2 <- lm(RND_ADJ_FE ~ `Test Veh Displacement (L)` + `CO (g/mi)`, data = mydata)

residuals <- residuals(reduced_model_2)
fitted_values <- fitted(reduced_model_2)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

# Extract Cook's distances
model_influence <- influence(reduced_model_1)
cooks_distance <- cooks.distance(reduced_model_1)

# Plot Cook's distances
plot(cooks_distance, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red")  # Add a horizontal line for reference


residuals <- residuals(reduced_model_2)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_2), residuals(reduced_model_2), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_2))), fitted(reduced_model_2),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")
# Extract Cook's distances
model_influence <- influence(reduced_model_2)
cooks_distance <- cooks.distance(reduced_model_2)

# Plot Cook's distances
plot(cooks_distance, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red")  # Add a horizontal line for reference


#without (CO)
reduced_model_3 <- lm(RND_ADJ_FE ~ `Test Veh Displacement (L)` + `Rated Horsepower`, data = mydata)

residuals <- residuals(reduced_model_3)
fitted_values <- fitted(reduced_model_3)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(reduced_model_3)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_3), residuals(reduced_model_3), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_3))), fitted(reduced_model_3),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")
# Extract Cook's distances
model_influence <- influence(reduced_model_3)
cooks_distance <- cooks.distance(reduced_model_3)

# Plot Cook's distances
plot(cooks_distance, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red")  # Add a horizontal line for reference


#without (C0_2)
reduced_model_4 <- lm(RND_ADJ_FE ~ `Test Veh Displacement (L)` + `Rated Horsepower` + `CO (g/mi)`, data = mydata)

residuals <- residuals(reduced_model_4)
fitted_values <- fitted(reduced_model_4)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference


residuals <- residuals(reduced_model_4)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_4), residuals(reduced_model_4), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_4))), fitted(reduced_model_4),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")

# Extract the variables used in your reduced model without CO2
selected_vars <- mydata[, c("RND_ADJ_FE", "Test Veh Displacement (L)", "Rated Horsepower", "CO (g/mi)")]

# Calculate Pearson's correlation coefficients
correlation_matrix <- cor(selected_vars)
print(correlation_matrix)

# Extract Cook's distances
model_influence <- influence(reduced_model_4)
cooks_distance <- cooks.distance(reduced_model_4)

# Plot Cook's distances
plot(cooks_distance, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red")  # Add a horizontal line for reference

#extra

reduced_model_5 <- lm(RND_ADJ_FE ~ `CO2 (g/mi)`, data = mydata)
residuals <- residuals(reduced_model_5)
fitted_values <- fitted(reduced_model_5)

# Create the residual vs. fitted plot
plot(fitted_values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference

residuals <- residuals(reduced_model_5)

# Create a QQ plot
qqnorm(residuals)
qqline(residuals)

plot( hatvalues(reduced_model_5), residuals(reduced_model_5), main = "Leverage vs. Residuals", xlab = "Leverage", ylab = "Residuals")

plot(sqrt(abs(resid(reduced_model_5))), fitted(reduced_model_5),
     xlab = "Fitted Values",
     ylab = "Square Root of Absolute Standardized Residuals",
     main = "Scale-Location Plot")

# Extract Cook's distances
model_influence <- influence(reduced_model_5)
cooks_distance <- cooks.distance(reduced_model_5)

# Plot Cook's distances
plot(cooks_distance, pch = 20, main = "Cook's Distance Plot", xlab = "Observation Number", ylab = "Cook's Distance")
abline(h = 4 / length(cooks_distance), col = "red")  # Add a horizontal line for reference


summary(full_model)
summary(reduced_model_1)
summary(reduced_model_2)
summary(reduced_model_3)
summary(reduced_model_4)
```
summary(reduced_model_5)


Vif(f_model)

