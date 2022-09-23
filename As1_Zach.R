
# Load the dataset on the DataFrame named "head_Neck_radio"
head_Neck_radio <- read.csv("D:/stats/Stat_4210/Assignment 1/Head_Neck_Radiomics.csv")

# Create another dataset after keeping deceased countries only
# New dataset is named "Patients_deceased"
Patients_deceased = head_Neck_radio[head_Neck_radio$Status == "Dead", ]

# Print the mean of life expectancy in Patients_deceased
print(mean(Patients_deceased$Age_at_diagnosis))
# Print the mean of Length_of_Survival in Patients_deceased
print(mean(Patients_deceased$Length_of_Survival))

# Generate a scatter plot with life expectancy on y-axis and Length_of_Survival on x-axis
plot(Patients_deceased$Length_of_Survival ~  Patients_deceased$Age_at_diagnosis, ylab="Length_of_Survival in days", xlab="Age in years")

# Run a simple linear regression with life expectancy as dependent variable
# and Length_of_Survival as indeoendent variable, and print the results
# options-scipen is used to suppress scientific notation
options(scipen=99)
regression_results <- lm(Patients_deceased$Length_of_Survival ~  Patients_deceased$Age_at_diagnosis)
print(summary(regression_results))

# Generate a scatter plot again, but this time with linear regression line
plot(Patients_deceased$Length_of_Survival ~  Patients_deceased$Age_at_diagnosis, ylab="Length_of_Survival in days", xlab="Age in years")
abline(regression_results)

Patients_deceased$Length_of_Survival

