# Purpose: Identify the number of missing values in a variable. 
# and perform hot-deck imputation to impute the missing data. 

# Sample data set
library(MASS)
data(survey)
names(survey)

#### --- IDENTIFY THE PROPORTION OF MISSING VALUES IN THE DATA SET --- ####

# Write a function to calculate the % missing per variable
pmiss <- function(x){round(sum(is.na(x))/length(x)*100, 2)}

# Test this function on one variable
pmiss(survey$Smoke)

# Apply this function to all columns in the data set
apply(survey, 2, pmiss)


#### --- IMPUTE MISSING VALUES FOR PULSE --- ####

# Identify row numbers without missing values for Pulse
not.missing <- which(!is.na(survey$Pulse))
not.missing

# Extract the non-missing values for Pulse into a new variable
pulse.nomiss <- survey$Pulse[not.missing]


# Count the # of missing 
nmiss <- sum(is.na(survey$Pulse))
nmiss

# Sample from the vector of non-missing Pulse values, 
# the number of values needed for imputation (nmiss)
# Set the seed here for reproducibility purposes
set.seed(42)
impute.Pulse <- sample(pulse.nomiss, nmiss, replace=TRUE)

# Identify row numbers with missing values for Pulse
is.missing <- which(is.na(survey$Pulse))

# Impute (fill in) the missing values for Pulse with these
# randomly sampled values. 

survey$Pulse[is.missing] <- impute.Pulse

# Confirm no more mismsing
table(is.na(survey$Pulse))


# -- Rinse & repeat for all variables with missing data. 
# Hint: Wrap this imputation process in a function!
