############################################################################### 
####-Chi-squared Test R-File-##################################################
############################################################################### 
setwd("D:/Uni/SoSe2017/Statistical Programming Languages/Project")

# Load the dataset
us_data = readRDS("us_pre.rds")

# Create a dummy variable that is 1 if US voters voted for Trump
trump_dummy = (us_data$X.question..vote_for_in_us_election == 1) * 1

# Function Chi-squared test of independence
x2test = function(party_dummy, data, col.nr) {
    # Computes the chisq statistics for a categorical variable vs. a dummy and checks the conditions that the expected frequencies are all
    # bigger than 1 and that less than 20% of the expected frequencies are bigger than 5 Returns a vector with Column Name, Column number,
    # p-Value from Chi-squared test, Indicator if all expected values are bigger than 1,
    
    # Initialize Variables for the condition check
    expVal_less1 = 0
    expVal_less5 = 0
    
    # Compute contingency table
    kt = xtabs(~party_dummy + data[, col.nr])
    
    # Check if expected frequencies are smaller than 1
    if (sum(chisq.test(kt)$expected < 1) >= 1) {
        expVal_less1 = 1
    }
    
    # Check if more than 20 percent of expected values are smaller than 5
    if (sum(chisq.test(kt)$expected >= 5)/(dim(chisq.test(kt)$expected)[1] * dim(chisq.test(kt)$expected)[2]) < 0.8) {
        expVal_less5 = 1
    }
    
    # Compute Chi-squared statistic
    outputX2 = chisq.test(kt)
    
    # get current column name from the data set
    name = colnames(data)[col.nr]
    
    # Check if p-value is smaller than 0.05, if so reject the Null-Hypothesis
    if (outputX2$p.value < 0.05) {
        out = c(name, col.nr, round(outputX2$p.value, 3), 1, expVal_less1, expVal_less5)
    } else {
        out = c(name, col.nr, round(outputX2$p.value, 3), 0, expVal_less1, expVal_less5)
    }
    
    # Give values back
    return(out)
}

# Create data frame to store results from the Chi-squared test
results = data.frame(VarName = character(), Column = numeric(), p_value = numeric(), Reject_H0 = numeric(), expVal_less1 = numeric(), expVal_less5 = numeric(), 
    stringsAsFactors = FALSE)

# init counter
cnt = 1

# iterate over all categorical variables in the data set and check with chi-squared if they are independent of the decision to vote for
# trump
for (i in 5:120) {
    results[cnt, c(1:6)] = x2test(trump_dummy, us_data, i)
    cnt = cnt + 1
}

# Save RDS
saveRDS(results, "Chi_squared_results_US.rds")
