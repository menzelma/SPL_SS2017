############################################################################### 
####-Logistic Regression R-File-###############################################
############################################################################### 
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)

# set working directory
setwd("D:/Uni/SoSe2017/Statistical Programming Languages/Project")

# Read the data
us_data = readRDS("us_pre.rds")
chisq = readRDS("Chi_squared_results_US.rds")

############################################################################### 
####-Data Preparation-#########################################################
############################################################################### 

# Set variable levels for the chi-square test results
chisq[, c(2:6)] = sapply(chisq[, c(2:6)], function(x) as.numeric(x))

# Keep Variables wich passed the tests in X2test function
chisq_cols = chisq$Reject_H0 - chisq$expVal_less1 - chisq$expVal_less5
chisq_cols[chisq_cols < 0] = 0
lr_cols = c(0, 0, 0, 1, chisq_cols)

# Create a weight vector
us_weights = us_data$X.meta..weight

# Pick only variables where Chi-square rejected H_0
us_chisq = us_data[, lr_cols == 1]

# Create Trump dummy
us_chisq$trump_dummy = (us_chisq$X.question..vote_for_in_us_election == 1) * 1

# Remove voted for in the last election
us_chisq$X.question..vote_for_in_us_election = NULL

# Remove variables with missing values
na_remove = sapply(us_chisq, function(y) sum(is.na(y))) == 0
us_final = us_chisq[, na_remove]

x_train = model.matrix(~. - 1, us_final[, c(1:60)])

############################################################################### 
####-Logistic Regression-######################################################
############################################################################### 

glm_model = glm(trump_dummy ~ ., family = binomial(link = "logit"), data = us_final, weights = us_weights)

summary(glm_model)
anova(glm_model, test = "Chisq")
lr_results = exp(cbind(OR = coef(glm_model), confint.default(glm_model)))

############################################################################### 
####-Lasso-####################################################################
############################################################################### 

lasso.model = glmnet(x_train, y = us_final$trump_dummy, alpha = 1, family = "binomial", weights = us_weights)
plot(lasso.model, xvar = "lambda")

lasso.cv = cv.glmnet(x_train, us_final$trump_dummy, us_weights, alpha = 1, nfolds = 10, family = "binomial")
plot(lasso.cv)

coef_lasso = coef(lasso.cv, s = "lambda.min")
