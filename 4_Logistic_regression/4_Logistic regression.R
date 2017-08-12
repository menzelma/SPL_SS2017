###############################################################################
####-Logistic Regression R-File-###############################################
###############################################################################
if (!require("glmnet")) install.packages("glmnet")
if(!require('xtable')) install.packages('xtable')
#set working directory
setwd('D:/Uni/SoSe2017/Statistical Programming Languages/Project')

#Read the data
us_data <- readRDS('us_pre.rds')
chisq <- readRDS('Chi_squared_results_US.rds')

###############################################################################
####-Data Preparation-#########################################################
###############################################################################

#Set variable levels for the chi-square test results
chisq[,c(2:6)] <- sapply(chisq[,c(2:6)],function(x) as.numeric(x))

#Keep Variables wich passed the tests in X2test function
chisq_cols <- chisq$Reject_H0-chisq$expVal_less1-chisq$expVal_less5
chisq_cols[chisq_cols<0] <- 0
lr_cols <- c(0,0,0,1,chisq_cols)

#Create a weight vector
us_weights <- us_data$X.meta..weight

#Pick only variables where Chi-square rejected H_0
us_chisq <- us_data[,lr_cols==1]

#Create Trump dummy
us_chisq$trump_dummy <- (us_chisq$X.question..vote_for_in_us_election==1)*1

#Remove voted for in the last election
us_chisq$X.question..vote_for_in_us_election <- NULL

#Remove variables with missing values 
na_remove <- sapply(us_chisq, function(y) sum(is.na(y)))==0
us_final <- us_chisq[,na_remove]

#Create design matrix by expanding factors to dummys. 
#This is mandatory to use the glmnet package
x_train <- model.matrix( ~ .-1, us_final[,c(1:60)])

###############################################################################
####-Logistic Regression-######################################################
###############################################################################

#Compute the Logistic Regression Model with all variables that passed Chi-squared test
glm_model <- glm(trump_dummy ~.,family=binomial(link='logit'),
                 data=us_final,weights = us_weights)
#Results of the logistic model
summary(glm_model)

#Bind the results to a data frame and output them in a latex table as text file
lr_results <- data.frame(cbind(exp(cbind(OddsRatio = coef(glm_model), confint.default(glm_model))),coef(summary(glm_model))[,4]))
lr_results <- round(lr_results,digits = 4)
colnames(lr_results)[c(2:4)] <- c('2.5% CI','97.5% CI','P-Value')
lr_results_sig <- lr_results[lr_results$`P-Value`<0.05,]
sink("lr_results.txt", append=FALSE, split=FALSE)
xtable(lr_results_sig[,c(1:3)])
sink()

#Compute the anove of the logistic model
anov_lr <- anova(glm_model, test="Chisq")

#Output the anova results
sink('lr_anova.txt',append = FALSE,split = FALSE)
xtable(anov_lr[,c(2,4,5)])
sink()

###############################################################################
####-Lasso-####################################################################
###############################################################################

#Compute the Lasso Logistic Regression model with 10-fold cross validation
set.seed(768)
lasso.cv <- cv.glmnet(x_train,us_final$trump_dummy,
                      us_weights,alpha=1,nfolds = 10,family='binomial',type.measure = 'deviance')

#Visualize the behaviour of the error with changing log-lambda
plot(lasso.cv)

#Return the results of the lambda with minimal error and of the largest lambda that is within on standard error of lambda.min
coef_lambda.min <- coef(lasso.cv,s='lambda.min')
coef_lambda.1se <- coef(lasso.cv,s='lambda.1se')

#Bind the results in a data frame and output them as latex table in a text file
lasso_results.min <- data.frame(name = coef_lambda.min@Dimnames[[1]][coef_lambda.min@i + 1], coeff.min = coef_lambda.min@x,stringsAsFactors = FALSE)
lasso_results.1se <- data.frame(name = coef_lambda.1se@Dimnames[[1]][coef_lambda.1se@i + 1], coefficient = coef_lambda.1se@x,stringsAsFactors = FALSE)

lasso_results.min$coeff.1se <- 0
rownames(lasso_results.min) <- lasso_results.min$name
for(i in c(1:(dim(lasso_results.1se)[1]))){
  check <- TRUE
  cnt <- 1
  while(check==TRUE){
    if(lasso_results.1se$name[i]==lasso_results.min$name[cnt]){
      lasso_results.min$coeff.1se[cnt] <- lasso_results.1se$coefficient[i]
      check <- FALSE
    }else{
      cnt <- cnt+1
    }
  }
}

sink('lasso_result.txt',append = FALSE,split = FALSE)
xtable(lasso_results.min[,c(2,3)])
sink()
