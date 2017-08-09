###############################################################################
####-Descriptive Analysis R-File-##############################################
###############################################################################
setwd('D:/Uni/SoSe2017/Statistical Programming Languages/Project')

us_data <- readRDS('us_pre.rds')

us_data$trump_dummy <- (us_data$X.question..vote_for_in_us_election==1)*1

#Simple statistics
summary(us_data$X.dem..age)
table(us_data$X.dem..gender)
table(us_data$X.dem..education_level)
table(us_data$X.question..vote_for_in_us_election)

###Descriptive demographics
normalized_table <- function(table){
  trump_voters <- sum(us_data$trump_dummy==1)
  non_trump_voters <- sum(us_data$trump_dummy==0)
  table[,1] <- table[,1]/non_trump_voters
  table[,2] <- table[,2]/trump_voters
  return(table)
}

for(i in 20){
  print(names(us_data)[i])
  print(normalized_table(table(us_data[,i],us_data$trump_dummy)))
}

#Histogram Age with Normal Distribution in it
age <- us_data$X.dem..age
hist(age, prob=TRUE,breaks = 50,col = 'lightgray')
curve(dnorm(x, mean=mean(age), sd=sd(age)), add=TRUE,col='blue')

#Barplot
gen_trump   <- us_data$X.dem..gender[us_data$trump_dummy==1]
gen_non   <- us_data$X.dem..gender[us_data$trump_dummy==0]
genAll <- rbind(table(gen_trump)/length(gen_trump), table(gen_non)/length(gen_non))
rownames(genAll) <- c("Trump", "Non_Trump")
barplot(rollAll, beside=TRUE,ylim = c(0,0.7), col=c("red", "blue"),
        legend.text=TRUE, xlab="1=Male, 2=Female", ylab="Proportions",
        main="Relative frequency of Gender of Trump vs. Non-Trump voters")

#Mosaic plot
immigr <- factor(us_data$X.dem..immigration[us_data$X.dem..employment_status==1&us_data$X.dem..immigration!=5|us_data$X.dem..employment_status==2&us_data$X.dem..immigration!=5|us_data$X.dem..employment_status==3&us_data$X.dem..immigration!=5])
empl <- factor(us_data$X.dem..employment_status[us_data$X.dem..employment_status==1&us_data$X.dem..immigration!=5|us_data$X.dem..employment_status==2&us_data$X.dem..immigration!=5|us_data$X.dem..employment_status==3&us_data$X.dem..immigration!=5])
CTab <- xtabs(~immigr+empl)
mosaicplot(CTab, cex.axis=1,ylab = 'Employment status',xlab = 'Immigration',main = 'Mosaic plot: Immigration vs. Employment status')

status <- factor(us_data$X.dem..status_national_economy[us_data$X.dem..status_national_economy!=6&us_data$X.dem..job_security!=5])
security <- factor(us_data$X.dem..job_security[us_data$X.dem..status_national_economy!=6&us_data$X.dem..job_security!=5])
CTab2 <- xtabs(~security+status)
mosaicplot(CTab2, cex.axis=1,ylab = 'Status national economy',xlab = 'Job security',main = 'Mosaic plot: Job security vs. Status national economy')

status <- factor(us_data$X.dem..status_national_economy[(us_data$X.dem..status_national_economy==5 |us_data$X.dem..status_national_economy==1)&us_data$X.dem..income_net_monthly!=13])
income <- factor(us_data$X.dem..income_net_monthly[(us_data$X.dem..status_national_economy==5 |us_data$X.dem..status_national_economy==1)&us_data$X.dem..income_net_monthly!=13])
CTab3 <- xtabs(~income+status)
mosaicplot(CTab3, cex.axis=1,ylab = 'Status national economy',xlab = 'Income',main = 'Mosaic plot: Income vs. Status national economy')


