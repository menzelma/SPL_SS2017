############################################################################### 
####-Descriptive Analysis R-File-##############################################
############################################################################### 
#Load package
if (!require("gridExtra")) install.packages("gridExtra")

setwd("D:/Uni/SoSe2017/Statistical Programming Languages/Project")

# Load the data
us_data = readRDS("us_pre.rds")
codebook = read.csv("codebook.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Create trump_dummy
us_data$trump_dummy = (us_data$X.question..vote_for_in_us_election == 1) * 1

# Simple statistics of Age, Gender, Education_level and voting behaviour
summary(us_data$X.dem..age)
table(us_data$X.dem..gender)
table(us_data$X.dem..education_level)
table(us_data$X.question..vote_for_in_us_election)

### Descriptive demographics
normalized_table = function(table) {
    trump_voters = sum(us_data$trump_dummy == 1)
    non_trump_voters = sum(us_data$trump_dummy == 0)
    table[, 1] = round(table[, 1]/non_trump_voters, digits = 3)
    table[, 2] = round(table[, 2]/trump_voters, digits = 3)
    return(table)
}

# Create probability tables for demographic variables and compare Trump voters wit Non-trump voters
for (i in c(5:28)) {
    nm = names(us_data)[i]
    nm = unlist(strsplit(nm, split = "..", fixed = TRUE))[2]
    nm = paste("[dem]", nm, sep = " ")
    tb = normalized_table(table(us_data[, i], us_data$trump_dummy))
    colnames(tb) = c("Non-Trump", "Trump")
    dm = dim(tb)[1]
    for (j in c(1:dm)) {
        rownames(tb)[j] = codebook$value[codebook$question_id_mapped == nm & codebook$code == j]
    }
    pdf(paste(nm, ".pdf", sep = ""))
    grid.table(tb)
    dev.off()
}

# Histogram Age with Normal Distribution in it
age = us_data$X.dem..age
hist(age, prob = TRUE, breaks = 50, col = "lightgray", main = "Histogram of age with Normal distribution curve")
curve(dnorm(x, mean = mean(age), sd = sd(age)), add = TRUE, col = "blue")

# Barplot Gender Trump vs. Non-Trump voters Create vectors with relevant variables
gen_trump = us_data$X.dem..gender[us_data$trump_dummy == 1]
gen_non = us_data$X.dem..gender[us_data$trump_dummy == 0]
# Create a merged table
genAll = rbind(table(gen_trump)/length(gen_trump), table(gen_non)/length(gen_non))
# Set rownames
rownames(genAll) = c("Trump", "Non-Trump")
# Plot
barplot(genAll, beside = TRUE, ylim = c(0, 0.7), col = c("red", "blue"), legend.text = TRUE, xlab = "1=Male, 2=Female", ylab = "Proportion", 
    main = "Relative frequency of Gender of Trump vs. Non-Trump voters")

# Mosaic plot Employment status vs. Immigration Create vectors with relevant variables and factor levels
immigr = factor(us_data$X.dem..immigration[us_data$X.dem..employment_status == 1 & us_data$X.dem..immigration != 5 | us_data$X.dem..employment_status == 
    2 & us_data$X.dem..immigration != 5 | us_data$X.dem..employment_status == 3 & us_data$X.dem..immigration != 5])
empl = factor(us_data$X.dem..employment_status[us_data$X.dem..employment_status == 1 & us_data$X.dem..immigration != 5 | us_data$X.dem..employment_status == 
    2 & us_data$X.dem..immigration != 5 | us_data$X.dem..employment_status == 3 & us_data$X.dem..immigration != 5])
# compute contingency table
CTab = xtabs(~immigr + empl)
# visualize contingency table in a mosaic plot
mosaicplot(CTab, cex.axis = 1, ylab = "Employment status", xlab = "Immigration", main = "Mosaic plot: Immigration vs. Employment status")

# Mosaic plot Status national Economy vs. Income Create vectors with relevant variables and factor levels
status = factor(us_data$X.dem..status_national_economy[(us_data$X.dem..status_national_economy == 5 | us_data$X.dem..status_national_economy == 
    1) & us_data$X.dem..income_net_monthly != 13])
income = factor(us_data$X.dem..income_net_monthly[(us_data$X.dem..status_national_economy == 5 | us_data$X.dem..status_national_economy == 
    1) & us_data$X.dem..income_net_monthly != 13])
# Rename factor levels
levels(status)[levels(status) == "1"] = "Very well"
levels(status)[levels(status) == "5"] = "Very poorly"
# compute contingency table
CTab3 = xtabs(~income + status)
# visualize contingency table in a mosaic plot
mosaicplot(CTab3, cex.axis = 1, ylab = "Status national economy", xlab = "Income", main = "Mosaic plot: Income vs. Status national economy")
