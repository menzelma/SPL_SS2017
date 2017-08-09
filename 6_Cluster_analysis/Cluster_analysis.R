############################################################################### 
####-Analysis of Cluster Results R-File-#######################################
############################################################################### 

# load packages
if (!require("boot")) install.packages("boot")
library(boot)

# set working directory
setwd("D:/Uni/SoSe2017/Statistical Programming Languages/Project")

# Load the data
dt_DE = readRDS("DE_pre.rds")
dt_FR = readRDS("FR_pre.rds")
dt_GB = readRDS("GB_pre.rds")
dt_clust = readRDS("Cluster.rds")
trump_complete = readRDS("trump_complete.rds")
trump_ward.D = readRDS("trump_ward.D.rds")
trump_ward.D2 = readRDS("trump_ward.D2.rds")
sz_complete = readRDS("sq_complete.rds")
sz_ward.D = readRDS("sz_ward.D.rds")
sz_ward.D2 = readRDS("sz_ward.D2.rds")

# Create data sets with for each country
party_DE = dt_DE[, c(126:131)]
party_FR = dt_FR[, c(126:132)]
party_GB = dt_FR[, c(127:132)]
# Split Cluster indices in one data set per country
DE_clust = dt_clust[c(min(dt_DE$ID):max(dt_DE$ID)), ]
FR_clust = dt_clust[c(min(dt_FR$ID):max(dt_FR$ID)), ]
GB_clust = dt_clust[c(min(dt_GB$ID):max(dt_GB$ID)), ]

# Proportions of AFD/FN/UKIP voters in the three countries
AFD_prop = sum(dt_DE$AFD_first == 1)/dim(dt_DE)[1]
FN_prop = sum(dt_FR$FN_first == 1)/dim(dt_FR)[1]
UKIP_prop = sum(dt_GB$UKIP_first == 1)/dim(dt_GB)[1]

############################################################################### 
####-Computes proportions of AFD/FN/UKIP voters in the cluster-################
############################################################################### 

proportions = function(col_nr_clust, trump_prop, sz, dt_clust. = dt_clust, DE_clust. = DE_clust, FR_clust. = FR_clust, GB_clust. = GB_clust) {
    nr_clust = max(dt_clust.[, col_nr_clust])
    df = data.frame(matrix(nrow = nr_clust, ncol = 4))
    for (i in 1:nr_clust) {
        df[i, 1] = trump_prop[nr_clust, i]/trump_prop[1, 1]
        if (sum(DE_clust.[, col_nr_clust] == i) > 0) {
            df[i, 2] = (sum(party_DE$AFD_first[DE_clust.[, col_nr_clust] == i] == 1)/sum(DE_clust.[, col_nr_clust] == i))/AFD_prop
        } else {
            df[i, 2] = 0
        }
        if (sum(FR_clust.[, col_nr_clust] == i) > 0) {
            df[i, 3] = (sum(party_FR$FN_first[FR_clust.[, col_nr_clust] == i] == 1)/sum(FR_clust.[, col_nr_clust] == i))/FN_prop
        } else {
            df[i, 3] = 0
        }
        if (sum(GB_clust.[, col_nr_clust] == i) > 0) {
            df[i, 4] = (sum(party_GB$UKIP_first[GB_clust.[, col_nr_clust] == i] == 1)/sum(GB_clust.[, col_nr_clust] == i))/UKIP_prop
        } else {
            df[i, 4] = 0
        }
        df[i, 5] = (sz[nr_clust, i]/(1/nr_clust))
    }
    colnames(df) = c("Trump", "AFD", "FN", "UKIP", "Weights")
    return(df)
}

result_ward.D_1 = proportions(2, trump_ward.D, sz_ward.D)
result_ward.D_2 = proportions(3, trump_ward.D, sz_ward.D)
result_ward.D2_1 = proportions(4, trump_ward.D2, sz_ward.D2)
result_ward.D2_2 = proportions(5, trump_ward.D2, sz_ward.D2)
result_ward.D_majority = proportions(6, trump_ward.D, sz_ward.D)
result_ward.D2_majority = proportions(7, trump_ward.D2, sz_ward.D2)
result_complete_majority = proportions(8, trump_complete, sz_complete)

############################################################################### 
####-Plot proportions of AFD/FN/UKIP voters in the cluster-####################
############################################################################### 

plot_result = function(res, title) {
    res = res[order(res[, 1]), ]
    plot(res[, 1], type = "o", col = "black", ylim = c(0, max(res[, c(1:4)]) + 1), ylab = "Noramlized proportions of party voters", main = title, 
        xlab = "Cluster Nr. with ascending proportions of Trump voters in cluster")
    points(res[, 2], type = "o", col = "red")
    points(res[, 3], type = "o", col = "blue")
    points(res[, 4], type = "o", col = "green")
    legend(1, max(res[, c(1:4)]) + 1, colnames(res[, c(1:4)]), pch = c(1, 1, 1, 1), lty = c(1, 1, 1, 1), col = c("black", "red", "blue", "green"))
}

plot_result(result_ward.D_2, "ward.D with 5 clusters")
plot_result(result_ward.D2_2, "ward.D2 with 4 clusters")
plot_result(result_ward.D_majority, "ward.D with 9 clusters")
plot_result(result_ward.D2_majority, "ward.D2 with 7 clusters")
plot_result(result_complete_majority, "complete with 18 clusters")

############################################################################### 
####-Compute weighted correlations between proportions of Trump and prop. of... 
# ...AFD/FN/UKIP voters-########################################################

correl = function(res1, res2, res3, res4, res5) {
    df = data.frame(matrix(nrow = 5, ncol = 3))
    colnames(df) = c("AFD", "FN", "UKIP")
    rownames(df) = c(deparse(substitute(res1)), deparse(substitute(res2)), deparse(substitute(res3)), deparse(substitute(res4)), deparse(substitute(res5)))
    df[1, ] = c(corr(res1[, c(1, 2)], w = res1[, 5]), corr(res1[, c(1, 3)], w = res1[, 5]), corr(res1[, c(1, 4)], w = res1[, 5]))
    df[2, ] = c(corr(res2[, c(1, 2)], w = res2[, 5]), corr(res2[, c(1, 3)], w = res2[, 5]), corr(res2[, c(1, 4)], w = res2[, 5]))
    df[3, ] = c(corr(res3[, c(1, 2)], w = res3[, 5]), corr(res3[, c(1, 3)], w = res3[, 5]), corr(res3[, c(1, 4)], w = res3[, 5]))
    df[4, ] = c(corr(res4[, c(1, 2)], w = res4[, 5]), corr(res4[, c(1, 3)], w = res4[, 5]), corr(res4[, c(1, 4)], w = res4[, 5]))
    df[5, ] = c(corr(res5[, c(1, 2)], w = res5[, 5]), corr(res5[, c(1, 3)], w = res5[, 5]), corr(res5[, c(1, 4)], w = res5[, 5]))
    print("Correaltions")
    return(df)
}

correl(result_ward.D_2, result_ward.D2_2, result_ward.D_majority, result_ward.D2_majority, result_complete_majority)

############################################################################### 
####-Computes proportions of voters in one country in the cluster-#############
############################################################################### 

proportions_country = function(col_nr_clust, trump_prop, sz, country_clust, country_dt, dt_clust. = dt_clust) {
    nr_clust = max(dt_clust.[, col_nr_clust])
    df = data.frame(matrix(nrow = nr_clust, ncol = dim(country_dt)[2] + 2))
    for (i in 1:nr_clust) {
        df[i, 1] = trump_prop[nr_clust, i]/trump_prop[1, 1]
        
        for (j in 1:dim(country_dt)[2]) {
            if (sum(country_clust[, col_nr_clust] == i) > 0) {
                df[i, j + 1] = (sum(country_dt[country_clust[, col_nr_clust] == i, j] == 1)/sum(country_clust[, col_nr_clust] == i))
                print(df[i, j + 1])
            } else {
                df[i, j + 1] = 0
            }
        }
        
        df[i, dim(country_dt)[2] + 2] = (sz[nr_clust, i]/(1/nr_clust))
    }
    colnames(df) = c("Trump", colnames(country_dt), "Weights")
    return(df)
}

DE_ward.D_1 = proportions_country(2, trump_ward.D, sz_ward.D, DE_clust, party_DE)
DE_ward.D_2 = proportions_country(3, trump_ward.D, sz_ward.D, DE_clust, party_DE)
DE_ward.D2_1 = proportions_country(4, trump_ward.D2, sz_ward.D2, DE_clust, party_DE)
DE_ward.D2_2 = proportions_country(5, trump_ward.D2, sz_ward.D2, DE_clust, party_DE)
DE_ward.D_majority = proportions_country(6, trump_ward.D, sz_ward.D, DE_clust, party_DE)
DE_ward.D2_majority = proportions_country(7, trump_ward.D2, sz_ward.D2, DE_clust, party_DE)
DE_complete_majority = proportions_country(8, trump_complete, sz_complete, DE_clust, party_DE)

correl_DE = function(res1, res2, res3, res4, res5) {
    df = data.frame(matrix(nrow = 5, ncol = 6))
    colnames(df) = colnames(party_DE)
    rownames(df) = c(deparse(substitute(res1)), deparse(substitute(res2)), deparse(substitute(res3)), deparse(substitute(res4)), deparse(substitute(res5)))
    df[1, ] = c(corr(res1[, c(1, 2)], w = res1[, 8]), corr(res1[, c(1, 3)], w = res1[, 8]), corr(res1[, c(1, 4)], w = res1[, 8]), corr(res1[, 
        c(1, 5)], w = res1[, 8]), corr(res1[, c(1, 6)], w = res1[, 8]), corr(res1[, c(1, 7)], w = res1[, 8]))
    df[2, ] = c(corr(res2[, c(1, 2)], w = res2[, 8]), corr(res2[, c(1, 3)], w = res2[, 8]), corr(res2[, c(1, 4)], w = res2[, 8]), corr(res2[, 
        c(1, 5)], w = res2[, 8]), corr(res2[, c(1, 6)], w = res2[, 8]), corr(res2[, c(1, 7)], w = res2[, 8]))
    df[3, ] = c(corr(res3[, c(1, 2)], w = res3[, 8]), corr(res3[, c(1, 3)], w = res3[, 8]), corr(res3[, c(1, 4)], w = res3[, 8]), corr(res3[, 
        c(1, 5)], w = res3[, 8]), corr(res3[, c(1, 6)], w = res3[, 8]), corr(res3[, c(1, 7)], w = res3[, 8]))
    df[4, ] = c(corr(res4[, c(1, 2)], w = res4[, 8]), corr(res4[, c(1, 3)], w = res4[, 8]), corr(res4[, c(1, 4)], w = res4[, 8]), corr(res4[, 
        c(1, 5)], w = res4[, 8]), corr(res4[, c(1, 6)], w = res4[, 8]), corr(res4[, c(1, 7)], w = res4[, 8]))
    df[5, ] = c(corr(res5[, c(1, 2)], w = res5[, 8]), corr(res5[, c(1, 3)], w = res5[, 8]), corr(res5[, c(1, 4)], w = res5[, 8]), corr(res5[, 
        c(1, 5)], w = res5[, 8]), corr(res5[, c(1, 6)], w = res5[, 8]), corr(res5[, c(1, 7)], w = res5[, 8]))
    print("Correlations")
    return(df)
}
correl_DE(DE_ward.D_2, DE_ward.D2_2, DE_ward.D_majority, DE_ward.D2_majority, DE_complete_majority)

