############################################################################### 
####-Clustering R-File-########################################################
############################################################################### 
#Script needs some time to run(~2 minutes on i7 with 16GB RAM) 
#Load packages
if (!require("cluster")) install.packages("cluster")
if (!require("fastcluster")) install.packages("fastcluster")

# Set Working directory
setwd("D:/Uni/SoSe2017/Statistical Programming Languages/Project")

# Read the data sets
dt = readRDS("cluster_data.rds")
dt_usa = readRDS("us_pre.rds")

############################################################################### 
####-Compute dissimiliarty matrix---###########################################
############################################################################### 

# Compute dissimiliarity matrix with the Gower Metric
diss = daisy(dt[, c(4:59, 61:119)], metric = "gower")

############################################################################### 
####-Compute different hierarchical clustering methods-########################
############################################################################### 

clust_single = hclust(diss, method = "single")
clust_complete = hclust(diss, method = "complete")
clust_average = hclust(diss, method = "average")
clust_centroid = hclust(diss, method = "centroid")
clust_mcquitty = hclust(diss, method = "mcquitty")
clust_ward.D = hclust(diss, method = "ward.D")
clust_ward.D2 = hclust(diss, method = "ward.D2")

############################################################################### 
####-Function Clustersizes-####################################################
############################################################################### 

clustersizes = function(clust, max.clusters) {
    # computes the sizes of clusters and returns results in a matrix
    mx = matrix(data = rep(0, max.clusters * max.clusters), nrow = max.clusters, ncol = max.clusters)
    vec = rep(0, max.clusters)
    for (i in 1:max.clusters) {
        helper = cutree(clust, k = i)
        for (j in 1:i) {
            mx[i, j] = sum(helper == j)/(dim(dt)[1])
        }
    }
    return(mx)
}

############################################################################### 
####-Results: Clustersizes-####################################################
############################################################################### 

sz_single = clustersizes(clust_single, 20)
sz_complete = clustersizes(clust_complete, 20)
sz_average = clustersizes(clust_average, 20)
sz_centroid = clustersizes(clust_centroid, 20)
sz_mcquitty = clustersizes(clust_mcquitty, 20)
sz_ward.D = clustersizes(clust_ward.D, 20)
sz_ward.D2 = clustersizes(clust_ward.D2, 20)

sz_test = function(clustersize) {
    size = dim(clustersize)[1]
    vec = rep(0, size)
    for (i in 1:size) {
        vec[i] = max(clustersize[i, ])
    }
    return(vec)
}

plot(sz_test(sz_single), type = "o", ylim = c(0, 1), xlim = c(1, 25), col = "orange3", xlab = "Nr. of cluster", ylab = "Max. clustersize", 
    lty = 3, lwd = 2)
lines(sz_test(sz_complete), type = "o", col = "black")
lines(sz_test(sz_average), type = "o", col = "aquamarine", lty = 5)
lines(sz_test(sz_centroid), type = "o", col = "darkorchid", lty = 4)
lines(sz_test(sz_mcquitty), type = "o", col = "green3")
lines(sz_test(sz_ward.D), type = "o", col = "blue", lty = 3)
lines(sz_test(sz_ward.D2), type = "o", col = "red", lty = 5)
legend(20.5, 0.8, c("single", "complete", "average", "centroid", "mcquitty", "ward.D", "ward.D2"), pch = c(1, 1, 1, 1, 1, 1, 1), lty = c(3, 
    1, 5, 4, 1, 3, 5), col = c("orange3", "black", "aquamarine", "darkorchid", "green3", "red", "blue"))

############################################################################### 
####-How many Trump voters in Us Data?-########################################
############################################################################### 

dt$trump = FALSE
dt$trump[1:1052] = dt_usa$X.question..vote_for_in_us_election == 1
print(paste("Percentage of Trump voters in US Data:", as.character(100 * round(sum(dt$trump)/sum(dt$X.dem..country_code == "US"), digits = 4))))
trump_prop = sum(dt$trump)/sum(dt$X.dem..country_code == "US")

############################################################################### 
####-How many Trump Voters in each Cluster?-###################################
############################################################################### 

trump_voters = function(clust, max.clusters) {
    # computes the proportion of trump voters compared to total US respondents in each cluster and returns results in a matrix
    mx = matrix(data = rep(0, max.clusters * max.clusters), nrow = max.clusters, ncol = max.clusters)
    for (i in 1:max.clusters) {
        helper = cutree(clust, k = i)
        for (j in 1:i) {
            if ((sum((dt$X.dem..country_code == "US") * helper == j) == 0)) {
                mx[i, j] = 0
            } else {
                mx[i, j] = sum(dt$trump * helper == j)/sum((dt$X.dem..country_code == "US") * helper == j)
            }
            
        }
    }
    return(mx)
}

trump_complete = trump_voters(clust_complete, 20)
trump_ward.D = trump_voters(clust_ward.D, 20)
trump_ward.D2 = trump_voters(clust_ward.D2, 20)

############################################################################### 
####-Define a Metric to find large Cluster with many or few Trump voters-######
############################################################################### 

trump_metric = function(trump_voters, clustersizes) {
    # Computes a Metric for relevance of clusters The score becomes higher if the clusters are able to seperate trump and non-trump voters in
    # bigger clusters
    size = dim(trump_voters)[1]
    vec = rep(0, size)
    for (i in 1:size) {
        for (j in 1:i) {
            vec[i] = vec[i] + abs(trump_voters[i, j] - trump_prop) * clustersizes[i, j]/i
        }
    }
    return(vec)
}

metric_complete = trump_metric(trump_complete, sz_complete)
metric_ward.D = trump_metric(trump_ward.D, sz_ward.D)
metric_ward.D2 = trump_metric(trump_ward.D2, sz_ward.D2)

# Plot results for trump_metric
plot(metric_complete, type = "o", ylim = c(0, max(metric_complete, metric_ward.D, metric_ward.D2) + 0.001), ylab = "Metric", xlab = "Nr. of cluster", 
    main = "Comparison of metric for
     different methods and amount of clusters")
points(metric_ward.D, col = "red", type = "o", lty = 3)
points(metric_ward.D2, col = "blue", type = "o", lty = 5)

legend(15, 0.04, c("complete", "ward.D", "ward.D2"), pch = c(1, 1, 1), lty = c(1, 3, 5), col = c("black", "red", "blue"))

# Candidates ward.D 2/3/5, ward.D2 2/4 (local maxima) But the cluster=2 candidates are local maxima because they are by definition better
# than the first split

############################################################################### 
####-Find largest cluster where majority of Trump voters exists-###############
############################################################################### 

large_clust_majority = function(trump_voters, clustersizes) {
    # Finds clusters where the majority of US Voters voted for Trump Returns the largest of these Clusters as Output Message
    size = dim(trump_voters)[1]
    vec = rep(0, 4)
    for (i in 1:size) {
        for (j in 1:i) {
            if (trump_voters[i, j] > 0.5 & clustersizes[i, j] > vec[1]) {
                vec[1] = clustersizes[i, j]
                vec[2] = i
                vec[3] = j
                vec[4] = trump_voters[i, j]
            }
        }
    }
    print(paste("Found largest Cluster with Trump Majority with", as.character(100 * round(vec[1], 3)), "percentage of total respondents in Cluster", 
        as.character(vec[3]), "with", as.character(vec[2]), "Clusters. Majority is:", as.character(round(vec[4], 3))))
}

large_clust_majority(trump_complete, sz_complete)
large_clust_majority(trump_ward.D, sz_ward.D)
large_clust_majority(trump_ward.D2, sz_ward.D2)

# Candidates Complete 18(9),Ward.D 9(7),Ward.D2 7(2)

############################################################################### 
####-Plot Trump vs. Non-trump voters in barplot for each cluster-##############
############################################################################### 

trump_barplot = function(clust, num.clust) {
    # Creates a barplot with all US respondents in a cluster seperated by Trump voters and Non-Trump voters
    counts = table(dt$trump[dt$X.dem..country_code == "US"], cutree(clust, k = num.clust)[dt$X.dem..country_code == "US"])
    
    nm = deparse(substitute(clust))
    
    barplot(counts, col = c("darkblue", "red"), beside = TRUE, xlab = "Nr. of Cluster", ylab = "Amount of US Voters in Cluster", main = paste("Method:", 
        nm, ", Nr. of Cluster:", as.character(num.clust)))
    
    legend("topright", c("Non-Trump", "Trump"), fill = c("blue", "red"))
    
    return(counts)
}

trump_barplot(clust_ward.D, 2)
trump_barplot(clust_ward.D, 5)
trump_barplot(clust_ward.D, 9)
trump_barplot(clust_ward.D2, 2)
trump_barplot(clust_ward.D2, 4)
trump_barplot(clust_ward.D2, 7)
trump_barplot(clust_complete, 18)

############################################################################### 
####-Save Candidate Cluster-###################################################
############################################################################### 

# Candidates from trump_metric: ward.D k=5, ward.D2 k=4 Candidates from large_clust_majority: Complete k=18,Ward.D k=9,Ward.D2 k=7

ward.D_metric = cutree(clust_ward.D, k = 5)
ward.D2_metric = cutree(clust_ward.D2, k = 4)
ward.D_majority = cutree(clust_ward.D, k = 9)
ward.D2_majority = cutree(clust_ward.D2, k = 7)
complete_majority = cutree(clust_complete, k = 18)

out = data.frame(cbind(dt$ID, ward.D_metric, ward.D2_metric, ward.D_majority, ward.D2_majority, complete_majority))
names(out)[1] = "ID"
saveRDS(out, "Cluster.rds")
saveRDS(trump_complete, "trump_complete.rds")
saveRDS(trump_ward.D, "trump_ward.D.rds")
saveRDS(trump_ward.D2, "trump_ward.D2.rds")
saveRDS(sz_complete, "sz_complete.rds")
saveRDS(sz_ward.D, "sz_ward.D.rds")
saveRDS(sz_ward.D2, "sz_ward.D2.rds")
