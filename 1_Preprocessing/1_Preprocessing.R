###############################################################################
####-Data Preprocessing R-File-################################################
###############################################################################

#Set working directory
setwd('D:/Uni/SoSe2017/Statistical Programming Languages/Project')

#Read the the two data sets (USA and EU)
dt_eur <- read.csv('data_coded_e28.csv')
dt_usa <- read.csv('data_coded_US.csv')

###############################################################################
####-Create ID Column with ascending ID Values-################################
###############################################################################

#Rename the first column in both data sets to 'ID'
names(dt_eur)[1] <- names(dt_usa)[1] <- 'ID'
 
#Set ID values in both data sets
dt_eur$ID <- seq(from=nrow(dt_usa)+1, to=nrow(dt_usa)+nrow(dt_eur))
dt_usa$ID <- seq(from=1, to=nrow(dt_usa))

###############################################################################
####-Are there missing values?-################################################
###############################################################################

sapply(dt_eur, function(x) sum(is.na(x)))
sapply(dt_usa, function(x) sum(is.na(x)))

###############################################################################
####-Householdsize mean imputation-############################################
###############################################################################
imp_eur <- floor(mean(dt_eur$X.dem..household_size,na.rm = TRUE))
imp_usa <- floor(mean(dt_usa$X.dem..household_size,na.rm = TRUE))
#Impute the missing values for the variable Householdsize by the mean
dt_eur$X.dem..household_size[is.na(dt_eur$X.dem..household_size)] <- imp_eur
dt_usa$X.dem..household_size[is.na(dt_usa$X.dem..household_size)] <- imp_usa

rm(imp_eur,imp_usa)
###############################################################################
####-Create a complete data set-###############################################
###############################################################################

#Create a data set that contains all observations from the survey (US+EU)
dt_common <- rbind(dt_usa[,c(1:13,15:41,43:68)],
                   dt_eur[,c(1:13,15:38,40:42,44,45,67:90)])

###############################################################################
####-Create a proper origin variable-##########################################
###############################################################################

#Set home country to origin as helper if respondent´s immigration response is 4
#4 means that the family has been in the country for long time
dt_origin <- data.frame(dt_common$ID)

#Assign values to the countries 
#(5=North america, 1=Western,Northern, Southern Europe, 2=Eastern Europe)
#Following origin code in Survey Codebook
dt_origin$country_origin <- (dt_common$X.dem..country_code=='US')*5+
  (dt_common$X.dem..country_code=='AT')*1+
  (dt_common$X.dem..country_code=='BE')*1+
  (dt_common$X.dem..country_code=='CY')*1+
  (dt_common$X.dem..country_code=='DE')*1+
  (dt_common$X.dem..country_code=='DK')*1+
  (dt_common$X.dem..country_code=='ES')*1+
  (dt_common$X.dem..country_code=='FI')*1+
  (dt_common$X.dem..country_code=='FR')*1+
  (dt_common$X.dem..country_code=='GB')*1+
  (dt_common$X.dem..country_code=='GR')*1+
  (dt_common$X.dem..country_code=='IE')*1+
  (dt_common$X.dem..country_code=='IT')*1+
  (dt_common$X.dem..country_code=='LU')*1+
  (dt_common$X.dem..country_code=='MT')*1+
  (dt_common$X.dem..country_code=='NL')*1+
  (dt_common$X.dem..country_code=='PT')*1+
  (dt_common$X.dem..country_code=='SE')*1+
  (dt_common$X.dem..country_code=='BG')*2+
  (dt_common$X.dem..country_code=='CZ')*2+
  (dt_common$X.dem..country_code=='EE')*2+
  (dt_common$X.dem..country_code=='HR')*2+
  (dt_common$X.dem..country_code=='HU')*2+
  (dt_common$X.dem..country_code=='LT')*2+
  (dt_common$X.dem..country_code=='LV')*2+
  (dt_common$X.dem..country_code=='PL')*2+
  (dt_common$X.dem..country_code=='RO')*2+
  (dt_common$X.dem..country_code=='SI')*2+
  (dt_common$X.dem..country_code=='SK')*2

#Create a helper variable that takes the first value from the origin variable
origin_helper <- as.numeric(sub("^\\D*(\\d+).*$", "\\1", 
                                as.character(dt_common$X.dem..origin)))

#If no value was available, set it to zero
origin_helper[is.na(origin_helper)] <- 0

#Combine the country_origin and the helper variable by adding them up
dt_origin$Origin1 <- (dt_common$X.dem..immigration==4)*dt_origin$country_origin+
  (dt_common$X.dem..immigration!=4)*as.numeric(origin_helper)

#Set all zeroes to NA 
#(there was no value neither in in the country_origin nor in the origin variable)
dt_origin$Origin1[dt_origin$Origin1==0] <- NA

#Delete the two temporal variables
dt_origin$country_origin <- NULL
rm(origin_helper)

###############################################################################
####-Social Media Networks-####################################################
###############################################################################

#Create a new data frame to encode how uses which social networks
dt_social <- data.frame(dt_common$ID)

#create a helper variable and insert the value from 
#social networks regualrly used as string and insert spaces at the beginning and
#the end so that we have always the patter space+value+space in the variable
dt_social$social_media_helper <- 
  paste(' ',as.character(dt_common$X.aud..social_networks_regularly_used),
        ' ',sep='')

#For all possible answers loop go trough the string created above and look if
#pattern space+number+space exists.if true write 1 to the dummy for this value
for (i in 1:16){
  nr <- paste(' ',as.character(i),' ',sep = '')
  dt_social[,i+2] <- grepl(nr,dt_social$social_media_helper,fixed = TRUE)*1
  names(dt_social)[i+2] <- paste('social_media_dummy_',as.character(i),sep = '')
}

#delete helper variable
dt_social$social_media_helper <- NULL

###############################################################################
####-Social Media Activity-####################################################
###############################################################################

#Create a new data frame to encode each respondents 
#first and second most done activites in social networks
dt_social_activity <- data.frame(dt_common$ID)

#Find the position in the answer where the respondent assigned a 1(most often)
# and 2 (second most)
frst <- gregexpr(pattern ='1',
                 as.character(dt_common$X.aud..social_media_activity_rank))
snd <- gregexpr(pattern ='2',
                as.character(dt_common$X.aud..social_media_activity_rank))

#As the order is fixed, the positons in the answer are encoded due to the 
#activity
dt_social_activity$comment_first <- (frst==1)*1
dt_social_activity$share_first <- (frst==5)*1
dt_social_activity$create_first <- (frst==9)*1
dt_social_activity$read_first <- (frst==13)*1
dt_social_activity$connect_first<- (frst==17)*1

dt_social_activity$comment_second <- (snd==1)*1
dt_social_activity$share_second <- (snd==5)*1
dt_social_activity$create_second <- (snd==9)*1
dt_social_activity$read_second <- (snd==13)*1
dt_social_activity$connect_second<- (snd==17)*1

#Remove helper
rm(frst,snd)

###############################################################################
####-Member Organization-######################################################
###############################################################################

#Create data frame to encode the membership in sports organization
dt_member <- data.frame(dt_common$ID)

#If value 1 to 5 are in answer, encode them to dummy variables
dt_member$member_sports <- grepl('1',
                                 as.character(
                                   dt_common$X.aud..member_organization),
                                 fixed = TRUE)*1
dt_member$member_party <- grepl('2',
                                as.character(
                                  dt_common$X.aud..member_organization),
                                fixed = TRUE)*1
dt_member$member_church <- grepl('3',
                                 as.character(
                                   dt_common$X.aud..member_organization),
                                 fixed = TRUE)*1
dt_member$member_other <- grepl('4',
                                as.character(
                                  dt_common$X.aud..member_organization),
                                fixed = TRUE)*1
dt_member$member_none <- grepl('5',
                               as.character(
                                 dt_common$X.aud..member_organization),
                               fixed = TRUE)*1

###############################################################################
####-Most important issue when voting-#########################################
###############################################################################

#Create data frame to encode the most important issues when voting
dt_importance <- data.frame(dt_common$ID)

#Create a helper variable and insert spaces at the beginning and end of the answer
#so that each answer has the form space+value+space
spaces <- paste(' ',
                as.character(
                  dt_common$X.question..important_issues_when_voting),
                ' ',sep='')

dt_importance$importance_helper <- spaces
rm(spaces)

#For each possible answer(1 to 16) encode them to dummy variables 
#if they occur in respondents answer
for (i in 1:16){
  nr <- paste(' ',as.character(i),' ',sep = '')
  dt_importance[,i+2] <- grepl(
    nr,dt_importance$importance_helper,fixed = TRUE)*1
  names(dt_importance)[i+2] <- paste(
    'importance_dummy_',as.character(i),sep = '')
}

###############################################################################
####-Political Leader-#########################################################
###############################################################################

#Create data frame to encode the preferred political leader
dt_leader <- data.frame(dt_common$ID)

#Find the position in the answer where the respondent assigned a 
#1(most preferred) and 2 (second most)
frst <- gregexpr(pattern ='1',
                 as.character(
                   dt_common$X.question..preferred_type_of_political_leader))
snd <- gregexpr(pattern ='2',
                as.character(
                  dt_common$X.question..preferred_type_of_political_leader))

#As the order is fixed, the positons in the answer are encoded due to the 
#leader type
dt_leader$mind_first <- (frst==1)*1
dt_leader$experience_first <- (frst==5)*1
dt_leader$change_first <- (frst==9)*1
dt_leader$cares_first <- (frst==13)*1
dt_leader$solution_first<- (frst==17)*1
dt_leader$people_first<- (frst==21)*1

dt_leader$mind_second <- (snd==1)*1
dt_leader$experience_second <- (snd==5)*1
dt_leader$change_second <- (snd==9)*1
dt_leader$cares_second <- (snd==13)*1
dt_leader$solution_second<- (snd==17)*1
dt_leader$people_second<- (snd==21)*1

#Helper variables are deleted
rm(frst,snd)

###############################################################################
####-Combine User Made Data Sets---############################################
###############################################################################
#Delete unencoded variables and attach the user
#created variables to the complete data set
dt_common[,c(11,29,30,33,43:45)] <- NULL
data_preprocessed <- cbind(dt_common,dt_origin[,2],dt_social[,c(2:17)],
                           dt_social_activity[,c(2:11)],dt_member[,c(2:6)],
                           dt_importance[,c(3:18)],dt_leader[,c(2:13)])

###############################################################################
####-Set answers to NA-########################################################
###############################################################################

#In order to do a clustering ordered type variables instead of factors are used
#to create more expressivness for the similiarity measure.
#As sometimes a option for non-answering the question was given, this option
#has to be set to NA as this option cannot be part of an ordered variable
dt_common$X.dem..income_net_monthly[
  dt_common$X.dem..income_net_monthly==13] <- NA

dt_common$X.dem..education_level[
  dt_common$X.dem..education_level==5] <- NA

dt_common$X.dem..disposable_income[
  dt_common$X.dem..disposable_income==6] <- NA

dt_common$X.dem..household_finances_past12months[
  dt_common$X.dem..household_finances_past12months==6] <- NA

dt_common$X.dem..financial_security[
  dt_common$X.dem..financial_security==5] <- NA

dt_common$X.dem..change_household_finances_next12months[
  dt_common$X.dem..change_household_finances_next12months==6] <- NA

dt_common$X.dem..job_security[
  dt_common$X.dem..job_security==5] <- NA

dt_common$X.dem..status_national_economy[
  dt_common$X.dem..status_national_economy==6] <- NA

dt_common$X.dem..change_economy_country_past12months[
  dt_common$X.dem..change_economy_country_past12months==6] <- NA

dt_common$X.dem..change_household_finances_next12months[
  dt_common$X.dem..change_household_finances_next12months==6] <- NA

dt_common$X.question..likelihood_to_demonstrate[
  dt_common$X.question..likelihood_to_demonstrate==5] <- NA

dt_common$X.question..frequency_of_voting[
  dt_common$X.question..frequency_of_voting==6] <- NA

dt_common$X.question..vote_next_national_election[
  dt_common$X.question..vote_next_national_election==5] <- NA

dt_common$X.question..currentplace_change_past5years[
  dt_common$X.question..currentplace_change_past5years==6] <- NA

dt_common$X.question..hometown_change_past5years[
  dt_common$X.question..hometown_change_past5years==6] <- NA

dt_common$X.question..family_friends_highereducation[
  dt_common$X.question..family_friends_highereducation==7] <- NA

#Create a data set for the clustering approach
data_cluster <- cbind(dt_common,dt_origin[,2],dt_social[,c(2:17)],
                      dt_social_activity[,c(2:11)],dt_member[,c(2:6)],
                      dt_importance[,c(3:18)],dt_leader[,c(2:13)])

###############################################################################
####-Set variable levels-######################################################
###############################################################################

#Set the variable levels for the clustering data
data_cluster[,c(5,7,8,10,12:15,18,40:43,59:119)] <- lapply(
  data_cluster[,c(5,7,8,10,12:15,18,40:43,59:119)], as.factor)
data_cluster[,c(6,11,16,17,19:39,44:58)] <- lapply(
  data_cluster[,c(6,11,16,17,19:39,44:58)], as.ordered)

#Set the variable levels for the complete data set
data_preprocessed[,c(5:8,10:119)] <- lapply(
  data_preprocessed[,c(5:8,10:110)],as.factor)

###############################################################################
####-Data US-##################################################################
###############################################################################

#Finalize US Data set by adding user created variables
dt_usa$X.dem..origin <- dt_origin$Origin1[c(1:1052)]
dt_usa[,c(14,30,31,34,45,46,47)] <- NULL
dt_usa <- cbind(dt_usa,dt_social[c(1:1052),c(2:17)],
                dt_social_activity[c(1:1052),c(2:11)],
                dt_member[c(1:1052),c(2:6)],
                dt_importance[c(1:1052),c(3:18)],
                dt_leader[c(1:1052),c(2:13)])

#Set variable levels in us data set
dt_usa[,c(5:8,10:120)] <- lapply(dt_usa[,c(5:8,10:120)],as.factor)

###############################################################################
####-Ranking Party-############################################################
###############################################################################

#Extract the most favorite and least favorite party for each 
#German respondent

#Create a data frame for the German respondents
dt_rankDE <- data.frame(dt_eur$ID)
frst <- gregexpr(pattern ='1',as.character(dt_eur$X.question..ranking_party_de))
last <- gregexpr(pattern ='6',as.character(dt_eur$X.question..ranking_party_de))

dt_rankDE$Union_first <- (frst==1)*1
dt_rankDE$SPD_first <- (frst==5)*1
dt_rankDE$Linke_first <- (frst==9)*1
dt_rankDE$Gruene_first <- (frst==13)*1
dt_rankDE$FDP_first<- (frst==17)*1
dt_rankDE$AFD_first<- (frst==21)*1

dt_rankDE$Union_last <- (last==1)*1
dt_rankDE$SPD_last <- (last==5)*1
dt_rankDE$Linke_last <- (last==9)*1
dt_rankDE$Gruene_last <- (last==13)*1
dt_rankDE$FDP_last<- (last==17)*1
dt_rankDE$AFD_last<- (last==21)*1
rm(frst,last)

#Extract the most favorite and least favorite party for each 
#respondent from France
dt_rankFR <- data.frame(dt_eur$ID)
frst <- gregexpr(pattern ='1',as.character(dt_eur$X.question..ranking_party_fr))
last <- gregexpr(pattern ='8',as.character(dt_eur$X.question..ranking_party_fr))

dt_rankFR$PS_first <- (frst==1)*1
dt_rankFR$LR_first <- (frst==5)*1
dt_rankFR$PR_first <- (frst==9)*1
dt_rankFR$EELV_first <- (frst==13)*1
dt_rankFR$FN_first<- (frst==17)*1
dt_rankFR$MoDem_first<- (frst==21)*1
dt_rankFR$PCF_first<- (frst==25)*1
dt_rankFR$EM_first<- (frst==29)*1

dt_rankFR$PS_last <- (last==1)*1
dt_rankFR$LR_last <- (last==5)*1
dt_rankFR$PR_last <- (last==9)*1
dt_rankFR$EELV_last <- (last==13)*1
dt_rankFR$FN_last<- (last==17)*1
dt_rankFR$MoDem_last<- (last==21)*1
dt_rankFR$PCF_last<- (last==25)*1
dt_rankFR$EM_last<- (last==29)*1
rm(frst,last)

#Extract the most favorite and least favorite party for each 
#respondent from Great Britain
dt_rankGB <- data.frame(dt_eur$ID)
frst <- gregexpr(pattern ='1',as.character(dt_eur$X.question..ranking_party_gb))
last <- gregexpr(pattern ='6',as.character(dt_eur$X.question..ranking_party_gb))

dt_rankGB$Cons_first <- (frst==1)*1
dt_rankGB$Lab_first <- (frst==5)*1
dt_rankGB$SNP_first <- (frst==9)*1
dt_rankGB$Lib_first <- (frst==13)*1
dt_rankGB$Green_first<- (frst==17)*1
dt_rankGB$UKIP_first<- (frst==21)*1


dt_rankGB$Cons_last <- (last==1)*1
dt_rankGB$Lab_last <- (last==5)*1
dt_rankGB$SNP_last <- (last==9)*1
dt_rankGB$Lib_last <- (last==13)*1
dt_rankGB$Green_last<- (last==17)*1
dt_rankGB$UKIP_last<- (last==21)*1

rm(frst,last)

###############################################################################
####-Data EUR-#################################################################
###############################################################################

#Attach user created variables to the EUR data set
dt_eur$X.dem..origin <- dt_origin$Origin1[c(1053:12335)]

dt_eur <- cbind(dt_eur,dt_social[c(1053:12335),c(2:17)],
                dt_social_activity[c(1053:12335),c(2:11)],
                dt_member[c(1053:12335),c(2:6)],
                dt_importance[c(1053:12335),c(3:18)],
                dt_leader[c(1053:12335),c(2:13)])

#Delete unencoded variables in the EUR data set
dt_eur[,c(14,30,31,34,67,68,69)] <- NULL

#Create data sets for Germany, France and Great Britain each with country
#specific questions
dt_DE <- dt_eur[dt_eur$X.dem..country_code=='DE',]
dt_FR <- dt_eur[dt_eur$X.dem..country_code=='FR',]
dt_GB <- dt_eur[dt_eur$X.dem..country_code=='GB',]

dt_eur[,c(39,42:60)] <- NULL
dt_DE[,c(39,42,44:53,56:60)] <- NULL
dt_DE <- cbind(dt_DE,dt_rankDE[dt_eur$X.dem..country_code=='DE',c(2:13)])
dt_FR[,c(39,42:44,46:53,55,57:60)] <- NULL
dt_FR <- cbind(dt_FR,dt_rankFR[dt_eur$X.dem..country_code=='FR',c(2:17)])
dt_GB[,c(42:52,55:59)] <- NULL
dt_GB <- cbind(dt_GB,dt_rankGB[dt_eur$X.dem..country_code=='GB',c(2:13)])

#Set variable levels in eur,DE,FR,GB data sets
dt_eur[,c(5:8,10:122)] <- lapply(dt_eur[,c(5:8,10:122)],as.factor)
dt_DE[,c(5:8,10:137)] <- lapply(dt_DE[,c(5:8,10:137)],as.factor)
dt_FR[,c(5:8,10:141)] <- lapply(dt_FR[,c(5:8,10:141)],as.factor)
dt_GB[,c(5:8,10:138)] <- lapply(dt_GB[,c(5:8,10:138)],as.factor)

###############################################################################
####-Save data sets to RDS---##################################################
###############################################################################

#Data set for clustering
saveRDS(data_cluster,'cluster_data.rds')
#Complete (US+EUR) data set
saveRDS(data_preprocessed,'data_pre.rds')
#Data set for US
saveRDS(dt_usa,'us_pre.rds')
#Data set for EUR
saveRDS(dt_eur,'eur_pre.rds')
#Data set for Germany
saveRDS(dt_DE,'DE_pre.rds')
#Data set for France
saveRDS(dt_FR,'FR_pre.rds')
#Data set for Great Britain
saveRDS(dt_GB,'GB_pre.rds')