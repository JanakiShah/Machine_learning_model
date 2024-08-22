########################################################################################
# PROJECT       :	Reproductive Justice 
# SPONSOR/PI    : Nina Cesare
# PROGRAM NAME  : Reproductive Justice - comparison to CPC finder 
# DESCRIPTION   : 
# PROGRAMMER    : Janaki Shah
# DATE WRITTEN  : 12/05/2023
########################################################################################
# INPUT FILES   : 	
# OUTPUT FILES  : 	
#######################################################################################

county_fips_fix <- function(fips){
  if(nchar(fips)==4){
    fips <- paste("0", fips, sep="")
  }
  else{
    fips <- fips
  }
  return(fips)
}

## Load packages and data
library(readxl)
library(e1071)
library(caret)
library(dplyr)

dat <- read.csv("//Dept/ReproductiveJusticeProject/08Data/02Raw_Data/da_abrprsr_2021_rw_manualCoding_10pct.csv")


## abortion services
# never ABORTION ALTERNATIVES ORGANIZATIONS (SIC)
# never PREGNANCY COUNSELING SVC & INFORMATION (SIC)
# Never OTHER SOCIAL ADVOCACY ORGANIZATIONS (NAICS)


## CPCs
# always ABORTION ALTERNATIVES ORGANIZATIONS (SIC)
# always FAMILY PLANNING INFORMATION CENTERS (SIC)
# often OTHER SOCIAL ADVOCACY ORGANIZATIONS (NAICS)


## Repeat the same steps for previous years as needed

## Create indicators

dat$socialAdvocacy <- 0
dat$socialAdvocacy[which(dat$NAICS8.DESCRIPTIONS == "OTHER SOCIAL ADVOCACY ORGANIZATIONS")] <- 1

dat$familyPlanningInfo <- 0
dat$familyPlanningInfo[which(dat$SIC6_DESCRIPTIONS == "FAMILY PLANNING INFORMATION CENTERS")] <- 1

dat$abortionAlternative <- 0
dat$abortionAlternative[which(dat$SIC6_DESCRIPTIONS == "ABORTION ALTERNATIVES ORGANIZATIONS")] <- 1

dat$pregCounseling <- 0
dat$pregCounseling[which(dat$SIC6_DESCRIPTIONS == "PREGNANCY COUNSELING SVC & INFORMATION")] <- 1


## recode CPC to numeric 
dat$CPC_recode <- as.character(dat$CPC)
dat$CPC_recode[which(dat$CPC == "Yes")] <- 1
dat$CPC_recode[which(dat$CPC == "No")] <- 0
dat$CPC_recode <- as.numeric(dat$CPC_recode)

dat$LOCATION.EMPLOYEE.SIZE.CODE_recode <- dat$LOCATION.EMPLOYEE.SIZE.CODE
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "A"] <- 1
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "B"] <- 2
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "C"] <- 3
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "D"] <- 4
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "E"] <- 5
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat$LOCATION.EMPLOYEE.SIZE.CODE == "F"] <- 6
dat$LOCATION.EMPLOYEE.SIZE.CODE_recode <- as.numeric(dat$LOCATION.EMPLOYEE.SIZE.CODE_recode)

dat$BUSINESS.STATUS.CODE <- as.numeric(dat$BUSINESS.STATUS.CODE)


## Use dictionary to select variables: "//Dept/Data_Warehouse/05Data_Management/02Data_Definition/Historical Business Full File Layout 2020.xlsx" 
# These seem relevant and are complete
#BUSINESS.STATUS.CODE
#LOCATION.EMPLOYEE.SIZE.CODE 

dat_sub <- dat[,c("CPC_recode","socialAdvocacy","familyPlanningInfo", "abortionAlternative","pregCounseling","LOCATION.EMPLOYEE.SIZE.CODE_recode","BUSINESS.STATUS.CODE")]
dat_sub <- dat_sub[which(!is.na(dat_sub$CPC_recode)),]

set.seed(280)

idx <- sample(1:nrow(dat_sub), 249)

dat_sub_train <- dat_sub[idx,]
dat_sub_test <- dat_sub[-idx,]


# default with factor response:
mod1 <- train(as.factor(CPC_recode) ~ ., data = dat_sub_train[,c("CPC_recode","socialAdvocacy","familyPlanningInfo", "abortionAlternative","pregCounseling")], method = "svmLinear")

print(mod1)
summary(mod1)

# test with train data
dat_sub_test$pred <- predict(mod1, dat_sub_test)

# Check accuracy:
confusionMatrix(table(dat_sub_test$CPC_recode, dat_sub_test$pred)) #




## pretty accurate but not great....test with the hand coded 2019 data as well...

dat_19 <- read.csv("//Dept/ReproductiveJusticeProject/08Data/02Raw_Data/da_abrprsr_2019_rw_manualCoding_10pct_manuallycoded.csv")


dat_19$NAICS8.DESCRIPTIONS <- dat_19$naics8_descriptions
dat_19$SIC6_DESCRIPTIONS <- dat_19$sic6_descriptions

dat_19$socialAdvocacy <- 0
dat_19$socialAdvocacy[which(dat_19$NAICS8.DESCRIPTIONS == "OTHER SOCIAL ADVOCACY ORGANIZATIONS")] <- 1

dat_19$familyPlanningInfo <- 0
dat_19$familyPlanningInfo[which(dat_19$SIC6_DESCRIPTIONS == "FAMILY PLANNING INFORMATION CENTERS")] <- 1

dat_19$abortionAlternative <- 0
dat_19$abortionAlternative[which(dat_19$SIC6_DESCRIPTIONS == "ABORTION ALTERNATIVES ORGANIZATIONS")] <- 1

dat_19$pregCounseling <- 0
dat_19$pregCounseling[which(dat_19$SIC6_DESCRIPTIONS == "PREGNANCY COUNSELING SVC & INFORMATION")] <- 1


## recode CPC to numeric 
dat_19$CPC_recode <- as.character(dat_19$CPC)
dat_19$CPC_recode[which(dat_19$CPC == "Yes")] <- 1
dat_19$CPC_recode[which(dat_19$CPC == "No")] <- 0
dat_19$CPC_recode <- as.numeric(dat_19$CPC_recode)


dat_19$LOCATION.EMPLOYEE.SIZE.CODE <- dat_19$location_employee_size_code
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode <- dat_19$LOCATION.EMPLOYEE.SIZE.CODE
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "A"] <- 1
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "B"] <- 2
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "C"] <- 3
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "D"] <- 4
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "E"] <- 5
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_19$LOCATION.EMPLOYEE.SIZE.CODE == "F"] <- 6
dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode <- as.numeric(dat_19$LOCATION.EMPLOYEE.SIZE.CODE_recode)

dat_19$BUSINESS.STATUS.CODE <- as.numeric(dat_19$business_status_code)

dat_19_sub <- dat_19[which(!is.na(dat_19$CPC_recode)),]

dat_19_sub$pred <- predict(mod1, dat_19_sub)


confusionMatrix(table(dat_19_sub$CPC_recode, dat_19_sub$pred)) # 76% accuracy...meh



##### County analyses ####

dat_full <- read.csv("//Dept/ReproductiveJusticeProject/08Data/02Raw_Data/da_abrprsr_2021_rw.csv")


dat_full$socialAdvocacy <- 0
dat_full$socialAdvocacy[which(dat_full$NAICS8.DESCRIPTIONS == "OTHER SOCIAL ADVOCACY ORGANIZATIONS")] <- 1

dat_full$familyPlanningInfo <- 0
dat_full$familyPlanningInfo[which(dat_full$SIC6_DESCRIPTIONS == "FAMILY PLANNING INFORMATION CENTERS")] <- 1

dat_full$abortionAlternative <- 0
dat_full$abortionAlternative[which(dat_full$SIC6_DESCRIPTIONS == "ABORTION ALTERNATIVES ORGANIZATIONS")] <- 1

dat_full$pregCounseling <- 0
dat_full$pregCounseling[which(dat_full$SIC6_DESCRIPTIONS == "PREGNANCY COUNSELING SVC & INFORMATION")] <- 1


dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode <- dat_full$LOCATION.EMPLOYEE.SIZE.CODE
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "A"] <- 1
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "B"] <- 2
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "C"] <- 3
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "D"] <- 4
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "E"] <- 5
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode[dat_full$LOCATION.EMPLOYEE.SIZE.CODE == "F"] <- 6
dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode <- as.numeric(dat_full$LOCATION.EMPLOYEE.SIZE.CODE_recode)

dat_full$BUSINESS.STATUS.CODE <- as.numeric(dat_full$BUSINESS.STATUS.CODE)


dat_full$pred <- predict(mod1, dat_full)
dat_full$pred <- as.numeric(as.character(dat_full$pred))


county <- read_excel("//ad.bu.edu/bumcfiles/SPH/DCC/Dept/ReproductiveJusticeProject/08Data/04Analytic_Data/SDOH/SDoH_Vars_COUNTY_Dec2023_Deliverable.xlsx")

dat_full_agg <- dat_full %>% group_by(FIPS.CODE) %>% summarize(cpc_count = sum(pred, na.rm = TRUE))
dat_full_agg$FIPS.CODE <- unlist(lapply(dat_full_agg$FIPS.CODE, function(x) county_fips_fix(as.character(x))))
names(dat_full_agg)[1] <- "StCoFIPS"

dat_full_agg_merge <- merge(county, dat_full_agg, by = "StCoFIPS", all = TRUE)
dat_full_agg_merge$cpc_count_std <- dat_full_agg_merge$cpc_count/dat_full_agg_merge$TotalPop2021x19 * 100000

cor_matrix <- cor(dat_full_agg_merge[,-c(1,31)], use = "pairwise.complete.obs")
write.csv(cor_matrix, "//Dept/ReproductiveJusticeProject/07Analyses_and_Reporting/03Analyses/prelim_CPC_correlations_20240319.csv", row.names = TRUE)



dat_full_agg_merge %>% group_by(AHS_URS) %>% summarize(mean_cpc = mean(cpc_count_std, na.rm = TRUE))
# There are more CPCs per 100,000 in rural places

anova(lm(dat_full_agg_merge$cpc_count_std ~ dat_full_agg_merge$AHS_URS))

#
#More CPCs in places with less rental expenditure
#More mobile housing
#More rental housing
#Larger white population
#Less language diversity
#Less education 
#Older populations 
#Fewer medical resources 
#More places of worship
#Less population in the labor force
