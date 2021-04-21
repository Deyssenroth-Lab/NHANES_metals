####### P30 Pilot Project #######

# Pilot analysis of Phase  (1998-1999) and Phase 5 samples(2006-2009) (urine + blood)
# Title:                                                                            
# Assessment of mercury and other metals in biospecimens from the 
# Strong Heart Family Study
#
#                                                                                                                                                                 
#   TABLE OF CONTENTS                                                               
#   0. Set-up: Load packages, load data, exclusion criteria
#   1. Create extra vars needed for analysis
#   2. Table 1. Distribution of urinary Hg levels, stratified
#   3. Table 2. ICC overall and by sex and study center
#           3A. 
#
#   Contact:                                                                        
#   Anne Nigra, ScM, PhD                                                            
#   Columbia University Mailman School of Public Health                             
#   Environmental Health Sciences                                                   
### end
###### 0. Set-up: Load packages and data ###### 
# Load necessary packages
library(Hmisc)
library(foreign)
library(reshape)
library(Hmisc)
library(reshape)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(colorspace)
library(tidyr)
library(survey)
library(openxlsx)
library(extrafontdb)
library(extrafont)
loadfonts()

convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "," , vector[ 3 ], ")" ) 
  return( vector )
}

######      0A. Load SHFS data, clean and tidy data #########
#Load urinary and blood Hg results from Vesna and other blood metals 
setwd("~/Desktop/Hg Exposure Assessment SHS/P30 Pilot ")
urines<-read.csv("uhgresults9919.csv",stringsAsFactors=FALSE);dim(urines)
bloods<-read.csv("bhgresults241019.csv",stringsAsFactors=FALSE);dim(bloods)
bloodmetals<-read.xlsx("SHFS_Blood_Metals_092020.xlsx")
urines$flag2<-1
table(urines$flag2)
bloods$flag3<-1
table(bloods$flag3)
describe(bloods$idno)
colnames(bloods)[2] <- "s3b_thg"
colnames(bloods)[3] <- "s5b_thg"
colnames(urines)[2] <- "s3u_thg"
colnames(urines)[3] <- "s5u_thg"
describe(bloodmetals$idno)

#Re-format blood metals file
library(tidyr)
myvars<-c("idno","Phase","BlNi")
Ni<-bloodmetals[myvars]
Niwide <- spread(Ni, Phase, BlNi)
colnames(Niwide)[2] <- "s3b_ni"
colnames(Niwide)[3] <- "s5b_ni"

myvars<-c("idno","Phase","BlZn")
Zn<-bloodmetals[myvars]
Znwide <- spread(Zn, Phase, BlZn)
colnames(Znwide)[2] <- "s3b_zn"
colnames(Znwide)[3] <- "s5b_zn"

myvars<-c("idno","Phase","BlPb")
Pb<-bloodmetals[myvars]
Pbwide <- spread(Pb, Phase, BlPb)
colnames(Pbwide)[2] <- "s3b_pb"
colnames(Pbwide)[3] <- "s5b_pb"

myvars<-c("idno","Phase","BlMn")
Mn<-bloodmetals[myvars]
Mnwide <- spread(Mn, Phase, BlMn)
colnames(Mnwide)[2] <- "s3b_mn"
colnames(Mnwide)[3] <- "s5b_mn"

myvars<-c("idno","Phase","BlSe")
Se<-bloodmetals[myvars]
Sewide <- spread(Se, Phase, BlSe)
colnames(Sewide)[2] <- "s3b_se"
colnames(Sewide)[3] <- "s5b_se"

myvars<-c("idno","Phase","BlAs")
As<-bloodmetals[myvars]
Aswide <- spread(As, Phase, BlAs)
colnames(Aswide)[2] <- "s3b_as"
colnames(Aswide)[3] <- "s5b_as"

myvars<-c("idno","Phase","BlCd")
Cd<-bloodmetals[myvars]
Cdwide <- spread(Cd, Phase, BlCd)
colnames(Cdwide)[2] <- "s3b_cd"
colnames(Cdwide)[3] <- "s5b_cd"

# Merge all blood metals together
shfsmetals<-merge(bloods, Niwide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Znwide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Pbwide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Mnwide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Sewide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Aswide,by="idno",all=T)
shfsmetals<-merge(shfsmetals, Cdwide,by="idno",all=T)

# Merge in urine Hg
shfsmetals<-merge(shfsmetals, urines,by="idno",all=T)


#Load pilot sample list
setwd("~/Desktop/Hg Exposure Assessment SHS/P30 Pilot ")
list<-read.csv("sampleparticipantlist.csv");dim(list)
list$flag<-1
table(list$flag)

#Are all of the samples present?
check<-merge(list,shfsmetals,by="idno",all=T)
#check<-merge(check,bloods,by="idno",all=T)

table(check$flag)
table(check$flag2)
table(check$flag3)
# Yes, all sample IDs expected are present
list$flag<-NULL
shfsmetals$flag2<-NULL
shfsmetals$flag3<-NULL

#Load SHFS database
setwd("~/Desktop/DietaryAs in SHFS")
file <- "~/Desktop/DietaryAs in SHFS/SHFS_P345_161010.csv"
shfs <- read.csv(file)
dim(shfs) #3838 1413
describe(shfs$s3age)

dat<-merge(shfsmetals,shfs,by="idno",all.x=T);dim(dat)#150
describe(dat$s3u_thg)
describe(dat$s3b_thg)
describe(dat$idno)

# Blood lead data is in ug/L; new var in ug/dL 
dat$s3b_pb_dL<-dat$s3b_pb / 10
dat$s5b_pb_dL<-dat$s5b_pb / 10




# Export small dataset for Allsion and Marcia:
#myvars<-c("idno","famid","center_n","sex_baseline","s3u_thg","s3b_thg","s5u_thg","s5b_thg",
#          "s3age","s5age","s3bmi","s5bmi","s3smoke","s5smoke")
#hgdata<-dat[myvars]
#write.xlsx(hgdata,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /hgdata2020.xlsx")



### end




######      0B. Load NHANES data, clean and tidy data #########

##----- Load NHANES 1999-2000 and 2007-2008 -----##
setwd("~/Google Drive/Research/As_Mort_Trends/NHANES data files")
# NHANES 1999-2000: Urinary mercury measured in females 16-49 (total mercury in ng/mL); 
#   Total blood Hg and inorganic Hg measured in females 16-49 (as umol/L or ug/L)

# NHANES 2007-2008: Urinary total Hg measured in 1/3 subsample (1/3 all)
#   Total blood Hg and inorganic Hg measured in children aged 1-5 and all females 16-49

##----- Load all NHANES data -----##
# NHANES 1999-2000 is urinary mercury in females 16-49 (URXUHG); blood mercury is children 1-5 and women childbearing age
# NHANES 2007-2008 is urinary mercury in 1/3 subset of all particiapnts >6yrs age; blood mercury is children 1-5 and women childbearing age
# NHANES 2011-2012 has blood selenium, blood manganese, serum zinc, 
# Things that are never available: blood arsenic, blood nickel
#demo
demo9900 <- read.xport("DEMO.XPT")
demo0708 <- read.xport("DEMO_E.XPT")
demo1112 <- read.xport("DEMO_G.XPT")
# blood metals 
hg9900 <- read.xport("LAB06.XPT") # includes blood mercury, lead, cadmium, selenium and urinary Hg
bl0708 <- read.xport("PBCD_E.XPT") # includes blood mercury, lead, cadmium
# other blood metals not in 99/00: As, Mn, Ni, Zn
# other blood metals not in 07/08: As, Mn, Ni, Zn, Se
bl1112 <- read.xport("PBCD_G.XPT")
zn1112<-read.xport("CUSEZN_G.XPT")
# urinary mercury
# urinary Hg for 9900 is in hg9900 with blood Hg
hg0708 <-read.xport("UHG_E.XPT") # urine only URXUHG
# creatinine
cr9900<-read.xport("LAB16.XPT")
cr9900<-cr9900[,c("SEQN","URXUCR")]

# Merge by year
nh9900<-merge(demo9900,hg9900,by="SEQN",all=T)
nh9900<-merge(nh9900,cr9900,by="SEQN",all=T)

nh0708 <-merge(demo0708,hg0708,by="SEQN",all=T)
nh0708 <-merge(nh0708,bl0708,by="SEQN",all=T)

nh1112 <-merge(demo1112,bl1112,by="SEQN",all=T)
nh1112 <-merge(nh1112,zn1112,by="SEQN",all=T)


describe(nh9900$LBXTHG) #N= 2414; blood measured in females ORA
describe(nh0708$LBXTHG) #N= 8266; blood measured in females ORA + children 1-5

describe(nh9900$URXUHG) #N= 1748; urine measured in ORA
describe(nh0708$URXUHG) #N= 2634; urine measured in 1/3 subsample



### end

######      0C. Create extra vars needed for analysis ###### 

##### NHANES 
# URXUHG = ug/L; URXUCR = mg/dL 
# In this case we divide the NHANES URXUCR values by 100 to get them in g/L
nh0708$crgL<-0
nh0708$crgL<-(nh0708$URXUCR) /100
describe(nh0708$crgL) 
describe(dat$s4u_crea) #same scale
nh0708$URXUHG.cr<-nh0708$URXUHG / nh0708$crgL
# URXUHG.cr should be in ug/g creatinine now 

nh9900$crgL<-nh9900$URXUCR /100
describe(nh9900$crgL) 
nh9900$URXUHG.cr<-nh9900$URXUHG / nh9900$crgL

# Convert serum Zn from ug/dL to ug/L
nh1112$BLZN<- nh1112$LBXSZN *10
# Final datasets for urine and blood by year:
urine0708<-nh0708[which(!is.na(nh0708$URXUHG)),] #2,657
describe(urine0708$WTSA2YR) #0 missing, good
urine9900<-nh9900[which(!is.na(nh9900$URXUHG)),] # 2634
describe(urine9900$WTMEC2YR) #0 missing, good

blood0708<-nh0708[which(!is.na(nh0708$LBXTHG)),] #8,266
blood9900<-nh9900[which(!is.na(nh9900$LBXTHG)),] # 2,414

check<-nh9900[which(!is.na(nh9900$URXUHG)),]; dim(check) # 1748
describe(check$LBXTHG) # 73 missing; N= 1675 overlap
both9900<-check[which(!is.na(check$LBXTHG)),];dim(both9900) # 1675
both9900$sumhg<- (both9900$URXUHG.cr)+(both9900$LBXTHG)


check2<-nh0708[which(!is.na(nh0708$URXUHG)),] ;dim(check2) # 2634
describe(check2$LBXTHG) #203 missing; N=2431 overlap
both0708<-check2[which(!is.na(check2$LBXTHG)),];dim(both0708) # 2431
both0708$sumhg<- (both0708$URXUHG.cr)+(both0708$LBXTHG)

library(survey)
urine9900.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=urine9900) # use exam sample weights, only apply to FORA 
blood9900.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=blood9900) # use exam sample weights, only apply to FORA  + kids
both9900.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=both9900) #use smaller weights
#probably incorrect weights for nh9900

# For NH 1112 (blood manganese)
blood1112.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=nh1112) #

urine0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=urine0708) #use urinary sample weights; 1/3 random subsample
blood0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=blood0708) #use exam sample weights; all participants 1yr+
both0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=both0708) #use smaller weights

##### SHFS 
dat$s3u_thg.cr <- dat$s3u_thg / dat$s3u_crea
dat$s5u_thg.cr <- dat$s5u_thg / dat$s5u_crea

# Fix urinary uranium creatinine correction
dat$u_cr_fixed <-dat$u / dat$s4u_crea  
describe(dat$u_cr) # mean =947.7
describe(dat$u_cr_fixed) # mean =0.05
dat$u_cr<-NULL
describe(dat$s4u_crea)

is.factor(dat$s3b_thg)
dat$s3b_thg<-as.numeric(dat$s3b_thg)
dat$s5b_thg<-as.numeric(dat$s5b_thg)

dats3<-dat[which(!is.na(dat$s3b_thg)&!is.na(dat$s3u_thg.cr)),];dim (dats3) #117
dats3$s3sumhg<-(dats3$s3b_thg)+(dats3$s3u_thg.cr)

dats5<-dat[which(!is.na(dat$s5b_thg)&!is.na(dat$s5u_thg.cr)),];dim (dats5) #141
dats5$s5sumhg<-(dats5$s5b_thg)+(dats5$s5u_thg.cr)

# How to correct urinary creatinine?
dat$cd[which(dat$idno==103010)] #0.65 ug/L
dat$cd_cr[which(dat$idno==103010)] #1.63 ug/g creatinine 
dat$s4u_crea[which(dat$idno==103010)] #0.4 mg/dL
# analytes in urine in SHFS codebook are reported as cd_cr(ug/gcreatinine) but are actually ugCd/Lurine divided by mgcreatinine/dL urine

describe(dat$s3u_crea) #mean 1.814 (units listed as mg/dL)
describe(dat$s5u_crea) #mean 1.73 (units listed as mg/dL)
describe(nh0708$URXUCR) #mean 124 (units listed as mg/dL)
# Probably what is happening is that the SHFS has the wrong units listed in codebook
# Probably the units for SHFS are g/L instead of mg/dL
# In this case we divide the NHANES URXUCR values by 100 to get them in g/L

#Phase 3 date: 5/9/1997 - 1/14/1999
#Phase 5 dates: 6/14/2006 - 9/28/2009

# Regress blood arsenic on urinary arsenobetaine to remove seafood
# First, for phase 3:
subdat<-dat[which(!is.na(dat$s3b_as)),]
fit <- glm(log(s3b_as)~log(subdat$ab_kations),
           family=gaussian(link="identity"), dat=subdat)
b <- summary(fit)$coeff[1,1]  						# This is the conditional mean from the model for individuals with 0 log ab_cations (intercept B0, condition mean when AB=1)
subdat$ln.s3b_as <- b             							# Create a variable in the subdatabase with the value of the same baseline conditional mean for everybody- this gives everybody the DMA value that corresponds to the conditional mean
residuals.s3b_as <- residuals(fit,type="response") 			# Save the residuals- this is the part that doesn't depend on AB and also gives you the variability between people
subdat.lowab <- subset(subdat, subdat$ab_kations < 1)
fit.b0 <- glm(log(s3b_as)~offset(ln.s3b_as),    #offset = you are not obtaining a coefficient that is multiplying this variable (b1)- so ln.dma is the same value for everyone (b)
              # SO offset tells you that "this variable is not a real coefficinet- it is just a stand in value- so instead give me another coefficient (alpha0) to calibrate"
              family=gaussian(link="identity"), dat=subdat.lowab) 		 	# Intercept from this model is the estimated constant factor to recalibrate the baseline conditional mean into the adjusted marginal mean
subdat$ln.s3b_as.adj <- summary(fit.b0)$coeff[1,1]+b+residuals.s3b_as	# Add the conditional mean, the calibration factor(marginal mean of the log-transformed DMA)and the residuals
# All the three together are the log-transformed arsenobetaine-adjusted individual DMA levels)
# alpha is the first thing (fit.b0$coeff(1,1), b is the conditional mean, and the residuals are the most important part
describe(subdat$ln.s3b_as.adj)
subdat$s3b_as.adj <- exp(subdat$ln.s3b_as.adj)
describe(subdat$s3b_as.adj)
myvars<-c("idno","s3b_as.adj")
tm<-subdat[myvars]
dat2<-merge(dat,tm,by="idno",all=T)
describe(dat2$s3b_as)
describe(dat2$s3b_as.adj)

# Next for phase 5:
# First, for phase 3:
subdat<-dat[which(!is.na(dat$s5b_as)),]
fit <- glm(log(s5b_as)~log(subdat$ab_kations),
           family=gaussian(link="identity"), dat=subdat)
b <- summary(fit)$coeff[1,1]  						# This is the conditional mean from the model for individuals with 0 log ab_cations (intercept B0, condition mean when AB=1)
subdat$ln.s5b_as <- b             							# Create a variable in the subdatabase with the value of the same baseline conditional mean for everybody- this gives everybody the DMA value that corresponds to the conditional mean
residuals.s5b_as <- residuals(fit,type="response") 			# Save the residuals- this is the part that doesn't depend on AB and also gives you the variability between people
subdat.lowab <- subset(subdat, subdat$ab_kations < 1)
fit.b0 <- glm(log(s5b_as)~offset(ln.s5b_as),    #offset = you are not obtaining a coefficient that is multiplying this variable (b1)- so ln.dma is the same value for everyone (b)
              # SO offset tells you that "this variable is not a real coefficinet- it is just a stand in value- so instead give me another coefficient (alpha0) to calibrate"
              family=gaussian(link="identity"), dat=subdat.lowab) 		 	# Intercept from this model is the estimated constant factor to recalibrate the baseline conditional mean into the adjusted marginal mean
subdat$ln.s5b_as.adj <- summary(fit.b0)$coeff[1,1]+b+residuals.s5b_as	# Add the conditional mean, the calibration factor(marginal mean of the log-transformed DMA)and the residuals
# All the three together are the log-transformed arsenobetaine-adjusted individual DMA levels)
# alpha is the first thing (fit.b0$coeff(1,1), b is the conditional mean, and the residuals are the most important part
describe(subdat$ln.s5b_as.adj)
subdat$s5b_as.adj <- exp(subdat$ln.s5b_as.adj)
describe(subdat$s5b_as.adj)
myvars<-c("idno","s5b_as.adj")
tm<-subdat[myvars]
dat3<-merge(dat2,tm,by="idno",all=T)
describe(dat3$s5b_as)
describe(dat3$s5b_as.adj)

dat<-dat3

#Create categorical education variable 
dat$edu_cat_baseline <- 0*(dat$edu_baseline < 12) + 1*(dat$edu_baseline >=12)
table(dat$edu_cat_baseline)
dat$edu_12_baseline <- "<12"
dat$edu_12_baseline[dat$edu_baseline >=12] <- ">=12"

#Create categorical BMI variable
dat$bmi_cat_baseline <- 0*(dat$bmi_baseline < 25) + 1*(dat$bmi_baseline >=25 & dat$bmi_baseline<30)+
  2*(dat$bmi_baseline>=30)
table(dat$bmi_cat_baseline)

#Create categorical age variable
dat$age_cat_baseline <- 0*(dat$age_baseline < 30) + 1*(dat$age_baseline >=30 & dat$age_baseline<50)+
  2*(dat$age_baseline>=50)
table(dat$age_cat_baseline)

#Dietary variables - rename 
names(dat)[names(dat)=="s4cereals_grams"] <- "cereals"
names(dat)[names(dat)=="s4rice_grams"] <- "rice"
names(dat)[names(dat)=="s4poultry_grams"] <- "poultry"
names(dat)[names(dat)=="s4redmeat_grams"] <- "redmeat"
names(dat)[names(dat)=="s4organmeat_grams"] <- "organmeat"
names(dat)[names(dat)=="s4processmeat_grams"] <- "processmeat"
names(dat)[names(dat)=="s4leafyvege_grams"] <- "leafyvege"
names(dat)[names(dat)=="s4rootvege_grams"] <- "rootvege"
names(dat)[names(dat)=="s4othervege_grams"] <- "othervege"
names(dat)[names(dat)=="s4legumes_grams"] <- "legumes"
names(dat)[names(dat)=="s4nutsandseeds_grams"] <- "nutsandseeds"
names(dat)[names(dat)=="s4fish_grams"] <- "fish"
names(dat)[names(dat)=="s4shellfish_grams"] <- "shellfish"
names(dat)[names(dat)=="s4corn_grams"] <- "corn"
names(dat)[names(dat)=="s4potatoes_grams"] <- "potatoes"
names(dat)[names(dat)=="s4friesandchips_grams"] <- "friesandchips"
names(dat)[names(dat)=="s4eggs_grams"] <- "eggs"
names(dat)[names(dat)=="s4milky_grams"] <- "milky"
names(dat)[names(dat)=="s4chocolate_grams"] <- "chocolate"
names(dat)[names(dat)=="s4mixedfoods_grams"] <- "mixedfoods"
names(dat)[names(dat)=="s4fruit_grams"] <- "fruit"
names(dat)[names(dat)=="s4fruitjuice_grams"] <- "fruitjuice"
names(dat)[names(dat)=="s4sauces_grams"] <- "sauces"
names(dat)[names(dat)=="s4drinks_grams"] <- "drinks"
names(dat)[names(dat)=="s4oilandfat_grams"] <- "oilandfat"
names(dat)[names(dat)=="s4alcohol_grams"] <- "alcohol"
names(dat)[names(dat)=="s4water_grams"] <- "water"

meats <- c("poultry", "redmeat", "organmeat", "processmeat")
seafood <- c("fish", "shellfish")
vegetables <- c("leafyvege", "rootvege", "othervege")
cerealsandrice <- c("cereals", "rice")
seeds <- c("corn", "legumes", "nutsandseeds")
otherfood <- c("potatoes", "friesandchips", "eggs", "milky", "fruit", "fruitjuice", "sauces", "oilandfat", "drinks", "alcohol")

diet.var <- c(meats, seafood, vegetables, cerealsandrice, seeds, otherfood)

# Create tertiles of dietary intake vars for Table 1
#Rice
rice.percentiles <- quantile(dat$rice, probs=c(seq(0,1,0.1),0.33,0.66), na.rm=T)
dat$rice.tert <- NA 
dat$rice.tert <- (
  1 * ( dat$rice <= rice.percentiles[["33%"]] ) +
    2 * ( rice.percentiles[["33%"]] < dat$rice & dat$rice <= rice.percentiles[["66%"]] ) +
    3 * ( rice.percentiles[["66%"]] < dat$rice )
)
table(dat$rice.tert)

# Fish
fish.percentiles <- quantile(dat$fish, probs=c(seq(0,1,0.1),0.33,0.66), na.rm=T)
dat$fish.tert <- NA 
dat$fish.tert <- (
  1 * ( dat$fish <= fish.percentiles[["33%"]] ) +
    2 * ( fish.percentiles[["33%"]] < dat$fish & dat$fish <= fish.percentiles[["66%"]] ) +
    3 * ( fish.percentiles[["66%"]] < dat$fish )
)
table(dat$fish.tert)

# Drinks
drinks.percentiles <- quantile(dat$drinks, probs=c(seq(0,1,0.1),0.33,0.66), na.rm=T)
dat$drinks.tert <- NA 
dat$drinks.tert <- (
  1 * ( dat$drinks <= drinks.percentiles[["33%"]] ) +
    2 * ( drinks.percentiles[["33%"]] < dat$drinks & dat$drinks <= drinks.percentiles[["66%"]] ) +
    3 * ( drinks.percentiles[["66%"]] < dat$drinks )
)
table(dat$drinks.tert)

# Organ meat
organmeat.percentiles <- quantile(dat$organmeat, probs=c(seq(0,1,0.1),0.33,0.66), na.rm=T)
dat$organmeat.tert <- NA 
dat$organmeat.tert <- (
  1 * ( dat$organmeat <= organmeat.percentiles[["33%"]] ) +
    2 * ( organmeat.percentiles[["33%"]] < dat$organmeat & dat$organmeat <= organmeat.percentiles[["66%"]] ) +
    3 * ( organmeat.percentiles[["66%"]] < dat$organmeat )
)
table(dat$organmeat.tert)

# Processed meat
processmeat.percentiles <- quantile(dat$processmeat, probs=c(seq(0,1,0.1),0.33,0.66), na.rm=T)
dat$processmeat.tert <- NA 
dat$processmeat.tert <- (
  1 * ( dat$processmeat <= processmeat.percentiles[["33%"]] ) +
    2 * ( processmeat.percentiles[["33%"]] < dat$processmeat & dat$processmeat <= processmeat.percentiles[["66%"]] ) +
    3 * ( processmeat.percentiles[["66%"]] < dat$processmeat )
)
table(dat$processmeat.tert)


###### 1. Tables of metal distributions: overall and stratified ######
###end
######       Table 1. Urine mercury  ###### 
## Table 1A. Distribution of urine total Hg overall and by study site
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3u_thg)),])
min<-min(dat$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(dat$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3u_thg,na.rm=T)
mean<-mean(dat$s3u_thg, na.rm=T)
gm1<-mean(log(dat$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5u_thg)),])
min<-min(dat$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(dat$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5u_thg,na.rm=T)
mean<-mean(dat$s5u_thg, na.rm=T)
gm1<-mean(log(dat$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3u_thg)),])
min<-min(az$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(az$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3u_thg,na.rm=T)
mean<=mean(az$s3u_thg, na.rm=T)
gm1<-mean(log(az$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5u_thg)),])
min<-min(az$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(az$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5u_thg,na.rm=T)
mean<-mean(az$s5u_thg, na.rm=T)
gm1<-mean(log(az$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3u_thg)),])
min<-min(ok$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(ok$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3u_thg,na.rm=T)
mean<=mean(ok$s3u_thg, na.rm=T)
gm1<-mean(log(ok$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5u_thg)),])
min<-min(ok$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(ok$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5u_thg,na.rm=T)
mean<-mean(ok$s5u_thg, na.rm=T)
gm1<-mean(log(ok$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3u_thg)),])
min<-min(sd$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(sd$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3u_thg,na.rm=T)
mean<=mean(sd$s3u_thg, na.rm=T)
gm1<-mean(log(sd$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5u_thg)),])
min<-min(sd$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(sd$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5u_thg,na.rm=T)
mean<-mean(sd$s5u_thg, na.rm=T)
gm1<-mean(log(sd$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3u_thg)),])
min<-min(males$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(males$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3u_thg,na.rm=T)
mean<=mean(males$s3u_thg, na.rm=T)
gm1<-mean(log(males$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5u_thg)),])
min<-min(males$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(males$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5u_thg,na.rm=T)
mean<-mean(males$s5u_thg, na.rm=T)
gm1<-mean(log(males$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3u_thg)),])
min<-min(females$s3u_thg,na.rm=T)
quant<-as.data.frame(quantile(females$s3u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3u_thg,na.rm=T)
mean<=mean(females$s3u_thg, na.rm=T)
gm1<-mean(log(females$s3u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5u_thg)),])
min<-min(females$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(females$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5u_thg,na.rm=T)
mean<-mean(females$s5u_thg, na.rm=T)
gm1<-mean(log(females$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht1<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht1)[1] <- "N"
rownames(sht1)[2] <- "Min"
rownames(sht1)[10] <- "Max"
rownames(sht1)[11] <- "AM"
rownames(sht1)[12] <- "Geo mean"
rownames(sht1)[13] <- "Group"
colnames(sht1)[1:12]<-"Urinary Hg"

## NHANES ##
# 0708
# Original distribution
nav<-nrow(nh0708[which(!is.na(nh0708$URXUHG)),])
min<-min(nh0708$URXUHG,na.rm=T)
quant<-as.data.frame(svyquantile(~URXUHG, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708$URXUHG,na.rm=T)
mean<-mean(nh0708$URXUHG,na.rm=T)
gm1<-mean(log(nh0708$URXUHG),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(nh9900[which(!is.na(nh9900$URXUHG)),])
min<-min(nh9900$URXUHG,na.rm=T)
quant<-as.data.frame(svyquantile(~URXUHG, urine9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh9900$URXUHG,na.rm=T)
mean<-mean(nh9900$URXUHG,na.rm=T)
gm1<-mean(log(nh9900$URXUHG),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT1<-cbind(NH0708,NH9900)

corr <- cor.test(x=dat$s3u_thg.cr, y=dat$s3b_thg, method = 'spearman')
corr #rho=0.52
corr <- cor.test(x=dat$s5u_thg.cr, y=dat$s5b_thg, method = 'spearman')
corr #rho=0.55

## Table 1A. Distribution of urine total Hg overall and by study site corrected for creatinine
describe(dat$s3u_thg.cr)
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3u_thg.cr)),])
min<-min(dat$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(dat$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3u_thg.cr,na.rm=T)
mean<-mean(dat$s3u_thg.cr, na.rm=T)
gm1<-mean(log(dat$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5u_thg)),])
min<-min(dat$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(dat$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5u_thg,na.rm=T)
mean<-mean(dat$s5u_thg, na.rm=T)
gm1<-mean(log(dat$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3u_thg.cr)),])
min<-min(az$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(az$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3u_thg.cr,na.rm=T)
mean<=mean(az$s3u_thg.cr, na.rm=T)
gm1<-mean(log(az$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5u_thg)),])
min<-min(az$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(az$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5u_thg,na.rm=T)
mean<-mean(az$s5u_thg, na.rm=T)
gm1<-mean(log(az$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3u_thg.cr)),])
min<-min(ok$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(ok$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3u_thg.cr,na.rm=T)
mean<=mean(ok$s3u_thg.cr, na.rm=T)
gm1<-mean(log(ok$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5u_thg)),])
min<-min(ok$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(ok$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5u_thg,na.rm=T)
mean<-mean(ok$s5u_thg, na.rm=T)
gm1<-mean(log(ok$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3u_thg.cr)),])
min<-min(sd$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(sd$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3u_thg.cr,na.rm=T)
mean<=mean(sd$s3u_thg.cr, na.rm=T)
gm1<-mean(log(sd$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5u_thg)),])
min<-min(sd$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(sd$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5u_thg,na.rm=T)
mean<-mean(sd$s5u_thg, na.rm=T)
gm1<-mean(log(sd$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3u_thg.cr)),])
min<-min(males$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(males$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3u_thg.cr,na.rm=T)
mean<=mean(males$s3u_thg.cr, na.rm=T)
gm1<-mean(log(males$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5u_thg)),])
min<-min(males$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(males$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5u_thg,na.rm=T)
mean<-mean(males$s5u_thg, na.rm=T)
gm1<-mean(log(males$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3u_thg.cr)),])
min<-min(females$s3u_thg.cr,na.rm=T)
quant<-as.data.frame(quantile(females$s3u_thg.cr,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3u_thg.cr,na.rm=T)
mean<=mean(females$s3u_thg.cr, na.rm=T)
gm1<-mean(log(females$s3u_thg.cr),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5u_thg)),])
min<-min(females$s5u_thg,na.rm=T)
quant<-as.data.frame(quantile(females$s5u_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5u_thg,na.rm=T)
mean<-mean(females$s5u_thg, na.rm=T)
gm1<-mean(log(females$s5u_thg),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht1A<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht1A)[1] <- "N"
rownames(sht1A)[2] <- "Min"
rownames(sht1A)[10] <- "Max"
rownames(sht1A)[11] <- "AM"
rownames(sht1A)[12] <- "Geo mean"
rownames(sht1A)[13] <- "Group"
colnames(sht1A)[1:12]<-"Urinary Hg"

## NHANES ##
# 0708
# Urine Hg/ urinary creatinine
describe(nh0708$URXUHG.cr)
# 0708
# Original distribution
nav<-nrow(nh0708[which(!is.na(nh0708$URXUHG.cr)),])
min<-min(nh0708$URXUHG.cr,na.rm=T)
quant<-as.data.frame(svyquantile(~URXUHG.cr, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708$URXUHG.cr,na.rm=T)
mean<-mean(nh0708$URXUHG.cr,na.rm=T)
gm1<-mean(log(nh0708$URXUHG.cr),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(nh9900[which(!is.na(nh9900$URXUHG.cr)),])
min<-min(nh9900$URXUHG.cr,na.rm=T)
quant<-as.data.frame(svyquantile(~URXUHG.cr, urine9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh9900$URXUHG.cr,na.rm=T)
mean<-mean(nh9900$URXUHG.cr,na.rm=T)
gm1<-mean(log(nh9900$URXUHG.cr),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT1A<-cbind(NH0708,NH9900)

setwd("~/Desktop/Hg Exposure Assessment SHS/P30 Pilot ")

folder.output <- "outputSept20"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}

write.xlsx(sht1,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable1 UHg.xlsx")
write.xlsx(sht1A,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable1A UHg.xlsx")
write.xlsx(NHT1,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable1 UHg.xlsx")
write.xlsx(NHT1A,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable1A UHg.xlsx")







### end
 
######       Table 2. Blood mercury  ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_thg)),])
min<-min(dat$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_thg,na.rm=T)
mean<-mean(dat$s3b_thg, na.rm=T)
gm1<-mean(log(dat$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_thg)),])
min<-min(dat$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_thg,na.rm=T)
mean<-mean(dat$s5b_thg, na.rm=T)
gm1<-mean(log(dat$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_thg)),])
min<-min(az$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_thg,na.rm=T)
mean<=mean(az$s3b_thg, na.rm=T)
gm1<-mean(log(az$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_thg)),])
min<-min(az$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_thg,na.rm=T)
mean<-mean(az$s5b_thg, na.rm=T)
gm1<-mean(log(az$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_thg)),])
min<-min(ok$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_thg,na.rm=T)
mean<=mean(ok$s3b_thg, na.rm=T)
gm1<-mean(log(ok$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_thg)),])
min<-min(ok$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_thg,na.rm=T)
mean<-mean(ok$s5b_thg, na.rm=T)
gm1<-mean(log(ok$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_thg)),])
min<-min(sd$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_thg,na.rm=T)
mean<=mean(sd$s3b_thg, na.rm=T)
gm1<-mean(log(sd$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_thg)),])
min<-min(sd$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_thg,na.rm=T)
mean<-mean(sd$s5b_thg, na.rm=T)
gm1<-mean(log(sd$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_thg)),])
min<-min(males$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_thg,na.rm=T)
mean<=mean(males$s3b_thg, na.rm=T)
gm1<-mean(log(males$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_thg)),])
min<-min(males$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_thg,na.rm=T)
mean<-mean(males$s5b_thg, na.rm=T)
gm1<-mean(log(males$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_thg)),])
min<-min(females$s3b_thg,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_thg,na.rm=T)
mean<=mean(females$s3b_thg, na.rm=T)
gm1<-mean(log(females$s3b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_thg)),])
min<-min(females$s5b_thg,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_thg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_thg,na.rm=T)
mean<-mean(females$s5b_thg, na.rm=T)
gm1<-mean(log(females$s5b_thg),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht2<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht2)[1] <- "N"
rownames(sht2)[2] <- "Min"
rownames(sht2)[10] <- "Max"
rownames(sht2)[11] <- "AM"
rownames(sht2)[12] <- "Geo mean"
rownames(sht2)[13] <- "Group"
colnames(sht2)[1:12]<-"Blood Hg"

## NHANES ##
# 0708
# Original distribution
nav<-nrow(nh0708[which(!is.na(nh0708$LBXTHG)),])
min<-min(nh0708$LBXTHG,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXTHG, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708$LBXTHG,na.rm=T)
mean<-mean(nh0708$LBXTHG,na.rm=T)
gm1<-mean(log(nh0708$LBXTHG),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(nh9900[which(!is.na(nh9900$LBXTHG)),])
min<-min(nh9900$LBXTHG,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXTHG, blood9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh9900$LBXTHG,na.rm=T)
mean<-mean(nh9900$LBXTHG,na.rm=T)
gm1<-mean(log(nh9900$LBXTHG),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT2<-cbind(NH0708,NH9900)

write.xlsx(sht2,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable2 BHg.xlsx")
write.xlsx(NHT2,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable2 BHg.xlsx")


### end
######       Table 3. Total sum blood + urine mercury  ######
describe(dats3$s3sumhg)
describe(dats5$s5sumhg)
class(dats5$s5sumhg)
## OVERALL
# Phase 3
nav<-nrow(dats3[which(!is.na(dats3$s3sumhg)),])
min<-min(dats3$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(dats3$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dats3$s3sumhg,na.rm=T)
mean<-mean(dats3$s3sumhg, na.rm=T)
gm1<-mean(log(dats3$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dats5[which(!is.na(dats5$s5sumhg)),])
min<-min(dats5$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(dats5$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dats5$s5sumhg,na.rm=T)
mean<-mean(dats5$s5sumhg, na.rm=T)
gm1<-mean(log(dats5$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dats3[which(dats3$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3sumhg)),])
min<-min(az$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(az$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3sumhg,na.rm=T)
mean<-mean(az$s3sumhg, na.rm=T)
gm1<-mean(log(az$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
az<-dats5[which(dats5$center_n=="AZ"),]
nav<-nrow(az[which(!is.na(az$s5sumhg)),])
min<-min(az$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(az$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5sumhg,na.rm=T)
mean<-mean(az$s5sumhg, na.rm=T)
gm1<-mean(log(az$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dats3[which(dats3$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3sumhg)),])
min<-min(ok$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(ok$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3sumhg,na.rm=T)
mean<=mean(ok$s3sumhg, na.rm=T)
gm1<-mean(log(ok$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
ok<-dats5[which(dats5$center_n=="OK"),]
nav<-nrow(ok[which(!is.na(ok$s5sumhg)),])
min<-min(ok$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(ok$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5sumhg,na.rm=T)
mean<-mean(ok$s5sumhg, na.rm=T)
gm1<-mean(log(ok$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dats3[which(dats3$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3sumhg)),])
min<-min(sd$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(sd$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3sumhg,na.rm=T)
mean<=mean(sd$s3sumhg, na.rm=T)
gm1<-mean(log(sd$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
sd<-dats5[which(dats5$center_n=="SD"),]
nav<-nrow(sd[which(!is.na(sd$s5sumhg)),])
min<-min(sd$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(sd$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5sumhg,na.rm=T)
mean<-mean(sd$s5sumhg, na.rm=T)
gm1<-mean(log(sd$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dats3[which(dats3$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3sumhg)),])
min<-min(males$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(males$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3sumhg,na.rm=T)
mean<=mean(males$s3sumhg, na.rm=T)
gm1<-mean(log(males$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
males<-dats5[which(dats5$female==0),]
nav<-nrow(males[which(!is.na(males$s5sumhg)),])
min<-min(males$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(males$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5sumhg,na.rm=T)
mean<-mean(males$s5sumhg, na.rm=T)
gm1<-mean(log(males$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dats3[which(dats3$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3sumhg)),])
min<-min(females$s3sumhg,na.rm=T)
quant<-as.data.frame(quantile(females$s3sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3sumhg,na.rm=T)
mean<=mean(females$s3sumhg, na.rm=T)
gm1<-mean(log(females$s3sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
females<-dats5[which(dats5$female==1),]
nav<-nrow(females[which(!is.na(females$s5sumhg)),])
min<-min(females$s5sumhg,na.rm=T)
quant<-as.data.frame(quantile(females$s5sumhg,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5sumhg,na.rm=T)
mean<-mean(females$s5sumhg, na.rm=T)
gm1<-mean(log(females$s5sumhg),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht3<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht3)[1] <- "N"
rownames(sht3)[2] <- "Min"
rownames(sht3)[10] <- "Max"
rownames(sht3)[11] <- "AM"
rownames(sht3)[12] <- "Geo mean"
rownames(sht3)[13] <- "Group"
colnames(sht3)[1:12]<-"Total Blood+Urine Hg"



## NHANES ##
# 0708
# Original distribution
nav<-nrow(both0708[which(!is.na(both0708$sumhg)),])
min<-min(both0708$sumhg,na.rm=T)
quant<-as.data.frame(svyquantile(~sumhg, both0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(both0708$sumhg,na.rm=T)
mean<-mean(both0708$sumhg,na.rm=T)
gm1<-mean(log(both0708$sumhg),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(both9900[which(!is.na(both9900$sumhg)),])
min<-min(both9900$sumhg,na.rm=T)
quant<-as.data.frame(svyquantile(~sumhg, both9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(both9900$sumhg,na.rm=T)
mean<-mean(both9900$sumhg,na.rm=T)
gm1<-mean(log(both9900$sumhg),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT3<-cbind(NH0708,NH9900)

write.xlsx(sht3,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable3 SumHg.xlsx")
write.xlsx(NHT3,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable3 SumHg.xlsx")



### end



######       Table 4. Blood lead ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_pb_dL)),])
min<-min(dat$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_pb_dL,na.rm=T)
mean<-mean(dat$s3b_pb_dL, na.rm=T)
gm1<-mean(log(dat$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_pb_dL)),])
min<-min(dat$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_pb_dL,na.rm=T)
mean<-mean(dat$s5b_pb_dL, na.rm=T)
gm1<-mean(log(dat$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_pb_dL)),])
min<-min(az$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_pb_dL,na.rm=T)
mean<=mean(az$s3b_pb_dL, na.rm=T)
gm1<-mean(log(az$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_pb_dL)),])
min<-min(az$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_pb_dL,na.rm=T)
mean<-mean(az$s5b_pb_dL, na.rm=T)
gm1<-mean(log(az$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_pb_dL)),])
min<-min(ok$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_pb_dL,na.rm=T)
mean<=mean(ok$s3b_pb_dL, na.rm=T)
gm1<-mean(log(ok$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_pb_dL)),])
min<-min(ok$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_pb_dL,na.rm=T)
mean<-mean(ok$s5b_pb_dL, na.rm=T)
gm1<-mean(log(ok$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_pb_dL)),])
min<-min(sd$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_pb_dL,na.rm=T)
mean<=mean(sd$s3b_pb_dL, na.rm=T)
gm1<-mean(log(sd$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_pb_dL)),])
min<-min(sd$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_pb_dL,na.rm=T)
mean<-mean(sd$s5b_pb_dL, na.rm=T)
gm1<-mean(log(sd$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_pb_dL)),])
min<-min(males$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_pb_dL,na.rm=T)
mean<=mean(males$s3b_pb_dL, na.rm=T)
gm1<-mean(log(males$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_pb_dL)),])
min<-min(males$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_pb_dL,na.rm=T)
mean<-mean(males$s5b_pb_dL, na.rm=T)
gm1<-mean(log(males$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_pb_dL)),])
min<-min(females$s3b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_pb_dL,na.rm=T)
mean<=mean(females$s3b_pb_dL, na.rm=T)
gm1<-mean(log(females$s3b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_pb_dL)),])
min<-min(females$s5b_pb_dL,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_pb_dL,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_pb_dL,na.rm=T)
mean<-mean(females$s5b_pb_dL, na.rm=T)
gm1<-mean(log(females$s5b_pb_dL),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht4<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht4)[1] <- "N"
rownames(sht4)[2] <- "Min"
rownames(sht4)[10] <- "Max"
rownames(sht4)[11] <- "AM"
rownames(sht4)[12] <- "Geo mean"
rownames(sht4)[13] <- "Group"
colnames(sht4)[1:12]<-"Blood Pb"

## NHANES ##
# 0708
# Original distribution
nav<-nrow(nh0708[which(!is.na(nh0708$LBXBPB)),])
min<-min(nh0708$LBXBPB,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBPB, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708$LBXBPB,na.rm=T)
mean<-mean(nh0708$LBXBPB,na.rm=T)
gm1<-mean(log(nh0708$LBXBPB),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(nh9900[which(!is.na(nh9900$LBXBPB)),])
min<-min(nh9900$LBXBPB,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBPB, blood9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh9900$LBXBPB,na.rm=T)
mean<-mean(nh9900$LBXBPB,na.rm=T)
gm1<-mean(log(nh9900$LBXBPB),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT4<-cbind(NH0708,NH9900)

write.xlsx(sht4,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable4 BPb.xlsx")
write.xlsx(NHT4,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable4 BPb.xlsx")







### end


######       Table 5. Blood manganese ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_mn)),])
min<-min(dat$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_mn,na.rm=T)
mean<-mean(dat$s3b_mn, na.rm=T)
gm1<-mean(log(dat$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_mn)),])
min<-min(dat$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_mn,na.rm=T)
mean<-mean(dat$s5b_mn, na.rm=T)
gm1<-mean(log(dat$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_mn)),])
min<-min(az$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_mn,na.rm=T)
mean<=mean(az$s3b_mn, na.rm=T)
gm1<-mean(log(az$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_mn)),])
min<-min(az$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_mn,na.rm=T)
mean<-mean(az$s5b_mn, na.rm=T)
gm1<-mean(log(az$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_mn)),])
min<-min(ok$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_mn,na.rm=T)
mean<=mean(ok$s3b_mn, na.rm=T)
gm1<-mean(log(ok$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_mn)),])
min<-min(ok$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_mn,na.rm=T)
mean<-mean(ok$s5b_mn, na.rm=T)
gm1<-mean(log(ok$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_mn)),])
min<-min(sd$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_mn,na.rm=T)
mean<=mean(sd$s3b_mn, na.rm=T)
gm1<-mean(log(sd$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_mn)),])
min<-min(sd$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_mn,na.rm=T)
mean<-mean(sd$s5b_mn, na.rm=T)
gm1<-mean(log(sd$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_mn)),])
min<-min(males$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_mn,na.rm=T)
mean<=mean(males$s3b_mn, na.rm=T)
gm1<-mean(log(males$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_mn)),])
min<-min(males$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_mn,na.rm=T)
mean<-mean(males$s5b_mn, na.rm=T)
gm1<-mean(log(males$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_mn)),])
min<-min(females$s3b_mn,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_mn,na.rm=T)
mean<=mean(females$s3b_mn, na.rm=T)
gm1<-mean(log(females$s3b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_mn)),])
min<-min(females$s5b_mn,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_mn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_mn,na.rm=T)
mean<-mean(females$s5b_mn, na.rm=T)
gm1<-mean(log(females$s5b_mn),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht5<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht5)[1] <- "N"
rownames(sht5)[2] <- "Min"
rownames(sht5)[10] <- "Max"
rownames(sht5)[11] <- "AM"
rownames(sht5)[12] <- "Geo mean"
rownames(sht5)[13] <- "Group"
colnames(sht5)[1:12]<-"Blood Mn"

## NHANES 1112 only one with Mn available
# 1112
# Original distribution
nav<-nrow(nh1112[which(!is.na(nh1112$LBXBMN)),])
min<-min(nh1112$LBXBMN,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBMN, blood1112.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh1112$LBXBMN,na.rm=T)
mean<-mean(nh1112$LBXBMN,na.rm=T)
gm1<-mean(log(nh1112$LBXBMN),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 1112"
NH1112<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH1112)[1] <- "N"
rownames(NH1112)[2] <- "Min"
rownames(NH1112)[10] <- "Max"
rownames(NH1112)[11] <- "AM"
rownames(NH1112)[12] <- "Geo mean"
rownames(NH1112)[13] <- "Group"

NHT5<-cbind(NH1112)

write.xlsx(sht5,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable5 BMn.xlsx")
write.xlsx(NHT5,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable5 BMn.xlsx")

### end



######       Table 6. Blood cadmium ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_cd)),])
min<-min(dat$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_cd,na.rm=T)
mean<-mean(dat$s3b_cd, na.rm=T)
gm1<-mean(log(dat$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_cd)),])
min<-min(dat$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_cd,na.rm=T)
mean<-mean(dat$s5b_cd, na.rm=T)
gm1<-mean(log(dat$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_cd)),])
min<-min(az$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_cd,na.rm=T)
mean<=mean(az$s3b_cd, na.rm=T)
gm1<-mean(log(az$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_cd)),])
min<-min(az$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_cd,na.rm=T)
mean<-mean(az$s5b_cd, na.rm=T)
gm1<-mean(log(az$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_cd)),])
min<-min(ok$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_cd,na.rm=T)
mean<=mean(ok$s3b_cd, na.rm=T)
gm1<-mean(log(ok$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_cd)),])
min<-min(ok$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_cd,na.rm=T)
mean<-mean(ok$s5b_cd, na.rm=T)
gm1<-mean(log(ok$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_cd)),])
min<-min(sd$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_cd,na.rm=T)
mean<=mean(sd$s3b_cd, na.rm=T)
gm1<-mean(log(sd$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_cd)),])
min<-min(sd$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_cd,na.rm=T)
mean<-mean(sd$s5b_cd, na.rm=T)
gm1<-mean(log(sd$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_cd)),])
min<-min(males$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_cd,na.rm=T)
mean<=mean(males$s3b_cd, na.rm=T)
gm1<-mean(log(males$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_cd)),])
min<-min(males$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_cd,na.rm=T)
mean<-mean(males$s5b_cd, na.rm=T)
gm1<-mean(log(males$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_cd)),])
min<-min(females$s3b_cd,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_cd,na.rm=T)
mean<=mean(females$s3b_cd, na.rm=T)
gm1<-mean(log(females$s3b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_cd)),])
min<-min(females$s5b_cd,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_cd,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_cd,na.rm=T)
mean<-mean(females$s5b_cd, na.rm=T)
gm1<-mean(log(females$s5b_cd),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht6<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht6)[1] <- "N"
rownames(sht6)[2] <- "Min"
rownames(sht6)[10] <- "Max"
rownames(sht6)[11] <- "AM"
rownames(sht6)[12] <- "Geo mean"
rownames(sht6)[13] <- "Group"
colnames(sht6)[1:12]<-"Blood Cd"

## NHANES ##
# 0708
# Original distribution
nav<-nrow(nh0708[which(!is.na(nh0708$LBXBCD)),])
min<-min(nh0708$LBXBCD,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBCD, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708$LBXBCD,na.rm=T)
mean<-mean(nh0708$LBXBCD,na.rm=T)
gm1<-mean(log(nh0708$LBXBCD),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"

## NHANES 9900 ##
# Original distribution
nav<-nrow(nh9900[which(!is.na(nh9900$LBXBCD)),])
min<-min(nh9900$LBXBCD,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBCD, blood9900.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh9900$LBXBCD,na.rm=T)
mean<-mean(nh9900$LBXBCD,na.rm=T)
gm1<-mean(log(nh9900$LBXBCD),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 9900"
NH9900<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH9900)[1] <- "N"
rownames(NH9900)[2] <- "Min"
rownames(NH9900)[10] <- "Max"
rownames(NH9900)[11] <- "AM"
rownames(NH9900)[12] <- "Geo mean"
rownames(NH9900)[13] <- "Group"

NHT6<-cbind(NH0708,NH9900)

write.xlsx(sht6,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable6 BCd.xlsx")
write.xlsx(NHT6,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable6 BCd.xlsx")







### end





######       Table 7. Blood selenium ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_se)),])
min<-min(dat$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_se,na.rm=T)
mean<-mean(dat$s3b_se, na.rm=T)
gm1<-mean(log(dat$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_se)),])
min<-min(dat$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_se,na.rm=T)
mean<-mean(dat$s5b_se, na.rm=T)
gm1<-mean(log(dat$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_se)),])
min<-min(az$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_se,na.rm=T)
mean<=mean(az$s3b_se, na.rm=T)
gm1<-mean(log(az$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_se)),])
min<-min(az$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_se,na.rm=T)
mean<-mean(az$s5b_se, na.rm=T)
gm1<-mean(log(az$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_se)),])
min<-min(ok$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_se,na.rm=T)
mean<=mean(ok$s3b_se, na.rm=T)
gm1<-mean(log(ok$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_se)),])
min<-min(ok$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_se,na.rm=T)
mean<-mean(ok$s5b_se, na.rm=T)
gm1<-mean(log(ok$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_se)),])
min<-min(sd$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_se,na.rm=T)
mean<=mean(sd$s3b_se, na.rm=T)
gm1<-mean(log(sd$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_se)),])
min<-min(sd$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_se,na.rm=T)
mean<-mean(sd$s5b_se, na.rm=T)
gm1<-mean(log(sd$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_se)),])
min<-min(males$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_se,na.rm=T)
mean<=mean(males$s3b_se, na.rm=T)
gm1<-mean(log(males$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_se)),])
min<-min(males$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_se,na.rm=T)
mean<-mean(males$s5b_se, na.rm=T)
gm1<-mean(log(males$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_se)),])
min<-min(females$s3b_se,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_se,na.rm=T)
mean<=mean(females$s3b_se, na.rm=T)
gm1<-mean(log(females$s3b_se),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_se)),])
min<-min(females$s5b_se,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_se,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_se,na.rm=T)
mean<-mean(females$s5b_se, na.rm=T)
gm1<-mean(log(females$s5b_se),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht7<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht7)[1] <- "N"
rownames(sht7)[2] <- "Min"
rownames(sht7)[10] <- "Max"
rownames(sht7)[11] <- "AM"
rownames(sht7)[12] <- "Geo mean"
rownames(sht7)[13] <- "Group"
colnames(sht7)[1:12]<-"Blood Se"

## NHANES 1112 only one with Se available
# 1112
# Original distribution
nav<-nrow(nh1112[which(!is.na(nh1112$LBXBSE)),])
min<-min(nh1112$LBXBSE,na.rm=T)
quant<-as.data.frame(svyquantile(~LBXBSE, blood1112.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh1112$LBXBSE,na.rm=T)
mean<-mean(nh1112$LBXBSE,na.rm=T)
gm1<-mean(log(nh1112$LBXBSE),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 1112"
NH1112<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH1112)[1] <- "N"
rownames(NH1112)[2] <- "Min"
rownames(NH1112)[10] <- "Max"
rownames(NH1112)[11] <- "AM"
rownames(NH1112)[12] <- "Geo mean"
rownames(NH1112)[13] <- "Group"

NHT7<-cbind(NH1112)

write.xlsx(sht7,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable7 BSe.xlsx")
write.xlsx(NHT7,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable7 BSe.xlsx")







### end






######       Table 8. Blood arsenic ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_as)),])
min<-min(dat$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_as,na.rm=T)
mean<-mean(dat$s3b_as, na.rm=T)
gm1<-mean(log(dat$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_as)),])
min<-min(dat$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_as,na.rm=T)
mean<-mean(dat$s5b_as, na.rm=T)
gm1<-mean(log(dat$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_as)),])
min<-min(az$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_as,na.rm=T)
mean<=mean(az$s3b_as, na.rm=T)
gm1<-mean(log(az$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_as)),])
min<-min(az$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_as,na.rm=T)
mean<-mean(az$s5b_as, na.rm=T)
gm1<-mean(log(az$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_as)),])
min<-min(ok$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_as,na.rm=T)
mean<=mean(ok$s3b_as, na.rm=T)
gm1<-mean(log(ok$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_as)),])
min<-min(ok$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_as,na.rm=T)
mean<-mean(ok$s5b_as, na.rm=T)
gm1<-mean(log(ok$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_as)),])
min<-min(sd$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_as,na.rm=T)
mean<=mean(sd$s3b_as, na.rm=T)
gm1<-mean(log(sd$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_as)),])
min<-min(sd$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_as,na.rm=T)
mean<-mean(sd$s5b_as, na.rm=T)
gm1<-mean(log(sd$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_as)),])
min<-min(males$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_as,na.rm=T)
mean<=mean(males$s3b_as, na.rm=T)
gm1<-mean(log(males$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_as)),])
min<-min(males$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_as,na.rm=T)
mean<-mean(males$s5b_as, na.rm=T)
gm1<-mean(log(males$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_as)),])
min<-min(females$s3b_as,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_as,na.rm=T)
mean<=mean(females$s3b_as, na.rm=T)
gm1<-mean(log(females$s3b_as),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_as)),])
min<-min(females$s5b_as,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_as,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_as,na.rm=T)
mean<-mean(females$s5b_as, na.rm=T)
gm1<-mean(log(females$s5b_as),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht8<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht8)[1] <- "N"
rownames(sht8)[2] <- "Min"
rownames(sht8)[10] <- "Max"
rownames(sht8)[11] <- "AM"
rownames(sht8)[12] <- "Geo mean"
rownames(sht8)[13] <- "Group"
colnames(sht8)[1:12]<-"Blood As"

# No NHANES blood As available

write.xlsx(sht8,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable8 BAs.xlsx")







### end









######       Table 9. Blood zinc ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_zn)),])
min<-min(dat$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_zn,na.rm=T)
mean<-mean(dat$s3b_zn, na.rm=T)
gm1<-mean(log(dat$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_zn)),])
min<-min(dat$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_zn,na.rm=T)
mean<-mean(dat$s5b_zn, na.rm=T)
gm1<-mean(log(dat$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_zn)),])
min<-min(az$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_zn,na.rm=T)
mean<=mean(az$s3b_zn, na.rm=T)
gm1<-mean(log(az$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_zn)),])
min<-min(az$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_zn,na.rm=T)
mean<-mean(az$s5b_zn, na.rm=T)
gm1<-mean(log(az$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_zn)),])
min<-min(ok$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_zn,na.rm=T)
mean<=mean(ok$s3b_zn, na.rm=T)
gm1<-mean(log(ok$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_zn)),])
min<-min(ok$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_zn,na.rm=T)
mean<-mean(ok$s5b_zn, na.rm=T)
gm1<-mean(log(ok$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_zn)),])
min<-min(sd$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_zn,na.rm=T)
mean<=mean(sd$s3b_zn, na.rm=T)
gm1<-mean(log(sd$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_zn)),])
min<-min(sd$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_zn,na.rm=T)
mean<-mean(sd$s5b_zn, na.rm=T)
gm1<-mean(log(sd$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_zn)),])
min<-min(males$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_zn,na.rm=T)
mean<=mean(males$s3b_zn, na.rm=T)
gm1<-mean(log(males$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_zn)),])
min<-min(males$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_zn,na.rm=T)
mean<-mean(males$s5b_zn, na.rm=T)
gm1<-mean(log(males$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_zn)),])
min<-min(females$s3b_zn,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_zn,na.rm=T)
mean<=mean(females$s3b_zn, na.rm=T)
gm1<-mean(log(females$s3b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_zn)),])
min<-min(females$s5b_zn,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_zn,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_zn,na.rm=T)
mean<-mean(females$s5b_zn, na.rm=T)
gm1<-mean(log(females$s5b_zn),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht9<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht9)[1] <- "N"
rownames(sht9)[2] <- "Min"
rownames(sht9)[10] <- "Max"
rownames(sht9)[11] <- "AM"
rownames(sht9)[12] <- "Geo mean"
rownames(sht9)[13] <- "Group"
colnames(sht9)[1:12]<-"Blood Zn"

## NHANES 1112 only one with Zn available
# 1112
# Original distribution

nav<-nrow(nh1112[which(!is.na(nh1112$BLZN)),])
min<-min(nh1112$BLZN,na.rm=T)
quant<-as.data.frame(svyquantile(~BLZN, blood1112.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh1112$BLZN,na.rm=T)
mean<-mean(nh1112$BLZN,na.rm=T)
gm1<-mean(log(nh1112$BLZN),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 1112"
NH1112<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH1112)[1] <- "N"
rownames(NH1112)[2] <- "Min"
rownames(NH1112)[10] <- "Max"
rownames(NH1112)[11] <- "AM"
rownames(NH1112)[12] <- "Geo mean"
rownames(NH1112)[13] <- "Group"

NHT9<-cbind(NH1112)

write.xlsx(sht9,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable9 BZn.xlsx")
write.xlsx(NHT9,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/NHTable9 SZn.xlsx")







### end










######       Table 8. Blood nickel ######
## Strong Heart Family Study ##
## OVERALL
# Phase 3
nav<-nrow(dat[which(!is.na(dat$s3b_ni)),])
min<-min(dat$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(dat$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s3b_ni,na.rm=T)
mean<-mean(dat$s3b_ni, na.rm=T)
gm1<-mean(log(dat$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Overall P3"
p3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(dat[which(!is.na(dat$s5b_ni)),])
min<-min(dat$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(dat$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(dat$s5b_ni,na.rm=T)
mean<-mean(dat$s5b_ni, na.rm=T)
gm1<-mean(log(dat$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Overall P5"
p5<-rbind(nav,min,quant,max,mean,gm,group)

## Arizona
az<-dat[which(dat$center_n=="AZ"),]
# Phase 3
nav<-nrow(az[which(!is.na(az$s3b_ni)),])
min<-min(az$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(az$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s3b_ni,na.rm=T)
mean<=mean(az$s3b_ni, na.rm=T)
gm1<-mean(log(az$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"AZ P3"
AZP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(az[which(!is.na(az$s5b_ni)),])
min<-min(az$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(az$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(az$s5b_ni,na.rm=T)
mean<-mean(az$s5b_ni, na.rm=T)
gm1<-mean(log(az$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"AZ P5"
AZP5<-rbind(nav,min,quant,max,mean,gm,group)


## Oklahoma
ok<-dat[which(dat$center_n=="OK"),]
# Phase 3
nav<-nrow(ok[which(!is.na(ok$s3b_ni)),])
min<-min(ok$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(ok$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s3b_ni,na.rm=T)
mean<=mean(ok$s3b_ni, na.rm=T)
gm1<-mean(log(ok$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"OK P3"
OKP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(ok[which(!is.na(ok$s5b_ni)),])
min<-min(ok$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(ok$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(ok$s5b_ni,na.rm=T)
mean<-mean(ok$s5b_ni, na.rm=T)
gm1<-mean(log(ok$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"OK P5"
OKP5<-rbind(nav,min,quant,max,mean,gm,group)

## N/S Dakota
sd<-dat[which(dat$center_n=="SD"),]
# Phase 3
nav<-nrow(sd[which(!is.na(sd$s3b_ni)),])
min<-min(sd$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(sd$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s3b_ni,na.rm=T)
mean<=mean(sd$s3b_ni, na.rm=T)
gm1<-mean(log(sd$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"SD P3"
SDP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(sd[which(!is.na(sd$s5b_ni)),])
min<-min(sd$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(sd$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(sd$s5b_ni,na.rm=T)
mean<-mean(sd$s5b_ni, na.rm=T)
gm1<-mean(log(sd$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"SD P5"
SDP5<-rbind(nav,min,quant,max,mean,gm,group)

## Males
males<-dat[which(dat$female==0),]
# Phase 3
nav<-nrow(males[which(!is.na(males$s3b_ni)),])
min<-min(males$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(males$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s3b_ni,na.rm=T)
mean<=mean(males$s3b_ni, na.rm=T)
gm1<-mean(log(males$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Males P3"
MaP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(males[which(!is.na(males$s5b_ni)),])
min<-min(males$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(males$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(males$s5b_ni,na.rm=T)
mean<-mean(males$s5b_ni, na.rm=T)
gm1<-mean(log(males$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Males P5"
MaP5<-rbind(nav,min,quant,max,mean,gm,group)

## Females
females<-dat[which(dat$female==1),]
# Phase 3
nav<-nrow(females[which(!is.na(females$s3b_ni)),])
min<-min(females$s3b_ni,na.rm=T)
quant<-as.data.frame(quantile(females$s3b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s3b_ni,na.rm=T)
mean<=mean(females$s3b_ni, na.rm=T)
gm1<-mean(log(females$s3b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Females P3"
FeP3<-rbind(nav,min,quant,max,mean,gm,group)

# Phase 5
nav<-nrow(females[which(!is.na(females$s5b_ni)),])
min<-min(females$s5b_ni,na.rm=T)
quant<-as.data.frame(quantile(females$s5b_ni,  probs = c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99), na.rm=TRUE))
max<-max(females$s5b_ni,na.rm=T)
mean<-mean(females$s5b_ni, na.rm=T)
gm1<-mean(log(females$s5b_ni),na.rm=T)
gm<-exp(gm1)
group<-"Females P5"
FeP5<-rbind(nav,min,quant,max,mean,gm,group)

sht10<-cbind(p3,p5,AZP3,AZP5,OKP3,OKP5,SDP3,SDP5,MaP3,MaP5,FeP3,FeP5)
rownames(sht10)[1] <- "N"
rownames(sht10)[2] <- "Min"
rownames(sht10)[10] <- "Max"
rownames(sht10)[11] <- "AM"
rownames(sht10)[12] <- "Geo mean"
rownames(sht10)[13] <- "Group"
colnames(sht10)[1:12]<-"Blood Ni"

# No NHANES blood As available

write.xlsx(sht10,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/SHTable10 BNi.xlsx")



### end

###### 1.1 Correlation matrix of all urine and blood metals ######
myvars<-c("sum_ias_cr","ab_kations_cr", "cd_cr","mo_cr","pb_cr", "w_cr","zn_cr","u_cr_fixed", "se_cr", "s3u_thg.cr","s3b_thg","s3b_pb","s3b_mn", "s3b_cd","s3b_se","s3b_as.adj","s3b_zn","s3b_ni")
cormat<-dat[myvars]
#First for phase 3 metals
vars <- c("uSumiAs", "uAsb", "uCd", "uMo","uPb", "uW","uZn","uU", "uSe", "uHg","bHg","bPb","bMn", "bCd","bSe","bAsAdj","bZn","bNi")
matrix <- rbind(log(dat$sum_ias_cr), log(dat$ab_kations_cr), log(dat$cd_cr), log(dat$mo_cr), 
                log(dat$pb_cr), log(dat$w_cr), log(dat$zn_cr), log(dat$u_cr_fixed), log(dat$se_cr),
                log(dat$s3u_thg.cr), log(dat$s3b_thg), log(dat$s3b_pb), log(dat$s3b_mn), log(dat$s3b_cd),
                log(dat$s3b_se), log(dat$s3b_as.adj), log(dat$s3b_zn), log(dat$s3b_ni)) 
row.names(matrix) <- vars
matrix <- t(matrix)

mat.corr <- round(cor(matrix, use = "pairwise.complete.obs", method="spearman"), 2) 
mat.corr

library(ggcorrplot)
p3<-ggcorrplot(mat.corr, lab=T, type="lower",title="Phase 3 Blood Metals")

#Next for phase 5 metals
vars <- c("uSumiAs", "uAsb", "uCd", "uMo","uPb", "uW","uZn","uU", "uSe", "uHg","bHg","bPb","bMn", "bCd","bSe","bAsAdj","bZn","bNi")
matrix <- rbind(log(dat$sum_ias_cr), log(dat$ab_kations_cr), log(dat$cd_cr), log(dat$mo_cr), 
                log(dat$pb_cr), log(dat$w_cr), log(dat$zn_cr), log(dat$u_cr_fixed), log(dat$se_cr),
                log(dat$s5u_thg.cr), log(dat$s5b_thg), log(dat$s5b_pb), log(dat$s5b_mn), log(dat$s5b_cd),
                log(dat$s5b_se), log(dat$s5b_as.adj), log(dat$s5b_zn), log(dat$s5b_ni)) 
row.names(matrix) <- vars
matrix <- t(matrix)

mat.corr <- round(cor(matrix, use = "pairwise.complete.obs", method="spearman"), 2) 
mat.corr

library(ggcorrplot)
p5<-ggcorrplot(mat.corr, lab=T, type="lower",title="Phase 5 Blood Metals")

library(ggpubr)
pdf(file=paste0( folder.output , "/Correllograms.pdf"), 20, 10)
ggarrange(p3,p5,ncol=2,legend="none")
dev.off()

### end 
###### 1.2 Table 0. Median and IQR of metals by participant characteristics ######
## Set-up

## 

# These are the variables for the rows in Table 1 (we want median, IQR of each metal by category)
metals<-c("sum_ias_cr","ab_kations_cr", "cd_cr","mo_cr","pb_cr", "w_cr","zn_cr","u_cr_fixed", "se_cr", "s3u_thg.cr","s3b_thg","s3b_pb","s3b_mn", "s3b_cd","s3b_se","s3b_as.adj","s3b_zn","s3b_ni")

# # Here we make sure the column headings are in the correct format
# dat$age_baseline <- as.numeric(dat$age_baseline)
# dat$female <- as.factor(dat$female)
# dat$edu_cat_baseline <- as.factor(dat$edu_cat_baseline)
# dat$bmi_baseline <- as.numeric(dat$bmi_baseline)
# dat$smoke_012_baseline <- as.factor(dat$smoke_012_baseline)
# dat$center_n <- as.factor(dat$center_n)


miqr <-  convert.to.ci ( round(quantile(dat$s3b_thg[dat$center_n=="AZ"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )


result <- character();table.metal<-character()
for ( i in metals ){
  
  dat$var <- dat[, i]
  #Numerical Variables
  if ( is.numeric(dat$var) ){
    
    print (i)
    
    AZiqr <-  convert.to.ci ( round(quantile(dat$var[dat$center_n=="AZ"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    SDiqr <-  convert.to.ci ( round(quantile(dat$var[dat$center_n=="SD"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    OKiqr <-  convert.to.ci ( round(quantile(dat$var[dat$center_n=="OK"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    Femaleiqr <-  convert.to.ci ( round(quantile(dat$var[dat$female==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    Maleiqr <-  convert.to.ci ( round(quantile(dat$var[dat$female==0], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    Age0iqr <-  convert.to.ci ( round(quantile(dat$var[dat$age_cat_baseline==0], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    Age1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$age_cat_baseline==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    Age2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$age_cat_baseline==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    CurrentSmokeiqr <-  convert.to.ci ( round(quantile(dat$var[dat$smoke_baseline=="Y"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    FormerSmokeiqr <-  convert.to.ci ( round(quantile(dat$var[dat$smoke_baseline=="E"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    NeverSmokeiqr <-  convert.to.ci ( round(quantile(dat$var[dat$smoke_baseline=="N"], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    BMI0iqr <-  convert.to.ci ( round(quantile(dat$var[dat$bmi_cat_baseline==0], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    BMI1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$bmi_cat_baseline==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    BMI2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$bmi_cat_baseline==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    fish1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$fish.tert==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    fish2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$fish.tert==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    fish3iqr <-  convert.to.ci ( round(quantile(dat$var[dat$fish.tert==3], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    rice1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$rice.tert==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    rice2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$rice.tert==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    rice3iqr <-  convert.to.ci ( round(quantile(dat$var[dat$rice.tert==3], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    drinks1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$drinks.tert==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    drinks2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$drinks.tert==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    drinks3iqr <-  convert.to.ci ( round(quantile(dat$var[dat$drinks.tert==3], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    organmeat1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$organmeat.tert==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    organmeat2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$organmeat.tert==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    organmeat3iqr <-  convert.to.ci ( round(quantile(dat$var[dat$organmeat.tert==3], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    processmeat1iqr <-  convert.to.ci ( round(quantile(dat$var[dat$processmeat.tert==1], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    processmeat2iqr <-  convert.to.ci ( round(quantile(dat$var[dat$processmeat.tert==2], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    processmeat3iqr <-  convert.to.ci ( round(quantile(dat$var[dat$processmeat.tert==3], probs=c(0.50, 0.25,0.75), na.rm=T), digits=2) )
    
  
    sample.size <- sum(!is.na(dat$var))
    result <- c (paste0(i),sample.size, AZiqr, SDiqr, OKiqr, Femaleiqr,Maleiqr, 
                 Age0iqr,Age1iqr,Age2iqr, CurrentSmokeiqr,FormerSmokeiqr,NeverSmokeiqr,
                 BMI0iqr,BMI1iqr,BMI2iqr, fish1iqr,fish2iqr,fish3iqr,
                 rice1iqr,rice2iqr,rice3iqr,drinks1iqr,drinks2iqr,drinks3iqr,
                 organmeat1iqr,organmeat2iqr,organmeat3iqr,processmeat1iqr,processmeat2iqr,processmeat3iqr)
  }
  table.metal <- rbind (table.metal, result)
  colnames(table.metal)<-c("Metal","N", "AZ","SD","OK","Female","Male",
                           "Age<30","Age 30-<50","Age 50+", "CurrentSmoker","FormerSmoker","NeverSmoker",
                           "BMI<25","BMI 25-30","BMI>30","FishTert1","FishTert2","FishTert3",
                           "RiceTert1","RiceTert2","RiceTert3","DrinksTert1","DrinksTert2","DrinksTert3",
                           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3")
  
  
  Table1 <- data.frame(t(table.metal[]))
  file <- paste0(folder.output, "/Table 1. MedianIQR by participant characteristics.csv")
  
  }
write.csv( Table1, file = file, row.names=T,col.names=T)
### end
######    1. Set up forestplots #########

# re-do this table with median, 25thp. and 75thp in each own column
result <- character();table.metal<-character()
for ( i in metals ){
  
  dat$var <- dat[, i]
  #Numerical Variables
  if ( is.numeric(dat$var) ){
    
    print (i)
  # Median values  
    AZmed <-    round(quantile(dat$var[dat$center_n=="AZ"], probs=c(0.50), na.rm=T), digits=2) 
    SDmed <-    round(quantile(dat$var[dat$center_n=="SD"], probs=c(0.50), na.rm=T), digits=2) 
    OKmed <-    round(quantile(dat$var[dat$center_n=="OK"], probs=c(0.50), na.rm=T), digits=2) 
    Femalemed <-    round(quantile(dat$var[dat$female==1], probs=c(0.50), na.rm=T), digits=2) 
    Malemed <-    round(quantile(dat$var[dat$female==0], probs=c(0.50), na.rm=T), digits=2) 
    Age0med <-    round(quantile(dat$var[dat$age_cat_baseline==0], probs=c(0.50), na.rm=T), digits=2) 
    Age1med <-    round(quantile(dat$var[dat$age_cat_baseline==1], probs=c(0.50), na.rm=T), digits=2) 
    Age2med <-    round(quantile(dat$var[dat$age_cat_baseline==2], probs=c(0.50), na.rm=T), digits=2) 
    CurrentSmokemed <-    round(quantile(dat$var[dat$smoke_baseline=="Y"], probs=c(0.50), na.rm=T), digits=2) 
    FormerSmokemed <-    round(quantile(dat$var[dat$smoke_baseline=="E"], probs=c(0.50), na.rm=T), digits=2) 
    NeverSmokemed <-    round(quantile(dat$var[dat$smoke_baseline=="N"], probs=c(0.50), na.rm=T), digits=2) 
    BMI0med <-    round(quantile(dat$var[dat$bmi_cat_baseline==0], probs=c(0.50), na.rm=T), digits=2) 
    BMI1med <-    round(quantile(dat$var[dat$bmi_cat_baseline==1], probs=c(0.50), na.rm=T), digits=2) 
    BMI2med <-    round(quantile(dat$var[dat$bmi_cat_baseline==2], probs=c(0.50), na.rm=T), digits=2) 
    fish1med <-    round(quantile(dat$var[dat$fish.tert==1], probs=c(0.50), na.rm=T), digits=2) 
    fish2med <-    round(quantile(dat$var[dat$fish.tert==2], probs=c(0.50), na.rm=T), digits=2) 
    fish3med <-    round(quantile(dat$var[dat$fish.tert==3], probs=c(0.50), na.rm=T), digits=2) 
    rice1med <-    round(quantile(dat$var[dat$rice.tert==1], probs=c(0.50), na.rm=T), digits=2) 
    rice2med <-    round(quantile(dat$var[dat$rice.tert==2], probs=c(0.50), na.rm=T), digits=2) 
    rice3med <-    round(quantile(dat$var[dat$rice.tert==3], probs=c(0.50), na.rm=T), digits=2) 
    drinks1med <-    round(quantile(dat$var[dat$drinks.tert==1], probs=c(0.50), na.rm=T), digits=2) 
    drinks2med <-    round(quantile(dat$var[dat$drinks.tert==2], probs=c(0.50), na.rm=T), digits=2) 
    drinks3med <-    round(quantile(dat$var[dat$drinks.tert==3], probs=c(0.50), na.rm=T), digits=2) 
    organmeat1med <-    round(quantile(dat$var[dat$organmeat.tert==1], probs=c(0.50), na.rm=T), digits=2) 
    organmeat2med <-    round(quantile(dat$var[dat$organmeat.tert==2], probs=c(0.50), na.rm=T), digits=2) 
    organmeat3med <-    round(quantile(dat$var[dat$organmeat.tert==3], probs=c(0.50), na.rm=T), digits=2) 
    processmeat1med <-    round(quantile(dat$var[dat$processmeat.tert==1], probs=c(0.50), na.rm=T), digits=2) 
    processmeat2med <-    round(quantile(dat$var[dat$processmeat.tert==2], probs=c(0.50), na.rm=T), digits=2) 
    processmeat3med <-    round(quantile(dat$var[dat$processmeat.tert==3], probs=c(0.50), na.rm=T), digits=2) 
    overallmed<-round(quantile(dat$var, probs=c(0.50), na.rm=T), digits=2)
    
# 25th p values  
    AZ25p <-    round(quantile(dat$var[dat$center_n=="AZ"], probs=c(0.25), na.rm=T), digits=2) 
    SD25p <-    round(quantile(dat$var[dat$center_n=="SD"], probs=c(0.25), na.rm=T), digits=2) 
    OK25p <-    round(quantile(dat$var[dat$center_n=="OK"], probs=c(0.25), na.rm=T), digits=2) 
    Female25p <-    round(quantile(dat$var[dat$female==1], probs=c(0.25), na.rm=T), digits=2) 
    Male25p <-    round(quantile(dat$var[dat$female==0], probs=c(0.25), na.rm=T), digits=2) 
    Age025p <-    round(quantile(dat$var[dat$age_cat_baseline==0], probs=c(0.25), na.rm=T), digits=2) 
    Age125p <-    round(quantile(dat$var[dat$age_cat_baseline==1], probs=c(0.25), na.rm=T), digits=2) 
    Age225p <-    round(quantile(dat$var[dat$age_cat_baseline==2], probs=c(0.25), na.rm=T), digits=2) 
    CurrentSmoke25p <-    round(quantile(dat$var[dat$smoke_baseline=="Y"], probs=c(0.25), na.rm=T), digits=2) 
    FormerSmoke25p <-    round(quantile(dat$var[dat$smoke_baseline=="E"], probs=c(0.25), na.rm=T), digits=2) 
    NeverSmoke25p <-    round(quantile(dat$var[dat$smoke_baseline=="N"], probs=c(0.25), na.rm=T), digits=2) 
    BMI025p <-    round(quantile(dat$var[dat$bmi_cat_baseline==0], probs=c(0.25), na.rm=T), digits=2) 
    BMI125p <-    round(quantile(dat$var[dat$bmi_cat_baseline==1], probs=c(0.25), na.rm=T), digits=2) 
    BMI225p <-    round(quantile(dat$var[dat$bmi_cat_baseline==2], probs=c(0.25), na.rm=T), digits=2) 
    fish125p <-    round(quantile(dat$var[dat$fish.tert==1], probs=c(0.25), na.rm=T), digits=2) 
    fish225p <-    round(quantile(dat$var[dat$fish.tert==2], probs=c(0.25), na.rm=T), digits=2) 
    fish325p <-    round(quantile(dat$var[dat$fish.tert==3], probs=c(0.25), na.rm=T), digits=2) 
    rice125p <-    round(quantile(dat$var[dat$rice.tert==1], probs=c(0.25), na.rm=T), digits=2) 
    rice225p <-    round(quantile(dat$var[dat$rice.tert==2], probs=c(0.25), na.rm=T), digits=2) 
    rice325p <-    round(quantile(dat$var[dat$rice.tert==3], probs=c(0.25), na.rm=T), digits=2) 
    drinks125p <-    round(quantile(dat$var[dat$drinks.tert==1], probs=c(0.25), na.rm=T), digits=2) 
    drinks225p <-    round(quantile(dat$var[dat$drinks.tert==2], probs=c(0.25), na.rm=T), digits=2) 
    drinks325p <-    round(quantile(dat$var[dat$drinks.tert==3], probs=c(0.25), na.rm=T), digits=2) 
    organmeat125p <-    round(quantile(dat$var[dat$organmeat.tert==1], probs=c(0.25), na.rm=T), digits=2) 
    organmeat225p <-    round(quantile(dat$var[dat$organmeat.tert==2], probs=c(0.25), na.rm=T), digits=2) 
    organmeat325p <-    round(quantile(dat$var[dat$organmeat.tert==3], probs=c(0.25), na.rm=T), digits=2) 
    processmeat125p <-    round(quantile(dat$var[dat$processmeat.tert==1], probs=c(0.25), na.rm=T), digits=2) 
    processmeat225p <-    round(quantile(dat$var[dat$processmeat.tert==2], probs=c(0.25), na.rm=T), digits=2) 
    processmeat325p <-    round(quantile(dat$var[dat$processmeat.tert==3], probs=c(0.25), na.rm=T), digits=2)
    overall25p<-round(quantile(dat$var, probs=c(0.25), na.rm=T), digits=2)
    
    
    # 75th p values  
    AZ75p <-    round(quantile(dat$var[dat$center_n=="AZ"], probs=c(0.75), na.rm=T), digits=2) 
    SD75p <-    round(quantile(dat$var[dat$center_n=="SD"], probs=c(0.75), na.rm=T), digits=2) 
    OK75p <-    round(quantile(dat$var[dat$center_n=="OK"], probs=c(0.75), na.rm=T), digits=2) 
    Female75p <-    round(quantile(dat$var[dat$female==1], probs=c(0.75), na.rm=T), digits=2) 
    Male75p <-    round(quantile(dat$var[dat$female==0], probs=c(0.75), na.rm=T), digits=2) 
    Age075p <-    round(quantile(dat$var[dat$age_cat_baseline==0], probs=c(0.75), na.rm=T), digits=2) 
    Age175p <-    round(quantile(dat$var[dat$age_cat_baseline==1], probs=c(0.75), na.rm=T), digits=2) 
    Age275p <-    round(quantile(dat$var[dat$age_cat_baseline==2], probs=c(0.75), na.rm=T), digits=2) 
    CurrentSmoke75p <-    round(quantile(dat$var[dat$smoke_baseline=="Y"], probs=c(0.75), na.rm=T), digits=2) 
    FormerSmoke75p <-    round(quantile(dat$var[dat$smoke_baseline=="E"], probs=c(0.75), na.rm=T), digits=2) 
    NeverSmoke75p <-    round(quantile(dat$var[dat$smoke_baseline=="N"], probs=c(0.75), na.rm=T), digits=2) 
    BMI075p <-    round(quantile(dat$var[dat$bmi_cat_baseline==0], probs=c(0.75), na.rm=T), digits=2) 
    BMI175p <-    round(quantile(dat$var[dat$bmi_cat_baseline==1], probs=c(0.75), na.rm=T), digits=2) 
    BMI275p <-    round(quantile(dat$var[dat$bmi_cat_baseline==2], probs=c(0.75), na.rm=T), digits=2) 
    fish175p <-    round(quantile(dat$var[dat$fish.tert==1], probs=c(0.75), na.rm=T), digits=2) 
    fish275p <-    round(quantile(dat$var[dat$fish.tert==2], probs=c(0.75), na.rm=T), digits=2) 
    fish375p <-    round(quantile(dat$var[dat$fish.tert==3], probs=c(0.75), na.rm=T), digits=2) 
    rice175p <-    round(quantile(dat$var[dat$rice.tert==1], probs=c(0.75), na.rm=T), digits=2) 
    rice275p <-    round(quantile(dat$var[dat$rice.tert==2], probs=c(0.75), na.rm=T), digits=2) 
    rice375p <-    round(quantile(dat$var[dat$rice.tert==3], probs=c(0.75), na.rm=T), digits=2) 
    drinks175p <-    round(quantile(dat$var[dat$drinks.tert==1], probs=c(0.75), na.rm=T), digits=2) 
    drinks275p <-    round(quantile(dat$var[dat$drinks.tert==2], probs=c(0.75), na.rm=T), digits=2) 
    drinks375p <-    round(quantile(dat$var[dat$drinks.tert==3], probs=c(0.75), na.rm=T), digits=2) 
    organmeat175p <-    round(quantile(dat$var[dat$organmeat.tert==1], probs=c(0.75), na.rm=T), digits=2) 
    organmeat275p <-    round(quantile(dat$var[dat$organmeat.tert==2], probs=c(0.75), na.rm=T), digits=2) 
    organmeat375p <-    round(quantile(dat$var[dat$organmeat.tert==3], probs=c(0.75), na.rm=T), digits=2) 
    processmeat175p <-    round(quantile(dat$var[dat$processmeat.tert==1], probs=c(0.75), na.rm=T), digits=2) 
    processmeat275p <-    round(quantile(dat$var[dat$processmeat.tert==2], probs=c(0.75), na.rm=T), digits=2) 
    processmeat375p <-    round(quantile(dat$var[dat$processmeat.tert==3], probs=c(0.75), na.rm=T), digits=2) 
    overall75p<-round(quantile(dat$var, probs=c(0.75), na.rm=T), digits=2)
    
    sample.size <- sum(!is.na(dat$var))
    median <- c (paste0(i),sample.size, AZmed, SDmed, OKmed, Femalemed,Malemed, 
                 Age0med,Age1med,Age2med, CurrentSmokemed,FormerSmokemed,NeverSmokemed,
                 BMI0med,BMI1med,BMI2med, fish1med,fish2med,fish3med,
                 rice1med,rice2med,rice3med,drinks1med,drinks2med,drinks3med,
                 organmeat1med,organmeat2med,organmeat3med,processmeat1med,processmeat2med,processmeat3med,overallmed)
    
    p25 <- c (paste0(i),sample.size, AZ25p, SD25p, OK25p, Female25p,Male25p, 
                 Age025p,Age125p,Age225p, CurrentSmoke25p,FormerSmoke25p,NeverSmoke25p,
                 BMI025p,BMI125p,BMI225p, fish125p,fish225p,fish325p,
                 rice125p,rice225p,rice325p,drinks125p,drinks225p,drinks325p,
                 organmeat125p,organmeat225p,organmeat325p,processmeat125p,processmeat225p,processmeat325p,overall25p)
    
    p75 <- c (paste0(i),sample.size, AZ75p, SD75p, OK75p, Female75p,Male75p, 
              Age075p,Age175p,Age275p, CurrentSmoke75p,FormerSmoke75p,NeverSmoke75p,
              BMI075p,BMI175p,BMI275p, fish175p,fish275p,fish375p,
              rice175p,rice275p,rice375p,drinks175p,drinks275p,drinks375p,
              organmeat175p,organmeat275p,organmeat375p,processmeat175p,processmeat275p,processmeat375p,overall75p)
    
  }
  table.metal <- rbind (table.metal, median, p25, p75)
  
  colnames(table.metal)<-c("Metal","N", "AZ","SD","OK","Female","Male",
                           "Age<30","Age 30-<50","Age 50+", "CurrentSmoker","FormerSmoker","NeverSmoker",
                           "BMI<25","BMI 25-30","BMI>30","FishTert1","FishTert2","FishTert3",
                           "RiceTert1","RiceTert2","RiceTert3","DrinksTert1","DrinksTert2","DrinksTert3",
                         "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3","Overall")
  Table1 <- data.frame(t(table.metal[]))
}
colnames(Table1)<-c("uiAs50", "uiAsp25","uiAsp75", "uAsb50", "uAsb25","uAsb75","uCd50","uCd25","uCd75",
                    "uMo50","uMo25","uMo75","uPb50","uPb25","uPb75", "uW50","uW25","uW75","uZn50","uZn25","uZn75",
                    "uU50","uU25","uU75", "uSe50","uSe25","uSe75", "uHg50","uHg25","uHg75","bHg50","bHg25","bHg75",
                    "bPb50","bPb25","bPb75","bMn50","bMn25","bMn75", "bCd50","bCd25","bCd75","bSe50","bSe25","bSe75",
                    "bAs.Adj50","bAs.Adj25","bAs.Adj75","bZn50","bZn25","bZn75","bNi50","bNi25","bNi75")

setwd("~/Desktop/Hg Exposure Assessment SHS/P30 Pilot ")

folder.output <- "outputSept20"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
### end
######        uiAs #####
## uiAs
myvars<-c("uiAs50","uiAsp25","uiAsp75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "np25"
colnames(hr)[3] <- "np75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$np25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$np75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$np25,",",hr$np75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0(folder.output, "/Median IQR forestplot - uiAs.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)
# limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5)
# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uiAs",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()


### end

######        uAsb #####
## uAsb
myvars<-c("uAsb50","uAsb25","uAsb75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uAsb.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uAsb",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end
######        uCd #####
## uCd
myvars<-c("uCd50","uCd25","uCd75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uCd.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uCd",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end



######        uMo #####
## uMo
myvars<-c("uMo50","uMo25","uMo75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uMo.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uMo",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()


### end

######        uPb #####
## uPb
myvars<-c("uPb50","uPb25","uPb75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uPb.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uPb",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        uW #####
## uW
myvars<-c("uW50","uW25","uW75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uW.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uW",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        uZn #####
## uZn
myvars<-c("uZn50","uZn25","uZn75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uZn.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uZn",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        uU #####
## uU
myvars<-c("uU50","uU25","uU75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uU.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uU",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        uSe #####
## uSe
myvars<-c("uSe50","uSe25","uSe75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uSe.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uSe",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        uHg #####
## uHg
myvars<-c("uHg50","uHg25","uHg75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - uHg.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - uHg",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()
### end

######        bHg #####
## bHg
myvars<-c("bHg50","bHg25","bHg75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bHg.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bHg",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        bPb #####
## bPb
myvars<-c("bPb50","bPb25","bPb75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bPb.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bPb",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end


######        bMn #####
## bMn
myvars<-c("bMn50","bMn25","bMn75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bMn.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bMn",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end

######        bCd #####
## bCd
myvars<-c("bCd50","bCd25","bCd75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bCd.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bCd",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()
### end

######        bSe #####
## bSe
myvars<-c("bSe50","bSe25","bSe75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bSe.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bSe",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end


######        bAs.Adj #####
## bAs.Adj
myvars<-c("bAs.Adj50","bAs.Adj25","bAs.Adj75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bAs.Adj.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bAs.Adj",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end


######        bZn #####
## bZn
myvars<-c("bZn50","bZn25","bZn75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bZn.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bZn",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()

### end


######        bNi #####
## bNi
myvars<-c("bNi50","bNi25","bNi75")
hr<-Table1[myvars]
hr <- hr[-1,]
hr <- hr[-1,]
colnames(hr)[1] <- "nmedian"
colnames(hr)[2] <- "n25"
colnames(hr)[3] <- "n75"

# Add overall median (IQR) 
class(hr$median)
hr$points<-as.character(hr$nmedian)
hr$points<-as.numeric(hr$points)
hr$low<-as.character(hr$n25)
hr$low<-as.numeric(hr$low)
hr$up<-as.character(hr$n75)
hr$up<-as.numeric(hr$up)

hr$label<-NA 
hr$label<- paste(hr$nmedian,", (",hr$n25,",",hr$n75,")")

group <- c( expression(bold("Center")),
            expression(bold("Sex")), 
            expression(bold("Age (years)")),
            expression(bold("Smoking status")),
            expression(bold(paste("Body Mass Index (kg/",bold(m^{2}), ")"))),  
            expression(bold("Fish intake")),
            expression(bold("Rice intake")),
            expression(bold("Non-alcoholic drinks intake")),
            expression(bold("Organ meat intake")),
            expression(bold("Processed meat intake")),
            expression(bold("Overall")))

label <- c("Arizona", "North/South Dakota", "Oklahoma",
           "Female", "Male",
           "Age<30","Age 30-<50","Age 50+",
           "Current Smoker", "Former Smoker", "Never Smoker",
           "<25", "25 - 30", ">30","Tertile 1","Tertile 2","Tertile 3",
           "Tertile 1","Tertile 2","Tertile 3","Tertile 1","Tertile 2","Tertile 3",
           "OrganMeatTert1","OrganMeatTert2","OrganMeatTert3","ProcessMeatTert1","ProcessMeatTert2","ProcessMeatTert3"
)

y1 <- c(39:37,35:34,32:30,28:26,24:22,20:18,16:14 ,12:10, 8:6,4:2,0.05) #sub groups (ex MALE), 0.05=overall position 
y2 <- c(40,36,33,29,25,21,17,13,9,5,0.05) #groups (ex SEX)
#y1 <- c(37:36, 34:33, 31:27, 25:24, 22:20, 18:16, 14:12, 10:8, 6:5, 3:2, 0.05 ) #sub groups (ex MALE), 0.05=overall position 
#y2 <- c(38, 35, 32, 26, 23, 19, 15, 11, 7, 4, 0.05) #groups (ex SEX)

pdf(file=paste0( folder.output , "/Median IQR forestplot - bNi.pdf"), 9,8) 
par(mfrow=c(1,1),mar=c(3,1,2,2), oma=c(0, 30, 2, 0))
plot(hr$points, y1,type="n",xlim=c(min(hr$low), max(hr$up)),ylim=c(0,40),xlab="",ylab="",axes=F
     # ,main="HR of HD Mortality", cex.main=1
)

for(c in 1:(nrow(hr)-1)){
  points(hr$points[c],y1[c], pch=22, bg=1, cex=as.vector(hr[c,"w"]*3))   #median and segments for each characteristic
  segments(hr$low[c], y1[c], hr$up[c], y1[c], lty=1, lwd=1)
}

points(hr$points[nrow(hr)],y1[nrow(hr)],pch=18,cex=4,col="#4aa4ba")
segments(hr$low[nrow(hr)],y1[nrow(hr)],hr$up[nrow(hr)],y1[nrow(hr)],lty=1,lwd=2)

#vertical line in the overall or$points
segments(hr$points[nrow(hr)],y1[nrow(hr)],hr$points[nrow(hr)],max(y1),lty=8,lwd=1, col="gray25")

axis(1, seq(min(hr$low), max(hr$up), length.out=5),labels=seq(min(hr$low), max(hr$up), length.out=5),cex.axis=0.7, font.axis=3, line=0, lty=1, lwd=1.5, lwd.ticks=0.5)

# Title
mtext(side=2,at=max(y2)+3,"Median (IQR) by subgroup - bNi",cex=0.9,font=2,las=1,adj=0,line=27)

#Group / Subgroup column
mtext(side=2, at=y2, group, cex=0.8, font=2, las=1, adj=0, line=27)
mtext(side=2, at=y1[-nrow(hr)], label,cex=0.8, font=1, las=1, adj=0, line=26)
mtext(side=2,at=max(y2)+1,"Subgroup",cex=0.9,font=2,las=1,adj=0,line=27)

#add label text
mtext(side=2,at=y1,hr$label,cex=0.8,font=1,las=1,adj=1,line=6)
mtext(side=2,at=max(y2)+1,"Median (IQR)",cex=0.9, font=2,las=1,adj=1,line=6)

dev.off()


### end



###### 2. Table 2. ICC overall and by sex ###### 
library(ICC)

### Urinary Hg
myvars<-c("idno","s3u_thg.cr","s5u_thg.cr", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3u_thg.cr"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5u_thg.cr"] <- "Phase5"

subdatlong<-gather(subdat, studyphase, uthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, uthg, data = subdatlong, CI.type = "S")
ICCUHgAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, uthg, data = subdatlongfem, CI.type = "S")
ICCUHgFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, uthg, data = subdatlongmal, CI.type = "S")
ICCUHgMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))

#k the number of measurements per individual or group. In an unbalanced design,
#k is always less than the mean number of measurements per individual/group
#and is calculated using the equation in Lessells and Boag (1987).
#varw the within individual or group variance
#vara the among individual or group variance

### Blood Hg
myvars<-c("idno","s3b_thg","s5b_thg", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_thg"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_thg"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBHgAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBHgFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBHgMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))


### Blood Lead
myvars<-c("idno","s3b_pb","s5b_pb", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_pb"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_pb"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBPbAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBPbFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBPbMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))

### Blood Mn
myvars<-c("idno","s3b_mn","s5b_mn", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_mn"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_mn"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBMnAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBMnFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBMnMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))


### Blood Cd
myvars<-c("idno","s3b_cd","s5b_cd", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_cd"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_cd"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBCdAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBCdFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBCdMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))


### Blood Se
myvars<-c("idno","s3b_se","s5b_se", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_se"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_se"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBSeAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBSeFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBSeMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))


### Blood As
myvars<-c("idno","s3b_as","s5b_as", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_as"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_as"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBAsAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBAsFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBAsMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))


### Blood Zn
myvars<-c("idno","s3b_zn","s5b_zn", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_zn"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_zn"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBZnAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBZnFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBZnMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))

### Blood Ni
myvars<-c("idno","s3b_ni","s5b_ni", "center_n","female")
subdat<-dat[myvars]
colnames(subdat)[colnames(subdat)=="s3b_ni"] <- "Phase3"
colnames(subdat)[colnames(subdat)=="s5b_ni"] <- "Phase5"
subdatlong<-gather(subdat, studyphase, bthg, Phase3:Phase5, factor_key=TRUE);dim(subdatlong) #
subdatlong$idno<-as.factor(subdatlong$idno)
subdatlongfem<-subdatlong[which(subdatlong$female==1),]
subdatlongmal<-subdatlong[which(subdatlong$female==0),]

fit<-ICCest(idno, bthg, data = subdatlong, CI.type = "S")
ICCBNiAll<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongfem, CI.type = "S")
ICCBNiFemales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))
fit<-ICCest(idno, bthg, data = subdatlongmal, CI.type = "S")
ICCBNiMales<-convert.to.ci(round(c(fit$ICC, fit$LowerCI, fit$UpperCI),2))

As<-rbind(ICCBAsAll,ICCBAsFemales,ICCBAsMales,"Blood As")
Cd<-rbind(ICCBCdAll,ICCBCdFemales,ICCBCdMales,"Blood Cd")
BHg<-rbind(ICCBHgAll,ICCBHgFemales,ICCBHgMales,"Blood Hg")
UHg<-rbind(ICCUHgAll,ICCUHgFemales,ICCUHgMales,"Urinary Hg")
Mn<-rbind(ICCBMnAll,ICCBMnFemales,ICCBMnMales,"Blood Mn")
Ni<-rbind(ICCBNiAll,ICCBNiFemales,ICCBNiMales,"Blood Ni")
Pb<-rbind(ICCBPbAll,ICCBPbFemales,ICCBPbMales,"Blood Pb")
Se<-rbind(ICCBSeAll,ICCBSeFemales,ICCBSeMales,"Blood Se")
Zn<-rbind(ICCBZnAll,ICCBZnFemales,ICCBZnMales,"Blood Zn")

ICCs<-cbind(As,Cd,BHg,UHg,Mn,Ni,Pb,Se,Zn)
rownames(ICCs)[1] <- "All"
rownames(ICCs)[2] <- "Females"
rownames(ICCs)[3] <- "Males"
rownames(ICCs)[4] <- "Metal"


write.xlsx(ICCs,row.names=T, file="~/Desktop/Hg Exposure Assessment SHS/P30 Pilot /outputSept20/Table 2. ICC values by metal.xlsx")


### end






###### 3. Exploratory histogram/box plotting #####
raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(hjust=0.5,lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

### end
######     Mercury ######
#new dataframes to plot
myvars<-c("idno", "s3u_thg.cr", "s5u_thg.cr", "female", "center_n")
urines<-dat[myvars]
urinelong<-gather(urines, studyphase, uhg, s3u_thg.cr:s5u_thg.cr, factor_key=TRUE);dim(urinelong) #
urinelong$studyphase<-as.character(urinelong$studyphase)
urinelong$studyphase[which(urinelong$studyphase=="s3u_thg.cr")]<-"Phase 3"
urinelong$studyphase[which(urinelong$studyphase=="s5u_thg.cr")]<-"Phase 5"
describe(urinelong$studyphase)
urinelong$Biospecimen<-"Urine"
colnames(urinelong)[colnames(urinelong)=="uhg"] <- "hg"


myvars<-c("idno", "s3b_thg", "s5b_thg", "female", "center_n")
bloods<-dat[myvars]
bloodlong<-gather(bloods, studyphase, uhg, s3b_thg:s5b_thg, factor_key=TRUE);dim(bloodlong) #
bloodlong$studyphase<-as.character(bloodlong$studyphase)
bloodlong$studyphase[which(bloodlong$studyphase=="s3b_thg")]<-"Phase 3"
bloodlong$studyphase[which(bloodlong$studyphase=="s5b_thg")]<-"Phase 5"
describe(bloodlong$studyphase)
bloodlong$Biospecimen<-"Blood"
colnames(bloodlong)[colnames(bloodlong)=="uhg"] <- "hg"


#dats3<-dat[which(!is.na(dat$s3b_thg)&!is.na(dat$s3u_thg.cr)),];dim (dats3) #117
#dats3$s3sumhg<-(dats3$s3b_thg)+(dats3$s3u_thg.cr)

myvars<-c("idno", "s3sumhg", "female", "center_n")
totals3<-dats3[myvars]
totals3$studyphase<-"Phase 3"
colnames(totals3)[colnames(totals3)=="s3sumhg"] <- "hg"
myvars<-c("idno", "s5sumhg", "female", "center_n")
totals5<-dats5[myvars]
totals5$studyphase<-"Phase 5"
colnames(totals5)[colnames(totals5)=="s5sumhg"] <- "hg"
totals<-rbind(totals3,totals5)
totals$Biospecimen<-"Total"

# Create one dataframe that rbinds these values with an indicator for the Hg species
allhg<-rbind(totals, bloodlong,urinelong)

# Histograms of creatinine corrected values by study phase
a<-ggplot(urinelong, aes(x=log(hg), color=studyphase, fill=studyphase)) +
  geom_histogram(position="identity",alpha=0.5,binwidth = 0.1)  +
  ggtitle("Distribution of log urinary mercury concentrations (ug/L) in pilot data") +
  xlab("Urine total mercury concentrations (ug/L), log-transformed") 
a

# Boxplots by study phase and biospecimen
a2<-ggplot(data=allhg, aes(x=Biospecimen, y=log(hg), fill=studyphase)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("Total","Urine","Blood")) +
   scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  labels = c("0.01","0.08","1.00","2.72", "7.39")) +
  labs(y=expression(paste("Log concentration (",mu,"g/L)"))
  )
a2



a3<-ggplot(data=allhg, aes(x=Biospecimen, y=log(hg), fill=studyphase)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("Total","Urine","Blood")) +
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  labels = c("0.01","0.08","1.00","2.72", "7.39")) +
  labs(y=expression(paste("Log concentration (",mu,"g/L)"))+
  facet_wrap(~female)
  )
a3
a3+facet_wrap(~female)
a3+facet_wrap(~center_n)


# Just urine and blood
# Boxplots by study phase and biospecimen
allhg2<-rbind( bloodlong,urinelong)

a2<-ggplot(data=allhg2, aes(x=Biospecimen, y=log(hg), fill=studyphase)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("Urine","Blood")) +
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  labels = c("0.01","0.08","1.00","2.72", "7.39")) +
  labs(y=expression(paste("Concentration (",mu,"g/L) plotted in log scale"), title="Distribution of blood and urine mercury")  )+
  theme_grey(base_size = 22) 

a2

## Add NHANES 0708 to the plot
myvars<-c("SEQN","RIAGENDR","URXUHG.cr")
nhur<- nh0708[myvars]
nhur$center_n<-"NHANES"
nhur$studyphase<-"NHANES"
nhur$Biospecimen<-"Urine"
colnames(nhur)[colnames(nhur)=="URXUHG.cr"] <- "hg"
colnames(nhur)[colnames(nhur)=="RIAGENDR"] <- "female"

myvars<-c("SEQN","RIAGENDR","LBXTHG")
nhbl<- nh0708[myvars]
nhbl$center_n<-"NHANES"
nhbl$studyphase<-"NHANES"
colnames(nhbl)[colnames(nhbl)=="LBXTHG"] <- "hg"
colnames(nhbl)[colnames(nhbl)=="RIAGENDR"] <- "female"
nhbl$Biospecimen<-"Blood"
nhboth<-rbind(nhur,nhbl)
colnames(nhboth)[colnames(nhboth)=="SEQN"] <- "idno"

allhg3<-rbind(allhg2, nhboth)

setwd("~/Desktop/Hg Exposure Assessment SHS/P30 Pilot ")

folder.output <- "outputSept20"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}

## Approah one: biospeciemns together
a2<-ggplot(data=allhg3, aes(x=Biospecimen, y=log(hg), fill=studyphase)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("Urine","Blood")) +
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  
                     labels = c("0.01","0.08","1.00","2.72", "7.39"),
                     name=expression(paste("Concentration (",mu,"g/L, log scale")))+
  theme_grey(base_size = 22)+
  labs(fill="Study phase")
a2

## Approah tw0: facet wrap together
a2<-ggplot(data=allhg3, aes(x=studyphase,y=log(hg), fill=studyphase)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("NHANES","Phase 3","Phase 5"),name=" ") +
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  
                     labels = c("0.01","0.08","1.00","2.72", "7.39"),
                     name=expression(paste("Concentration (",mu,"g/L, log scale)")))+
  theme_grey(base_size = 30)+
  facet_wrap(~Biospecimen)+
  labs(fill="Study phase")+
  theme(legend.position ="none")
a2


pdf(file=paste0( folder.output , "/Hg Boxplots by study.pdf"), 16, 10)
a2
dev.off()

allhg3$female
allhg3$sex<-"Male"
allhg3$sex[which(allhg3$female=="1")]<-"Female"
table(allhg3$sex)
table(allhg3$female)
sf3allhg<-allhg3[which(allhg3$center_n=="OK"|allhg3$center_n=="SD"|allhg3$center_n=="AZ"),]
## PLot differences in urine and blood by sex at baseline (phase 3)
a2<-ggplot(data=sf3allhg, aes(x=sex,y=log(hg), fill=sex)) + 
  geom_boxplot()+ scale_x_discrete(limits=c("Female","Male"),name=" ") +
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  
                     labels = c("0.01","0.08","1.00","2.72", "7.39"),
                     name=expression(paste("Total Hg concentration (",mu,"g/L, log scale)")))+
  theme_grey(base_size = 30)+
  facet_wrap(~Biospecimen)+
  labs(fill="sex")+
  theme(legend.position ="none")
a2
pdf(file=paste0( folder.output , "/histograms by sex.pdf"), 16, 10)
a2
dev.off()

# Are sex differences at baseline sig different? 
library(lme4)
sf3allhg3<-allhg3[which(allhg3$studyphase=="Phase 3"|allhg3$studyphase=="Phase 5"),] #urines and blood both years, SHFS only

bloodsst<-sf3allhg3[which(sf3allhg3$Biospecimen=="Blood"),] #bloods both years
urinesst<-sf3allhg3[which(sf3allhg3$Biospecimen=="Urine"),] #urines both years
urinep3<-urinesst[which(urinesst$studyphase=="Phase 3"),] # urines at baseline only
bloodp3<-bloodsst[which(bloodsst$studyphase=="Phase 3"),] # bloods at baseline only


#Baseline urine differents by sex?
fit <- glm(hg~-1+as.factor(sex),								#
           family=gaussian(link="identity"), data=urinep3)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall
summary(fit)
fit <- glm(hg~+as.factor(sex),								#
           family=gaussian(link="identity"), data=urinep3)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall
summary(fit) # yes differnt 


#Baseline bloods differents by sex?
fit <- glm(hg~-1+as.factor(sex),								#
           family=gaussian(link="identity"), data=bloodp3)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall
summary(fit)
fit <- glm(hg~+as.factor(sex),								#
           family=gaussian(link="identity"), data=bloodp3)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall
summary(fit) # no not different

#Is change in urines different by sex?
fit1<-lmer(hg~female+studyphase+female:studyphase+(1|idno), urinesst) #
summary(fit1)
# Is change in bloods different by sex?
fit1<-lmer(hg~female+studyphase+female:studyphase+(1|idno), bloodsst) # 
summary(fit1)


# Urine
mean(log(urinelong$hg[which(urinelong$studyphase=="Phase 3")]),na.rm=T); #0.1400253
mean(log(urinelong$hg[which(urinelong$studyphase=="Phase 5")]),na.rm=T); #0.1972964
pairwise.t.test(log(urinelong$hg), urinelong$studyphase, paired=TRUE) # p-value  0.0011
t.test(urines$s3u_thg.cr,urines$s5u_thg.cr,paired=TRUE) #mean of differences:  -0.1522702  

fit1<-lmer(hg~studyphase+(1|idno), urinelong)
summary(fit1)
confint(fit1) # studyphasePhase 5 0.04566378 0.2509264



### end






######     Boxplots: All blood metals by phase ######
## Convert dat to long format: 2 rows per person, with column for each metal

# #Hg
# myvars<-c('idno','female','center_n','s3b_thg', 's5b_thg')
# dat2<-dat[myvars]
# thg_long <- gather(dat2, phase, THg, s3b_thg:s5b_thg, factor_key=TRUE)
# thg_long$phase<-as.character(thg_long$phase)
# thg_long$phase[(thg_long$phase=="s3b_thg")]<-"phase3"
# thg_long$phase[(thg_long$phase=="s5b_thg")]<-"phase5"
# 
# #Ni
# myvars<-c('idno','s3b_ni', 's5b_ni')
# dat2<-dat[myvars]
# tni_long <- gather(dat2, phase, Ni, s3b_ni:s5b_ni, factor_key=TRUE)
# tni_long$phase<-as.character(tni_long$phase)
# tni_long$phase[(tni_long$phase=="s3b_ni")]<-"phase3"
# tni_long$phase[(tni_long$phase=="s5b_ni")]<-"phase5"
# 
# #Zn
# myvars<-c('idno','s3b_zn', 's5b_zn')
# dat2<-dat[myvars]
# tzn_long <- gather(dat2, phase, Zn, s3b_zn:s5b_zn, factor_key=TRUE)
# tzn_long$phase<-as.character(tzn_long$phase)
# tzn_long$phase[(tzn_long$phase=="s3b_zn")]<-"phase3"
# tzn_long$phase[(tzn_long$phase=="s5b_zn")]<-"phase5"
# 
# #Pb
# myvars<-c('idno','s3b_pb', 's5b_pb')
# dat2<-dat[myvars]
# tpb_long <- gather(dat2, phase, Pb, s3b_pb:s5b_pb, factor_key=TRUE)
# tpb_long$phase<-as.character(tpb_long$phase)
# tpb_long$phase[(tpb_long$phase=="s3b_pb")]<-"phase3"
# tpb_long$phase[(tpb_long$phase=="s5b_pb")]<-"phase5"
# 
# #Mn
# myvars<-c('idno','s3b_mn', 's5b_mn')
# dat2<-dat[myvars]
# tmn_long <- gather(dat2, phase, Mn, s3b_mn:s5b_mn, factor_key=TRUE)
# tmn_long$phase<-as.character(tmn_long$phase)
# tmn_long$phase[(tmn_long$phase=="s3b_mn")]<-"phase3"
# tmn_long$phase[(tmn_long$phase=="s5b_mn")]<-"phase5"
# 
# #Se
# myvars<-c('idno','s3b_se', 's5b_se')
# dat2<-dat[myvars]
# tse_long <- gather(dat2, phase, Se, s3b_se:s5b_se, factor_key=TRUE)
# tse_long$phase<-as.character(tse_long$phase)
# tse_long$phase[(tse_long$phase=="s3b_se")]<-"phase3"
# tse_long$phase[(tse_long$phase=="s5b_se")]<-"phase5"
# 
# #As
# myvars<-c('idno','s3b_as', 's5b_as')
# dat2<-dat[myvars]
# tas_long <- gather(dat2, phase, As, s3b_as:s5b_as, factor_key=TRUE)
# tas_long$phase<-as.character(tas_long$phase)
# tas_long$phase[(tas_long$phase=="s3b_as")]<-"phase3"
# tas_long$phase[(tas_long$phase=="s5b_as")]<-"phase5"
# 
# #Cd
# myvars<-c('idno','s3b_cd', 's5b_cd')
# dat2<-dat[myvars]
# tcd_long <- gather(dat2, phase, Cd, s3b_cd:s5b_cd, factor_key=TRUE)
# tcd_long$phase<-as.character(tcd_long$phase)
# tcd_long$phase[(tcd_long$phase=="s3b_cd")]<-"phase3"
# tcd_long$phase[(tcd_long$phase=="s5b_cd")]<-"phase5"
# 
# datlong<-merge(thg_long,tni_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tzn_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tpb_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tmn_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tse_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tas_long,by=c("idno","phase"),all=T)
# datlong<-merge(datlong,tcd_long,by=c("idno","phase"),all=T)
# 
# # Blood lead
# ggplot(data=datlong, aes(y=log(Pb),x=as.factor(phase)))+
#   geom_boxplot()+geom_point()+  
#   theme_bw() +
#   raincloud_theme+
#   #scale_y_continuous(limits=c(max(log(datlong$Pb,na.rm=T)),min(log(datlong$Pb,na.rm=T))), breaks=c(-0.488, 0.634,1.756,2.302585,2.70805, 4.60517,6.214608, 6.58 ) , labels =c("0.6","1.9","5.8","10.0","15.0","100", "500", "720") )+
#   #scale_y_continuous(limits=c(max(log(datlong$Pb,na.rm=T)),min(log(datlong$Pb,na.rm=T))), breaks=c(-0.488, 0.634,1.756,2.302585,2.70805, 4.60517,6.214608, 6.58 ) , labels =c("0.6","1.9","5.8","10.0","15.0","100", "500", "720") )+
#   labs(y =expression(paste("Blood lead concentrations, µg/L"),size=14), x = "")+
#   scale_x_discrete(labels = c("Phase 3","Phase 5"))+
#   ggtitle("Blood lead")+
#   theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
#   theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
#   theme(plot.title = element_text(family="Avenir Next", size=30))+  
#   theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"),axis.title.x=element_text(size=20), text = element_text(size=20))
# 
#  



### Building a truly long dataset, to plot the NHANES, P3, and P5 all together, with facet_wrap by metal:
#Hg
myvars<-c('idno','female','center_n','s3b_thg', 's5b_thg')
dat2<-dat[myvars]
thg_long <- gather(dat2, phase, Concentration, s3b_thg:s5b_thg, factor_key=TRUE)
thg_long$phase<-as.character(thg_long$phase)
thg_long$phase[(thg_long$phase=="s3b_thg")]<-"phase3"
thg_long$phase[(thg_long$phase=="s5b_thg")]<-"phase5"
thg_long$Metal<-"Mercury"

#Ni
myvars<-c('idno','female','center_n','s3b_ni', 's5b_ni')
dat2<-dat[myvars]
tni_long <- gather(dat2, phase, Concentration, s3b_ni:s5b_ni, factor_key=TRUE)
tni_long$phase<-as.character(tni_long$phase)
tni_long$phase[(tni_long$phase=="s3b_ni")]<-"phase3"
tni_long$phase[(tni_long$phase=="s5b_ni")]<-"phase5"
tni_long$Metal<-"Nickel"

#Zn
myvars<-c('idno','female','center_n','s3b_zn', 's5b_zn')
dat2<-dat[myvars]
tzn_long <- gather(dat2, phase, Concentration, s3b_zn:s5b_zn, factor_key=TRUE)
tzn_long$phase<-as.character(tzn_long$phase)
tzn_long$phase[(tzn_long$phase=="s3b_zn")]<-"phase3"
tzn_long$phase[(tzn_long$phase=="s5b_zn")]<-"phase5"
tzn_long$Metal<-"Zinc"

#Pb
myvars<-c('idno','female','center_n','s3b_pb_dL', 's5b_pb_dL')
dat2<-dat[myvars]
tpb_long <- gather(dat2, phase, Concentration, s3b_pb_dL:s5b_pb_dL, factor_key=TRUE)
tpb_long$phase<-as.character(tpb_long$phase)
tpb_long$phase[(tpb_long$phase=="s3b_pb_dL")]<-"phase3"
tpb_long$phase[(tpb_long$phase=="s5b_pb_dL")]<-"phase5"
tpb_long$Metal<-"Lead"

#Mn
myvars<-c('idno','female','center_n','s3b_mn', 's5b_mn')
dat2<-dat[myvars]
tmn_long <- gather(dat2, phase, Concentration, s3b_mn:s5b_mn, factor_key=TRUE)
tmn_long$phase<-as.character(tmn_long$phase)
tmn_long$phase[(tmn_long$phase=="s3b_mn")]<-"phase3"
tmn_long$phase[(tmn_long$phase=="s5b_mn")]<-"phase5"
tmn_long$Metal<-"Manganese"

#Se
myvars<-c('idno','female','center_n','s3b_se', 's5b_se')
dat2<-dat[myvars]
tse_long <- gather(dat2, phase, Concentration, s3b_se:s5b_se, factor_key=TRUE)
tse_long$phase<-as.character(tse_long$phase)
tse_long$phase[(tse_long$phase=="s3b_se")]<-"phase3"
tse_long$phase[(tse_long$phase=="s5b_se")]<-"phase5"
tse_long$Metal<-"Selenium"

#As
myvars<-c('idno','female','center_n','s3b_as', 's5b_as')
dat2<-dat[myvars]
tas_long <- gather(dat2, phase, Concentration, s3b_as:s5b_as, factor_key=TRUE)
tas_long$phase<-as.character(tas_long$phase)
tas_long$phase[(tas_long$phase=="s3b_as")]<-"phase3"
tas_long$phase[(tas_long$phase=="s5b_as")]<-"phase5"
tas_long$Metal<-"Arsenic"

#Cd
myvars<-c('idno','female','center_n','s3b_cd', 's5b_cd')
dat2<-dat[myvars]
tcd_long <- gather(dat2, phase, Concentration, s3b_cd:s5b_cd, factor_key=TRUE)
tcd_long$phase<-as.character(tcd_long$phase)
tcd_long$phase[(tcd_long$phase=="s3b_cd")]<-"phase3"
tcd_long$phase[(tcd_long$phase=="s5b_cd")]<-"phase5"
tcd_long$Metal<-"Cadmium"

datlong<-rbind(thg_long,tni_long)
datlong<-rbind(datlong,tzn_long)
datlong<-rbind(datlong,tpb_long)
datlong<-rbind(datlong,tmn_long)
datlong<-rbind(datlong,tse_long)
datlong<-rbind(datlong,tas_long)
datlong<-rbind(datlong,tcd_long)

## Repeat for NHANES
# Blood Hg
myvars<-c("SEQN","WTMEC2YR","RIAGENDR","LBXTHG")
nhHg<-nh0708[myvars]
colnames(nhHg)[colnames(nhHg)=="SEQN"] <- "idno"
colnames(nhHg)[colnames(nhHg)=="LBXTHG"] <- "Concentration"
nhHg$Metal<-"Mercury"
nhHg$center_n<-"NHANES"
nhHg$phase<-"NHANES"

# Lead
myvars<-c("SEQN","WTMEC2YR","RIAGENDR","LBXBPB")
nhPb<-nh0708[myvars]
colnames(nhPb)[colnames(nhPb)=="SEQN"] <- "idno"
colnames(nhPb)[colnames(nhPb)=="LBXBPB"] <- "Concentration"
nhPb$Metal<-"Lead"
nhPb$center_n<-"NHANES"
nhPb$phase<-"NHANES"

# Manganese - 1112
myvars<-c("SEQN","WTMEC2YR","RIAGENDR","LBXBMN")
nhMn<-nh1112[myvars]
colnames(nhMn)[colnames(nhMn)=="SEQN"] <- "idno"
colnames(nhMn)[colnames(nhMn)=="LBXBMN"] <- "Concentration"
nhMn$Metal<-"Manganese"
nhMn$center_n<-"NHANES"
nhMn$phase<-"NHANES"

# Selenium -1112
myvars<-c("SEQN","WTMEC2YR","RIAGENDR","LBXBSE")
nhSe<-nh1112[myvars]
colnames(nhSe)[colnames(nhSe)=="SEQN"] <- "idno"
colnames(nhSe)[colnames(nhSe)=="LBXBSE"] <- "Concentration"
nhSe$Metal<-"Selenium"
nhSe$center_n<-"NHANES"
nhSe$phase<-"NHANES"

# Cadmium
myvars<-c("SEQN","WTMEC2YR","RIAGENDR","LBXBCD")
nhCd<-nh0708[myvars]
colnames(nhCd)[colnames(nhCd)=="SEQN"] <- "idno"
colnames(nhCd)[colnames(nhCd)=="LBXBCD"] <- "Concentration"
nhCd$Metal<-"Cadmium"
nhCd$center_n<-"NHANES"
nhCd$phase<-"NHANES"

nhlong<-rbind(nhHg,nhPb)
nhlong<-rbind(nhlong,nhMn)
nhlong<-rbind(nhlong,nhSe)
nhlong<-rbind(nhlong,nhCd)
nhlong$female[which(nhlong$RIAGENDR==1)]<-0
nhlong$female[which(nhlong$RIAGENDR==2)]<-1
nhlong$RIAGENDR<-NULL

### Rbind NHANES and SHFS together to do joint boxplots
datlongB<-datlong
datlongB$WTMEC2YR<-1

datlong<-rbind(datlongB,nhlong)
describe(datlong$WTMEC2YR);class(datlong$WTMEC2YR)
table(datlong$phase)


# All blood metals
ggplot(data=datlong, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  facet_wrap(~Metal,ncol=4,scales="free")+
  geom_boxplot()+geom_point()+  
  theme_bw() +
  raincloud_theme+
  #scale_y_continuous(limits=c(max(log(datlong$Pb,na.rm=T)),min(log(datlong$Pb,na.rm=T))), breaks=c(-0.488, 0.634,1.756,2.302585,2.70805, 4.60517,6.214608, 6.58 ) , labels =c("0.6","1.9","5.8","10.0","15.0","100", "500", "720") )+
  #scale_y_continuous(limits=c(max(log(datlong$Pb,na.rm=T)),min(log(datlong$Pb,na.rm=T))), breaks=c(-0.488, 0.634,1.756,2.302585,2.70805, 4.60517,6.214608, 6.58 ) , labels =c("0.6","1.9","5.8","10.0","15.0","100", "500", "720") )+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  scale_x_discrete(labels = c("Phase 3","Phase 5"))+
  ggtitle("Blood metals")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"),axis.title.x=element_text(size=20), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#5dbcd2"))

### We can't fix the y-axis labels!  Do each metal individually, then add together

#Hg
MetalPlot<-datlong[which(datlong$Metal=="Mercury"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Mercury<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Mercury")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#5dbcd2","#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

#Ni
MetalPlot<-datlong[which(datlong$Metal=="Nickel"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Nickel<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Nickel")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Zinc"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Zinc<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Zinc")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Lead"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Lead<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Lead")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#5dbcd2","#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Manganese"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Manganese<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Manganese")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#5dbcd2","#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Selenium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Selenium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Selenium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#5dbcd2","#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Arsenic"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Arsenic<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Arsenic")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlong[which(datlong$Metal=="Cadmium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Cadmium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(phase),fill=as.factor(phase)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Cadmium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
   scale_fill_manual(values = c("#5dbcd2","#195e83", "#31a354"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())


leg<-get_legend(Cadmium)
leg<-as_ggplot(leg)

library(ggpubr)
pdf(file=paste0( folder.output , "/Boxplot Blood Metals By Phase.pdf"), 20, 20)
ggarrange(Mercury,Nickel,Zinc,Lead,Cadmium,Arsenic,Selenium,Manganese,leg, ncol=3,nrow=3,legend="none")
dev.off()


### end
######     Boxplots: All P3 blood metals by center ######
datlongP3<-datlong[which(datlong$phase=="NHANES"|datlong$phase=="phase3"),]
table(datlongP3$phase)
#Hg
MetalPlot<-datlongP3[which(datlongP3$Metal=="Mercury"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Mercury<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Mercury")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455","#5dbcd2"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

#Ni
MetalPlot<-datlongP3[which(datlongP3$Metal=="Nickel"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Nickel<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Nickel")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Zinc"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Zinc<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Zinc")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Lead"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Lead<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Lead")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455","#5dbcd2"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Manganese"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Manganese<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Manganese")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455","#5dbcd2"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Selenium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Selenium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Selenium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455","#5dbcd2"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Arsenic"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Arsenic<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Arsenic")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#195e83", "#31a354","#b97455"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Cadmium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Cadmium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(center_n),fill=as.factor(center_n)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Cadmium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  scale_fill_manual(values = c("#195e83", "#31a354","#b97455","#5dbcd2"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())


leg<-get_legend(Cadmium)
leg<-as_ggplot(leg)

library(ggpubr)
pdf(file=paste0( folder.output , "/Boxplot P3 Blood Metals By Center.pdf"), 20, 20)
ggarrange(Mercury,Nickel,Zinc,Lead,Cadmium,Arsenic,Selenium,Manganese,leg, ncol=3,nrow=3,legend="none")
dev.off()



### 
######     Boxplots: All P3 blood metals by sex ######
datlongP3<-datlong[which(datlong$phase=="NHANES"|datlong$phase=="phase3"),]
table(datlongP3$phase)
datlongP3$PlotGroup<-0
datlongP3$PlotGroup[which(datlongP3$phase=="phase3"&datlongP3$female==1)]<-"SHFS, Female"
datlongP3$PlotGroup[which(datlongP3$phase=="phase3"&datlongP3$female==0)]<-"SHFS, Male"
datlongP3$PlotGroup[which(datlongP3$phase=="NHANES"&datlongP3$female==0)]<-"NHANES, Male"
datlongP3$PlotGroup[which(datlongP3$phase=="NHANES"&datlongP3$female==1)]<-"NHANES, Female"
describe(datlongP3$PlotGroup)
#Hg
MetalPlot<-datlongP3[which(datlongP3$Metal=="Mercury"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Mercury<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Mercury")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

#Ni
MetalPlot<-datlongP3[which(datlongP3$Metal=="Nickel"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Nickel<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Nickel")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Zinc"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Zinc<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Zinc")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Lead"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Lead<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Lead")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Manganese"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Manganese<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Manganese")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Selenium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Selenium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Selenium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Arsenic"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Arsenic<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Arsenic")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())

MetalPlot<-datlongP3[which(datlongP3$Metal=="Cadmium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Cadmium<-ggplot(data=MetalPlot, aes(y=log(Concentration),x=as.factor(PlotGroup),fill=as.factor(PlotGroup)))+
  geom_boxplot(aes(weight=WTMEC2YR))+geom_point(position = position_jitter(width = .1))+  
  theme_bw() +
  raincloud_theme+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  labs(y =expression(paste("Blood metal concentrations, µg/L"),size=14), x = "")+
  ggtitle("Cadmium")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.text.x= element_blank())


leg<-get_legend(Cadmium)
leg<-as_ggplot(leg)

library(ggpubr)
pdf(file=paste0( folder.output , "/Boxplot P3 Blood Metals By Sex.pdf"), 20, 20)
ggarrange(Mercury,Nickel,Zinc,Lead,Cadmium,Arsenic,Selenium,Manganese,leg, ncol=3,nrow=3,legend="none")
dev.off()



### 

###### 4. Spaghetti plots   #####
### end
#######    Old, Hg ######
# Spaghetti plots indicating ICCs, stratified by center and sex
#Urines Overall
p <- ggplot(data = urinelong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line()+
geom_point(aes(x = studyphase, y = log(hg), group = idno))+
stat_summary(aes(y = log(hg)), fun.y=mean, colour="blue", geom="line",group=1)
p

#Urines Stratified by sex
p <- ggplot(data = urinelong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line()+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ female)
p

# Just urines
p <- ggplot(data = urinelong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line(color="grey")+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)
p


folder.output <- "outputSept20"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
pdf(file=paste0( folder.output , "/ICC plots urine by sex.pdf"), 10, 10)
p
dev.off()


# Stratified by center
# Urines
p <- ggplot(data = urinelong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)+
  facet_grid( ~ center_n)+
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  
                     labels = c("0.01","0.08","1.00","2.72", "7.39"),
                     name=expression(paste("Urine Hg concentration (",mu,"g/L, log scale)")))+  ggtitle("Within-person change in urine mercury levels")+
  xlab("Study Phase")+
  ylab("Urine mercury concentration in ug/L, log scale")+theme_grey(base_size = 30)

p
pdf(file=paste0( folder.output , "/ICC plots urine by center.pdf"), 20, 10)
p
dev.off()
# Bloods overall
p <- ggplot(data = bloodlong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  ggtitle("Within-person change in blood mercury levels")+
  xlab("Study Phase")+
  ylab("Blood mercury concentration in ug/L, log scale")
p

# Blood by sex
p <- ggplot(data = bloodlong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  ggtitle("Within-person change in blood mercury levels")+
  facet_grid( ~ female)+
  xlab("Study Phase")+
  ylab("Blood mercury concentration in ug/L, log scale")
p

# Blood by center
p <- ggplot(data = bloodlong, aes(x = studyphase, y = log(hg), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = studyphase, y = log(hg), group = idno))+
  stat_summary(aes(y = log(hg),group=1), fun.y=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  ggtitle("Within-person change in blood mercury levels")+
  facet_grid( ~ center_n)+
  xlab("Study Phase")+
  ylab("Blood mercury concentration in ug/L, log scale")+ 
  scale_y_continuous(breaks = c(-5,-2.5, 0.0,1.0,2.0),  
          labels = c("0.01","0.08","1.00","2.72", "7.39"),
           name=expression(paste("Blood Hg concentration (",mu,"g/L, log scale)")))+  ggtitle("Within-person change in urine mercury levels")+
  xlab("Study Phase")+
  ylab("Urine mercury concentration in ug/L, log scale")+theme_grey(base_size = 30)

p

pdf(file=paste0( folder.output , "/ICC plots blood by center.pdf"), 20, 10)
p
dev.off()

# Simply paired t-test for evaluating whether these differences are significant
# Blood
bloodlong <- bloodlong[order(bloodlong$idno, bloodlong$studyphase),]
mean(log(bloodlong$hg[which(bloodlong$studyphase=="Phase 3")]),na.rm=T); #0.5964947
mean(log(bloodlong$hg[which(bloodlong$studyphase=="Phase 5")]),na.rm=T); #0.3980918
pairwise.t.test(log(bloodlong$hg), bloodlong$studyphase, paired=TRUE) # p-value  1.6e-09
t.test(bloods$s3b_thg,bloods$s5b_thg,paired=TRUE) #mean of differences: 0.2733984 

library(lme4)
fit1<-lmer(hg~studyphase+(1|idno), bloodlong)
summary(fit1)

# Urine
mean(log(urinelong$hg[which(urinelong$studyphase=="Phase 3")]),na.rm=T); #0.1400253
mean(log(urinelong$hg[which(urinelong$studyphase=="Phase 5")]),na.rm=T); #0.1972964
pairwise.t.test(log(urinelong$hg), urinelong$studyphase, paired=TRUE) # p-value  0.0011
  t.test(urines$s3u_thg.cr,urines$s5u_thg.cr,paired=TRUE) #mean of differences:  -0.1522702  

fit1<-lmer(hg~studyphase+(1|idno), urinelong)
summary(fit1)
confint(fit1) # studyphasePhase 5 0.04566378 0.2509264

# Totals
fit1<-lmer(hg~studyphase+(1|idno), totals)
summary(fit1)
confint(fit1) # studyphasePhase 5 -0.3017976 0.05444796


### end

#######    Blood metal ICC plots ######
spag<-datlong[which(datlong$phase!="NHANES"),]
table(spag$phase)
table(spag$Metal)

#Hg
MetalPlot<-spag[which(spag$Metal=="Mercury"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Mercury<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood mercury")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
   raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Mercury

#Ni
MetalPlot<-spag[which(spag$Metal=="Nickel"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Nickel<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Nickel")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Nickel

#Zn
MetalPlot<-spag[which(spag$Metal=="Zinc"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Zinc<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Zinc")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Zinc

#Lead
MetalPlot<-spag[which(spag$Metal=="Lead"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Lead<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Lead")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Lead

#Cd
MetalPlot<-spag[which(spag$Metal=="Cadmium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Cadmium<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Cadmium")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Cadmium

#As
MetalPlot<-spag[which(spag$Metal=="Arsenic"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Arsenic<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Arsenic")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Arsenic


#Se
MetalPlot<-spag[which(spag$Metal=="Selenium"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Selenium<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Selenium")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Selenium

#Mn
MetalPlot<-spag[which(spag$Metal=="Manganese"),]
mincon<-min(log(MetalPlot$Concentration),na.rm=T)
maxcon<-max(log(MetalPlot$Concentration),na.rm=T)
Manganese<-ggplot(data = MetalPlot, aes(x = phase, y = log(Concentration), group = idno))+ geom_line( color="grey")+
  geom_point(aes(x = phase, y = log(Concentration), group = idno))+
  #stat_summary(aes(y = log(Concentration),group=1), fun=mean, colour="blue", geom="line",group=1)+
  facet_wrap( ~ center_n)+
  scale_y_continuous(limits = c(mincon, maxcon), breaks = seq(mincon, maxcon, length.out=5),
                     labels=round(exp(seq(mincon,maxcon, length.out=5)),digits=1))+
  ggtitle("Blood Manganese")+
  xlab("Study Phase")+
  ylab("Urine metal concentration in ug/L, log scale")+theme_grey(base_size = 30)+
  theme_bw() +
  raincloud_theme+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")
Manganese





library(ggpubr)
pdf(file=paste0( folder.output , "/ICC plots by center.pdf"), 20, 20)
ggarrange(Mercury,Nickel,Zinc,Lead,Cadmium,Arsenic,Selenium,Manganese, ncol=3,nrow=3,legend="none")
dev.off()

### end
###### 5. Dendrogram analyses ######
### end
######      0. S3 blood metals only ######
myvars<-c("s3b_thg","s3b_pb","s3b_mn", "s3b_cd","s3b_se","s3b_as","s3b_zn","s3b_ni")
todendro<-dat[myvars]

todendro$As<-todendro$s3b_as
todendro$Cd<-todendro$s3b_cd
todendro$BHg<-todendro$s3b_thg
todendro$Pb<-todendro$s3b_pb
todendro$Zn<-todendro$s3b_zn
todendro$Mn<-todendro$s3b_mn
todendro$Ni<-todendro$s3b_ni
todendro$Se<-todendro$s3b_se

todendro$s3b_as<-NULL
todendro$s3b_cd<-NULL
todendro$s3b_thg<-NULL
todendro$s3b_pb<-NULL
todendro$s3b_zn<-NULL
todendro$s3b_mn<-NULL
todendro$s3b_ni<-NULL
todendro$s3b_se<-NULL

# Rescale 
den2<-as.data.frame(scale(todendro, center=TRUE, scale=TRUE))

# Transpose into the opposite orientation using 't'
den<-t(den2)

#Compute the dissimilarity matrix using Euclidean distances (you can use whatever distance you want)
##STEP2: euclidean & ward
d1 <- dist(den, method="euclidean")

hc1 <- hclust(d1, "ward.D")
plot(hc1, hang=-1, main="S3 Blood Metals",label=row.names(den))
library(ggplot2)
library(ggdendro)

pdf(file=paste0( folder.output , "/Blood S3 metals dendrogram.pdf"), 15, 10)
ggdendrogram(hc1)+
theme_bw() +
  raincloud_theme+
  ggtitle("Hierarchial cluster analysis of\nPhase 3 blood metals, Ward Method")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.title.y= element_blank())
dev.off()


###end

######      0. S5 blood metals only ######
myvars<-c("s5b_thg","s5b_pb","s5b_mn", "s5b_cd","s5b_se","s5b_as","s5b_zn","s5b_ni")
todendro<-dat[myvars]

todendro$As<-todendro$s5b_as
todendro$Cd<-todendro$s5b_cd
todendro$BHg<-todendro$s5b_thg
todendro$Pb<-todendro$s5b_pb
todendro$Zn<-todendro$s5b_zn
todendro$Mn<-todendro$s5b_mn
todendro$Ni<-todendro$s5b_ni
todendro$Se<-todendro$s5b_se

todendro$s5b_as<-NULL
todendro$s5b_cd<-NULL
todendro$s5b_thg<-NULL
todendro$s5b_pb<-NULL
todendro$s5b_zn<-NULL
todendro$s5b_mn<-NULL
todendro$s5b_ni<-NULL
todendro$s5b_se<-NULL

# Rescale 
den2<-as.data.frame(scale(todendro, center=TRUE, scale=TRUE))

# Transpose into the opposite orientation using 't'
den<-t(den2)

#Compute the dissimilarity matrix using Euclidean distances (you can use whatever distance you want)
##STEP2: euclidean & ward
d1 <- dist(den, method="euclidean")

hc1 <- hclust(d1, "ward.D")
plot(hc1, hang=-1, main="S5 Blood Metals",label=row.names(den))
library(ggplot2)
library(ggdendro)
ggdendrogram(hc1)




###end

######      0. S3 blood metals + S4 urine metals ######
myvars<-c("sum_ias_cr","ab_kations_cr", "cd_cr","mo_cr","pb_cr", "w_cr","zn_cr","u_cr_fixed", "se_cr", "s3u_thg.cr","s3b_thg","s3b_pb","s3b_mn", "s3b_cd","s3b_se","s3b_as.adj","s3b_zn","s3b_ni")
todendro<-dat[myvars]

todendro$UiAs<-todendro$sum_ias_cr
todendro$UAb<-todendro$ab_kations_cr
todendro$UCd<-todendro$cd_cr
todendro$UMo<-todendro$mo_cr
todendro$UPb<-todendro$pb_cr
todendro$UW<-todendro$w_cr
todendro$UZn<-todendro$zn_cr
todendro$UU<-todendro$u_cr_fixed
todendro$USe<-todendro$se_cr
todendro$UHg<-todendro$s3u_thg.cr
todendro$BAsAdj<-todendro$s3b_as.adj
todendro$BCd<-todendro$s3b_cd
todendro$BHg<-todendro$s3b_thg
todendro$BPb<-todendro$s3b_pb
todendro$BZn<-todendro$s3b_zn
todendro$BMn<-todendro$s3b_mn
todendro$BNi<-todendro$s3b_ni
todendro$BSe<-todendro$s3b_se

todendro$sum_ias_cr<-NULL
todendro$ab_kations_cr<-NULL
todendro$cd_cr<-NULL
todendro$mo_cr<-NULL
todendro$pb_cr<-NULL
todendro$w_cr<-NULL
todendro$zn_cr<-NULL
todendro$u_cr_fixed<-NULL
todendro$se_cr<-NULL
todendro$s3u_thg.cr<-NULL
todendro$s3b_as.adj<-NULL
todendro$s3b_cd<-NULL
todendro$s3b_thg<-NULL
todendro$s3b_pb<-NULL
todendro$s3b_zn<-NULL
todendro$s3b_mn<-NULL
todendro$s3b_ni<-NULL
todendro$s3b_se<-NULL

# Rescale 
den2<-as.data.frame(scale(todendro, center=TRUE, scale=TRUE))

# Transpose into the opposite orientation using 't'
den<-t(den2)

#Compute the dissimilarity matrix using Euclidean distances (you can use whatever distance you want)
##STEP2: euclidean & ward
d1 <- dist(den, method="euclidean")

hc1 <- hclust(d1, "ward.D")
plot(hc1, hang=-1, main="S3 Blood Metals +S4 urine metals",label=row.names(den))
library(ggplot2)
library(ggdendro)

pdf(file=paste0( folder.output , "/Blood and urine (creat corr) metals dendrogram.pdf"), 15, 10)
ggdendrogram(hc1)+
theme_bw() +
  raincloud_theme+
  ggtitle("HCA of phase 3 blood metals and\nphase 4 urinary metals (creat corrected), Ward Method")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))+
  theme(plot.title = element_text(family="Avenir Next", size=30))+  
  theme(axis.line = element_line(), panel.border = element_blank(),axis.text=element_text(size=24),axis.title = element_text(size = 20, face = "bold"), text = element_text(size=20))+
  theme(legend.position = "none")+ scale_fill_manual(values = c("#6a51a3", "#9e9ac8","#fd8d3c","#fdbe85"))+  theme(axis.title.x= element_blank())+  theme(axis.title.y= element_blank())
dev.off()

# Principle component analysis
denfull<- na.omit(den2) 
pca.obj <- prcomp(denfull)
summary(pca.obj)
library(factoextra)
#Eigen values scree plot
fviz_eig(pca.obj)
eig.val <- get_eigenvalue(pca.obj)
eig.val # Keep 4 PCs

# Results for Variables
res.var <- get_pca_var(pca.obj)
res.var$coord          # Coordinates
res.var$contrib[,'Dim.2']        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(pca.obj)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# Graph of individuals, with similar individuals grouped together
fviz_pca_ind(pca.obj,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_ind(pca.obj,
             label = "none", # hide individual labels
             #habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

# Graph of variables. Positive correlated variables point to the same side of the plot. 
# Negative correlated variables point to opposite sides of the graph
fviz_pca_var(pca.obj,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(pca.obj, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca.obj, choice = "var", axes = 2, top = 10)

# Biplot of individuals and variables
fviz_pca_biplot(pca.obj, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

###end





##### 5. Table 1: Participant characteristics at baseline #####


# Age at Phase 3
fit <- glm(s3age~-1+as.factor(center_n),								# by what?
           family=gaussian(link="identity"), data=dat)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall

summary(fit)


fit <- glm(female~-1+as.factor(center_n),								# by what?
           family=gaussian(link="identity"), data=dat)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall

summary(fit)
fit <- glm(s3bmi~-1+as.factor(center_n),								# by what?
           family=gaussian(link="identity"), data=dat)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall

summary(fit)
dat$s3smokeY<-0
dat$s3smokeY[which(dat$s3smoke=="Y")]<-1
table(dat$s3smokeY)
fit <- glm(s3smokeY~-1+as.factor(center_n),								# by what?
           family=gaussian(link="identity"), data=dat)	# "-1" gives you the mean values for each factor level, w/out intercept; +1 gives overall

summary(fit)


quantile(dat$s3u_thg,  probs = c(0.25, 0.5, 0.75), na.rm=TRUE)


### end














