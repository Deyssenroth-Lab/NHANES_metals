####### NHANES - METALS IN WOMEN #######

# Assessment of NHANES metals concentrations in women of child-bearing age
# Title:                                                                            
# Assessment of mercury and other metals in biospecimens from the 
# Strong Heart Family Study
#
#                                                                                                                                                                 
#   TABLE OF CONTENTS                                                               
#   0. Set-up: Load packages, load data, exclusion criteria
#   1. 
#   2.  
#   3.  
#
#   Contact:                                                                        
#   Marisa Sobel, MPH                                                            
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
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(colorspace)
library(tidyr)
library(tidyverse)
library(stringr)
library(janitor)
library(tidylog)
library(survey)
library(openxlsx)
library(extrafontdb)
library(extrafont)
loadfonts()

datapath <- ".Data/NHANES_data/"
folder.output <- "Output/07-08"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ))
}
outpath <- paste0("./", folder.output)

convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "," , vector[ 3 ], ")" ) 
  return( vector )
}

######      0B. Load NHANES data, clean and tidy data #########

##----- Load NHANES 2007-2008 -----##
datapath <- ".Data/NHANES_data/07-08/"
folder.output <- "Output/07-08/"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ))
}
outpath <- paste0("./", folder.output)

setwd("/Users/marisasobel/Desktop/Maya & Metals/NHANES_metals/NHANES_data/07-08")

# NHANES 2007-2008 is urinary mercury in 1/3 subset of all participants >6yrs age; blood mercury is children 1-5 and women childbearing age
# Demographics
# Urinary Metals: creatinine + metals [Ba, Be, Cd, Co, Cs, Mo, Pb, Pt, Sb, Tl, W, U]
# Urinary Arsenic: creatinine + totals + speciated 
# Blood Metals: 

# DATASETS
# demo
demo0708 <- read.xport("DEMO_E.XPT") %>% as_tibble()
dim(demo0708)
# pregnancy 
preg0708 <- read.xport("UCPREG_E.XPT") %>% as_tibble()
dim(preg0708)
# urinary metals 
ur0708 <- read.xport("UHM_E.XPT") %>% as_tibble()
dim(ur0708)
# urinary arsenic
as0708 <- read.xport("UAS_E.XPT") %>% as_tibble() %>% select(- WTSA2YR, -URXUCR) # remove duplicate variables 
dim(as0708)
# blood metals
#bl0708 <- read.xport("PBCD_E.XPT") # includes blood mercury, lead, cadmium --> see if Annie has a file!

# CHECKING VARIABLES 
# merge (Annie's way)
nh0708 <-merge(demo0708,ur0708,by="SEQN",all=T)
nh0708 <-merge(nh0708,as0708,by="SEQN",all=T)
dim(nh0708) # 10149 col, 85 var
colnames(nh0708) # --> same problem 

# checking the two repeated variables are the same (weights and urinary creatinine) --> they are, remove from 2nd file before merging 

# JOIN --- N=10149 for 07-08 year ---> N= 2,720 with urinary measurements 
nh0708 <- left_join(demo0708, ur0708, by = "SEQN") %>% 
  left_join(as0708, by = "SEQN") %>% 
  left_join(preg0708, by = "SEQN")

# MAKE CREATININE INTO CORRECT UNITS + MAKE NEW URINARY METAL VAR
nh0708 = nh0708 %>% 
  mutate(cr_g_l = URXUCR / 100) %>% 
  mutate(
    uba_ugg = URXUBA / cr_g_l, 
    ube_ugg = URXUBE / cr_g_l, 
    ucd_ugg = URXUCD / cr_g_l, 
    uco_ugg = URXUCO / cr_g_l, 
    ucs_ugg = URXUCS / cr_g_l, 
    umo_ugg = URXUMO / cr_g_l, 
    upb_ugg = URXUPB / cr_g_l, 
    upt_ugg = URXUPT / cr_g_l, 
    usb_ugg = URXUSB / cr_g_l, 
    utl_ugg = URXUTL / cr_g_l, 
    uw_ugg = URXUTU / cr_g_l, 
    uu_ugg = URXUUR / cr_g_l)

# 7492 missing urinary creatinine 
nh0708 %>% 
  select(URXUCR, contains("URX"), contains("ugg")) %>% 
  filter(is.na(URXUCR)) %>% 
  tabyl(URXUCR)

# pregnancy status by demographic variable 
nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,096
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 3,845
  tabyl(RIDEXPRG) %>% adorn_totals()                # correct numbers based on NHANES DEMO documentation
                                                    # 57 pregnant, 1096 not, 55 cannot ascertain 
# pregnancy status by lab variable 
nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,096
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 3,845
  tabyl(URXPREG) %>% adorn_totals()      # 57 positive, 1094 negative, 28 not done, 29 missing 
  
# CREATE DATASET --- limit to females aged 20-44 with urinary creatinine (N=388)
nh0708_f = nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,096
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 3,845
  drop_na(URXUCR)                                   # drop missing urinary creatinine --> remove 820

### METALS ###
u_metals_ALOD <- nh0708_f %>% select(contains("URD")) %>% select(1:12)
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

# PERCENT AT/ABOVE LOD 
ba_pct <- nh0708_f %>% tabyl(URDUBALC) %>% filter(URDUBALC == 0) %>% pull(valid_percent) 
be_pct <- nh0708_f %>% tabyl(URDUBELC) %>% filter(URDUBELC == 0) %>% pull(valid_percent)
cd_pct <- nh0708_f %>% tabyl(URDUCDLC) %>% filter(URDUCDLC == 0) %>% pull(valid_percent) 
co_pct <- nh0708_f %>% tabyl(URDUCOLC) %>% filter(URDUCOLC == 0) %>% pull(valid_percent)
cs_pct <- nh0708_f %>% tabyl(URDUCSLC) %>% filter(URDUCSLC == 0) %>% pull(valid_percent)
mo_pct <- nh0708_f %>% tabyl(URDUMOLC) %>% filter(URDUMOLC == 0) %>% pull(valid_percent) 
pb_pct <- nh0708_f %>% tabyl(URDUPBLC) %>% filter(URDUPBLC == 0) %>% pull(valid_percent) 
pt_pct <- nh0708_f %>% tabyl(URDUPTLC) %>% filter(URDUPTLC == 0) %>% pull(valid_percent) 
sb_pct <- nh0708_f %>% tabyl(URDUSBLC) %>% filter(URDUSBLC == 0) %>% pull(valid_percent)
tl_pct <- nh0708_f %>% tabyl(URDUTLLC) %>% filter(URDUTLLC == 0) %>% pull(valid_percent) 
w_pct <- nh0708_f %>% tabyl(URDUTULC) %>% filter(URDUTULC == 0) %>% pull(valid_percent) 
u_pct <- nh0708_f %>% tabyl(URDUURLC) %>% filter(URDUURLC == 0) %>% pull(valid_percent) 

metals_ALOD_table <- rbind(ba_pct, be_pct, cd_pct, co_pct, cs_pct, mo_pct, pb_pct, pt_pct, sb_pct, tl_pct, w_pct, u_pct) 

metals_ALOD_table <- metals_ALOD_table %>% 
  as_tibble() %>% 
  rename(percent = V1) %>% 
  mutate(
    metal = u_metals_names, 
    use = ifelse(percent >= 0.4, "yes", "no")
  )

metals_ALOD_table

u_metals_ALOD <- nh0708_f %>% select(contains("URD")) %>% select(1:12) %>% select(-contains(c("be", "pt")))
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

# RAW 
# get subset of interest 
metals <- nh0708_f %>% select(contains("ugg")) %>% select(-contains(c("be", "pt"))) 
dim(metals)

urine0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh0708_f) #use urinary sample weights; 1/3 random subsample

stats <- tibble(
  n = map_dbl(metals, function(x) length(x[!is.na(x)])),
  min = map_dbl(metals, min, na.rm = TRUE), 
  max = map_dbl(metals, max, na.rm = TRUE), 
  means = map_dbl(metals, mean, na.rm = TRUE), 
  geometric_means = map_dbl(metals, function(x) exp(mean(log(x), na.rm = TRUE))),
  svy_25th = map_dbl(metals, function(x) svyquantile(~x, urine0708.svy, 0.25 , ci=F, na.rm=T)),
  svy_median = map_dbl(metals, function(x) svyquantile(~x, urine0708.svy, 0.5 , ci=F, na.rm=T)),
  svy_75th = map_dbl(metals, function(x) svyquantile(~x, urine0708.svy, 0.75 , ci=F, na.rm=T))) %>% 
  mutate(
    metal = u_metals_names, 
    NHANES = "2007-2008") %>% 
  relocate(metal)

stats

# SET SURVEY DESIGN ---
urine0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=nh0708_f) #use urinary sample weights; 1/3 random subsample
# Original distribution
nav<-nrow(nh0708_f[which(!is.na(nh0708_f$URXUBA)),])
min<-min(nh0708_f$URXUBA,na.rm=T)
quant<-as.data.frame(svyquantile(~URXUBA, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2<- data.frame(t(quant[]))
max<-max(nh0708_f$URXUBA,na.rm=T)
mean<-mean(nh0708_f$URXUBA,na.rm=T)
gm1<-mean(log(nh0708_f$URXUBA),na.rm=T)
gm<-exp(gm1)
group<-"NHANES 0708"
NH0708<-rbind(nav,min,quant2,max,mean,gm,group)
rownames(NH0708)[1] <- "N"
rownames(NH0708)[2] <- "Min"
rownames(NH0708)[10] <- "Max"
rownames(NH0708)[11] <- "AM"
rownames(NH0708)[12] <- "Geo mean"
rownames(NH0708)[13] <- "Group"
NH0708


