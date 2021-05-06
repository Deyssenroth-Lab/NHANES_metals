####### NHANES - METALS IN WOMEN #######
# 
# Title: Assessment of NHANES metals concentrations in women of child-bearing age                                                                         
#                                                                                                                                                                 
#   TABLE OF CONTENTS                                                               
#   0. Set-up: Load packages, load data, exclusion criteria
#   1. NHANES 2007-2008
#   2. NHANES 2009-2010
#   3. NHANES 2011-2012
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


# Set paths
setwd("/Users/marisasobel/Desktop/Maya & Metals/NHANES_metals")
datapath <- ".Data/NHANES_data/"
folder.output <- "Output/09-10"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ))
}
outpath <- paste0("./", folder.output)

# FXNS
convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "," , vector[ 3 ], ")" ) 
  return( vector )
}

######      0.A Load NHANES 2007-2008 -----####
# NHANES 2007-2008 is urinary mercury in 1/3 subset of all participants >6yrs age; blood mercury is children 1-5 and women childbearing age
# Demographics
# Urinary u_metals: creatinine + u_metals [Ba, Be, Cd, Co, Cs, Mo, Pb, Pt, Sb, Tl, W, U]
# Urinary Arsenic: creatinine + totals + speciated 
# Blood u_metals: Cd, Pb, Hg (total)

# SET WD
setwd("/Users/marisasobel/Desktop/Maya & Metals/NHANES_metals/NHANES_data/07-08")

## -------- DATASETS -------- ##
# demo
demo0708 <- read.xport("DEMO_E.XPT") %>% as_tibble()
dim(demo0708)
# pregnancy 
preg0708 <- read.xport("UCPREG_E.XPT") %>% as_tibble()
dim(preg0708)
# urinary u_metals 
ur0708 <- read.xport("UHM_E.XPT") %>% as_tibble()
dim(ur0708)
# urinary arsenic
as0708 <- read.xport("UAS_E.XPT") %>% as_tibble() %>% select(- WTSA2YR, -URXUCR) # remove duplicate variables 
dim(as0708)
# blood u_metals
bl0708 <- read.xport("PBCD_E.XPT") # includes blood mercury, lead, cadmium --> see if Annie has a file!
dim(bl0708)

## -------- MERGE -------- ##
# merge (Annie's way)
nh0708 <-merge(demo0708,preg0708,by="SEQN",all=T)
nh0708 <-merge(nh0708,ur0708,by="SEQN",all=T)
nh0708 <-merge(nh0708,as0708,by="SEQN",all=T)
nh0708 <-merge(nh0708,bl0708,by="SEQN",all=T)
nh0708 <- as_tibble(nh0708)
dim(nh0708) # 10149 col, 93 var
colnames(nh0708) # --> same problem 
# checking the two repeated variables are the same (weights and urinary creatinine) --> they are, remove from 2nd file before merging 
# nh0708 has demo, urine u_metals, as urine metabolites, blood u_metals 
#
######      0.B Load NHANES 2009-2010 ####
# NHANES 2009-2010 is urinary mercury in 1/3 subset of all participants >6yrs age; blood mercury is children 1-5 and women childbearing age
# Demographics
# Urinary u_metals: creatinine + u_metals [Ba, Be, Cd, Co, Cs, Mo, Pb, Pt, Sb, Tl, W, U]
# Urinary Arsenic: creatinine + totals + speciated 
# Blood u_metals: Cd, Pb, Hg (total)

# SET WD
setwd("/Users/marisasobel/Desktop/Maya & Metals/NHANES_metals/NHANES_data/09-10")

## -------- DATASETS -------- ##
# demo
demo0910 <- read.xport("DEMO_F.XPT") %>% as_tibble()
dim(demo0910)
# pregnancy 
preg0910 <- read.xport("UCPREG_F.XPT") %>% as_tibble()
dim(preg0910)
# urinary u_metals 
ur0910 <- read.xport("UHM_F.XPT") %>% as_tibble()
dim(ur0910)
# urinary arsenic
as0910 <- read.xport("UAS_F.XPT") %>% as_tibble() %>% select(- WTSA2YR, -URXUCR) # remove duplicate variables 
dim(as0910)
# blood u_metals
bl0910 <- read.xport("PBCD_F.XPT") # includes blood mercury, lead, cadmium --> see if Annie has a file!
dim(bl0910)

## -------- MERGE -------- ##
# merge (Annie's way)
nh0910 <-merge(demo0910,ur0910,by="SEQN",all=T)
nh0910 <-merge(nh0910,as0910,by="SEQN",all=T)
nh0910 <-merge(nh0910,bl0910,by="SEQN",all=T)
nh0910 <-merge(nh0910,preg0910,by="SEQN",all=T)
nh0910 <- as_tibble(nh0910)
dim(nh0910) # 10537 col, 95 var
colnames(nh0910) # --> same problem 
# checking the two repeated variables are the same (weights and urinary creatinine) --> they are, remove from 2nd file before merging 
# nh0910 has demo, urine u_metals, as urine metabolites, blood u_metals 
#
######      0.C Load NHANES 2011-2012 ####
###### 1. NHANES 2007-2008 ####
######      1.A Clean & Tidy ####
# pregnancy status by demographic variable 
nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,053
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 1,208
  tabyl(RIDEXPRG) %>% adorn_totals()                # correct numbers based on NHANES DEMO documentation
                                                    # 57 pregnant, 1096 not, 55 cannot ascertain 
# pregnancy status by lab variable 
nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,053
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 1,208
  tabyl(URXPREG) %>% adorn_totals()                 # 57 positive, 1094 negative, 28 not done, 29 missing 

# pregnancy status comparison
nh0708 %>% 
  as_tibble() %>% 
  tabyl(RIDEXPRG, URXPREG) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_title()

# pregnancy status - NEW VARIABLE 
nh0708 %>% 
  as_tibble() %>% 
  mutate(preg_status = ifelse(RIDEXPRG == 1, 1, 0)) %>% 
  mutate(preg_status1 = case_when(
    RIDEXPRG == 1 & URXPREG == 1 ~ 1, 
    URXPREG == 2 ~ 0)) %>% 
  tabyl(preg_status1) # use this one

# race/ethnicity:    
nh0708 %>%                                          # 1 = Mexican-American 
  tabyl(RIDRETH1)                                   # 2 = Other Hispanic 
                                                    # 3 = Non-Hispanic White 
                                                    # 4 = Non-Hispanic Black
                                                    # 5 = Other race (incl. multi-racial)

# CREATE DATASET --- limit to females aged 20-44 (N=1,208) and make pregnancy variable 
nh0708_f = nh0708 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,053
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 1,208
  mutate(preg_status = case_when(                   # preg_status = 1  --> pregnant 
    RIDEXPRG == 1 & URXPREG == 1 ~ 1,               # preg_status = 0  --> not pregnant
    URXPREG == 2 ~ 0))                              # preg_status = NA --> missing or not known 

######          - URINARY DATA ####
# N=2,720 with urinary metal measurements in TOTAL (from original NHANES data)
# 820 missing urinary creatinine in subset 
nh0708_f %>% 
  select(URXUCR, contains("URX")) %>% 
  filter(is.na(URXUCR)) %>% 
  tabyl(URXUCR)

# MAKE CREATININE INTO CORRECT UNITS + MAKE NEW URINARY METAL VAR
u_metals = nh0708_f %>% 
  drop_na(URXUCR) %>%                               # drop missing urinary creatinine --> remove 388
  filter(!is.na(URXUBA)) %>%                        # drop missing urinary u_metals     --> remove 6
  mutate(cr_g_l = URXUCR / 100) %>%                 # urinary creatinine in mg/dL     --> /100 for g/L
  mutate(                                           # create u_metals in ug/g cr        --> /cr_g_l for ug/g creatinine 
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


# PERCENT AT/ABOVE LOD 
u_metals_ALOD <- u_metals %>% select(contains("URD")) %>% select(1:12)
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

ba_pct <- u_metals %>% tabyl(URDUBALC) %>% filter(URDUBALC == 0) %>% pull(percent) 
be_pct <- u_metals %>% tabyl(URDUBELC) %>% filter(URDUBELC == 0) %>% pull(percent)
cd_pct <- u_metals %>% tabyl(URDUCDLC) %>% filter(URDUCDLC == 0) %>% pull(percent) 
co_pct <- u_metals %>% tabyl(URDUCOLC) %>% filter(URDUCOLC == 0) %>% pull(percent)
cs_pct <- u_metals %>% tabyl(URDUCSLC) %>% filter(URDUCSLC == 0) %>% pull(percent)
mo_pct <- u_metals %>% tabyl(URDUMOLC) %>% filter(URDUMOLC == 0) %>% pull(percent) 
pb_pct <- u_metals %>% tabyl(URDUPBLC) %>% filter(URDUPBLC == 0) %>% pull(percent) 
pt_pct <- u_metals %>% tabyl(URDUPTLC) %>% filter(URDUPTLC == 0) %>% pull(percent) 
sb_pct <- u_metals %>% tabyl(URDUSBLC) %>% filter(URDUSBLC == 0) %>% pull(percent)
tl_pct <- u_metals %>% tabyl(URDUTLLC) %>% filter(URDUTLLC == 0) %>% pull(percent) 
w_pct <- u_metals %>% tabyl(URDUTULC) %>% filter(URDUTULC == 0) %>% pull(percent) 
u_pct <- u_metals %>% tabyl(URDUURLC) %>% filter(URDUURLC == 0) %>% pull(percent) 

u_metals_ALOD_table <- rbind(ba_pct, be_pct, cd_pct, co_pct, cs_pct, mo_pct, pb_pct, pt_pct, sb_pct, tl_pct, w_pct, u_pct) 
colnames(u_metals_ALOD_table) <- "percent_aLOD"

u_metals_ALOD_table <- u_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = u_metals_names, 
    use = ifelse(percent_aLOD >= 0.4, "yes", "no")) %>% 
  relocate(metal)

u_metals_ALOD_table

# URINE u_metals TO USE (2007-2008)
u_metals_ALOD <- nh0708_f %>% select(contains("URD")) %>% select(1:12) %>% select(-contains(c("be", "pt")))
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

# u_metals DATAFRAME ONLY --- grab urine u_metals corrected for urinary creatinine & survey variables  
u_metals <- u_metals %>% 
  select(contains("ugg"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTSA2YR) %>% 
  select(-contains(c("be", "pt"))) %>% 
  drop_na()
dim(u_metals)
head(u_metals)

######          - As SPECIES DATA ####
# N=2,720 with urinary As species measurements in TOTAL (from original NHANES data)
colnames(as0708)
# 820 missing urinary creatinine in subset 
nh0708_f %>% 
  select(URXUCR, contains("URX")) %>% 
  filter(is.na(URXUCR)) %>% 
  tabyl(URXUCR)

# MAKE CREATININE INTO CORRECT UNITS + MAKE NEW URINARY AS VAR (N=372)
as_metals = nh0708_f %>% 
  drop_na(URXUCR) %>%                               # drop missing urinary creatinine --> remove 388
  drop_na(URXUAS) %>%                               # drop missing total As           --> remove 11
  drop_na(URXUAS3) %>%                              # drop missing As species         --> remove 5
  mutate(cr_g_l = URXUCR / 100) %>%                 # urinary creatinine in mg/dL     --> /100 for g/L
  mutate(                                           # create u_metals in ug/g cr      --> /cr_g_l for ug/g creatinine 
    uas_ugg = URXUAS / cr_g_l, 
    uas3_ugg = URXUAS3 / cr_g_l, 
    uas5_ugg = URXUAS5 / cr_g_l, 
    uab_ugg = URXUAB / cr_g_l, 
    uac_ugg = URXUAC / cr_g_l, 
    udma_ugg = URXUDMA / cr_g_l, 
    umma_ugg = URXUMMA / cr_g_l, 
    utmo_ugg = URXUTM / cr_g_l)

# PERCENT AT/ABOVE LOD 
as_metals_ALOD <- as_metals %>% select(contains("URD")) %>% select(13:20)
as_metals_names <- colnames(as_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_remove("L")
as_metals_names <- str_replace(as_metals_names, "DA", "DMA")
as_metals_names <- str_replace(as_metals_names, "TM", "TMO")

as_pct <- as_metals %>% tabyl(URDUASLC) %>% filter(URDUASLC == 0) %>% pull(percent) 
a3_pct <- as_metals %>% tabyl(URDUA3LC) %>% filter(URDUA3LC == 0) %>% pull(percent)
a5_pct <- as_metals %>% tabyl(URDUA5LC) %>% filter(URDUA5LC == 0) %>% pull(percent) 
ab_pct <- as_metals %>% tabyl(URDUABLC) %>% filter(URDUABLC == 0) %>% pull(percent)
ac_pct <- as_metals %>% tabyl(URDUACLC) %>% filter(URDUACLC == 0) %>% pull(percent)
dma_pct <- as_metals %>% tabyl(URDUDALC) %>% filter(URDUDALC == 0) %>% pull(percent) 
mma_pct <- as_metals %>% tabyl(URDUMMAL) %>% filter(URDUMMAL == 0) %>% pull(percent) 
tmo_pct <- as_metals %>% tabyl(URDUTMLC) %>% filter(URDUTMLC == 0) %>% pull(percent) 

as_metals_ALOD_table <- rbind(as_pct, a3_pct, a5_pct, ab_pct, ac_pct, dma_pct, mma_pct, tmo_pct) 
colnames(as_metals_ALOD_table) <- "percent_aLOD"

as_metals_ALOD_table <- as_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = as_metals_names, 
    use = ifelse(percent_aLOD >= 0.4, "yes", "no")) %>% 
  relocate(metal)

as_metals_ALOD_table

# URINE As metals TO USE (2007-2008)
as_metals_ALOD <- as_metals %>% select(contains("URD")) %>% select(13:20) %>% select(-contains(c("a3", "a5", "ac", "tm")))
as_metals_names <- colnames(as_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_remove("L")
as_metals_names <- str_replace(as_metals_names, "DA", "DMA")

# As metals DATAFRAME ONLY --- grab as species corrected for urinary creatinine & survey variables (N=372)
as_metals <- as_metals %>% 
  select(contains("ugg"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTSA2YR) %>% 
  select(-contains(c("as3", "as5", "ac", "tm"))) %>% 
  drop_na()
dim(as_metals)
head(as_metals)

######          - BLOOD DATA ####
# N=9307 with urinary As species measurements in TOTAL (from original NHANES data)
dim(bl0708)
colnames(bl0708)

# drop missing blood metals
b_metals = nh0708_f %>% 
  drop_na(LBXBCD)                                   # drop missing blood metals --> remove 106

# PERCENT AT/ABOVE LOD 
b_metals_total <- b_metals %>% count(SEQN) %>% tally(n) %>% pull(n)
b_metals_ALOD <- b_metals %>% select(contains("LBX")) 
b_metals_names <- colnames(b_metals_ALOD) %>% str_remove("LBXB") %>% str_remove("LBX")

cd_n <- b_metals %>% filter(LBXBCD <= 0.14) %>% count(SEQN) %>% tally(n) %>% pull(n)
cd_pct <- cd_n / b_metals_total
pb_n <- b_metals %>% filter(LBXBPB <= 0.18) %>% count(SEQN) %>% tally(n) %>% pull(n)
pb_pct <- pb_n / b_metals_total
thg_n <- b_metals %>% filter(LBDTHGLC == 1) %>% count(SEQN) %>% tally(n) %>% pull(n)
thg_pct <- thg_n / b_metals_total 

b_metals_ALOD_table <- rbind(cd_pct, pb_pct, thg_pct) 
colnames(b_metals_ALOD_table) <- "percent_BLOD"

b_metals_ALOD_table <- b_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = b_metals_names, 
    use = ifelse(percent_BLOD < 0.4, "yes", "no")) %>% 
  relocate(metal)

b_metals_ALOD_table

# Blood metals DATAFRAME ONLY --- grab as species corrected for urinary creatinine & survey variables (N=1102)
b_metals <- b_metals %>% 
  select(contains("LBX"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTMEC2YR) 
dim(b_metals)
head(b_metals)

######      1.B URINARY ANALYSES ####
u_metals_names
# SET SURVEY DESIGN 
urine0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=u_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, urine0708.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), urine0708.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(uba_ugg), urine0708.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(u_metals$uba_ugg)

######          - Ba ####
nav <- nrow(u_metals[which(!is.na(u_metals$uba_ugg)),])
min <- min(u_metals$uba_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uba_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uba_ugg,na.rm=T)
mean <- svymean(~uba_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(uba_ugg), urine0708.svy, na.rm = TRUE))
NH0708_U <- rbind(nav,min,quant2,max,mean,gm)
#
######          - Cd ####
nav <- nrow(u_metals[which(!is.na(u_metals$ucd_ugg)),])
min <- min(u_metals$ucd_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~ucd_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$ucd_ugg,na.rm=T)
mean <- svymean(~ucd_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(ucd_ugg), urine0708.svy, na.rm = TRUE))
NH0708_cd <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U, NH0708_cd)
#
######          - Co ####
nav <- nrow(u_metals[which(!is.na(u_metals$uco_ugg)),])
min <- min(u_metals$uco_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uco_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uco_ugg,na.rm=T)
mean <- svymean(~uco_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(uco_ugg), urine0708.svy, na.rm = TRUE))
NH0708_co <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U  <-cbind(NH0708_U,NH0708_co)
#
######          - Cs ####
nav <- nrow(u_metals[which(!is.na(u_metals$ucs_ugg)),])
min <- min(u_metals$ucs_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~ucs_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$ucs_ugg,na.rm=T)
mean <- svymean(~ucs_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(ucs_ugg), urine0708.svy, na.rm = TRUE))
NH0708_cs <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_cs)
#
######          - Mo ####
nav <- nrow(u_metals[which(!is.na(u_metals$umo_ugg)),])
min <- min(u_metals$umo_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~umo_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$umo_ugg,na.rm=T)
mean <- svymean(~umo_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(umo_ugg), urine0708.svy, na.rm = TRUE))
NH0708_mo <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_mo)
#

######          - Pb ####
nav <- nrow(u_metals[which(!is.na(u_metals$upb_ugg)),])
min <- min(u_metals$upb_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~upb_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$upb_ugg,na.rm=T)
mean <- svymean(~upb_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(upb_ugg), urine0708.svy, na.rm = TRUE))
NH0708_pb <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_pb)
#
######          - Sb ####
nav <- nrow(u_metals[which(!is.na(u_metals$usb_ugg)),])
min <- min(u_metals$usb_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~usb_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$usb_ugg,na.rm=T)
mean <- svymean(~usb_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(usb_ugg), urine0708.svy, na.rm = TRUE))
NH0708_sb <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_sb)
#
######          - Tl ####
nav <- nrow(u_metals[which(!is.na(u_metals$utl_ugg)),])
min <- min(u_metals$utl_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~utl_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$utl_ugg,na.rm=T)
mean <- svymean(~utl_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(utl_ugg), urine0708.svy, na.rm = TRUE))
NH0708_tl <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_tl)
#
######          - W  ####
nav <- nrow(u_metals[which(!is.na(u_metals$uw_ugg)),])
min <- min(u_metals$uw_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uw_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uw_ugg,na.rm=T)
mean <- svymean(~uw_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(uw_ugg), urine0708.svy, na.rm = TRUE))
NH0708_w <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_w)
#
######          - U  ####
nav <- nrow(u_metals[which(!is.na(u_metals$uu_ugg)),])
min <- min(u_metals$uu_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uu_ugg, urine0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uu_ugg,na.rm=T)
mean <- svymean(~uu_ugg, urine0708.svy, na.rm=T)
gm <- exp(svymean(~log(uu_ugg), urine0708.svy, na.rm = TRUE))
NH0708_u <- rbind(nav,min,quant2,max,mean,gm)
NH0708_U <- cbind(NH0708_U,NH0708_u)
######          --------TABLE ####
rownames(NH0708_U)[1] <- "N"
rownames(NH0708_U)[2] <- "Min"
rownames(NH0708_U)[10] <- "Max"
rownames(NH0708_U)[11] <- "Mean"
rownames(NH0708_U)[12] <- "Geometric Mean"
colnames(NH0708_U) <- u_metals_names
NH0708_U <- round(NH0708_U, digits = 5)
NH0708_U

######      1. C ARSENIC SPECIES ANALYSES ####
as_metals_names
# SET SURVEY DESIGN 
as0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=as_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, as0708.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), as0708.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(uas_ugg), as0708.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(as_metals$uas_ugg)


######          - AS ####
nav <- nrow(as_metals[which(!is.na(as_metals$uas_ugg)),])
min <- min(as_metals$uas_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uas_ugg, as0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$uas_ugg,na.rm=T)
mean <- svymean(~uas_ugg, as0708.svy, na.rm=T)
gm <- exp(svymean(~log(uas_ugg), as0708.svy, na.rm = TRUE))
NH0708_AS <- rbind(nav,min,quant2,max,mean,gm)
#
######          - AB ####
nav <- nrow(as_metals[which(!is.na(as_metals$uab_ugg)),])
min <- min(as_metals$uab_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uab_ugg, as0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$uab_ugg,na.rm=T)
mean <- svymean(~uab_ugg, as0708.svy, na.rm=T)
gm <- exp(svymean(~log(uab_ugg), as0708.svy, na.rm = TRUE))
NH0708_AB <- rbind(nav,min,quant2,max,mean,gm)
NH0708_AS <- cbind(NH0708_AS, NH0708_AB)
#
######          - DMA ####
nav <- nrow(as_metals[which(!is.na(as_metals$udma_ugg)),])
min <- min(as_metals$udma_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~udma_ugg, as0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$udma_ugg,na.rm=T)
mean <- svymean(~udma_ugg, as0708.svy, na.rm=T)
gm <- exp(svymean(~log(udma_ugg), as0708.svy, na.rm = TRUE))
NH0708_dma <- rbind(nav,min,quant2,max,mean,gm)
NH0708_AS <- cbind(NH0708_AS, NH0708_dma)
#
######          - MMA ####
nav <- nrow(as_metals[which(!is.na(as_metals$umma_ugg)),])
min <- min(as_metals$umma_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~umma_ugg, as0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$umma_ugg,na.rm=T)
mean <- svymean(~umma_ugg, as0708.svy, na.rm=T)
gm <- exp(svymean(~log(umma_ugg), as0708.svy, na.rm = TRUE))
NH0708_mma <- rbind(nav,min,quant2,max,mean,gm)
NH0708_AS <- cbind(NH0708_AS, NH0708_mma)
#
######          --------TABLE ####
rownames(NH0708_AS)[1] <- "N"
rownames(NH0708_AS)[2] <- "Min"
rownames(NH0708_AS)[10] <- "Max"
rownames(NH0708_AS)[11] <- "Mean"
rownames(NH0708_AS)[12] <- "Geometric Mean"
colnames(NH0708_AS) <- as_metals_names
NH0708_AS <- round(NH0708_AS, digits = 5)
NH0708_AS

######      1. D BlOOD ANALYSES ####
b_metals_names
# SET SURVEY DESIGN 
blood0708.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=b_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, blood0708.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), blood0708.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(LBXBCD), blood0708.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(b_metals$LBXBCD)


######          - CD ####
nav <- nrow(b_metals[which(!is.na(b_metals$LBXBCD)),])
min <- min(b_metals$LBXBCD,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXBCD, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXBCD,na.rm=T)
mean <- svymean(~LBXBCD, blood0708.svy, na.rm=T)
gm <- exp(svymean(~log(LBXBCD), blood0708.svy, na.rm = TRUE))
NH0708_B <- rbind(nav,min,quant2,max,mean,gm)
#
######          - PB ####
min <- min(b_metals$LBXBPB,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXBPB, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXBPB,na.rm=T)
mean <- svymean(~LBXBPB, blood0708.svy, na.rm=T)
gm <- exp(svymean(~log(LBXBPB), blood0708.svy, na.rm = TRUE))
NH0708_PB <- rbind(nav,min,quant2,max,mean,gm)
NH0708_B <- cbind(NH0708_B, NH0708_PB)
#
######          - THG ####
min <- min(b_metals$LBXTHG,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXTHG, blood0708.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXTHG,na.rm=T)
mean <- svymean(~LBXTHG, blood0708.svy, na.rm=T)
gm <- exp(svymean(~log(LBXTHG), blood0708.svy, na.rm = TRUE))
NH0708_THG <- rbind(nav,min,quant2,max,mean,gm)
NH0708_B <- cbind(NH0708_B, NH0708_THG)
#
######          --------TABLE ####
rownames(NH0708_B)[1] <- "N"
rownames(NH0708_B)[2] <- "Min"
rownames(NH0708_B)[10] <- "Max"
rownames(NH0708_B)[11] <- "Mean"
rownames(NH0708_B)[12] <- "Geometric Mean"
colnames(NH0708_B) <- b_metals_names
NH0708_B <- round(NH0708_B, digits = 5)
NH0708_B


###### 2. NHANES 2009-2010 ####
######      2.A Clean & Tidy ####
# pregnancy status by demographic variable 
nh0910 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,312
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 1,405
  tabyl(RIDEXPRG) %>% adorn_totals()                # correct numbers based on NHANES DEMO documentation
                                                    # 68 pregnant, 1266 not, 71 cannot ascertain 
# pregnancy status by lab variable 
nh0910 %>% 
  filter(RIAGENDR == 2) %>%                       # 1 = male, 2 = female            --> remove 5,312
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%     # restrict age                    --> remove 1,405
  tabyl(URXPREG) %>% adorn_totals()               # 68 positive, 1288 negative, 21 not done, 28 missing 

# pregnancy status comparison
nh0910 %>%
  as_tibble() %>%
  tabyl(RIDEXPRG, URXPREG) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_title()

# pregnancy status - NEW VARIABLE 
nh0910 %>%
  as_tibble() %>%
  mutate(preg_status = ifelse(RIDEXPRG == 1, 1, 0)) %>%
  mutate(preg_status1 = case_when(
    RIDEXPRG == 1 & URXPREG == 1 ~ 1,
    URXPREG == 2 ~ 0)) %>%
  tabyl(preg_status1) # use this one

# race/ethnicity:    
nh0910 %>%                                          # 1 = Mexican-American 
  tabyl(RIDRETH1)                                   # 2 = Other Hispanic 
                                                    # 3 = Non-Hispanic White 
                                                    # 4 = Non-Hispanic Black
                                                    # 5 = Other race (incl. multi-racial)

# CREATE DATASET --- limit to females aged 20-44 (N=1,208) and make pregnancy variable 
nh0910_f = nh0910 %>% 
  filter(RIAGENDR == 2) %>%                         # 1 = male, 2 = female            --> remove 5,312
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 44) %>%       # restrict age                    --> remove 1,405
  mutate(preg_status = case_when(                   # preg_status = 1  --> pregnant
    RIDEXPRG == 1 & URXPREG == 1 ~ 1,               # preg_status = 0  --> not pregnant
    URXPREG == 2 ~ 0))                              # preg_status = NA --> missing or not known
#
######          - URINARY DATA ####
# N=2941 with urinary metal measurements in TOTAL (from original NHANES data)
# 929 missing urinary creatinine in subset 
nh0910_f %>% 
  select(URXUCR, contains("URX")) %>% 
  filter(is.na(URXUCR)) %>% 
  tabyl(URXUCR)

# MAKE CREATININE INTO CORRECT UNITS + MAKE NEW URINARY METAL VAR
u_metals = nh0910_f %>% 
  drop_na(URXUCR) %>%                               # drop missing urinary creatinine --> remove 929
  filter(!is.na(URXUBA)) %>%                        # drop missing urinary u_metals   --> remove 9
  mutate(cr_g_l = URXUCR / 100) %>%                 # urinary creatinine in mg/dL     --> /100 for g/L
  mutate(                                           # create u_metals in ug/g cr      --> /cr_g_l for ug/g creatinine 
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


# PERCENT AT/ABOVE LOD 
u_metals_ALOD <- u_metals %>% select(contains("URD")) %>% select(1:12)
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

ba_pct <- u_metals %>% tabyl(URDUBALC) %>% filter(URDUBALC == 0) %>% pull(percent) 
be_pct <- u_metals %>% tabyl(URDUBELC) %>% filter(URDUBELC == 0) %>% pull(percent)
be_pct <- 0
cd_pct <- u_metals %>% tabyl(URDUCDLC) %>% filter(URDUCDLC == 0) %>% pull(percent) 
co_pct <- u_metals %>% tabyl(URDUCOLC) %>% filter(URDUCOLC == 0) %>% pull(percent)
cs_pct <- u_metals %>% tabyl(URDUCSLC) %>% filter(URDUCSLC == 0) %>% pull(percent)
mo_pct <- u_metals %>% tabyl(URDUMOLC) %>% filter(URDUMOLC == 0) %>% pull(percent) 
pb_pct <- u_metals %>% tabyl(URDUPBLC) %>% filter(URDUPBLC == 0) %>% pull(percent) 
pt_pct <- u_metals %>% tabyl(URDUPTLC) %>% filter(URDUPTLC == 0) %>% pull(percent) 
sb_pct <- u_metals %>% tabyl(URDUSBLC) %>% filter(URDUSBLC == 0) %>% pull(percent)
tl_pct <- u_metals %>% tabyl(URDUTLLC) %>% filter(URDUTLLC == 0) %>% pull(percent) 
w_pct <- u_metals %>% tabyl(URDUTULC) %>% filter(URDUTULC == 0) %>% pull(percent) 
u_pct <- u_metals %>% tabyl(URDUURLC) %>% filter(URDUURLC == 0) %>% pull(percent) 

u_metals_ALOD_table <- rbind(ba_pct, be_pct, cd_pct, co_pct, cs_pct, mo_pct, pb_pct, pt_pct, sb_pct, tl_pct, w_pct, u_pct) 
colnames(u_metals_ALOD_table) <- "percent_aLOD"

u_metals_ALOD_table <- u_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = u_metals_names, 
    use = ifelse(percent_aLOD >= 0.4, "yes", "no")) %>% 
  relocate(metal)

u_metals_ALOD_table

# URINE u_metals TO USE (2009-2010)
u_metals_ALOD <- nh0910_f %>% select(contains("URD")) %>% select(1:12) %>% select(-contains(c("be", "pt")))
u_metals_names <- colnames(u_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_to_title()

# u_metals DATAFRAME ONLY --- grab urine u_metals corrected for urinary creatinine & survey variables (N=467)
u_metals <- u_metals %>% 
  select(contains("ugg"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTSA2YR) %>%  
  select(-contains(c("be", "pt"))) %>% 
  drop_na(-preg_status)
dim(u_metals)
head(u_metals)

######          - As SPECIES DATA ####
# N=2941 with urinary As species measurements in TOTAL (from original NHANES data)
dim(as0910)
colnames(as0910)
# 929 missing urinary creatinine in subset 
nh0910_f %>% 
  select(URXUCR, contains("URX")) %>% 
  filter(is.na(URXUCR)) %>% 
  tabyl(URXUCR)

# MAKE CREATININE INTO CORRECT UNITS + MAKE NEW URINARY AS VAR (N=471)
as_metals = nh0910_f %>% 
  drop_na(URXUCR) %>%                               # drop missing urinary creatinine --> remove 929
  drop_na(URXUAS) %>%                               # drop missing total As           --> remove 5
  # drop_na(URXUAS3) %>%                            # none missing As species
  mutate(cr_g_l = URXUCR / 100) %>%                 # urinary creatinine in mg/dL     --> /100 for g/L
  mutate(                                           # create u_metals in ug/g cr      --> /cr_g_l for ug/g creatinine 
    uas_ugg = URXUAS / cr_g_l, 
    uas3_ugg = URXUAS3 / cr_g_l, 
    uas5_ugg = URXUAS5 / cr_g_l, 
    uab_ugg = URXUAB / cr_g_l, 
    uac_ugg = URXUAC / cr_g_l, 
    udma_ugg = URXUDMA / cr_g_l, 
    umma_ugg = URXUMMA / cr_g_l, 
    utmo_ugg = URXUTM / cr_g_l)

# PERCENT AT/ABOVE LOD 
as_metals_ALOD <- as_metals %>% select(contains("URD")) %>% select(13:20)
as_metals_names <- colnames(as_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_remove("L")
as_metals_names <- str_replace(as_metals_names, "DA", "DMA")
as_metals_names <- str_replace(as_metals_names, "TM", "TMO")

as_pct <- as_metals %>% tabyl(URDUASLC) %>% filter(URDUASLC == 0) %>% pull(percent) 
a3_pct <- as_metals %>% tabyl(URDUA3LC) %>% filter(URDUA3LC == 0) %>% pull(percent)
a5_pct <- as_metals %>% tabyl(URDUA5LC) %>% filter(URDUA5LC == 0) %>% pull(percent) 
ab_pct <- as_metals %>% tabyl(URDUABLC) %>% filter(URDUABLC == 0) %>% pull(percent)
ac_pct <- as_metals %>% tabyl(URDUACLC) %>% filter(URDUACLC == 0) %>% pull(percent)
dma_pct <- as_metals %>% tabyl(URDUDALC) %>% filter(URDUDALC == 0) %>% pull(percent) 
mma_pct <- as_metals %>% tabyl(URDUMMAL) %>% filter(URDUMMAL == 0) %>% pull(percent) 
tmo_pct <- as_metals %>% tabyl(URDUTMLC) %>% filter(URDUTMLC == 0) %>% pull(percent) 

as_metals_ALOD_table <- rbind(as_pct, a3_pct, a5_pct, ab_pct, ac_pct, dma_pct, mma_pct, tmo_pct) 
colnames(as_metals_ALOD_table) <- "percent_aLOD"

as_metals_ALOD_table <- as_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = as_metals_names, 
    use = ifelse(percent_aLOD >= 0.4, "yes", "no")) %>% 
  relocate(metal)

as_metals_ALOD_table

# URINE As metals TO USE (2009-2010)
as_metals_ALOD <- as_metals %>% select(contains("URD")) %>% select(13:20) %>% select(-contains(c("a3", "a5", "ac", "tm")))
as_metals_names <- colnames(as_metals_ALOD) %>% str_remove("URDU") %>% str_remove("LC") %>% str_remove("L")
as_metals_names <- str_replace(as_metals_names, "DA", "DMA")

# As metals DATAFRAME ONLY --- grab as species corrected for urinary creatinine & survey variables (N=471)
as_metals <- as_metals %>% 
  select(contains("ugg"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTSA2YR) %>% 
  select(-contains(c("as3", "as5", "ac", "tm"))) %>% 
  drop_na(-preg_status)
dim(as_metals)
head(as_metals)
#
######          - BLOOD DATA ####
# N=9835 with urinary As species measurements in TOTAL (from original NHANES data)
dim(bl0910)
colnames(bl0910)

# drop missing blood metals
b_metals = nh0910_f %>% 
  drop_na(LBXBCD)                                   # drop missing blood metals --> remove 85

# PERCENT AT/ABOVE LOD 
b_metals_total <- b_metals %>% count(SEQN) %>% tally(n) %>% pull(n)
b_metals_ALOD <- b_metals %>% select(contains("LBX")) 
b_metals_names <- colnames(b_metals_ALOD) %>% str_remove("LBXB") %>% str_remove("LBX")

cd_n <- b_metals %>% filter(LBXBCD <= 0.14) %>% count(SEQN) %>% tally(n) %>% pull(n)
cd_pct <- cd_n / b_metals_total
pb_n <- b_metals %>% filter(LBXBPB <= 0.18) %>% count(SEQN) %>% tally(n) %>% pull(n)
pb_pct <- pb_n / b_metals_total
thg_n <- b_metals %>% filter(LBDTHGLC == 1) %>% count(SEQN) %>% tally(n) %>% pull(n)
thg_pct <- thg_n / b_metals_total 

b_metals_ALOD_table <- rbind(cd_pct, pb_pct, thg_pct) 
colnames(b_metals_ALOD_table) <- "percent_BLOD"

b_metals_ALOD_table <- b_metals_ALOD_table %>% 
  as_tibble() %>% 
  mutate(
    metal = b_metals_names, 
    use = ifelse(percent_BLOD < 0.4, "yes", "no")) %>% 
  relocate(metal)

b_metals_ALOD_table

# Blood metals DATAFRAME ONLY --- grab as species corrected for urinary creatinine & survey variables (N=1320)
b_metals <- b_metals %>% 
  select(contains("LBX"), RIDRETH1, preg_status, SDMVPSU, SDMVSTRA, WTMEC2YR) 
dim(b_metals)
head(b_metals)


######      2.B URINARY ANALYSES ####
u_metals_names
# SET SURVEY DESIGN 
urine0910.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=u_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, urine0910.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), urine0910.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(uba_ugg), urine0910.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(u_metals$uba_ugg)

######          - Ba ####
nav <- nrow(u_metals[which(!is.na(u_metals$uba_ugg)),])
min <- min(u_metals$uba_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uba_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uba_ugg,na.rm=T)
mean <- svymean(~uba_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(uba_ugg), urine0910.svy, na.rm = TRUE))
NH0910_U <- rbind(nav,min,quant2,max,mean,gm)
#
######          - Cd ####
nav <- nrow(u_metals[which(!is.na(u_metals$ucd_ugg)),])
min <- min(u_metals$ucd_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~ucd_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$ucd_ugg,na.rm=T)
mean <- svymean(~ucd_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(ucd_ugg), urine0910.svy, na.rm = TRUE))
NH0910_cd <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U, NH0910_cd)
#
######          - Co ####
nav <- nrow(u_metals[which(!is.na(u_metals$uco_ugg)),])
min <- min(u_metals$uco_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uco_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uco_ugg,na.rm=T)
mean <- svymean(~uco_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(uco_ugg), urine0910.svy, na.rm = TRUE))
NH0910_co <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U  <-cbind(NH0910_U,NH0910_co)
#
######          - Cs ####
nav <- nrow(u_metals[which(!is.na(u_metals$ucs_ugg)),])
min <- min(u_metals$ucs_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~ucs_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$ucs_ugg,na.rm=T)
mean <- svymean(~ucs_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(ucs_ugg), urine0910.svy, na.rm = TRUE))
NH0910_cs <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_cs)
#
######          - Mo ####
nav <- nrow(u_metals[which(!is.na(u_metals$umo_ugg)),])
min <- min(u_metals$umo_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~umo_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$umo_ugg,na.rm=T)
mean <- svymean(~umo_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(umo_ugg), urine0910.svy, na.rm = TRUE))
NH0910_mo <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_mo)
#

######          - Pb ####
nav <- nrow(u_metals[which(!is.na(u_metals$upb_ugg)),])
min <- min(u_metals$upb_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~upb_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$upb_ugg,na.rm=T)
mean <- svymean(~upb_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(upb_ugg), urine0910.svy, na.rm = TRUE))
NH0910_pb <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_pb)
#
######          - Sb ####
nav <- nrow(u_metals[which(!is.na(u_metals$usb_ugg)),])
min <- min(u_metals$usb_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~usb_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$usb_ugg,na.rm=T)
mean <- svymean(~usb_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(usb_ugg), urine0910.svy, na.rm = TRUE))
NH0910_sb <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_sb)
#
######          - Tl ####
nav <- nrow(u_metals[which(!is.na(u_metals$utl_ugg)),])
min <- min(u_metals$utl_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~utl_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$utl_ugg,na.rm=T)
mean <- svymean(~utl_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(utl_ugg), urine0910.svy, na.rm = TRUE))
NH0910_tl <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_tl)
#
######          - W  ####
nav <- nrow(u_metals[which(!is.na(u_metals$uw_ugg)),])
min <- min(u_metals$uw_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uw_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uw_ugg,na.rm=T)
mean <- svymean(~uw_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(uw_ugg), urine0910.svy, na.rm = TRUE))
NH0910_w <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_w)
#
######          - U  ####
nav <- nrow(u_metals[which(!is.na(u_metals$uu_ugg)),])
min <- min(u_metals$uu_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uu_ugg, urine0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(u_metals$uu_ugg,na.rm=T)
mean <- svymean(~uu_ugg, urine0910.svy, na.rm=T)
gm <- exp(svymean(~log(uu_ugg), urine0910.svy, na.rm = TRUE))
NH0910_u <- rbind(nav,min,quant2,max,mean,gm)
NH0910_U <- cbind(NH0910_U,NH0910_u)
######          --------TABLE ####
rownames(NH0910_U)[1] <- "N"
rownames(NH0910_U)[2] <- "Min"
rownames(NH0910_U)[10] <- "Max"
rownames(NH0910_U)[11] <- "Mean"
rownames(NH0910_U)[12] <- "Geometric Mean"
colnames(NH0910_U) <- u_metals_names
NH0910_U <- round(NH0910_U, digits = 5)
NH0910_U



######      2. C ARSENIC SPECIES ANALYSES ####
as_metals_names
# SET SURVEY DESIGN 
as0910.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTSA2YR,nest=T,data=as_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, as0910.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), as0910.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(uas_ugg), as0910.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(as_metals$uas_ugg)


######          - AS ####
nav <- nrow(as_metals[which(!is.na(as_metals$uas_ugg)),])
min <- min(as_metals$uas_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uas_ugg, as0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$uas_ugg,na.rm=T)
mean <- svymean(~uas_ugg, as0910.svy, na.rm=T)
gm <- exp(svymean(~log(uas_ugg), as0910.svy, na.rm = TRUE))
NH0910_AS <- rbind(nav,min,quant2,max,mean,gm)
#
######          - AB ####
nav <- nrow(as_metals[which(!is.na(as_metals$uab_ugg)),])
min <- min(as_metals$uab_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~uab_ugg, as0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$uab_ugg,na.rm=T)
mean <- svymean(~uab_ugg, as0910.svy, na.rm=T)
gm <- exp(svymean(~log(uab_ugg), as0910.svy, na.rm = TRUE))
NH0910_AB <- rbind(nav,min,quant2,max,mean,gm)
NH0910_AS <- cbind(NH0910_AS, NH0910_AB)
#
######          - DMA ####
nav <- nrow(as_metals[which(!is.na(as_metals$udma_ugg)),])
min <- min(as_metals$udma_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~udma_ugg, as0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$udma_ugg,na.rm=T)
mean <- svymean(~udma_ugg, as0910.svy, na.rm=T)
gm <- exp(svymean(~log(udma_ugg), as0910.svy, na.rm = TRUE))
NH0910_dma <- rbind(nav,min,quant2,max,mean,gm)
NH0910_AS <- cbind(NH0910_AS, NH0910_dma)
#
######          - MMA ####
nav <- nrow(as_metals[which(!is.na(as_metals$umma_ugg)),])
min <- min(as_metals$umma_ugg,na.rm=T)
quant <- as.data.frame(svyquantile(~umma_ugg, as0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(as_metals$umma_ugg,na.rm=T)
mean <- svymean(~umma_ugg, as0910.svy, na.rm=T)
gm <- exp(svymean(~log(umma_ugg), as0910.svy, na.rm = TRUE))
NH0910_mma <- rbind(nav,min,quant2,max,mean,gm)
NH0910_AS <- cbind(NH0910_AS, NH0910_mma)
#
######          --------TABLE ####
rownames(NH0910_AS)[1] <- "N"
rownames(NH0910_AS)[2] <- "Min"
rownames(NH0910_AS)[10] <- "Max"
rownames(NH0910_AS)[11] <- "Mean"
rownames(NH0910_AS)[12] <- "Geometric Mean"
colnames(NH0910_AS) <- as_metals_names
NH0910_AS <- round(NH0910_AS, digits = 5)
NH0910_AS
#
######      2. D BlOOD ANALYSES ####
b_metals_names
# SET SURVEY DESIGN 
blood0910.svy <- svydesign(ids=~SDMVPSU,strata=~SDMVSTRA,weights=~WTMEC2YR,nest=T,data=b_metals) #use urinary sample weights; 1/3 random subsample

# SET MEAN FXN
svy_mean <- function(x) { 
  mean_svy <- svymean(~x, blood0910.svy, na.rm = TRUE)
  m_svy <- mean_svy[1]
  return(m_svy)
}

# SET GEO MEAN FXN
svy_geo_mean <- function(x) { 
  meanlog_svy <- exp(svymean(~log(x), blood0910.svy, na.rm = TRUE))
  gm_svy <- meanlog_svy[1]
  return(gm_svy)
}
# check fxn 
meanlog_svy <- exp(svymean(~log(LBXBCD), blood0910.svy, na.rm = TRUE))
gm_svy <- meanlog_svy[1]
gm_svy
svy_geo_mean(b_metals$LBXBCD)


######          - CD ####
nav <- nrow(b_metals[which(!is.na(b_metals$LBXBCD)),])
min <- min(b_metals$LBXBCD,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXBCD, blood0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXBCD,na.rm=T)
mean <- svymean(~LBXBCD, blood0910.svy, na.rm=T)
gm <- exp(svymean(~log(LBXBCD), blood0910.svy, na.rm = TRUE))
NH0910_B <- rbind(nav,min,quant2,max,mean,gm)
#
######          - PB ####
min <- min(b_metals$LBXBPB,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXBPB, blood0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXBPB,na.rm=T)
mean <- svymean(~LBXBPB, blood0910.svy, na.rm=T)
gm <- exp(svymean(~log(LBXBPB), blood0910.svy, na.rm = TRUE))
NH0910_PB <- rbind(nav,min,quant2,max,mean,gm)
NH0910_B <- cbind(NH0910_B, NH0910_PB)
#
######          - THG ####
min <- min(b_metals$LBXTHG,na.rm=T)
quant <- as.data.frame(svyquantile(~LBXTHG, blood0910.svy, c(0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99),ci=F,na.rm=T))
quant2 <- data.frame(t(quant[]))
max <- max(b_metals$LBXTHG,na.rm=T)
mean <- svymean(~LBXTHG, blood0910.svy, na.rm=T)
gm <- exp(svymean(~log(LBXTHG), blood0910.svy, na.rm = TRUE))
NH0910_THG <- rbind(nav,min,quant2,max,mean,gm)
NH0910_B <- cbind(NH0910_B, NH0910_THG)
#
######          --------TABLE ####
rownames(NH0910_B)[1] <- "N"
rownames(NH0910_B)[2] <- "Min"
rownames(NH0910_B)[10] <- "Max"
rownames(NH0910_B)[11] <- "Mean"
rownames(NH0910_B)[12] <- "Geometric Mean"
colnames(NH0910_B) <- b_metals_names
NH0910_B <- round(NH0910_B, digits = 5)
NH0910_B


###### 3. NHANES 2011-2012 ####
######      3.A Clean & Tidy ####
######      3.B URINARY ANALYSES ####
######      3.C As SPECIES ANALYSES ####
######      3.D BLOOD ANALYSES ####
######
######
###### APPENDIX: TABLES ####
######  ----- SET OUTPATH FOR YEAR ----- ####
# 2007-2008 ####
NH0708_U
NH0708_AS
NH0708_B
write_csv(NH0708_U, file = paste0(outpath, "/07-08_urine.csv"))
write_csv(NH0708_AS, file = paste0(outpath, "/07-08_as-species.csv"))
write_csv(NH0708_B, file = paste0(outpath, "/07-08_blood.csv"))
# 2009-2010 ####
NH0910_U
NH0910_AS
NH0910_B
write_csv(NH0910_U, file = paste0(outpath, "/09-10_urine.csv"))
write_csv(NH0910_AS, file = paste0(outpath, "/09-10_as-species.csv"))
write_csv(NH0910_B, file = paste0(outpath, "/09-10_blood.csv"))
# 2011-2012 ####
NH1112_U
NH1112_AS
NH1112_B
######
######
#### ATTEMPT WITH MAP FXN #### 

stats <- tibble(
  n = map_dbl(u_metals, function(x) length(x[!is.na(x)])),
  min = map_dbl(u_metals, min, na.rm = TRUE), 
  max = map_dbl(u_metals, max, na.rm = TRUE), 
  means = map_dbl(u_metals, mean, na.rm = TRUE), 
  geometric_means = map_dbl(u_metals, function(x) svy_geo_mean),
  svy_25th = map_dbl(u_metals, function(x) svyquantile(~x, urine0708.svy, 0.25 , ci=F, na.rm=T)),
  svy_median = map_dbl(u_metals, function(x) svyquantile(~x, urine0708.svy, 0.5 , ci=F, na.rm=T)),
  svy_75th = map_dbl(u_metals, function(x) svyquantile(~x, urine0708.svy, 0.75 , ci=F, na.rm=T))) %>%
  mutate(
    metal = u_metals_names, 
    NHANES = "2007-2008") %>% 
  relocate(metal)

stats
