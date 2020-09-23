#Madeline Masucci
#2020 PhUSE US Hackathon
#September 21-23

#Load packages
library(dplyr, lib.loc = "/opt/R/4.0.2/lib/R/library")

#Link to PODR
connect_podr('phuse_obqfkbynv2')
#Pass: ci52FCPHn75A

#Load SDTM Datasets
dm <- read_podr('dm','janssen_synthetic')
vs <- read_podr('vs','janssen_synthetic')
ex <- read_podr('ex','janssen_synthetic')
ds <- read_podr('ds','janssen_synthetic')
lb <- read_podr('lb','janssen_synthetic')
ae <- read_podr('ae','janssen_synthetic')
sv <- read_podr('sv','janssen_synthetic')
zr <- read_podr('zr','janssen_synthetic')

###Keep DM Variables needed in ADSL
adsl <- dm %>%
  select(STUDYID, USUBJID, ARM, ARMCD, ACTARM, ACTARMCD, AGE, AGEU, SEX, 
         RACE, COUNTRY, ETHNIC, DTHDTC, DTHFL, INVNAM, SITEID, SUBJID)

###Derive variables TRT01P(N), TRT01A(N), REGION1, AGEGR1(N), AGEGR2(N), RACEGR1(N), DTHDT
adsl <- adsl %>%
  mutate(TRT01P=ARM,
         TRT01PN=ifelse(ARMCD=='TRTA', 1, 
                        ifelse(ARMCD=='TRTB', 2, NA_real_)),
         TRT01A=ACTARM,
         TRT01AN=ifelse(ACTARMCD=='TRTA', 1, 
                        ifelse(ACTARMCD=='TRTB', 2, NA_real_)),
         REGION1=ifelse((COUNTRY=='GBR' |COUNTRY=='BEL' | COUNTRY=='SWE' | COUNTRY=='POL' ), 'Europe',
                        ifelse((COUNTRY=='USA'|COUNTRY=='CAN'), 'North America', NA_character_)),
         AGEGR1=ifelse((18<=AGE & AGE<=59), '18-59', 
                       ifelse(AGE>=60, '>=60', NA_character_)),
         AGEGR1N=ifelse((18<=AGE & AGE<=59), 1, 
                        ifelse(AGE>=60, 2, NA_real_)),
         AGEGR2=ifelse((18<=AGE & AGE<=30), '18-30', 
                       ifelse((31<=AGE & AGE<=50), '31-50', 
                       ifelse((51<=AGE & AGE<=64), '51-64', 
                       ifelse(AGE>=65, '>=65', NA_character_)))),
         AGEGR2N=ifelse((18<=AGE & AGE<=30), 1, 
                        ifelse((31<=AGE & AGE<=50), 2, 
                        ifelse((51<=AGE & AGE<=64), 3, 
                        ifelse(AGE>=65, 4, NA_real_)))),
         RACEGR1=ifelse(RACE=='WHITE', 'White', 
                        ifelse(RACE=='BLACK OR AFRICAN AMERICAN', 'Black or African American', 
                        ifelse(RACE=='ASIAN', 'Asian', 'Other'))),
         RACEGR1N=ifelse(RACE=='WHITE', 1, 
                         ifelse(RACE=='BLACK OR AFRICAN AMERICAN', 2, 
                         ifelse(RACE=='ASIAN', 3, 4))),
         DTHDT=as.Date(DTHDTC))

### Derive ITTFL, SAFFL, MITTFL, RANDFL, PKEVLFL, COMPLFL

#RANDFL
ds_ <- ds %>%
  filter(DSDECOD=='RANDOMIZED') %>%
  distinct(USUBJID) %>%
  select(USUBJID)
randfl_ <- ds_ %>%
  mutate(RANDFL='Y')
adsl <- adsl %>%
  left_join(., randfl_, by='USUBJID')

#ITTFL
vs_ <- vs %>%
  filter(VSTESTCD=='WEIGHT', VSBLFL=='Y') %>%
  distinct(USUBJID) %>%
  select(USUBJID)
ittfl_ <- inner_join(ds_, vs_, by='USUBJID') %>%
  mutate(ITTFL='Y')
adsl <- adsl %>%
  left_join(., ittfl_, by='USUBJID')

#SAFFL
ex_ <- ex %>%
  filter(length(EXSTDTC)>=10) %>%
  distinct(USUBJID) %>%
  select(USUBJID)
saffl_ <- inner_join(ds_, ex_, by='USUBJID') %>%
  mutate(SAFFL='Y')
adsl <- adsl %>%
  left_join(., saffl_, by='USUBJID')

#MITTFL
zr_ <- zr %>%
  filter(ZRTESTCD=='DOSE', ZRORRES=='5.0 MG') %>%
  distinct(USUBJID) %>%
  select(USUBJID) %>%
  mutate(DOSEFL='Y')
dm_ <- dm %>%
  filter(ARM=='TREATMENT B') %>%
  distinct(USUBJID) %>%
  select(USUBJID)%>%
  mutate(TRTARMFL='Y')
vs2_ <- vs %>%
  filter(VSTESTCD=='WEIGHT', VISITNUM>2000) %>%
  distinct(USUBJID) %>%
  select(USUBJID) %>%
  mutate(PBLWGTFL='Y')
mittfl_ <- full_join(zr_, dm_, by='USUBJID') %>%
  full_join(.,vs2_, by='USUBJID') %>%
  filter(PBLWGTFL=='Y' & (is.na(TRTARMFL) | (TRTARMFL=='Y' & DOSEFL=='Y'))) %>%
  mutate(MITTFL='Y') %>%
  select(USUBJID, MITTFL)
adsl <- adsl %>%
  left_join(., mittfl_, by='USUBJID')

#PKEVLFL


#COMPLFL
ds2_ <- ds %>%
  filter(DSSCAT=='TREATMENT', DSDECOD=='COMPLETED') %>%
  select(USUBJID)
complfl_ <- ds2_ %>%
  mutate(COMPLFL='Y')
adsl <- adsl %>%
  left_join(., complfl_, by='USUBJID')

###Derive ARFSTDT, ARFSTDTM, ARFENDT, TRTSDT, TRTSDTM, TRTEDT, TRTEDTM, RANDDT, RFICDT, TRTDURD

#ARFSTDT(M)
ex1_ <- ex %>%
  filter(length(EXSTDTC)>=10, EXSEQ==1) %>%
  select(USUBJID, EXSTDTC)
ds3_ <- ds %>%
  filter(DSDECOD=='RANDOMIZED') %>%
  select(USUBJID, DSSTDTC)
arfstdt_ <- full_join(ex1_, ds3_, by='USUBJID') %>%
  mutate(ARFSTDT=ifelse(!is.na(EXSTDTC) & length(EXSTDTC<=10), substr(EXSTDTC,1,10), DSSTDTC),
         ARFSTDTM=substr(EXSTDTC,12,16)) %>%
  select(USUBJID, ARFSTDT, ARFSTDTM)
adsl <- adsl %>%
  left_join(., arfstdt_, by='USUBJID')

#ARFENDT
ex2_ <- ex %>%
  group_by(USUBJID) %>%
  filter(length(EXSTDTC)>=10, EXSEQ==max(EXSEQ)) %>%
  select(USUBJID, EXSTDTC)
arfendt_ <- full_join(ex2_, ds3_, by='USUBJID') %>%
  mutate(ARFENDT=ifelse(!is.na(EXSTDTC) & length(EXSTDTC<=10), substr(EXSTDTC,1,10), DSSTDTC)) %>%
  select(USUBJID, ARFENDT)
adsl <- adsl %>%
  left_join(., arfendt_, by='USUBJID')

#TRTSDT

#TRTSDTM

#TRTEDT

#TRTEDTM

#RANDDT

#RFICDT

#TRTDURD
















