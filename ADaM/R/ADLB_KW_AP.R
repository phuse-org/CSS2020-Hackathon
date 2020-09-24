
# Prep Environment -------------------------------------------------------------

source('ADaM/R/podr_connections.R')
source("ADaM/R/Functions_KW_AP/setup.R")

# ADD USERNAME HERE
connect_podr("")

lb <- read_podr("lb", libname="janssen_synthetic")
dm <- read_podr("dm", libname="janssen_synthetic")
ex <- read_podr("ex", libname="janssen_synthetic")
ds <- read_podr("ds", libname="janssen_synthetic")
vs <- read_podr("vs", libname="janssen_synthetic")




# Data Manipulation -------------------------------------------------------

lb_0 <- lb %>% 
  mutate(PARAM    = paste0(LBTEST," (", LBSTRESU,")") ) %>% 
  rename(ANRLO    = LBSTNRLO,
         ANRHI    = LBSTNRHI,
         PARAMCD  = LBTESTCD,
         PARCAT1  = LBCAT
  ) 
# Create ADT, ATM, ADTM (with adadt function)
lb_01 <- adadt(indsn    = lb_0, 
               date_var = "LBDTC",
               biodata= lb)

# Derive Visits
lb_02 <- lb_01 %>% 
  mutate(AVISIT = case_when(
    VISIT %in% c("SCREENING",
                 "RUN-IN",
                 "BASELINE DAY 1",
                 "WEEK 2",
                 "WEEK 4",
                 "WEEK 10",
                 "END OF TREATMENT") ~ VISIT,
    TRUE ~ NA_character_),
    AVISITN = case_when(
      VISIT == "SCREENING"        ~ 1,
      VISIT == "RUN-IN"           ~ 2,
      VISIT == "BASELINE DAY 1"   ~ 3,
      VISIT == "WEEK 2"           ~ 4,
      VISIT == "WEEK 4"           ~ 5,
      VISIT == "WEEK 10"          ~ 6,
      VISIT == "END OF TREATMENT" ~ 9999,
      TRUE ~ NA_real_)
  )    %>% 
  filter(VISIT %in% c("SCREENING",
                      "RUN-IN",
                      "BASELINE DAY 1",
                      "WEEK 2",
                      "WEEK 4",
                      "WEEK 10",
                      "END OF TREATMENT"))

# Derive Treatment Variables
trtf <- list(
  "TREATMENT A"     = "TREATMENT A",
  "TREATMENT B" = "TREATMENT B"
)

trtnf <- list(
  "TREATMENT A"     = 1,
  "TREATMENT B" = 2
)

dm_01 <- dm %>% 
  mutate(TRT01A = case_when(!(ACTARMCD %in% c('Scrnfail', 'NOTASSGN', 'NOTTRT')) ~ unlist(trtf)[ACTARM]),
         TRT01AN = case_when(!(ACTARMCD %in% c('Scrnfail', 'NOTASSGN', 'NOTTRT')) ~ unlist(trtnf)[ACTARM]),
         TRT01P = case_when(!(ARMCD %in% c('Scrnfail', 'NOTASSGN', 'NOTTRT')) ~ unlist(trtf)[ARM]),
         TRT01PN = case_when(!(ARMCD %in% c('Scrnfail', 'NOTASSGN', 'NOTTRT')) ~ unlist(trtnf)[ARM])) %>% 
  select(-DOMAIN)

lb_03 <- right_join(lb_02, dm_01, by = c('USUBJID', "STUDYID")) 

# Derive AVAL/AVALC
lb_04 <- lb_03 %>% 
  mutate(
    LBSTRESC2= case_when(substr(LBSTRESC,1,1) == '>' | substr(LBSTRESC,1,1) == '<' ~ substr(LBSTRESC,2,8) ),
    AVAL = case_when(!is.na(LBSTRESC2) ~ as.numeric(LBSTRESC2), !is.na(LBSTRESN) ~ LBSTRESN),
    AVALC = case_when(substr(LBSTRESC,1,1) != '>' & substr(LBSTRESC,1,1) != '<' & is.na(LBSTRESN) ~ LBSTRESC)
  )

lb_05 <- lb_04 %>% 
  adarfstdt(keepvars = "ARFSTDT")


# Create ABLFL BASE BASEC CHG
lb_06 <- lb_05 %>% 
  group_by(usubjid, paramcd) %>% 
  filter(is.na(lborres) == 0 & 
           is.na(adt) == 0 & 
           is.na(arfstdt) == 0 & 
           as_date(adt) <= as_date(arfstdt)) %>% 
  arrange(usubjid, paramcd, (as_datetime(adtm))) %>% 
  mutate(
    ablfl = case_when(
      row_number() == 1 ~ "Y",
      TRUE ~ NA_character_
    )
  ) %>% 
  full_join(., filter(lb_05,
                        is.na(lborres) != 0 |
                        is.na(adt) != 0 |
                        is.na(arfstdt) != 0 | 
                        as_date(adt) > as_date(arfstdt))) %>% 
  arrange(usubjid, paramcd, as_datetime(adtm)) %>% 
  mutate(
    base = case_when(
      ablfl == "Y" ~ aval,
      TRUE ~ NA_real_
    ),
    basec = case_when(
      ablfl == "Y" ~ avalc,
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(base) %>% 
  fill(basec) %>% 
  mutate(
    chg = case_when(
      is.na(aval) == 0 & is.na(base) == 0 ~ aval - base,
      TRUE ~ NA_real_
    ),
    pchg = case_when(
      is.na(aval) == 0 & is.na(base) == 0 ~ ((aval - base) / base) * 100,
      TRUE ~ NA_real_
    ),
    apoblfl = case_when(
      is.na(adt) == 0 & is.na(arfstdt) == 0 & as_date(adt) > as_date(arfstdt) ~ "Y",
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup() 

# Create CRIT CRITFL 
critfls <- read_excel("ADaM/PHUSE CSS_2020_hackathon_AD usecase.xlsx", sheet = "ADLB_Criteria")
critfls <- critfls[-1,]
names(critfls) <- tolower(names(critfls))

# Create CRIT:
lb_07 <- lb_06 %>% 
  mutate(# Derive CRIT: variables 
    crit1 =
      case_when((paramcd %in% c("SEALB", "SEURATE", "WBC")   & aval<anrlo & pchg <(-25)) ~	"<LLN and >25% decrease from Baseline",
                (paramcd %in% c("ALT","AST") & aval>(3*anrhi)) ~	"> 3 x ULN",	
                (paramcd %in% c("AMYLASEP", "LIPASET")  & aval > (2*anrhi)) ~	"> 2 x ULN",	
                (paramcd %in% c("BILI","PLAT","MG", "PHOS")     & aval>anrhi & pchg >25) ~	"> ULN and > 25% increase from Baseline",	
                (paramcd == "BICARB"   & aval < 16) ~	"< 16 mmol/L",	
                (paramcd == "CLCTONN"  & aval > 5.85) ~	"> 5.85 pmol/L",	
                (paramcd == "CA"       & aval > anrhi & pchg >10) ~	"> ULN and > 10% increase from Baseline",	
                (paramcd == "CK"       & aval > 1000) ~	"> 1000 U/L",	
                (paramcd == "GFRBSCRT" & aval <80 & pchg < (-30)) ~	"<80 and >30% decrease from Baseline",	
                (paramcd == "K"        & aval< anrlo & pchg < (-15)) ~	"<LLN and >15% decrease from Baseline",	
                (paramcd == "SODIUM"   & aval< anrlo & chg < (-5)) ~	"< LLN and decrease of > 5 mmol/L from Baseline",	 
                (paramcd == "HGB"      & chg <= (-20)) ~	">= 20 g/L decrease from Baseline",	
                (paramcd == "BHYXBTR"  & aval > 2) ~ "> 2mmol/L",
                TRUE ~ NA_character_),
    crit2 =
      case_when((paramcd %in% c("ALT","AST") & aval>(5*anrhi))  ~	"> 5 x ULN",	
                (paramcd %in% c("AMYLASEP", "LIPASET")  & aval > (3*anrhi))  ~	"> 3 x ULN",	
                (paramcd == "BILI"     & aval > (2*anrhi))  ~	"> 2 x ULN",		
                (paramcd == "CLCTONN"  & aval > 14.63)  ~	"> 14.63 pmol/L",		
                (paramcd == "GFRBSCRT" & pchg < (-50))  ~	"> 50% decrease from Baseline",	
                (paramcd %in% c("MG", "PHOS")  & aval < anrlo & pchg <(-25))  ~	"< LLN and > 25% decrease from Baseline",	
                (paramcd == "K"        & aval> anrhi & pchg > 15)  ~	"> ULN and > 15% increase from Baseline",	
                (paramcd == "SODIUM"   & aval > anrhi & chg > 5 )  ~	"> ULN and increase of > 5 mmol/L from Baseline",	 
                (paramcd == "HGB"      & chg >= 20)  ~	">= 20 g/L increase from Baseline",	
                (paramcd == "WBC"      & aval > anrhi & pchg >50)  ~   "> ULN and > 50% increase from Baseline",
                TRUE ~ NA_character_),
    crit3 =
      case_when((paramcd %in% c("ALT","AST") & aval>(8*anrhi)) ~	"> 8 x ULN",	
                (paramcd %in% c("AMYLASEP", "LIPASET")  & aval > (5*anrhi)) ~	"> 5 x ULN",	
                (paramcd == "CLCTONN"  & aval > 29.26) ~	"> 29.26 pmol/L",		
                (paramcd == "SODIUM"   & aval<125 )   ~   "<125 mmol/L",
                TRUE ~ NA_character_),
    # Derive CRITFL: 
    crit1fl = case_when(
      !is.na(crit1) ~ "Y",
      TRUE ~ NA_character_),
    crit2fl = case_when(
      !is.na(crit2) ~ "Y",
      TRUE ~ NA_character_),
    crit3fl = case_when(
      !is.na(crit3) ~ "Y",
      TRUE ~ NA_character_)
    
  )

# Create ANRIND BNRIND 
lb_08 <- lb_07 %>% 
  mutate(
    anrind = case_when(
      is.na(lbnrind) == 0 ~ lbnrind,
      is.na(aval) == 0 & is.na(anrlo) == 0 & is.na(anrhi) == 0 & anrlo <= aval & aval <= anrhi ~ "NORMAL",
      is.na(aval) == 0 & is.na(anrlo) == 0 & is.na(anrhi) == 0 & aval < anrlo ~ "LOW",
      is.na(aval) == 0 & is.na(anrlo) == 0 & is.na(anrhi) == 0 & aval > anrlo ~ "HIGH"
    )
  ) %>% 
  group_by(usubjid, paramcd) %>% 
  arrange(usubjid, paramcd, (as_datetime(adtm))) %>% 
  mutate(bnrind = ifelse(ablfl == "Y", anrind, NA)) %>% 
  fill(bnrind) %>% 
  ungroup()

# Create variables that use ex: TRTSTDT, TRTEDT, SAFFL
names(ex) <- tolower(names(ex))

ex_first <- ex %>% 
  select(usubjid, exstdtc, exseq) %>% 
  filter(is.na(exstdtc) == 0) %>% 
  mutate(date = case_when(
    nchar(exstdtc) == 4 ~ paste0(exstdtc,"-01-01"),
    nchar(exstdtc) == 7 ~ paste0(exstdtc,"-01"),
    nchar(exstdtc) >=10 ~ substr(exstdtc,1,10)
  )) %>% 
  mutate(date= date_from_dtc(date)) %>% 
  group_by(usubjid) %>% 
  arrange(usubjid, exstdtc, exseq) %>% 
  filter(row_number() == 1) %>% 
  mutate(trtsdt = as_date(ifelse(nchar(exstdtc)<10, NA, date))) %>% 
  select(usubjid,trtsdt)

ex_last <- ex %>% 
  select(usubjid, exstdtc, exseq) %>% 
  filter(is.na(exstdtc) == 0) %>% 
  mutate(date = case_when(
    nchar(exstdtc) == 4 ~ paste0(exstdtc,"-01-01"),
    nchar(exstdtc) == 7 ~ paste0(exstdtc,"-01"),
    nchar(exstdtc) >=10 ~ substr(exstdtc,1,10)
  )) %>% 
  mutate(date= date_from_dtc(date)) %>% 
  group_by(usubjid) %>% 
  arrange(usubjid, desc(exstdtc), desc(exseq)) %>% 
  filter(row_number() == 1) %>% 
  mutate(trtedt = as_date(ifelse(nchar(exstdtc)<10, NA, date))) %>% 
  select(usubjid,trtedt)

ex_saf <- ex %>% 
  filter(is.na(exstdtc) == 0 & is.na(exdose) == 0) %>% 
  group_by(usubjid) %>% 
  mutate(saffl = "Y") %>%
  filter(row_number() == 1) %>% 
  select(usubjid, saffl)

lb_09 <- lb_08 %>% 
  left_join(ex_first) %>% 
  left_join(ex_last) %>% 
  left_join(ex_saf)


# Create RANDFL, ITTFL, MITTFL
names(ds) <- tolower(names(ds))

ds_rand <- ds %>% 
  filter(dsdecod == "RANDOMIZED") %>%
  mutate(randfl ="Y") %>% 
  select(usubjid, randfl)

names(vs) <- tolower(names(vs))

vs_itt <- vs %>% 
  filter(vsblfl == "Y" & vstestcd == "WEIGHT" & is.na(vsorres) == 0) %>% 
  mutate(ittfl = "Y") %>% 
  left_join(ds_rand) %>% 
  filter(randfl == "Y") %>% 
  select(usubjid, ittfl) 

ex_5 <- ex %>% 
  filter(exdose == 5) %>% 
  group_by(usubjid) %>% 
  filter(row_number() == 1) %>% 
  select(usubjid)

vs_mitt <- vs %>% 
  filter(vstestcd == "WEIGHT" & is.na(vsorres) == 0) %>% 
  group_by(usubjid) %>% 
  arrange(usubjid, vsdtc) %>% 
  mutate(mittfl = lag(vsblfl)) %>% 
  left_join(vs_itt) %>% 
  filter(mittfl=="Y" & ittfl == "Y") %>% 
  select(usubjid, mittfl) %>% 
  inner_join(ex_5)

lb_10 <- lb_09 %>% 
  left_join(ds_rand) %>% 
  left_join(vs_itt) %>% 
  left_join(vs_mitt)

lb_11 <- lb_10 %>%
  mutate(ady =
           case_when(adt >= arfstdt ~ adt-arfstdt+1,
                     TRUE ~ adt-arfstdt)
  )

# Create PARAMN 

# Read excel with codelist
paramn_xl <- read_excel("ADaM/PHUSE CSS_2020_hackathon_AD usecase.xlsx", sheet = "Codelist") %>% 
  filter(CODELST %in% c("PARAMCD_LB", "PARAMN_LB"))

names(paramn_xl) <- tolower(names(paramn_xl))

# PARAMCD
paramcd_lb <- paramn_xl %>% 
  filter(codelst=="PARAMCD_LB") %>% 
  rename(paramcd = codeval)  %>% 
  select(decod, paramcd)

# PARAMN
paramn_lb  <- paramn_xl %>% 
  filter(codelst=="PARAMN_LB") %>% 
  rename(paramn = codeval)  %>%  
  select(decod, paramn)

param_lb <-left_join(paramcd_lb,paramn_lb, by="decod" )

# Join with lb data
lb_12 <-left_join(lb_11,param_lb, by="paramcd" )

# Apply names and metadata labels
names(lb_12) <- toupper(names(lb_12))
adlb <-apply_metadata(lb_12, "ADaM/PHUSE CSS_2020_hackathon_AD usecase.xlsx", "ADLB")
