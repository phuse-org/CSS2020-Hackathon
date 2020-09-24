library(tidyverse)
library(Tplyr)

# TO-DO: Review ordering 
# To-DO: Include combined treatment group

# Connect to PHUSE Open Data Repository
connect_podr("phuse_kzkaa4ro3j")

# Read in Safety pop subjects with Treatment Emergent AEs
adae <- read_podr("adae", libname = "cdisc_pilot_adam") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y") 

# create combined trt group
adae2() <- adae %>%
  filter(TRTA == "Xanomeline High Dose" | TRTA == "Xanomeline Low Dose") %>%
  mutate(TRTAN = 4, TRTA = "Xanomeline Low and High Dose") %>%
  bind_rows(adae)

# Read in Safety pop subjects from ADSL
adsl <- read_podr("adsl", libname = "cdisc_pilot_adam") %>%
  filter(SAFFL == "Y")

# create combined trt group
adsl2 <- adsl %>%
  filter(TRT01A == "Xanomeline High Dose" | TRT01A == "Xanomeline Low Dose") %>%
  mutate(TRT01AN = 4, TRT01A = "Xanomeline Low and High Dose") %>%
  bind_rows(adsl)

# Calculate big N
bigN <- adsl2 %>%
  group_by(TRT01A) %>%
  summarise(N = n())

adaeN <- adae2 %>%
  group_by(TRTA) %>%
  summarise(N = n_distinct(USUBJID))

adae_table <- tplyr_table(adae, TRTA) %>%
  # TO-DO: add combined trt group - why have attempts failed?
  # add_treat_grps("Xanomeline Low and High Dose" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
  set_pop_data(adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count("Number of patients reporting at least one treatment-emergent adverse event") %>%
      set_format_strings(f_str("a (xx.x%)", distinct, distinct_pct)) %>%
      set_distinct_by(USUBJID)
  ) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>%
      set_distinct_by(USUBJID) %>%
      #set_order_count_method('bycount') %>%  
      #set_result_order_var(distinct_n) %>%  
      # Pull the order from the Xanomeline High Dose variable 
      #set_ordering_cols('Xanomeline High Dose') %>% 
      add_risk_diff(
        c("Xanomeline High Dose", "Placebo"),
        c("Xanomeline Low Dose", "Placebo"),
        # c('Xanomeline Low and High Dose', 'Placebo'),
        args = list("conf.level" = .95)
      ) %>%
      # set_format_strings('riskdiff' = f_str('xx.x (xx.x, xx.x)', dif, low, high)) %>%
      set_nest_count(TRUE)
  )

adae_build <- adae_table %>%
  build()
