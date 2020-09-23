library(Tplyr)
library(tidyverse)

connect_podr('phuse_p7r82p0vul')

adae <- read_podr('adae', libname='cdisc_pilot_adam')
adsl <- read_podr('adsl', libname='cdisc_pilot_adam')

adae_table <- tplyr_table(adae, TRTA) %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  add_layer(
    group_count('Number of patients who experienced at least 1 adverse event')
  ) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_format_strings(f_str('a (xx.x%) [x]', distinct, distinct_pct, n)) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo'),
        args=list('conf.level'=.9)
      ) %>% 
      set_nest_count(TRUE)
  ) %>% 
  build()