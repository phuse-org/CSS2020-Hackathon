Study Size Adjusted Percentages
================
Mike Stackhouse
9/22/2020

## Study-size Adjusted Percentages

Pooled studies present a challenge in presenting apporopriately
represented proportions when looking at subject totals in tables like
Adverse Events. This document will explore proper calculation of
Study-size adjusted percentages, as recommended. For more information on
this problem, review [this YouTube
video](https://www.youtube.com/watch?v=GGU6-Pmhq-g).

## Recommended weighting scheme

The weighting scheme recommended by PHUSE takes the proportion of
subjects in an individual pooled study, within a treatment group
compared to the total number of subjected in the pooled studies within
that treatment group overall. This weight is then applied when
aggregating the incidence across studies to identify the total
percentage.

## Mock some data

We don’t have a pooled study available currently, so let’s mock some up
by randomly sampling subjects from the CDISC pilot into different
‘studies’.

``` r
# Note that you will need your own PODR account for this to work for you
connect_podr('phuse_p7r82p0vul')

# Read ADSL and ADAE
adae <- read_podr('adae', libname='cdisc_pilot_adam')
```

    ## Loading required package: magrittr

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
adsl <- read_podr('adsl', libname='cdisc_pilot_adam')
```

With the data loaded, let’s create some new ADSL datasets.

``` r
set.seed(1234)
# Get the set of subjects
all_subs <- adsl$USUBJID
# Sample 20 for the first group
study1_subs <- sample(all_subs, 35)
# Remove those subjects from the pool of available subjects
all_subs <- setdiff(all_subs, study1_subs)
# Repeat for study 2
study2_subs <- sample(all_subs, 45)
# Remove from total set
all_subs <- setdiff(all_subs, study2_subs)
# Repeat for study 3
study3_subs <- sample(all_subs, 60)

# Filter each of the ADSLs down
adsl1 <- adsl %>% 
  filter(USUBJID %in% study1_subs) 

adsl2 <- adsl %>% 
  filter(USUBJID %in% study2_subs) %>% 
  mutate(STUDYID = "CDISCPILOT02")

adsl3 <- adsl %>% 
  filter(USUBJID %in% study3_subs) %>% 
  mutate(STUDYID = "CDISCPILOT03")

# Combine data together
adsl_pooled <- bind_rows(adsl1, adsl2, adsl3)

rm(all_subs, study1_subs, study2_subs, study3_subs)
```

Great - we have 3 different ADSLs. Let’s mock up the matching ADAE
datasets

``` r
# Merge to subset ADAE to just subjects in the associated ADSL dataset
adae1 <- adsl1 %>% 
  select(STUDYID, USUBJID) %>% 
  inner_join(adae, by='USUBJID') %>% 
  mutate(STUDYID = STUDYID.x) %>% 
  select(-STUDYID.x, -STUDYID.y)

adae2 <- adsl2 %>% 
  select(STUDYID, USUBJID) %>% 
  inner_join(adae, by='USUBJID') %>% 
  mutate(STUDYID = STUDYID.x) %>% 
  select(-STUDYID.x, -STUDYID.y)

adae3 <- adsl3 %>% 
  select(STUDYID, USUBJID) %>% 
  inner_join(adae, by='USUBJID') %>% 
  mutate(STUDYID = STUDYID.x) %>% 
  select(-STUDYID.x, -STUDYID.y)

# Combine data together
adae_pooled <- bind_rows(adae1, adae2, adae3)

rm(adsl1, adsl2, adsl3, adae1, adae2, adae3)
```

Awesome - now we have some pooled datasets to play with. Like CDISC
data, subjects are marked in their respective studies within the STUDYID
variable.

## Deriving Weights

The next step is to derive weights. In order to derive weights, we need
two pieces of information: - The total number of subjects in a treatment
arm - The total number of subjects by study in a treatment arm - which
we can reuse later as a denominator as well

Simple enough - let’s go.

``` r
# Start with treatment arm overall
total_arm <- adsl_pooled %>% 
  group_by(TRT01P) %>% 
  summarize(total_arm = n(), .groups='drop')

# Now get per study per arm
total_study_arm <- adsl_pooled %>% 
  group_by(TRT01P, STUDYID) %>% 
  summarize(total_study_arm = n(), .groups='drop')

denoms <- full_join(total_study_arm, total_arm, by = "TRT01P") %>% 
  mutate(weight = total_study_arm / total_arm) 

rm(total_arm, total_study_arm)

kable(head(denoms))
```

| TRT01P               | STUDYID      | total\_study\_arm | total\_arm |    weight |
| :------------------- | :----------- | ----------------: | ---------: | --------: |
| Placebo              | CDISCPILOT01 |                10 |         46 | 0.2173913 |
| Placebo              | CDISCPILOT02 |                18 |         46 | 0.3913043 |
| Placebo              | CDISCPILOT03 |                18 |         46 | 0.3913043 |
| Xanomeline High Dose | CDISCPILOT01 |                 8 |         39 | 0.2051282 |
| Xanomeline High Dose | CDISCPILOT02 |                14 |         39 | 0.3589744 |
| Xanomeline High Dose | CDISCPILOT03 |                17 |         39 | 0.4358974 |

Wonderful - we have some really useful information now - we have the
denominator to use when calculating the incidence per study, and then
the weight to use when aggregating the study-size adjusted percentage.
Now we can go ahead to the next step - counting\!

## Counting Adverse Events

Next we need to count our adverse events. This is the first step before
we can additionally derive percentages. Based on the summaries we’re
deriving, we must do a few other things as well:

  - Group by STUDYID, which you typically wouldn’t do
  - Group by Treatment Group

For simplicity sake, we’re just going to summarize preferred term.

``` r
counts_by_study <- adae_pooled %>% 
  group_by(STUDYID, TRTA, AEDECOD) %>% 
  distinct(USUBJID) %>%
  summarize(counts = n(), .groups='drop')

kable(head(counts_by_study))
```

| STUDYID      | TRTA    | AEDECOD                     | counts |
| :----------- | :------ | :-------------------------- | -----: |
| CDISCPILOT01 | Placebo | AGITATION                   |      1 |
| CDISCPILOT01 | Placebo | APPLICATION SITE DERMATITIS |      1 |
| CDISCPILOT01 | Placebo | APPLICATION SITE ERYTHEMA   |      2 |
| CDISCPILOT01 | Placebo | APPLICATION SITE IRRITATION |      1 |
| CDISCPILOT01 | Placebo | BRADYCARDIA                 |      1 |
| CDISCPILOT01 | Placebo | CONFUSIONAL STATE           |      1 |

Great - Now let’s incorporate the denominators and the weights that we
derived earlier. This will set us up to perform the final calculation of
study-size adjusted percentages.

``` r
counts_and_weight <- counts_by_study %>% 
  # Note that we're joining variables with different names.
  # Left joins in R allow you to specify different variable
  # names that you want to join by doing left_var = right_var
  left_join(denoms, by=c('STUDYID', 'TRTA' = 'TRT01P'))

kable(head(counts_and_weight))
```

| STUDYID      | TRTA    | AEDECOD                     | counts | total\_study\_arm | total\_arm |    weight |
| :----------- | :------ | :-------------------------- | -----: | ----------------: | ---------: | --------: |
| CDISCPILOT01 | Placebo | AGITATION                   |      1 |                10 |         46 | 0.2173913 |
| CDISCPILOT01 | Placebo | APPLICATION SITE DERMATITIS |      1 |                10 |         46 | 0.2173913 |
| CDISCPILOT01 | Placebo | APPLICATION SITE ERYTHEMA   |      2 |                10 |         46 | 0.2173913 |
| CDISCPILOT01 | Placebo | APPLICATION SITE IRRITATION |      1 |                10 |         46 | 0.2173913 |
| CDISCPILOT01 | Placebo | BRADYCARDIA                 |      1 |                10 |         46 | 0.2173913 |
| CDISCPILOT01 | Placebo | CONFUSIONAL STATE           |      1 |                10 |         46 | 0.2173913 |

# Deriving Study-size adjusted percentage

So let’s think about what has to happen next.

  - The percentage in each study, by treatment, by adverse event needs
    to be calculated
  - Those percentages should be adjusted by weight
  - The percentages for each adverse event by study by treatment should
    be aggregated.

There are a few steps, but the overall process is not very complicated.
Let’s go through step by step.

``` r
adjusted_pcts <- counts_and_weight %>% 
  # Calculate percent incidence of AEs weighted by study and treatment group
  mutate(weighted_pct_by_study = weight * (counts / total_study_arm)) %>% 
  # Group by treatment group and adverse event and sum 
  # now we're aggregating the incidence of the AEs together within a treatment group
  group_by(TRTA, AEDECOD) %>% 
  summarize(adj_pct_n = sum(weighted_pct_by_study), .groups='drop') %>% 
  # Lastly, let's do a little string formatting to make it pretty
  mutate(adj_pct = format(round(adj_pct_n * 100, digits=1), digits=1, width=4, nsmall=1)) %>% 
  select(TRTA, AEDECOD, adj_pct)

kable(head(adjusted_pcts))
```

| TRTA    | AEDECOD                     | adj\_pct |
| :------ | :-------------------------- | :------- |
| Placebo | AGITATION                   | 4.3      |
| Placebo | ANXIETY                     | 2.2      |
| Placebo | APPLICATION SITE DERMATITIS | 4.3      |
| Placebo | APPLICATION SITE ERYTHEMA   | 4.3      |
| Placebo | APPLICATION SITE INDURATION | 2.2      |
| Placebo | APPLICATION SITE IRRITATION | 4.3      |

Wonderful - we have one last step of pivoting these data for
presentation in a final output, and filling in the empty values

``` r
adjusted_pcts_t <- adjusted_pcts %>% 
  pivot_wider(id_cols = AEDECOD, names_from=TRTA, values_from = adj_pct, names_prefix='adj_pct_') %>% 
  mutate_at(2:4, replace_na, replace=' 0.0')

kable(head(adjusted_pcts_t))
```

| AEDECOD                     | adj\_pct\_Placebo | adj\_pct\_Xanomeline High Dose | adj\_pct\_Xanomeline Low Dose |
| :-------------------------- | :---------------- | :----------------------------- | :---------------------------- |
| AGITATION                   | 4.3               | 0.0                            | 1.8                           |
| ANXIETY                     | 2.2               | 0.0                            | 5.5                           |
| APPLICATION SITE DERMATITIS | 4.3               | 10.3                           | 12.7                          |
| APPLICATION SITE ERYTHEMA   | 4.3               | 17.9                           | 12.7                          |
| APPLICATION SITE INDURATION | 2.2               | 0.0                            | 0.0                           |
| APPLICATION SITE IRRITATION | 4.3               | 10.3                           | 10.9                          |

## Incorporating Into a Final Table

Everything above calculates the study-size adjusted percentage - but we
need the rest of the table too. Let’s use
[Tplyr](https://atorus-research.github.io/Tplyr/) to get the rest of the
data we need.

``` r
# Create the Tplyr table and specify the treatment variable
adae_table <- tplyr_table(adae_pooled, TRTA) %>% 
  # Set the population dataset
  set_pop_data(adsl_pooled) %>%
  # Set the population treatment variable
  set_pop_treat_var(TRT01P) %>% 
  # Add a new layer to the table
  add_layer(
    # Create a count summary on AEDECOD
    group_count(AEDECOD) %>% 
      # Set counts distinct by USUBJID
      set_distinct_by(USUBJID) %>% 
      # Set the order method for the results 
      # to by count
      set_order_count_method('bycount') %>% 
      # Use the distinct subject counts for the order
      set_result_order_var(distinct_n) %>% 
      # Pull the order from the Xanomeline High Dose variable
      set_ordering_cols('Xanomeline High Dose')
  ) 

adae_table_built <- adae_table %>% 
  # Crunch the numbers
  build()

kable(head(adae_table_built))
```

| row\_label1                     | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose | ord\_layer\_index | ord\_layer\_1 |
| :------------------------------ | :------------ | :------------------------- | :------------------------ | ----------------: | ------------: |
| ABDOMINAL DISCOMFORT            | 0 ( 0.0%)     | 1 ( 2.6%)                  | 0 ( 0.0%)                 |                 1 |             1 |
| ABDOMINAL PAIN                  | 0 ( 0.0%)     | 1 ( 2.6%)                  | 3 ( 5.5%)                 |                 1 |             1 |
| ACTINIC KERATOSIS               | 0 ( 0.0%)     | 1 ( 2.6%)                  | 0 ( 0.0%)                 |                 1 |             1 |
| AGITATION                       | 2 ( 4.3%)     | 0 ( 0.0%)                  | 1 ( 1.8%)                 |                 1 |             0 |
| ALCOHOL USE                     | 0 ( 0.0%)     | 1 ( 2.6%)                  | 0 ( 0.0%)                 |                 1 |             1 |
| ALLERGIC GRANULOMATOUS ANGIITIS | 0 ( 0.0%)     | 1 ( 2.6%)                  | 0 ( 0.0%)                 |                 1 |             1 |

Great - now we have the table data we need. Let’s get the adjusted
percentages in and do some styling\!

``` r
final <- adae_table_built %>% 
  # Join in the adjusted percentages
  left_join(adjusted_pcts_t, by = c('row_label1' = 'AEDECOD')) %>% 
  # Sort by descending incidence from the Xanomeline High Dose group, and then alphabetically be preferred term
  arrange(desc(ord_layer_1), row_label1) %>% 
  # Select off the variables in the order I want them
  select(row_label1, var1_Placebo, adj_pct_Placebo, `var1_Xanomeline Low Dose`, `adj_pct_Xanomeline Low Dose`,
         `var1_Xanomeline High Dose`, `adj_pct_Xanomeline High Dose`) %>% 
  # Add a nested set of column headers using Tplyr::add_column_headers. Using the adae_table object from above,
  # the overall N counts are auto replaced for the **Treatment Group** strings below
  add_column_headers(
    paste0('Preferred Term | Placebo (N=**Placebo**) {n(%) | Study Size-Adjusted %} | ' ,
           'Xanomeline Low Dose (N=**Xanomeline Low Dose**) {n(%) | Study Size-Adjusted %} |',
           'Xanomeline High Dose (N=**Xanomeline High Dose**) {n(%) | Study Size-Adjusted %}'),
     header_n = header_n(adae_table)
  )
```

And finally - let’s do some styling using the library Huxtable

``` r
ht <- as_hux(final, add_colnames=FALSE) %>%
  set_bold(1:2, 1:ncol(final), TRUE) %>% # bold the first row
  set_align(1:2, 1:ncol(final), 'center') %>% # Center align the first row 
  set_align(3:nrow(final), 3:ncol(final), 'center') %>% # Center align the results
  set_valign(1:2, 1:ncol(final), 'bottom') %>% # Bottom align the first row
  set_bottom_border(2, 1:ncol(final), 1) %>% # Put a border under the first row
  set_width(1.1) %>% # Set the table width
  set_col_width(c(.3, rep(.7/6, 6))) %>%  # Set the column widths
  merge_cells(1, 2:3) %>% # Merge placebo spanning header
  merge_cells(1, 4:5) %>% # Merge low dose spanning header
  merge_cells(1, 6:7) %>%  # Merge high dose spanning header
  set_background_color(seq(3, nrow(final), 2), everywhere, "grey95")
cat(print_md(ht))
```

    ## Warning in to_md.huxtable(ht, ...): Markdown cannot handle cells with colspan/
    ## rowspan > 1

    ## Warning in to_md.huxtable(ht, ...): Can't vary column alignment in markdown;
    ## using first row

    ## -----------------------------------------------------------------------------
    ##                             **Placebo**    **Xanomeline**    **Xanomeline**  
    ##                              **(N=46)**      **Low Dose**     **High Dose**  
    ##                                                **(N=55)**        **(N=39)**  
    ## ---------------------- -------- -------- -------- -------- -------- ---------
    ##     **Preferred Term** **n(%)** **Stud** **n(%)** **Stud** **n(%)** **Stud** 
    ##                                    **y**             **y**             **y** 
    ##                                 **Size**          **Size**          **Size** 
    ##                                 **-Adj**          **-Adj**          **-Adj** 
    ##                                 **uste**          **uste**          **uste** 
    ##                                    **d**             **d**             **d** 
    ##                                    **%**             **%**             **%** 
    ##                                                                              
    ##       APPLICATION SITE      1 (      2.2     16 (     29.1     12 (     30.8 
    ##               PRURITUS    2.2%)            29.1%)            30.8%)          
    ##                                                                              
    ##               PRURITUS      6 (     13.0     15 (     27.3     11 (     28.2 
    ##                          13.0%)            27.3%)            28.2%)          
    ##                                                                              
    ##              DIZZINESS      2 (      4.3      5 (      9.1      9 (     23.1 
    ##                           4.3%)             9.1%)            23.1%)          
    ##                                                                              
    ##       APPLICATION SITE      2 (      4.3      7 (     12.7      7 (     17.9 
    ##               ERYTHEMA    4.3%)            12.7%)            17.9%)          
    ##                                                                              
    ##               ERYTHEMA      6 (     13.0      9 (     16.4      7 (     17.9 
    ##                          13.0%)            16.4%)            17.9%)          
    ##                                                                              
    ##          HYPERHIDROSIS      1 (      2.2      1 (      1.8      6 (     15.4 
    ##                           2.2%)             1.8%)            15.4%)          
    ##                                                                              
    ##      SINUS BRADYCARDIA      1 (      2.2      6 (     10.9      6 (     15.4 
    ##                           2.2%)            10.9%)            15.4%)          
    ##                                                                              
    ##       APPLICATION SITE      2 (      4.3      7 (     12.7      4 (     10.3 
    ##             DERMATITIS    4.3%)            12.7%)            10.3%)          
    ##                                                                              
    ##       APPLICATION SITE      2 (      4.3      6 (     10.9      4 (     10.3 
    ##             IRRITATION    4.3%)            10.9%)            10.3%)          
    ##                                                                              
    ##        NASOPHARYNGITIS      2 (      4.3      2 (      3.6      4 (     10.3 
    ##                           4.3%)             3.6%)            10.3%)          
    ##                                                                              
    ##               VOMITING      1 (      2.2      3 (      5.5      4 (     10.3 
    ##                           2.2%)             5.5%)            10.3%)          
    ##                                                                              
    ##               HEADACHE      5 (     10.9      1 (      1.8      3 (      7.7 
    ##                          10.9%)             1.8%)             7.7%)          
    ##                                                                              
    ##       NASAL CONGESTION      2 (      4.3      0 (      0.0      3 (      7.7 
    ##                           4.3%)             0.0%)             7.7%)          
    ##                                                                              
    ##                 NAUSEA      1 (      2.2      3 (      5.5      3 (      7.7 
    ##                           2.2%)             5.5%)             7.7%)          
    ##                                                                              
    ##                   RASH      3 (      6.5     11 (     20.0      3 (      7.7 
    ##                           6.5%)            20.0%)             7.7%)          
    ##                                                                              
    ##               SALIVARY      0 (      0.0      0 (      0.0      3 (      7.7 
    ##         HYPERSECRETION    0.0%)             0.0%)             7.7%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      2 (      3.6      2 (      5.1 
    ##               VESICLES    0.0%)             3.6%)             5.1%)          
    ##                                                                              
    ##    ATRIAL FIBRILLATION      1 (      2.2      1 (      1.8      2 (      5.1 
    ##                           2.2%)             1.8%)             5.1%)          
    ##                                                                              
    ##              CONTUSION      0 (      0.0      1 (      1.8      2 (      5.1 
    ##                           0.0%)             1.8%)             5.1%)          
    ##                                                                              
    ##              EPISTAXIS      0 (      0.0      0 (      0.0      2 (      5.1 
    ##                           0.0%)             0.0%)             5.1%)          
    ##                                                                              
    ##             FLANK PAIN      0 (      0.0      0 (      0.0      2 (      5.1 
    ##                           0.0%)             0.0%)             5.1%)          
    ##                                                                              
    ##                MALAISE      0 (      0.0      1 (      1.8      2 (      5.1 
    ##                           0.0%)             1.8%)             5.1%)          
    ##                                                                              
    ##        SKIN IRRITATION      2 (      4.3      4 (      7.3      2 (      5.1 
    ##                           4.3%)             7.3%)             5.1%)          
    ##                                                                              
    ##   ABDOMINAL DISCOMFORT      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##         ABDOMINAL PAIN      0 (      0.0      3 (      5.5      1 (      2.6 
    ##                           0.0%)             5.5%)             2.6%)          
    ##                                                                              
    ##      ACTINIC KERATOSIS      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##            ALCOHOL USE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##               ALLERGIC      0 (      0.0      0 (      0.0      1 (      2.6 
    ##          GRANULOMATOUS    0.0%)             0.0%)             2.6%)          
    ##               ANGIITIS                                                       
    ##                                                                              
    ##                AMNESIA      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##  APPLICATION SITE PAIN      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##           PERSPIRATION    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##               REACTION    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      1 (      1.8      1 (      2.6 
    ##               SWELLING    0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      1 (      1.8      1 (      2.6 
    ##              URTICARIA    0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##              ARTHRITIS      1 (      2.2      0 (      0.0      1 (      2.6 
    ##                           2.2%)             0.0%)             2.6%)          
    ##                                                                              
    ##         ATRIAL FLUTTER      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                           0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##       ATRIOVENTRICULAR      2 (      4.3      0 (      0.0      1 (      2.6 
    ##    BLOCK SECOND DEGREE    4.3%)             0.0%)             2.6%)          
    ##                                                                              
    ##              BACK PAIN      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                           0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##                 BIOPSY      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                BLISTER      0 (      0.0      4 (      7.3      1 (      2.6 
    ##                           0.0%)             7.3%)             2.6%)          
    ##                                                                              
    ##      BLOOD CHOLESTEROL      0 (      0.0      0 (      0.0      1 (      2.6 
    ##              INCREASED    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      BURNING SENSATION      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      CALCULUS URETHRAL      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                 CHILLS      1 (      2.2      1 (      1.8      1 (      2.6 
    ##                           2.2%)             1.8%)             2.6%)          
    ##                                                                              
    ##      CONFUSIONAL STATE      1 (      2.2      2 (      3.6      1 (      2.6 
    ##                           2.2%)             3.6%)             2.6%)          
    ##                                                                              
    ##                  COUGH      2 (      4.3      5 (      9.1      1 (      2.6 
    ##                           4.3%)             9.1%)             2.6%)          
    ##                                                                              
    ##               CYSTITIS      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##     DECREASED APPETITE      1 (      2.2      0 (      0.0      1 (      2.6 
    ##                           2.2%)             0.0%)             2.6%)          
    ##                                                                              
    ##               DELUSION      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##         DEPRESSED MOOD      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                           0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##              DIARRHOEA      5 (     10.9      3 (      5.5      1 (      2.6 
    ##                          10.9%)             5.5%)             2.6%)          
    ##                                                                              
    ##              DYSPEPSIA      1 (      2.2      1 (      1.8      1 (      2.6 
    ##                           2.2%)             1.8%)             2.6%)          
    ##                                                                              
    ##            EXCORIATION      1 (      2.2      1 (      1.8      1 (      2.6 
    ##                           2.2%)             1.8%)             2.6%)          
    ##                                                                              
    ##  FACIAL BONES FRACTURE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                   FALL      1 (      2.2      1 (      1.8      1 (      2.6 
    ##                           2.2%)             1.8%)             2.6%)          
    ##                                                                              
    ##                FATIGUE      0 (      0.0      4 (      7.3      1 (      2.6 
    ##                           0.0%)             7.3%)             2.6%)          
    ##                                                                              
    ##       FEELING ABNORMAL      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##           FEELING COLD      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##          HALLUCINATION      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##     INCREASED APPETITE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##               INSOMNIA      1 (      2.2      0 (      0.0      1 (      2.6 
    ##                           2.2%)             0.0%)             2.6%)          
    ##                                                                              
    ##               LETHARGY      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##       LIBIDO DECREASED      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##               LISTLESS      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      LOWER RESPIRATORY      0 (      0.0      0 (      0.0      1 (      2.6 
    ##        TRACT INFECTION    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##    MICTURITION URGENCY      1 (      2.2      1 (      1.8      1 (      2.6 
    ##                           2.2%)             1.8%)             2.6%)          
    ##                                                                              
    ##                MYALGIA      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##  MYOCARDIAL INFARCTION      2 (      4.3      2 (      3.6      1 (      2.6 
    ##                           4.3%)             3.6%)             2.6%)          
    ##                                                                              
    ##        NEPHROLITHIASIS      1 (      2.2      0 (      0.0      1 (      2.6 
    ##                           2.2%)             0.0%)             2.6%)          
    ##                                                                              
    ##              NIGHTMARE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                   PAIN      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                           0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##               PAROSMIA      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      PHARYNGOLARYNGEAL      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                   PAIN    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##       PRODUCTIVE COUGH      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##        PROSTATE CANCER      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##   PRURITUS GENERALISED      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                PYREXIA      2 (      4.3      0 (      0.0      1 (      2.6 
    ##                           4.3%)             0.0%)             2.6%)          
    ##                                                                              
    ##    RASH MACULO-PAPULAR      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##           RASH PAPULAR      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##          RASH PRURITIC      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      RESPIRATORY TRACT      0 (      0.0      0 (      0.0      1 (      2.6 
    ##             CONGESTION    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##               RHINITIS      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##            RHINORRHOEA      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                           0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##       SEASONAL ALLERGY      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##     STOMACH DISCOMFORT      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##                SYNCOPE      0 (      0.0      2 (      3.6      1 (      2.6 
    ##                           0.0%)             3.6%)             2.6%)          
    ##                                                                              
    ##    TRANSIENT ISCHAEMIC      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                 ATTACK    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      UPPER RESPIRATORY      4 (      8.7      0 (      0.0      1 (      2.6 
    ##        TRACT INFECTION    8.7%)             0.0%)             2.6%)          
    ##                                                                              
    ##          URINARY TRACT      2 (      4.3      0 (      0.0      1 (      2.6 
    ##              INFECTION    4.3%)             0.0%)             2.6%)          
    ##                                                                              
    ##              URTICARIA      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##            VENTRICULAR      0 (      0.0      0 (      0.0      1 (      2.6 
    ##          EXTRASYSTOLES    0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##     VENTRICULAR SEPTAL      0 (      0.0      1 (      1.8      1 (      2.6 
    ##                 DEFECT    0.0%)             1.8%)             2.6%)          
    ##                                                                              
    ##         VISION BLURRED      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##      WOUND HAEMORRHAGE      0 (      0.0      0 (      0.0      1 (      2.6 
    ##                           0.0%)             0.0%)             2.6%)          
    ##                                                                              
    ##              AGITATION      2 (      4.3      1 (      1.8      0 (      0.0 
    ##                           4.3%)             1.8%)             0.0%)          
    ##                                                                              
    ##                ANXIETY      1 (      2.2      3 (      5.5      0 (      0.0 
    ##                           2.2%)             5.5%)             0.0%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      1 (      1.8      0 (      0.0 
    ##               BLEEDING    0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##       APPLICATION SITE      0 (      0.0      1 (      1.8      0 (      0.0 
    ##         DISCOLOURATION    0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##       APPLICATION SITE      1 (      2.2      0 (      0.0      0 (      0.0 
    ##             INDURATION    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##             ARTHRALGIA      0 (      0.0      2 (      3.6      0 (      0.0 
    ##                           0.0%)             3.6%)             0.0%)          
    ##                                                                              
    ##               ASTHENIA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##       ATRIOVENTRICULAR      0 (      0.0      1 (      1.8      0 (      0.0 
    ##     BLOCK FIRST DEGREE    0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##       BALANCE DISORDER      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##       BENIGN PROSTATIC      1 (      2.2      0 (      0.0      0 (      0.0 
    ##            HYPERPLASIA    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##         BLOOD ALKALINE      1 (      2.2      0 (      0.0      0 (      0.0 
    ##  PHOSPHATASE INCREASED    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##    BLOOD URINE PRESENT      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##            BRADYCARDIA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##             BRONCHITIS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##    BUNDLE BRANCH BLOCK      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                  RIGHT    2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##        CARDIAC FAILURE      1 (      2.2      0 (      0.0      0 (      0.0 
    ##             CONGESTIVE    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##     CATARACT OPERATION      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##             CELLULITIS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##             CERVICITIS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##             COLD SWEAT      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##           COLON CANCER      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##         CONJUNCTIVITIS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##  COORDINATION ABNORMAL      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##                   CYST      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##             CYSTOSCOPY      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##            DEHYDRATION      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##         DISORIENTATION      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##              DYSPHAGIA      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##              DYSPHONIA      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##               DYSPNOEA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##                DYSURIA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##          EAR INFECTION      2 (      4.3      0 (      0.0      0 (      0.0 
    ##                           4.3%)             0.0%)             0.0%)          
    ##                                                                              
    ##               EAR PAIN      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##   ELECTROCARDIOGRAM ST      2 (      4.3      0 (      0.0      0 (      0.0 
    ##     SEGMENT DEPRESSION    4.3%)             0.0%)             0.0%)          
    ##                                                                              
    ##    ELECTROCARDIOGRAM T      1 (      2.2      1 (      1.8      0 (      0.0 
    ##         WAVE AMPLITUDE    2.2%)             1.8%)             0.0%)          
    ##              DECREASED                                                       
    ##                                                                              
    ##    ELECTROCARDIOGRAM T      2 (      4.3      1 (      1.8      0 (      0.0 
    ##         WAVE INVERSION    4.3%)             1.8%)             0.0%)          
    ##                                                                              
    ##              EMPHYSEMA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##               ENURESIS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##            EYE ALLERGY      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##           EYE PRURITUS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##           EYE SWELLING      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##           FOOD CRAVING      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##  GASTROENTERITIS VIRAL      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##      GASTROOESOPHAGEAL      1 (      2.2      0 (      0.0      0 (      0.0 
    ##         REFLUX DISEASE    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##               GLAUCOMA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##              GLOSSITIS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##   HEART RATE INCREASED      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##   HEART RATE IRREGULAR      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##  HEMIANOPIA HOMONYMOUS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##       HYPERSENSITIVITY      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##          HYPONATRAEMIA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##            HYPOTENSION      2 (      4.3      0 (      0.0      0 (      0.0 
    ##                           4.3%)             0.0%)             0.0%)          
    ##                                                                              
    ##           INFLAMMATION      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##           IRRITABILITY      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##    LOCALISED INFECTION      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##      MALIGNANT FIBROUS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##           HISTIOCYTOMA    0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##      MUSCULAR WEAKNESS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##    NASAL MUCOSA BIOPSY      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##                 OEDEMA      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##      OEDEMA PERIPHERAL      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##          ONYCHOMYCOSIS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##            ORTHOSTATIC      1 (      2.2      0 (      0.0      0 (      0.0 
    ##            HYPOTENSION    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##           PALPITATIONS      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##    PARKINSON'S DISEASE      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##            PELVIC PAIN      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##              PNEUMONIA      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##      RASH ERYTHEMATOUS      0 (      0.0      2 (      3.6      0 (      0.0 
    ##                           0.0%)             3.6%)             0.0%)          
    ##                                                                              
    ##    SECRETION DISCHARGE      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##          SHOULDER PAIN      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##       SKIN EXFOLIATION      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##             SOMNOLENCE      1 (      2.2      1 (      1.8      0 (      0.0 
    ##                           2.2%)             1.8%)             0.0%)          
    ##                                                                              
    ##       SUPRAVENTRICULAR      1 (      2.2      0 (      0.0      0 (      0.0 
    ##          EXTRASYSTOLES    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##       SUPRAVENTRICULAR      0 (      0.0      1 (      1.8      0 (      0.0 
    ##            TACHYCARDIA    0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##               SWELLING      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##            TACHYCARDIA      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##                  ULCER      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##        VAGINAL MYCOSIS      1 (      2.2      0 (      0.0      0 (      0.0 
    ##                           2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##            VENTRICULAR      1 (      2.2      0 (      0.0      0 (      0.0 
    ##            HYPERTROPHY    2.2%)             0.0%)             0.0%)          
    ##                                                                              
    ##        VIRAL INFECTION      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ##                                                                              
    ##                  WOUND      0 (      0.0      1 (      1.8      0 (      0.0 
    ##                           0.0%)             1.8%)             0.0%)          
    ## -----------------------------------------------------------------------------
