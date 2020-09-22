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
  set_col_width(c(.2, rep(.8/6, 6))) %>%  # Set the column widths
  merge_cells(1, 2:3) %>% # Merge placebo spanning header
  merge_cells(1, 4:5) %>% # Merge low dose spanning header
  merge_cells(1, 6:7) %>%  # Merge high dose spanning header
  set_background_color(seq(3, nrow(final), 2), everywhere, "grey95") %>% 
  set_font_size(1:nrow(final), 1:ncol(final), 10)
```

<table class="huxtable" style="border-collapse: collapse; border: 0px; margin-bottom: 2em; margin-top: 2em; width: 110%; margin-left: auto; margin-right: auto;  " id="tab:styling">

<col style="width: 20%">

<col style="width: 13.3333333333333%">

<col style="width: 13.3333333333333%">

<col style="width: 13.3333333333333%">

<col style="width: 13.3333333333333%">

<col style="width: 13.3333333333333%">

<col style="width: 13.3333333333333%">

<tr>

<td style="vertical-align: bottom; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

</td>

<td colspan="2" style="vertical-align: bottom; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Placebo (N=46)

</td>

<td colspan="2" style="vertical-align: bottom; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Xanomeline Low Dose (N=55)

</td>

<td colspan="2" style="vertical-align: bottom; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Xanomeline High Dose (N=39)

</td>

</tr>

<tr>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Preferred Term

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

n(%)

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Study Size-Adjusted %

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

n(%)

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Study Size-Adjusted %

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

n(%)

</td>

<td style="vertical-align: bottom; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 0pt 0pt 1pt 0pt;    padding: 6pt 6pt 6pt 6pt; font-weight: bold; font-size: 10pt;">

Study Size-Adjusted %

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE PRURITUS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

16 ( 29.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

29.1

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

12 ( 30.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; border-style: solid solid solid solid; border-width: 1pt 0pt 0pt 0pt;    padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

30.8

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PRURITUS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

6 ( 13.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

13.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

15 ( 27.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

27.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

11 ( 28.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

28.2

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DIZZINESS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5 ( 9.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

9.1

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

9 ( 23.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

23.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE ERYTHEMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7 ( 12.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

12.7

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7 ( 17.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

17.9

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ERYTHEMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

6 ( 13.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

13.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

9 ( 16.4%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

16.4

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

7 ( 17.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

17.9

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

HYPERHIDROSIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

6 ( 15.4%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

15.4

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

SINUS BRADYCARDIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

6 ( 10.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

10.9

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

6 ( 15.4%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

15.4

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE DERMATITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7 ( 12.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

12.7

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4 ( 10.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

10.3

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE IRRITATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

6 ( 10.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

10.9

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4 ( 10.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

10.3

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

NASOPHARYNGITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4 ( 10.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

10.3

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

VOMITING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 5.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4 ( 10.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

10.3

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

HEADACHE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5 ( 10.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

10.9

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3 ( 7.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7.7

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

NASAL CONGESTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 7.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

7.7

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

NAUSEA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3 ( 5.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3 ( 7.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7.7

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

RASH

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 6.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

6.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

11 ( 20.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

20.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 7.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

7.7

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SALIVARY HYPERSECRETION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3 ( 7.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7.7

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE VESICLES

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ATRIAL FIBRILLATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CONTUSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

EPISTAXIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

FLANK PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

MALAISE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

SKIN IRRITATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4 ( 7.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

7.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 5.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.1

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ABDOMINAL DISCOMFORT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ABDOMINAL PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 5.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ACTINIC KERATOSIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ALCOHOL USE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ALLERGIC GRANULOMATOUS ANGIITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

AMNESIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE PERSPIRATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE REACTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE SWELLING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE URTICARIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ARTHRITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ATRIAL FLUTTER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ATRIOVENTRICULAR BLOCK SECOND DEGREE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BACK PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BIOPSY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BLISTER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4 ( 7.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BLOOD CHOLESTEROL INCREASED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BURNING SENSATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CALCULUS URETHRAL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

CHILLS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CONFUSIONAL STATE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

COUGH

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5 ( 9.1%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

9.1

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CYSTITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DECREASED APPETITE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DELUSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DEPRESSED MOOD

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DIARRHOEA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5 ( 10.9%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

10.9

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3 ( 5.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

5.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DYSPEPSIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

EXCORIATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

FACIAL BONES FRACTURE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

FALL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

FATIGUE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4 ( 7.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

7.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

FEELING ABNORMAL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

FEELING COLD

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

HALLUCINATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

INCREASED APPETITE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

INSOMNIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

LETHARGY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

LIBIDO DECREASED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

LISTLESS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

LOWER RESPIRATORY TRACT INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

MICTURITION URGENCY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

MYALGIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

MYOCARDIAL INFARCTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

NEPHROLITHIASIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

NIGHTMARE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PAROSMIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PHARYNGOLARYNGEAL PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PRODUCTIVE COUGH

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PROSTATE CANCER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PRURITUS GENERALISED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PYREXIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

RASH MACULO-PAPULAR

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

RASH PAPULAR

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

RASH PRURITIC

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

RESPIRATORY TRACT CONGESTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

RHINITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

RHINORRHOEA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SEASONAL ALLERGY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

STOMACH DISCOMFORT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SYNCOPE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

TRANSIENT ISCHAEMIC ATTACK

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

UPPER RESPIRATORY TRACT INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4 ( 8.7%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

8.7

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

URINARY TRACT INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

URTICARIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

VENTRICULAR EXTRASYSTOLES

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

VENTRICULAR SEPTAL DEFECT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

VISION BLURRED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

WOUND HAEMORRHAGE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.6

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

AGITATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ANXIETY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3 ( 5.5%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

5.5

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE BLEEDING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

APPLICATION SITE DISCOLOURATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

APPLICATION SITE INDURATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ARTHRALGIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ASTHENIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ATRIOVENTRICULAR BLOCK FIRST DEGREE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BALANCE DISORDER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BENIGN PROSTATIC HYPERPLASIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BLOOD ALKALINE PHOSPHATASE INCREASED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BLOOD URINE PRESENT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BRADYCARDIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

BRONCHITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

BUNDLE BRANCH BLOCK RIGHT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

CARDIAC FAILURE CONGESTIVE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CATARACT OPERATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

CELLULITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CERVICITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

COLD SWEAT

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

COLON CANCER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

CONJUNCTIVITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

COORDINATION ABNORMAL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

CYST

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

CYSTOSCOPY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DEHYDRATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DISORIENTATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DYSPHAGIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DYSPHONIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

DYSPNOEA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

DYSURIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

EAR INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

EAR PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ELECTROCARDIOGRAM ST SEGMENT DEPRESSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ELECTROCARDIOGRAM T WAVE AMPLITUDE DECREASED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ELECTROCARDIOGRAM T WAVE INVERSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

EMPHYSEMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ENURESIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

EYE ALLERGY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

EYE PRURITUS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

EYE SWELLING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

FOOD CRAVING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

GASTROENTERITIS VIRAL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

GASTROOESOPHAGEAL REFLUX DISEASE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

GLAUCOMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

GLOSSITIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

HEART RATE INCREASED

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

HEART RATE IRREGULAR

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

HEMIANOPIA HOMONYMOUS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

HYPERSENSITIVITY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

HYPONATRAEMIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

HYPOTENSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2 ( 4.3%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

4.3

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

INFLAMMATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

IRRITABILITY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

LOCALISED INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

MALIGNANT FIBROUS HISTIOCYTOMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

MUSCULAR WEAKNESS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

NASAL MUCOSA BIOPSY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

OEDEMA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

OEDEMA PERIPHERAL

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

ONYCHOMYCOSIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ORTHOSTATIC HYPOTENSION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PALPITATIONS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PARKINSON’S DISEASE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

PELVIC PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

PNEUMONIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

RASH ERYTHEMATOUS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2 ( 3.6%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

3.6

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SECRETION DISCHARGE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

SHOULDER PAIN

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SKIN EXFOLIATION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

SOMNOLENCE

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SUPRAVENTRICULAR EXTRASYSTOLES

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

SUPRAVENTRICULAR TACHYCARDIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

SWELLING

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

TACHYCARDIA

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

ULCER

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

VAGINAL MYCOSIS

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

VENTRICULAR HYPERTROPHY

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 2.2%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

2.2

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

VIRAL INFECTION

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; background-color: rgb(242, 242, 242); font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

<tr>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

WOUND

</td>

<td style="vertical-align: top; text-align: left; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1 ( 1.8%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

1.8

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0 ( 0.0%)

</td>

<td style="vertical-align: top; text-align: center; white-space: normal; padding: 6pt 6pt 6pt 6pt; font-weight: normal; font-size: 10pt;">

0.0

</td>

</tr>

</table>
