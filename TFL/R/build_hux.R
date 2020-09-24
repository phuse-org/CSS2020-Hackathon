library(huxtable)

final <- adae_build %>%
  # Sort by descending incidence from the Xanomeline High Dose group
  #arrange(desc(ord_layer_1), row_label1) %>%
  select(
    row_label1, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`,
    `rdiff_Xanomeline Low Dose_Placebo`, `rdiff_Xanomeline High Dose_Placebo`
  ) %>%
  # the overall N counts are auto replaced for the **Treatment Group** strings below
  add_column_headers(
    paste0(
      "System Organ Class {Preferred Term} |",
      "Placebo (N=**Placebo**) {n(%)} |",
      "Xanomeline Low Dose (N=**Xanomeline Low Dose**) {n(%)} |",
      "Xanomeline High Dose (N=**Xanomeline High Dose**) {n(%)} |",
      "Risk Difference Xanomeline Low Dose - Placebo {(95% CI)} |",
      "Risk Difference Xanomeline High Dose - Placebo {(95% CI)}"
    ),
    header_n = header_n(adae_table)
  )

ht <- as_hux(final, add_colnames = FALSE) %>%
  set_bold(1:2, 1:ncol(final), TRUE) %>% # bold the first row
  set_align(1:2, 1:ncol(final), "center") %>% # Center align the first row
  set_align(3:nrow(final), 3:ncol(final), "center") %>% # Center align the results
  set_valign(1:2, 1:ncol(final), "bottom") %>% # Bottom align the first row
  set_bottom_border(2, 1:ncol(final), 1) %>% # Put a border under the first row
  set_width(1.1) %>% # Set the table width
  #set_col_width(c(.3, rep(.7 / 6, 6))) %>% # Set the column width
  set_background_color(seq(3, nrow(final), 2), everywhere, "grey95")
