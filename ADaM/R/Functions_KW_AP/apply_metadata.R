
apply_metadata <- function(data, xls_path, name, name_col =  "Variable Name", 
                           label_col = "Variable Label", row_offset = 2L) {

  read_meta <- function() readxl::read_excel(xls_path, sheet = name)
  
  tryCatch(read_meta(), error = function(c) {
    c$message <- paste0(c$message, " (in ", xls_path, ")")
    stop(c)
  })
  
  meta <- read_meta() 
  meta_name_col <- tidyselect::vars_select(tbl_vars(meta), !!enquo(name_col))
  meta_label_col <- tidyselect::vars_select(tbl_vars(meta), !!enquo(label_col))
  meta <- meta[row_offset:nrow(meta), c(meta_name_col, meta_label_col)]
  
  meta_vars <- meta[[meta_name_col]]
  meta_labels <- meta[[meta_label_col]]
  # keep only required variables
  df <- data[, meta_vars]

  # Create label for time variable
  df <- sjlabelled::set_label(df,  meta_labels)
  
  df
  
}
