



cdisc_prep <- function(tab, standard = NULL, standard_path  = NULL,
                       remove_nonstandard = TRUE,
                       remove_blank_perm = TRUE) {
  
  # Read standard
  if(is.null(standard)) {
    assert_that(!is.null(standard_path))
    standard <- readxl::read_excel(standard_path)
  }
  
  # Blank out NAs
  tab[is.na(tab)] <- ""
  
  # Check/Remove nonstandard variables
  tab <- check_cdisc_nonstandard(tab, standard, remove_nonstandard)
  
  # Check variables for missing values(just removes blank perms right now)
  tab <- check_cdisc_core(tab, standard, remove_blank_perm)
  
  # Add labels and data types
  tab <- add_cdisc_metadata(tab, standard)
  
  tab
}

check_cdisc_nonstandard <- function(tab, standard, remove_nonstandard = FALSE) {
  
  # An index representing if each column is found in the standard
  var_is_standard <- names(tab) %in% standard$`Variable Name`
  
  # Warn or remove nonstandard variables
  if(!all(var_is_standard)) {
    if(remove_nonstandard) {
      warning(paste0("Nonstandard variables removed: ",
                     paste0(names(tab)[var_is_standard], collapse = "", sep = " ")))
      tab <- tab[var_is_standard]
      
    } else {
      warning(paste0("Nonstandard variables found: ",
                     paste0(names(tab)[var_is_standard], collapse = "", sep = " ")))
    }
  }
  
  tab
}

add_cdisc_metadata <- function(tab, standard) {
  for(i in seq_along(names(tab))) {
    # If nonstandard name, ignore
    if(names(tab)[i] %in% standard$`Variable Name`) {
      
      # Check variable type and coerce. 
      var_type <- standard %>%
        filter(`Variable Name` == names(tab)[i]) %>%
        select(Type)
      if(var_type == "Char") tab[[i]] <- as.character(tab[[i]])
      else tab[[i]] <- as.numeric(tab[[i]])
      
      # Add label from standard.
      attr(tab[[i]], "label") <- standard %>%
        filter(`Variable Name` == names(tab)[i]) %>%
        select(`Variable Label`) %>%
        extract2(1)
    }
  }
  
  tab
}


check_cdisc_core <- function(tab, standard, remove_blank_perm = FALSE) {
  
  # Vector created. TRUE if varaible is blank and perm core
  blank_perm_vars <- map_lgl(names(tab), is_blank_perm, tab, standard)
  
  # Remove or warn for variables.
  if(remove_blank_perm) {
    warning(paste0("Blank permisable variables removed: ",
                   paste0(names(tab)[blank_perm_vars], sep = " ", collapse = "")))
    tab <- tab[!blank_perm_vars]
  } else {
    warning(paste0("Blank permisable variables found: ",
                   paste0(names(tab)[blank_perm_vars], sep = " ", collapse = "")))
  }
  
  tab
}

is_blank_perm <- function(name, table, standard) {
  
  core_i <- standard %>%
    filter(`Variable Name` == name) %>%
    select(Core) %>%
    extract2(1)
  
  is_blank <- all(is.na(table[, name])) | all(table[, name] == "")
  
  core_i == "Perm" && is_blank
}