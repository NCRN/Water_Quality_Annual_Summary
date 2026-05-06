
check_congruency <- function(data_filename, metadata_filename) {
  
  problems <- 0
  
  # do the files exist?

  if (file.exists(data_filename)==F){
    msg_beginning <- "'" 
    msg_end <- ' does not exist. Check for typos or missing information and try again.'
    msg <- paste0(msg_beginning, data_filename, msg_end)
    problems <- problems + 1
    stop(msg)
  } else {
    msg_beginning <- ""
    msg_end <- ' exists'
    msg <- paste0(msg_beginning, data_filename, msg_end, '\n')
    cat(msg)
  }
  if (file.exists(metadata_filename)==F){
    msg <- paste0(msg_beginning, metadata_filename, msg_end)
    problems <- problems + 1
    stop(msg)
  } else {
    msg_beginning <- ""
    msg_end <- ' exists'
    msg <- paste0(msg_beginning, metadata_filename, msg_end, '\n')
    cat(msg)
  }
  
  # are the files csv?
  msg_beginning <- "'"
  msg_end <- ' is not a CSV file. Check for typos or missing information and try again.'
  if (endsWith(data_filename, '.csv')==F){
    msg <- paste0(msg_beginning, data_filename, msg_end)
    problems <- problems + 1
    stop(msg)
  } else {
    msg_beginning <- ""
    msg_end <- ' is a CSV'
    msg <- paste0(msg_beginning, data_filename, msg_end, '\n')
    cat(msg)
  }
  if (endsWith(metadata_filename, '.csv')==F){
    msg <- paste0(msg_beginning, metadata_filename, msg_end)
    problems <- problems + 1
    stop(msg)
  } else {
    msg_beginning <- ""
    msg_end <- ' is a CSV'
    msg <- paste0(msg_beginning, metadata_filename, msg_end, '\n')
    cat(msg)
  }
  
  data <- read.csv(data_filename)
  metadata <- read.csv(metadata_filename)
  wqp_template <- read.csv('templates/wqp_template.csv')
  metadata_template <- read.csv('templates/wqp_ncrnwater_metadata_template.csv')
  
  # ---------- do the files match the format we expect? ---------- #
  # are the columns present and named properly?
  msg_beginning <- "The column names in '" 
  msg_end <- ' do not match the required format.'
  
  if (all(colnames(wqp_template) != colnames(data))){
    problems <- problems + 1
    msg <- paste0(msg_beginning, data_filename, msg_end)
    warning(msg)
    in_template_not_data <- which(colnames(wqp_template) %in% colnames(data) == F)
    in_data_not_template <- which(colnames(data) %in% colnames(wqp_template) == F)
    if (length(in_template_not_data)>0){
      msg_beginning <- paste0("These columns are in WQP format but not in ",data_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_template_not_data, collapse = ', '), msg_end)
      warning(msg)
    }
    if (length(in_data_not_template)>0){
      msg_beginning <- paste0("These columns are in ",data_filename," but not in WQP format:\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_data_not_template, collapse = ', '), msg_end)
      warning(msg)
    }
    stop()
  } else {
    msg_beginning <- "All column names in "
    msg_end <- ' match WQP format'
    msg <- paste0(msg_beginning, data_filename, msg_end, '\n')
    cat(msg)
  }
  if (all(colnames(metadata_template) != colnames(metadata))){
    problems <- problems + 1
    msg <- paste0(msg_beginning, data_filename, msg_end)
    warning(msg)
    in_template_not_data <- which(colnames(metadata_template) %in% colnames(metadata) == F)
    in_data_not_template <- which(colnames(metadata) %in% colnames(metadata_template) == F)
    if (length(in_template_not_data)>0){
      msg_beginning <- paste0("These columns are in NCRNWater metadata but not in ",metadata_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_template_not_data, collapse = ', '), msg_end)
      warning(msg)
    }
    if (length(in_data_not_template)>0){
      msg_beginning <- paste0("These columns are in ",metadata_filename," but not in NCRNWater metadata:\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_data_not_template, collapse = ', '), msg_end)
      warning(msg)
    }
    stop()
  } else {
    msg_beginning <- "All column names in "
    msg_end <- ' match NCRNWater metadata format'
    msg <- paste0(msg_beginning, metadata_filename, msg_end, '\n')
    cat(msg)
  }
  
  # are sites present and named properly?
  msg_beginning <- "The site names in '" 
  msg_end <- "' do not match."
  if (all(data$MonitoringLocationName %>% unique %in% metadata$SiteName %>% unique) == F){
    problems <- problems + 1
    msg <- paste0(msg_beginning, data_filename, msg_end)
    warning(msg)
    data_locs <- data$MonitoringLocationName %>% unique
    metadata_locs <- metadata$SiteName %>% unique
    in_metadata_not_data <- which(colnames(data_locs) %in% colnames(metadata_locs) == F)
    in_data_not_metadata <- which(colnames(metadata_locs) %in% colnames(data_locs) == F)
    if (length(in_metadata_not_data)>0){
      msg_beginning <- paste0("These site names are in ",metadata_filename," but not in ",data_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_metadata_not_data, collapse = ', '), msg_end)
      warning(msg)
    }
    if (length(in_data_not_metadata)>0){
      msg_beginning <- paste0("These site names are in ",data_filename," but not in ",metadata_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_data_not_metadata, collapse = ', '), msg_end)
      warning(msg)
    }
  } else {
    msg_beginning <- "All site names in "
    msg_end <- ' match the site names in '
    msg <- paste0(msg_beginning, data_filename, msg_end, metadata_filename, '\n')
    cat(msg)
  }

    # are sites present and named properly?
  msg_beginning <- "The site codes in '" 
  msg_end <- "' do not match."
  if (all(data$MonitoringLocationIdentifier %>% unique %in% metadata$SiteCode %>% unique) == F){
    problems <- problems + 1
    msg <- paste0(msg_beginning, data_filename, msg_end)
    warning(msg)
    data_locs <- data$MonitoringLocationIdentifier %>% unique
    metadata_locs <- metadata$SiteCode %>% unique
    in_metadata_not_data <- which(colnames(data_locs) %in% colnames(metadata_locs) == F)
    in_data_not_metadata <- which(colnames(metadata_locs) %in% colnames(data_locs) == F)
    if (length(in_metadata_not_data)>0){
      msg_beginning <- paste0("These site codes are in ",metadata_filename," but not in ",data_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_metadata_not_data, collapse = ', '), msg_end)
      warning(msg)
    }
    if (length(in_data_not_metadata)>0){
      msg_beginning <- paste0("These site codes are in ",data_filename," but not in ",metadata_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_data_not_metadata, collapse = ', '), msg_end)
      warning(msg)
    }
  } else {
    msg_beginning <- "All site codes in "
    msg_end <- ' match the site codes in '
    msg <- paste0(msg_beginning, data_filename, msg_end, metadata_filename, '\n')
    cat(msg)
  }
  
  if (problems == 0){
    cat('\nOK to proceed!\n')
  }
  
}

