
check_congruency <- function(data_filename, metadata_filename, data_template='templates/wqp_template.csv', metadata_template = 'templates/wqp_ncrnwater_metadata_template.csv') {
  
  cat(paste0('Checking congruency between ', data_filename, ' and ', metadata_filename, ' ...\n\n'))
  
  # package together for convenience
  files <- c(data_filename, metadata_filename)
  
  # instantiate
  results <- list()
  results[['problems']] <- 0
  results[['msgs']] <- c()
  
  # do the files exist?
  results <- check_congruency_files_exist(files, results)

  # are the files csv?
  results <- check_congruency_files_are_csvs(files, results)

  # if the files exist and are the correct filetype, we're ready to test the files themselves
  # package together for convenience
  files <- list(
    'user' = list(
      'data' = list(
        'df' = read.csv(data_filename)
        ,'fname' = data_filename
      )
      ,'metadata' = list(
        'df' = read.csv(metadata_filename)
        ,'fname' = metadata_filename
      )
    )
    ,'template' = list(
      'data' = list(
        'df' = read.csv(data_template)
        ,'fname' = ifelse(data_template=='templates/wqp_template.csv', 'WQP format', data_template)
      )
      ,'metadata' = list(
        'df' = read.csv(metadata_template)
        ,'fname' = ifelse(metadata_template=='templates/wqp_ncrnwater_metadata_template.csv', 'NCRNWater metadata format', metadata_template)
      )
    )
  )
  
  results <- check_congruency_within_files(files, results)
  # 
  # # ---------- do the files match the format we expect? ---------- #
  # # are the columns present and named properly?
  # msg_beginning <- "The column names in '" 
  # msg_end <- ' do not match the required format.'
  # 
  # if (all(colnames(wqp_template) != colnames(data))){
  #   problems <- problems + 1
  #   msg <- paste0(msg_beginning, data_filename, msg_end)
  #   warning(msg)
  #   in_template_not_data <- which(colnames(wqp_template) %in% colnames(data) == F)
  #   in_data_not_template <- which(colnames(data) %in% colnames(wqp_template) == F)
  #   if (length(in_template_not_data)>0){
  #     msg_beginning <- paste0("These columns are in WQP format but not in ",data_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_template_not_data, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   if (length(in_data_not_template)>0){
  #     msg_beginning <- paste0("These columns are in ",data_filename," but not in WQP format:\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_data_not_template, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   stop()
  # } else {
  #   msg_beginning <- "All column names in "
  #   msg_end <- ' match WQP format'
  #   msg <- paste0(msg_beginning, data_filename, msg_end, '\n')
  #   cat(msg)
  # }
  # if (all(colnames(metadata_template) != colnames(metadata))){
  #   problems <- problems + 1
  #   msg <- paste0(msg_beginning, data_filename, msg_end)
  #   warning(msg)
  #   in_template_not_data <- which(colnames(metadata_template) %in% colnames(metadata) == F)
  #   in_data_not_template <- which(colnames(metadata) %in% colnames(metadata_template) == F)
  #   if (length(in_template_not_data)>0){
  #     msg_beginning <- paste0("These columns are in NCRNWater metadata but not in ",metadata_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_template_not_data, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   if (length(in_data_not_template)>0){
  #     msg_beginning <- paste0("These columns are in ",metadata_filename," but not in NCRNWater metadata:\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_data_not_template, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   stop()
  # } else {
  #   msg_beginning <- "All column names in "
  #   msg_end <- ' match NCRNWater metadata format'
  #   msg <- paste0(msg_beginning, metadata_filename, msg_end, '\n')
  #   cat(msg)
  # }
  # 
  # # are sites present and named properly?
  # msg_beginning <- "The site names in '" 
  # msg_end <- "' do not match."
  # if (all(data$MonitoringLocationName %>% unique %in% metadata$SiteName %>% unique) == F){
  #   problems <- problems + 1
  #   msg <- paste0(msg_beginning, data_filename, msg_end)
  #   warning(msg)
  #   data_locs <- data$MonitoringLocationName %>% unique
  #   metadata_locs <- metadata$SiteName %>% unique
  #   in_metadata_not_data <- which(colnames(data_locs) %in% colnames(metadata_locs) == F)
  #   in_data_not_metadata <- which(colnames(metadata_locs) %in% colnames(data_locs) == F)
  #   if (length(in_metadata_not_data)>0){
  #     msg_beginning <- paste0("These site names are in ",metadata_filename," but not in ",data_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_metadata_not_data, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   if (length(in_data_not_metadata)>0){
  #     msg_beginning <- paste0("These site names are in ",data_filename," but not in ",metadata_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_data_not_metadata, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  # } else {
  #   msg_beginning <- "All site names in "
  #   msg_end <- ' match the site names in '
  #   msg <- paste0(msg_beginning, data_filename, msg_end, metadata_filename, '\n')
  #   cat(msg)
  # }
  # 
  #   # are sites present and named properly?
  # msg_beginning <- "The site codes in '" 
  # msg_end <- "' do not match."
  # if (all(data$MonitoringLocationIdentifier %>% unique %in% metadata$SiteCode %>% unique) == F){
  #   problems <- problems + 1
  #   msg <- paste0(msg_beginning, data_filename, msg_end)
  #   warning(msg)
  #   data_locs <- data$MonitoringLocationIdentifier %>% unique
  #   metadata_locs <- metadata$SiteCode %>% unique
  #   in_metadata_not_data <- which(colnames(data_locs) %in% colnames(metadata_locs) == F)
  #   in_data_not_metadata <- which(colnames(metadata_locs) %in% colnames(data_locs) == F)
  #   if (length(in_metadata_not_data)>0){
  #     msg_beginning <- paste0("These site codes are in ",metadata_filename," but not in ",data_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_metadata_not_data, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   if (length(in_data_not_metadata)>0){
  #     msg_beginning <- paste0("These site codes are in ",data_filename," but not in ",metadata_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_data_not_metadata, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  # } else {
  #   msg_beginning <- "All site codes in "
  #   msg_end <- ' match the site codes in '
  #   msg <- paste0(msg_beginning, data_filename, msg_end, metadata_filename, '\n')
  #   cat(msg)
  # }
  # 
  
  msgs <- paste(results[['msgs']], collapse = '\n')
  if (results$problems == 0){
    cat(msgs)
    cat('\n\nOK to proceed!\n')
  } else {
    cat(paste0('Your files have ',results[['problems']],' congruency problems. Resolve those problems before proceeding.'))
    cat(msgs)
    stop()
  }
  
}

check_congruency_within_files <- function(files, results) {
  
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  # template local variables
  data_template <- files[['template']][['data']][['df']]
  data_template_filename <- files[['template']][['data']][['fname']]
  metadata_template <- files[['template']][['metadata']][['df']]
  metadata_template_filename <- files[['template']][['metadata']][['fname']]
  
  # user local variables
  data <- files[['user']][['data']][['df']]
  data_filename <- files[['user']][['data']][['fname']]
  metadata <- files[['user']][['metadata']][['df']]
  metadata_filename <- files[['user']][['metadata']][['fname']]
  
  # are the columns present and named properly?


  if (all(colnames(data_template) != colnames(data))){
    msg_beginning <- "The column names in '"
    msg_end <- ' do not match the required format.'
    msg <- paste0(msg_beginning, data_filename, msg_end)
    tmp[['msgs']] <- c(tmp[['msgs']], msg)

    in_template_not_user <- which(colnames(data_template) %in% colnames(data) == F)
    in_user_not_template <- which(colnames(data) %in% colnames(data_template) == F)
    
    if (length(in_template_not_user)>0){
      msg_beginning <- paste0(length(in_template_not_data), " columns are in ",data_template_filename," but not in ",data_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_template_not_user, collapse = ', '), msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
      tmp[['msgs']] <- c(tmp[['msgs']], msg)
    } else {
      msg_beginning <- paste0("All columns in ",data_template_filename," are present in ")
      msg_end <- '\n'
      msg <- paste0(msg_beginning, data_filename, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
    
    if (length(in_user_not_template)>0){
      msg_beginning <- paste0(length(in_user_not_template), " columns are in ",data," but not in WQP format:\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(in_user_not_template, collapse = ', '), msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
      tmp[['msgs']] <- c(tmp[['msgs']], msg)
    } else {
      msg_beginning <- paste0("All columns in ")
      msg_end <- ' are present in the WQP template\n'
      msg <- paste0(msg_beginning, data, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
    
  } else {
    msg_beginning <- "The column names in "
    msg_end <- paste0(' are the same as those in ', data_template_filename)
    msg <- paste0(msg_beginning, data_filename, msg_end)
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  
  
  # if (all(colnames(metadata_template) != colnames(metadata))){
  #   problems <- problems + 1
  #   msg <- paste0(msg_beginning, data_filename, msg_end)
  #   warning(msg)
  #   in_template_not_data <- which(colnames(metadata_template) %in% colnames(metadata) == F)
  #   in_data_not_template <- which(colnames(metadata) %in% colnames(metadata_template) == F)
  #   if (length(in_template_not_data)>0){
  #     msg_beginning <- paste0("These columns are in NCRNWater metadata but not in ",metadata_filename,":\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_template_not_data, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   if (length(in_data_not_template)>0){
  #     msg_beginning <- paste0("These columns are in ",metadata_filename," but not in NCRNWater metadata:\n")
  #     msg_end <- "\n"
  #     msg <- paste0(msg_beginning, paste(in_data_not_template, collapse = ', '), msg_end)
  #     warning(msg)
  #   }
  #   stop()
  # } else {
  #   msg_beginning <- "All column names in "
  #   msg_end <- ' match NCRNWater metadata format'
  #   msg <- paste0(msg_beginning, metadata_filename, msg_end, '\n')
  #   cat(msg)
  # }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  return(results)
  
}

check_congruency_among_files <- function(files, results) {
  
}

check_congruency_files_exist <- function(files, results) {
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  for (f in files){
    if (file.exists(f)==F){
      msg_beginning <- "'" 
      msg_end <- ' does not exist. Check for typos or missing information and try again.'
      msg <- paste0(msg_beginning, f, msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- ""
      msg_end <- ' exists'
      msg <- paste0(msg_beginning, f, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  return(results)
}

check_congruency_files_are_csvs <- function(files, results) {
  tmp <- list(
    'msgs' = c()
    ,problems = 0
  )
  
  for (f in files){
    if (endsWith(f, '.csv')==F){
      msg_beginning <- "'" 
      msg_end <- ' is not a CSV file. Check for typos or missing information and try again.'
      msg <- paste0(msg_beginning, f, msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- ""
      msg_end <- ' is a CSV'
      msg <- paste0(msg_beginning, f, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  return(results)
}

