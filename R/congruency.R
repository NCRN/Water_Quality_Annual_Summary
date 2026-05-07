
check_congruency <- function(data_filename, metadata_filename, data_template='templates/wqp.csv', metadata_template = 'templates/wqp_ncrnwater_metadata.csv') {
  
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
        ,'fname' = ifelse(data_template=='templates/wqp.csv', 'WQP format', data_template)
      )
      ,'metadata' = list(
        'df' = read.csv(metadata_template)
        ,'fname' = ifelse(metadata_template=='templates/wqp_ncrnwater_metadata.csv', 'NCRNWater metadata format', metadata_template)
      )
    )
  )
  
  # check user data against template data
  # check user metadata against template metadata
  results <- check_congruency_user_versus_template(files, results)
  
  # check user data against user metadata
  results <- check_congruency_user_versus_user(files, results)
  
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

  
  msgs <- paste(results[['msgs']], collapse = '\n')
  if (results$problems == 0){
    cat(msgs)
    cat('\n')
    cat(paste0('Values are congruent: ', data_filename,' and ',metadata_filename, '\n'))
    cat(paste0('Shape and format are congruent: ', data_filename,' and ',data_template, '\n'))
    cat(paste0('Shape and format are congruent: ', metadata_filename,' and ',metadata_template, '\n'))
    cat('\nOK to proceed!')
  } else {
    cat(msgs)
    stop(paste0('Your files have ',results[['problems']],' congruency problem(s). Resolve those problems before proceeding.\n'))
  }
  
}

check_congruency_user_versus_template <- function(files, results) {
  
  # are the columns present and named properly?
  results <- congruency_helper_column_names(files, results)
  
  # TODO: type checking. Check that the columns that NCRNWater needs to be certain formats are actually those types.
  # e.g., numbers are numeric, dates are formatted properly, factors, characters, etc.
  
  # TODO: null checking. Check that required columns have values.
  # e.g., thresholds (metadata$UpperPoint, metadata$LowerPoint) are required because things break otherwise

  return(results)
  
}

check_congruency_user_versus_user <- function(files, results) {
  
  # are the site names consistent?
  # results <- congruency_helper_site_names(files, results)
  
  # are the site codes consistent?
  # results <- congruency_helper_site_codes(files, results)  
  
  return(results)
  
}

congruency_helper_site_names <- function(files, results) {
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


congruency_helper_column_names <- function(files, results) {
  
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
  
  
  if (any(colnames(data_template) %in% colnames(data) == F) | any(colnames(data) %in% colnames(data_template)==F)){
    msg_beginning <- "\nThe column names in "
    msg_end <- ' do not match the required format.'
    msg <- paste0(msg_beginning, data_filename, msg_end)
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
    
    in_template_not_user <- which(colnames(data_template) %in% colnames(data) == F)
    in_user_not_template <- which(colnames(data) %in% colnames(data_template) == F)
    
    if (length(in_template_not_user)>0){
      colnames_in_template_not_user <- colnames(data_template)[in_template_not_user]
      msg_beginning <- paste0(length(in_template_not_user), " column(s) are in ",data_template_filename," but not in ",data_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(colnames_in_template_not_user, collapse = ', '), msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- paste0("All columns in ",data_template_filename," are present in ")
      msg_end <- '\n'
      msg <- paste0(msg_beginning, data_filename, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
    
    if (length(in_user_not_template)>0){
      colnames_in_user_not_template <- colnames(data)[in_user_not_template]
      msg_beginning <- paste0(length(in_user_not_template), " column(s) are in ",data_filename," but not in ",data_template_filename,":\n")
      msg_end <- "\n"
      msg <- paste0(msg_beginning, paste(colnames_in_user_not_template, collapse = ', '), msg_end)
      tmp[['problems']] <- tmp[['problems']] + 1
    } else {
      msg_beginning <- paste0("All columns in ")
      msg_end <- paste0(' are present in ',data_template_filename,'\n')
      msg <- paste0(msg_beginning, data_filename, msg_end)
    }
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
    
  } else {
    msg_beginning <- "The column names in "
    msg_end <- paste0(' are the same as those in ', data_template_filename, '\n')
    msg <- paste0(msg_beginning, data_filename, msg_end)
    tmp[['msgs']] <- c(tmp[['msgs']], msg)
  }
  
  results[['problems']] <- results[['problems']] + tmp[['problems']]
  results[['msgs']] <- c(results[['msgs']], tmp[['msgs']])
  
  return(results)
}
