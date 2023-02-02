# Title : Custom Utilities for Nationscape Analysis
# Author : Alex Bass
# Date : 2 Feb 2023

combine_nationscape <- function(list_of_files, vars_to_keep){

  logger::log_info(glue::glue("There are {length(list_of_files)} files to combine"))
  
  for (file_path in list_of_files){
    data <- haven::read_sav(file_path)
    filtered_data <- data[vars_to_keep]
    if(exists('final_data')){
      final_data <- rbind(final_data, filtered_data)
    } else{
      final_data <- filtered_data
    }
  }
  
  logger::log_info(glue::glue('This data has sucessfully combined and has {nrow(final_data)} rows and {ncol(final_data)} columns'))
  
  return(final_data)
  
}
