#' Helper file to set inputs and outputs for panel functions based on an Excel file with raw features
#'
#' This function loads raw features and related data from an Excel file, in order to help with testing. 
#' It will give inputs and outputs that can be passed to panel functions in order to help with testing.
#'
#' @param testpath The path to the directory where the Excel file is located. If NULL, testthat folder will be considered
#' @param csv_file_name The name of the Excel file.
#' @param features_sheet_names A character vector containing names of sheets that contain features data.
#' @param features_sheet_range A list of character vectors specifying ranges for each features sheet.
#' @param tickers_reference A character vector specifying the range of tickers in the Excel file.
#' @param dates_reference A character vector specifying the range of dates in the Excel file.
#' @param output_sheet_name The name of the sheet containing expected output data.
#' @param output_sheet_range A character vector specifying the range of the output sheet.
#'
#' @return A data frame containing the loaded features and expected output data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- load_features_excel(
#'           testpath = "/path/to/your/excels/",
#'           csv_file_name = "your_excel_file.xlsx",
#'           features_sheet_names = c("Sheet1", "Sheet2"),
#'           features_sheet_range = list(c("A1:B10"), c("C1:D10")),
#'           tickers_reference = "E1:E10",
#'           dates_reference = "F1:F10",
#'           output_sheet_name = "OutputSheet",
#'           output_sheet_range = "A1:C10"
#'         )
#' }
load_inputs_outputs_panels_excel <- function(file_path = NULL, csv_file_name, 
                          features_sheet_names, features_sheet_range,
                          tickers_sheet_range, dates_sheet_range,
                          output_sheet_name, output_sheet_range,
                          industry_classification_column_name,
                          env = parent.frame()){
  

    # Define the file path and sheet names
  if(is.null(file_path)){ #If Null, assume testdata folder
    file_path <- test_path("testdata", csv_file_name)
  } else { #else, consider indication
  file_path <- paste(file_path,"/",csv_file_name, sep = "")
  }
  #List of inputs
  input_list <- list()
  
  # Load the test data and the expected result data
  for(i in 1:length(features_sheet_names)){
  #Features_list  
    input_list[[i]] <- suppressMessages(as.data.frame(readxl::read_excel(#Supress Messages
      file_path, sheet = features_sheet_names[i], range = features_sheet_range, col_names = FALSE))) #F_i
  }
  
  #Sector column position
  sector_sheet_number <- which(features_sheet_names == industry_classification_column_name)
  #Change zeros to NA, as it would be done in practice
  for(j in 1:ncol(input_list[[sector_sheet_number]])){
    input_list[[sector_sheet_number]][,j][which(input_list[[sector_sheet_number]][,j] == 0)] <- NA
  }
  
  
  tickers <- suppressMessages(as.data.frame(readxl::read_excel(
    file_path, sheet = features_sheet_names[1], range = tickers_sheet_range, col_names = FALSE))) #Tickers
  
  dates <-  suppressMessages(as.Date(t(readxl::read_excel(
    file_path, sheet = features_sheet_names[1], range = dates_sheet_range, col_names = FALSE)), format = "%Y-%m-%d")) #Dates
  
  
  #Expected data
  expected_result <- suppressMessages(as.data.frame(readxl::read_excel(file_path, sheet = output_sheet_name, range = output_sheet_range)))
  expected_result$dates <- as.factor(expected_result$dates)
 
  
  
  #Return results
  results <- list()
  inputs <- list()
  
  #Set all possible inputs
  inputs[[1]] <- input_list
  inputs[[2]] <- tickers
  inputs[[3]] <- dates
  inputs[[4]] <- features_sheet_names
  names(inputs) <- c("feature_list", "tickers", "dates", "features_names")
  results[[1]] <- inputs
  
  #Set output
  results[[2]] <- expected_result
  
  #Rename
  names(results) <-  c("inputs", "outputs")
  return(results)
  

}







