
#' Check Columns and Read CSV Data
#'
#' This function reads a CSV file, checks if its column names match a predefined set of expected columns,
#' and then assigns the data frame to a global variable named `masterdata`. It provides messages
#' to the user about missing or unexpected columns.
#'
#' @param file_path A character string specifying the path to the CSV file to be read.
#' @param output_dir A character string specifying the output directory. This parameter is currently used for a message and does not affect the function's output.
#' @importFrom stringr str_replace_all
#' @return The function does not return a value. Instead, it assigns a data frame to the `masterdata` variable
#' in the global environment and prints messages and warnings to the console.
#'
#' @export
#'
#' @examples
#' # Assuming 'data.csv' exists with the required columns
#' # and the appropriate packages (readr, dplyr, stringr) are loaded.
#' # import_data(file_path = "data.csv", output_dir = "my_output_folder")
#'
import_data <- function(file_path, output_dir) {
  # 1. Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("📂 Output directory created: ", output_dir)
  } else {
    message("📁 Using existing output directory: ", output_dir)
  }
  
  # 2. Save output directory globally
  assign("master_output_dir", output_dir, envir = .GlobalEnv)
  
  # 3. Define expected and optional column names
  required_columns <- c(
    "ghru_id", "Sample collection date", "Specimen type", "Isolate type", "AMK", "AMP", "FEP", "CRO", "CIP", "COL", "GEN", 
    "IPM", "MEM", "TZP", "SXT", "species", "ST", "Yersiniabactin", "Colibactin", "Aerobactin", "Salmochelin", "RmpADC", 
    "virulence_score", "rmpA2", "AGly_acquired", "Col_acquired", "Fcyn_acquired", "Flq_acquired", "Gly_acquired", "MLS_acquired", 
    "Phe_acquired", "Rif_acquired", "Sul_acquired", "Tet_acquired", "Tgc_acquired", "Tmt_acquired", "Bla_acquired",
    "Bla_inhR_acquired", "Bla_ESBL_acquired", "Bla_ESBL_inhR_acquired", "Bla_Carb_acquired", "Bla_chr", "SHV_mutations", 
    "Omp_mutations", "Col_mutations", "Flq_mutations", "resistance_score", "K_locus", "O_locus")
  optional_columns <- c("Laboratory Name")
  
  # 3. Read the CSV data
  data <- readr::read_csv(file_path, col_names = TRUE)
  actual_columns <- colnames(data)
  
  # 4. Compare expected vs actual columns
  missing_required <- setdiff(required_columns, actual_columns)
  missing_optional <- setdiff(optional_columns, actual_columns)
  unexpected <- setdiff(actual_columns, c(required_columns, optional_columns))
  
  if (length(missing_required) == 0 && length(unexpected) == 0) {
    message("✅ Data loaded and all required columns are present.")
  } else {
    warning("⚠️ Data loaded but column names are missing or misordered.")

    if (length(missing_required) > 0) {
      message("Missing required columns: ", paste(missing_required, collapse = ", "))
    }
    if (length(unexpected) > 0) {
      message("Unexpected columns: ", paste(unexpected, collapse = ", "))
    }
  }
  if (length(missing_optional) > 0) {
    message("Missing optional columns: ", paste(missing_optional, collapse = ", "))
  }
  # 5. Replace spaces in column names with underscore
  
  colnames(data) <- stringr::str_replace_all(colnames(data), " ", "_")
  
  # 6. Assign data to global environment
  assign("masterdata", data, envir = .GlobalEnv)
  
  #7. Set output directory message
  message("📁 Output directory set to: ", output_dir)
  
}
