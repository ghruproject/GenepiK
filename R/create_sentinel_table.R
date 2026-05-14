#' Create Pivot Table of ST vs Carbapenem Genes
#'
#' This function creates a pivot table of sequence types (ST) versus carbapenemase genes
#' found in the `Bla_Carb_acquired` column. It counts occurrences of each gene,
#' adds row totals (per ST) and column totals (across STs), and saves the result as a CSV.
#'
#' @param masterdata A data frame containing at least the columns `ST` and `Bla_Carb_acquired`.
#' @param output_dir A character string specifying the directory to save the CSV.
#' @return Invisibly returns a data frame with ST as rows and carbapenem genes as columns,
#'         including row and column totals.
#' @importFrom dplyr count mutate summarise across bind_rows
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' pivot <- create_ST_gene_pivot(masterdata, "~/git_repos/GenepiK/test_output/")
#' head(pivot)
#' 

create_sentinel_table<- function(masterdata, output_dir) {
  if (!"Laboratory_Name" %in% colnames(masterdata)) {
    stop("masterdata must contain a column named 'Laboratory_Name'")
  }
  
  # 1. Subset relevant columns
  
  hospital_data <- masterdata[c(
    "ghru_id", "Isolate_type", "Laboratory_Name"
  )]
  
  # 2. Sentinel table 
  
  plot_sentinel_isolate_table <- file.path(output_dir, "hospital_summary.csv")
  
  hospital_summary <- hospital_data |>
    dplyr::count(Laboratory_Name, Isolate_type) |>
    tidyr::pivot_wider(
      names_from = Isolate_type,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  write.csv(hospital_summary, file = plot_sentinel_isolate_table  , row.names = FALSE)
  message("✅ Summary of isolates submitted by sentinel table saved to: ", plot_sentinel_isolate_table )
  

}
