#' Create Pivot tables of Antimicrobial Susceptibility Testing (AST) by Isolate Type
#'
#' This function generates a pivot table to visualize the proportion of antimicrobial
#' susceptibility interpretations (Susceptible, Intermediate, Resistant)
#' for a set of drugs, by 'Isolate_type'. The tables are saved as a .CSV file.
#'
#' @param masterdata A data frame containing the AST data. It must include the columns
#'   'ghru_id', 'Isolate_type', and the specified antimicrobial columns.
#' @param output_dir A character string specifying the directory where the plot will be saved.
#' @return Invisibly returns a data frame with ST as rows and carbapenem genes as columns,
#'         including row and column totals.
#' @export
#'
#' @examples
#' # Assuming 'masterdata' is a data frame loaded in the environment
#' pivot <- create_ast_pivot(masterdata, output_dir)

create_ast_pivot <- function(masterdata, output_dir) {
  
  # 1. Subset relevant AST columns
  
  AST_data <- masterdata[c(
    "ghru_id", "Isolate_type", "AMK", "AMP", "FEP", "CRO", "CIP",
    "COL", "GEN", "IPM", "MEM", "TZP", "SXT"
  )]
  
  # 2. Pivot from wide to long format
  AST_data_long <- tidyr::pivot_longer(
    AST_data,
      cols = AMK:SXT,
      names_to = "Antimicrobial",
      values_to = "Interpretation"
    )
  
  # 3. Set factor levels for interpretation
  AST_data_long$Interpretation <- factor(
    AST_data_long$Interpretation,
    levels = c("S", "I", "R")
  )
  
  # 4. AST resistance table 
  
  plot_path_table_carba_r <- file.path(output_dir, "clinical_ast_table_carba_r.csv")
  
  pivot_AST_carba_r <- AST_data_long |>
    dplyr::filter(Isolate_type == "CARBA-R") |>
    dplyr::count(Antimicrobial, Interpretation) |>
    tidyr::pivot_wider(
      names_from = Interpretation,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  write.csv(pivot_AST_carba_r, file = plot_path_table_carba_r  , row.names = FALSE)
  message("✅ Carbapenem-resistant pivot table saved to: ", plot_path_table_carba_r )
  
  plot_path_table_carba_s <- file.path(output_dir, "clinical_ast_table_carba_s.csv")
  
  pivot_AST_carba_s <- AST_data_long |>
    dplyr::filter(Isolate_type == "CARBA-S") |>
    dplyr::count(Antimicrobial, Interpretation) |>
    tidyr::pivot_wider(
      names_from = Interpretation,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  write.csv(pivot_AST_carba_s, file = plot_path_table_carba_s  , row.names = FALSE)
  message("✅ Carbapenem-susceptible pivot table saved to: ", plot_path_table_carba_s )
  
}
