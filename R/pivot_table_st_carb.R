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
#' @export
#'
#' @examples
#' pivot <- create_ST_gene_pivot(masterdata, "~/git_repos/GenepiK/test_output/")
#' head(pivot)


create_ST_carb_gene_pivot <- function(masterdata, output_dir) {
  
  # Check required columns
  required_cols <- c("ST", "Isolate_type", "Bla_Carb_acquired")
  missing <- setdiff(required_cols, colnames(masterdata))
  if (length(missing) > 0) {
    stop("masterdata must contain columns: ", paste(missing, collapse = ", "))
  }
  
  # Create pivot
  pivot <- masterdata |>
    dplyr::count(ST, Bla_Carb_acquired) |>
    tidyr::pivot_wider(
      names_from = Bla_Carb_acquired,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  pivot_carba_R <- masterdata |>
    dplyr::filter(Isolate_type == "CARBA-R") |>
    dplyr::count(ST, Bla_Carb_acquired) |>
    tidyr::pivot_wider(
      names_from = Bla_Carb_acquired,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  pivot_carba_S <- masterdata |>
    dplyr::filter(Isolate_type == "CARBA-S") |>
    dplyr::count(ST, Bla_Carb_acquired) |>
    tidyr::pivot_wider(
      names_from = Bla_Carb_acquired,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric))))
  
  # Column totals
  totals <- pivot |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum)) |>
    dplyr::mutate(ST = "Total")
  
  totals_carba_R <- pivot_carba_R |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum)) |>
    dplyr::mutate(ST = "Total")

  totals_carba_S <- pivot_carba_S |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum)) |>
    dplyr::mutate(ST = "Total") 
  
  # Bind total row
  pivot <- dplyr::bind_rows(pivot, totals)
  
  pivot_carba_R <- dplyr::bind_rows(pivot_carba_R, totals_carba_R)
  
  pivot_carba_S <- dplyr::bind_rows(pivot_carba_S, totals_carba_S)
  
  # Save to CSV
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_csv <- file.path(output_dir, "ST_vs_carbapenem_genes.csv")
  write.csv(pivot, file = output_csv, row.names = FALSE)
  message("✅ Pivot table saved to: ", output_csv)
  
  output_csv_carba_R <- file.path(output_dir, "ST_vs_carbapenem_genes_carba_R.csv")
  write.csv(pivot_carba_R, file = output_csv_carba_R, row.names = FALSE)
  message("✅ Carbapenem-resistant pivot table saved to: ", output_csv_carba_R)
  
  output_csv_carba_S <- file.path(output_dir, "ST_vs_carbapenem_genes_carba_S.csv")
  write.csv(pivot_carba_S, file = output_csv_carba_S, row.names = FALSE)
  message("✅ Carbapenem-susceptible pivot table saved to: ", output_csv_carba_S)
  
  invisible(pivot)
}
