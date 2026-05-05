#' Create a table os top n ST Distribution
#'
#' This function generates a stacked barplot showing the distribution of the
#' top 20 most frequent Sequence Types (STs) and their association with the
#' presence or absence of carbapenemase genes. The plot is saved as a PNG file.
#'
#' @param masterdata A data frame containing a column named `ST`.
#' @param output_dir A character string specifying the directory where the plot will be saved.
#' @param top_n An integer specifying how many top STs to include (default 10).
#' @return Invisibly returns a data frame containing the top sequence types and percentages.
#'
#' @export
#'
#' @examples
#' # Assuming 'masterdata' is a data frame loaded in the environment
#' topN_ST_counts_csv(masterdata, "~/git_repos/GenepiK/test_output/", 10)
topN_ST_counts_csv <- function(masterdata, output_dir, top_n) {
  if(!"ST" %in% colnames(masterdata)) {
    stop("masterdata must contain a column named 'ST'")
  }

  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Count occurrences of each ST
  ST_counts <- masterdata |>
    dplyr::count(ST, name = "Number") |>
    dplyr::arrange(dplyr::desc(Number)) |>
    dplyr::slice_head(n = top_n)

  # Calculate percentage relative to total dataset
  total <- nrow(masterdata)
  ST_counts <- ST_counts |>
    dplyr::mutate(Percentage = round((Number / total) * 100, 2))

  # Write CSV
  output_file <- file.path(output_dir, paste0("top", top_n, "_ST_counts.csv"))
  write.csv(ST_counts, file = output_file, row.names = FALSE)

  message("✅ Top ", top_n, " ST counts saved to: ", output_file)
  
  invisible(ST_counts)
}
