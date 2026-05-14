#' Create Barplots of AST by Carbapenem Resistance (MIC)
#'
#' This function generates a barplot to visualize the proportion of AST
#' interpretations (S, I, R) for a set of drugs, faceted by a manually
#' created carbapenem resistance group based on IPM and MEM interpretations.
#' The plot is saved as a PNG file.
#'
#' @param masterdata A data frame containing the AST data. It must include
#'   the columns 'ghru_id', 'IPM', 'MEM', and other specified antimicrobial columns.
#' @param output_dir A character string specifying the directory where the plot will be saved.
#' @param legend_size A numeric value controlling the text size across the plot.
#' @return The function does not return a value. It prints a ggplot object to a PNG file
#'   and provides a message confirming the save location.
#'
#' @export
#'
#' @examples
#' # Assuming 'masterdata' is a data frame loaded in the environment
#' # create_ast_barplots_MIC(masterdata = my_data, output_dir = "plots", legend_size = 20)
create_ast_barplots_MIC <- function(masterdata, output_dir, legend_size) {
  # 1. Subset relevant AST columns
  AST_data <- masterdata[c(
    "ghru_id","AMK", "AMP", "FEP", "CRO", "CIP",
    "COL", "GEN", "IPM", "MEM", "TZP", "SXT"
  )]

  # 2. Create resistance indicator
  AST_data$carba_resistance <- NA
  AST_data$carba_resistance[AST_data$IPM == "S" | AST_data$MEM == "S"] <- "CARBA-S"
  AST_data$carba_resistance[AST_data$IPM == "R" | AST_data$IPM == "I"|
                              AST_data$MEM == "R" | AST_data$MEM == "I"] <- "CARBA-R"

  # 3. Pivot from wide to long format
  AST_data_long <- tidyr::pivot_longer(
    AST_data,
      cols = AMK:SXT,
      names_to = "Antimicrobial",
      values_to = "Interpretation"
    )

  # 4. Set factor levels for interpretation
  AST_data_long$Interpretation <- factor(
    AST_data_long$Interpretation,
    levels = c("S", "I", "R")
  )

  # 5. Plot and save to PNG
  plot_path <- file.path(output_dir, "ast_barplots_MIC_facet.png")
  grDevices::png(filename = plot_path, height = 12, width = 12, res = 350, units = "in")

  print(
    ggplot2::ggplot(AST_data_long, ggplot2::aes(x = Antimicrobial, fill = Interpretation)) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_fill_manual(values = c(
        "R" = '#BC4B51',
        "I" = "#F4A259",
        "S" = "#8CB369"
      )) +
      ggplot2::labs(y = "Proportion") +
      ggplot2::theme(text = ggplot2::element_text(size = legend_size)) +
      ggplot2::facet_wrap(~carba_resistance, ncol = 1)
  )

  grDevices::dev.off()
  message("✅ AST barplot saved to: ", plot_path)
}
