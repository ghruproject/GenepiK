#' Create Upset Plots for Virulence Factors, Faceted by Isolate Type
#'
#' This function generates and saves a series of Upset plots, one for each
#' unique `Isolate_type` present in the data. Each plot visualizes the
#' co-occurrence of virulence factors and colors the intersections by
#' `virulence_score`. The plots are saved as PNG files.
#'
#' @param data A data frame containing the data. It must include the columns
#'   `Isolate_type`, `virulence_score`, and the virulence factor columns defined
#'   within the function (`Yersiniabactin`, `Colibactin`, etc.).
#' @param output_dir A character string specifying the directory where the plots will be saved.
#' @param legend_size A numeric value controlling the text size across the plot.
#' @param label_size An optional numeric value controlling the axis and set label
#'   text size separately. Defaults to `legend_size`.
#' @return The function does not return a value. It creates one or more PNG files
#'   in the specified output directory.
#'
#' @export
#'
#' @examples
#' # create_virulence_upset_plots(data = my_data, output_dir = "plots", legend_size = 12)
create_virulence_upset_plots <- function(data, output_dir, legend_size = 12, label_size = legend_size) {
  virulence_classes <- c("Yersiniabactin", "Colibactin", "Aerobactin", "Salmochelin", "RmpADC")

  required_cols <- c("Isolate_type", "virulence_score", virulence_classes)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data_converted <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(virulence_classes),
        ~ !is.na(.) & . != "-"
      )
    )

  isolate_types <- unique(data_converted$Isolate_type)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (type in isolate_types) {
    subset_data <- data_converted |>
      dplyr::filter(.data$Isolate_type == type)

    upset_plot <- ComplexUpset::upset(
      subset_data,
      intersect = virulence_classes,
      name = "Virulence Factors",
      width_ratio = 0.1,
      set_sizes = FALSE,
      base_annotations = list(
        "Frequency" = ComplexUpset::intersection_size(
          ggplot2::aes(fill = as.factor(.data$virulence_score))
        ) +
          ggplot2::scale_fill_manual(
            name = "Virulence score",
            values = c("0" = "#B2B09B", "1" = "mediumblue", "2" = "#43AA8B", "3" = "deeppink3", "4" = "maroon3", "5" = "deeppink4")
          )
      ),
      themes = ComplexUpset::upset_default_themes() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = label_size, angle = 90, hjust = 1),
          axis.text.y = ggplot2::element_text(size = label_size),
          axis.title.x = ggplot2::element_text(size = label_size, face = "bold"),
          axis.title.y = ggplot2::element_text(size = label_size, face = "bold"),
          legend.title = ggplot2::element_text(size = legend_size),
          legend.text = ggplot2::element_text(size = legend_size),
          legend.spacing = grid::unit(5, "pt"),
          plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
          strip.text = ggplot2::element_text(size = label_size)
        )
    )

    file_name <- paste0("virulence_upset_", type, ".png")
    plot_path <- file.path(output_dir, file_name)
    grDevices::png(filename = plot_path, height = 12, width = 18, res = 350, units = "in")
    print(upset_plot)
    grDevices::dev.off()
    message("✅ Plot for '", type, "' saved to: ", plot_path)
  }
}
