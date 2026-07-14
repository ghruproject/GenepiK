#' Create a Stacked Barplot of ST Distribution
#'
#' This function generates a stacked barplot showing the distribution of the
#' top 20 most frequent Sequence Types (STs) and their association with the
#' presence or absence of carbapenemase genes. The plot is saved as a PNG file.
#'
#' @param masterdata A data frame containing at least `ghru_id`, `Isolate_type`,
#'   `ST`, and `Bla_Carb_acquired`.
#' @param output_dir A character string specifying the directory where the plot will be saved.
#' @param legend_size A numeric variable specifying the size of the axis text.
#' @return The function does not return a value. It prints a ggplot object to a PNG file
#'   and provides a message confirming the save location.
#'
#' @export
#'
#' @examples
#' # Assuming 'masterdata' is a data frame loaded in the environment
#' # ST_barplot(masterdata = my_data, output_dir = "plots",legend_size= 20) 

create_ST_barplot <- function(masterdata, output_dir, legend_size) {
  # 1. Subset relevant AST columns
  ST_distribution <- masterdata[c(
    "ghru_id","Isolate_type", "ST", "Bla_Carb_acquired"
  )]
  
  # 2. Create resistance indicator
  ST_distribution$carba_presence <- ifelse(ST_distribution$Bla_Carb_acquired == "-", "No", "Yes")
  
  # 3. Identify top 20 STs
  ST_top20 <- ST_distribution |>
    dplyr::count(ST, sort = TRUE) |>
    dplyr::slice_head(n = 20) |>
    dplyr::pull(ST)
  
  ST_top20_carba_R <- ST_distribution |>
    dplyr::filter(Isolate_type == "CARBA-R") |>
    dplyr::count(ST, sort = TRUE) |>
    dplyr::slice_head(n = 20) |>
    dplyr::pull(ST)
  
  ST_top20_carba_S <- ST_distribution |>
    dplyr::filter(Isolate_type == "CARBA-S") |>
    dplyr::count(ST, sort = TRUE) |>
    dplyr::slice_head(n = 20) |>
    dplyr::pull(ST)
  
  # 4. Plot and save to PNG
  plot_path <- file.path(output_dir, "ST_distribution.png")
  grDevices::png(filename = plot_path, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot2::ggplot(
      dplyr::filter(ST_distribution, ST %in% ST_top20),
      ggplot2::aes(x = forcats::fct_infreq(ST), fill = carba_presence)) +
      ggplot2::geom_bar(position = "stack") +
      ggplot2::scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      ggplot2::labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      ggplot2::theme(text = ggplot2::element_text(size = legend_size),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  grDevices::dev.off()
  
  plot_path_carba_r <- file.path(output_dir, "ST_distribution_carba_R.png")
  grDevices::png(filename = plot_path_carba_r, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot2::ggplot(
      ST_distribution |>
        dplyr::filter(Isolate_type == "CARBA-R") |>
        dplyr::filter(ST %in% ST_top20_carba_R),
      ggplot2::aes(x = forcats::fct_infreq(ST), fill = carba_presence)) +
      ggplot2::geom_bar(position = "stack") +
      ggplot2::scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      ggplot2::labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      ggplot2::theme(text = ggplot2::element_text(size = legend_size),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  grDevices::dev.off()
  
  plot_path_carba_s <- file.path(output_dir, "ST_distribution_carba_S.png")
  grDevices::png(filename = plot_path_carba_s, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot2::ggplot(
      ST_distribution |>
        dplyr::filter(Isolate_type == "CARBA-S") |>
        dplyr::filter(ST %in% ST_top20_carba_S),
      ggplot2::aes(x = forcats::fct_infreq(ST), fill = carba_presence)) +
      ggplot2::geom_bar(position = "stack") +
      ggplot2::scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      ggplot2::labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      ggplot2::theme(text = ggplot2::element_text(size = legend_size),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  grDevices::dev.off()
}
