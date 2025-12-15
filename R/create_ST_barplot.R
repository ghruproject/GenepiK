#' Create a Stacked Barplot of ST Distribution
#'
#' This function generates a stacked barplot showing the distribution of the
#' top 20 most frequent Sequence Types (STs) and their association with the
#' presence or absence of carbapenemase genes. The plot is saved as a PNG file.
#'
#' @param file_path A character string specifying the path to the CSV file to be read.
#'   This parameter is not currently used within the function body as written,
#'   which operates on a globally assigned 'masterdata' object. For a reusable
#'   package, it's recommended to modify the function to accept 'masterdata' as an
#'   argument directly.
#' @param output_dir A character string specifying the directory where the plot will be saved.
#' @param legend_size A numeric variable specifying the size of the axis text.
#' @importFrom dplyr %>% count pull filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs facet_wrap theme element_text
#' @importFrom forcats fct_infreq
#' @importFrom grDevices png dev.off
#' @importFrom utils file.exists
#' @importFrom stats filter
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
  ST_top20 <- ST_distribution %>%
    count(ST, sort = TRUE) %>%
    slice_head(n = 20) %>%
    pull(ST)
  
  ST_top20_carba_R <- ST_distribution %>%
    filter(Isolate_type== "CARBA-R") %>%
    count(ST, sort = TRUE) %>%
    slice_head(n = 20) %>%
    pull(ST)
  
  ST_top20_carba_S <- ST_distribution %>%
    filter(Isolate_type== "CARBA-S") %>%
    count(ST, sort = TRUE) %>%
    slice_head(n = 20) %>%
    pull(ST)
  
  # 4. Plot and save to PNG
  plot_path <- file.path(output_dir, "ST_distribution.png")
  png(filename = plot_path, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot(
      ST_distribution %>% filter(ST %in% ST_top20),
      aes(x = fct_infreq(ST), fill = carba_presence)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
  dev.off()
  
  plot_path_carba_r <- file.path(output_dir, "ST_distribution_carba_R.png")
  png(filename = plot_path_carba_r, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot(
      ST_distribution %>% 
        filter(Isolate_type== "CARBA-R") %>%
        filter(ST %in% ST_top20_carba_R),
      aes(x = fct_infreq(ST), fill = carba_presence)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
  dev.off()
  
  plot_path_carba_s <- file.path(output_dir, "ST_distribution_carba_S.png")
  png(filename = plot_path_carba_s, height = 12, width = 18, res = 350, units = "in")
  
  print(
    ggplot(
      ST_distribution %>% 
        filter(Isolate_type== "CARBA-S") %>%
        filter(ST %in% ST_top20_carba_S),
      aes(x = fct_infreq(ST), fill = carba_presence)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c(
        "Yes" = '#BC4B51',
        "No" = "#8CB369"
      )) +
      labs(y = "Number of entries",
           x = "ST",
           fill = "Presence of 
carbapenemase genes") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
  dev.off()
}
