#' Count Carbapenem Gene Combinations and Plot 
#'
#' This function counts the occurrences of each unique combination of
#' carbapenem genes in the `Bla_Carb_acquired` column, keeping combinations intact,
#' calculates percentages, saves a CSV, and generates a bar plot.
#' The "-" entries are treated as "CARB-S".
#'
#' @param masterdata A data frame containing the column `Bla_Carb_acquired`.
#' @param output_dir A character string specifying the directory to save the CSV and plot.
#' @param legend_size A number specifying the size of the legend. 
#' @return Invisibly returns a data frame with columns: Gene_Combination, Count, Percentage.
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs theme element_text
#' @importFrom grDevices png dev.off
#' @export
#'
#' @examples
#' count_carb_gene_combinations_plot(masterdata, "~/git_repos/GenepiK/test_output/", legend_size= 20)


count_carb_gene_combinations_plot <- function(masterdata, output_dir,legend_size) {
  
  if(!"Bla_Carb_acquired" %in% colnames(masterdata)) {
    stop("masterdata must contain a column named 'Bla_Carb_acquired'")
  }
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Replace missing/empty with "No carbapenemase"
  gene_col <- as.character(masterdata$Bla_Carb_acquired)
  gene_col[is.na(gene_col) | gene_col == "" | gene_col == "-"] <- "No carbapenemase"
  
  gene_col_carba_r <- as.character(masterdata$Bla_Carb_acquired[masterdata$Isolate_type== "CARBA-R"])
  gene_col_carba_r[is.na(gene_col_carba_r) | gene_col_carba_r == "" | gene_col_carba_r == "-"] <- "No carbapenemase"
  
  
  gene_col_carba_s <- as.character(masterdata$Bla_Carb_acquired[masterdata$Isolate_type== "CARBA-S"])
  gene_col_carba_s[is.na(gene_col_carba_s) | gene_col_carba_s == "" | gene_col_carba_s == "-"] <- "No carbapenemase"
  
  
  # Count unique combinations
  combo_counts <- as.data.frame(table(gene_col))
  colnames(combo_counts) <- c("Gene_Combination", "Count")
  
  combo_counts_carba_r <- as.data.frame(table(gene_col_carba_r))
  colnames(combo_counts_carba_r) <- c("Gene_Combination", "Count")
  
  combo_counts_carba_s <- as.data.frame(table(gene_col_carba_s))
  colnames(combo_counts_carba_s) <- c("Gene_Combination", "Count")
  
  # Calculate percentage relative to total isolates
  total_isolates <- nrow(masterdata)
  combo_counts$Percentage <- round((combo_counts$Count / total_isolates) * 100, 2)
  
  total_isolates_carba_r <- nrow(masterdata[masterdata$Isolate_type== "CARBA-R",])
  combo_counts_carba_r$Percentage <- round((combo_counts_carba_r$Count / total_isolates_carba_r) * 100, 2)
  
  total_isolates_carba_s <- nrow(masterdata[masterdata$Isolate_type== "CARBA-S",])
  combo_counts_carba_s$Percentage <- round((combo_counts_carba_s$Count / total_isolates_carba_s) * 100, 2)
  
  # Save CSV
  output_csv <- file.path(output_dir, "carbapenem_gene_combinations.csv")
  write.csv(combo_counts, file = output_csv, row.names = FALSE)
  message("✅ Carbapenem gene combination summary saved to: ", output_csv)
  
  output_csv_carba_r <- file.path(output_dir, "carbapenem_gene_combinations_carba_R.csv")
  write.csv(combo_counts_carba_r, file = output_csv_carba_r, row.names = FALSE)
  message("✅ Carbapenem-resistance carbapenem gene combination summary saved to: ", output_csv_carba_r)

  output_csv_carba_s <- file.path(output_dir, "carbapenem_gene_combinations_carba_S.csv")
  write.csv(combo_counts_carba_s, file = output_csv_carba_s, row.names = FALSE)
  message("✅ Carbapenem-susceptible carbapenem gene combination summary saved to: ", output_csv_carba_s)
  
  ##Bar plots
  
  # Create bar plot overal
  output_png <- file.path(output_dir, "carbapenem_gene_combinations_plot.png")
  png(filename = output_png, height = 8, width = 12, res = 300, units = "in")
  
  # Explicitly print the ggplot object
  print(
    ggplot(combo_counts, aes(x = reorder(Gene_Combination, -Count), y = Count, fill = Gene_Combination)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Number of Isolates", fill = "Gene Combination") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(legend.position="none")
    
  )
  dev.off()
  message("✅ Carbapenem gene combination plot saved to: ", output_png)
  
  
  # Create bar plot carba-R
  output_png_carba_r <- file.path(output_dir, "carbapenem_gene_combinations_plot_carba_r.png")
  png(filename = output_png_carba_r, height = 8, width = 12, res = 300, units = "in")
  
  # Explicitly print the ggplot object
  print(
    ggplot(combo_counts_carba_r, aes(x = reorder(Gene_Combination, -Count), y = Count, fill = Gene_Combination)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Number of Isolates", fill = "Gene Combination") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(legend.position="none")
    
  )
  dev.off()
  message("✅ Carbapenem-resistance carbapenem gene combination plot saved to: ", output_png)
  
  # Create bar plot carba-S
  output_png_carba_s <- file.path(output_dir, "carbapenem_gene_combinations_plot_carba_s.png")
  png(filename = output_png_carba_s, height = 8, width = 12, res = 300, units = "in")
  
  # Explicitly print the ggplot object
  print(
    ggplot(combo_counts_carba_s, aes(x = reorder(Gene_Combination, -Count), y = Count, fill = Gene_Combination)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Number of Isolates", fill = "Gene Combination") +
      theme(text = element_text(size = legend_size),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(legend.position="none")
    
  )
  dev.off()
  message("✅ Carbapenem-resistance carbapenem gene combination plot saved to: ", output_png)
  
  invisible(combo_counts)
}

