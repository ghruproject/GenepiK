pkgname <- "GenepiK"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('GenepiK')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("combine_upset_plots")
### * combine_upset_plots

flush(stderr()); flush(stdout())

### Name: combine_upset_plots
### Title: Combine Multiple Upset Plots into a Single Image
### Aliases: combine_upset_plots

### ** Examples

# Assuming the plots are saved in a folder named 'plots/'
# and your working directory is set to the project root.
# plot_files <- c(
#   "virulence_upset_CARBA-S.png",
#   "virulence_upset_CARBA-R.png",
#   "resistance_upset_CARBA-S.png",
#   "resistance_upset_CARBA-R.png"
# )
# combine_upset_plots(
#   plot_dir = "plots",
#   output_dir = "plots",
#   plot_filenames = plot_files,
#   output_filename = "combined_upset_plot.png"
# )



cleanEx()
nameEx("count_carb_gene_combinations_plot")
### * count_carb_gene_combinations_plot

flush(stderr()); flush(stdout())

### Name: count_carb_gene_combinations_plot
### Title: Count Carbapenem Gene Combinations and Plot
### Aliases: count_carb_gene_combinations_plot

### ** Examples

count_carb_gene_combinations_plot(masterdata, "~/git_repos/GenepiK/test_output/", legend_size= 20)



cleanEx()
nameEx("create_ST_barplot")
### * create_ST_barplot

flush(stderr()); flush(stdout())

### Name: create_ST_barplot
### Title: Create a Stacked Barplot of ST Distribution
### Aliases: create_ST_barplot

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
# ST_barplot(masterdata = my_data, output_dir = "plots",legend_size= 20) 



cleanEx()
nameEx("create_ST_carb_gene_pivot")
### * create_ST_carb_gene_pivot

flush(stderr()); flush(stdout())

### Name: create_ST_carb_gene_pivot
### Title: Create Pivot Table of ST vs Carbapenem Genes
### Aliases: create_ST_carb_gene_pivot

### ** Examples

pivot <- create_ST_gene_pivot(masterdata, "~/git_repos/GenepiK/test_output/")
head(pivot)



cleanEx()
nameEx("create_ast_barplots")
### * create_ast_barplots

flush(stderr()); flush(stdout())

### Name: create_ast_barplots
### Title: Create Barplots of Antimicrobial Susceptibility Testing (AST) by
###   Isolate Type
### Aliases: create_ast_barplots

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
# create_ast_barplots(masterdata = my_data, output_dir = "plots", legend_size= 20)



cleanEx()
nameEx("create_ast_barplots_MIC")
### * create_ast_barplots_MIC

flush(stderr()); flush(stdout())

### Name: create_ast_barplots_MIC
### Title: Create Barplots of AST by Carbapenem Resistance (MIC)
### Aliases: create_ast_barplots_MIC

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
# create_ast_barplots_MIC(masterdata = my_data, output_dir = "plots")



cleanEx()
nameEx("create_ast_barplots_gene")
### * create_ast_barplots_gene

flush(stderr()); flush(stdout())

### Name: create_ast_barplots_gene
### Title: Create Barplots of AST by Carbapenem Resistance (Gene)
### Aliases: create_ast_barplots_gene

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
# create_ast_barplots_gene(masterdata = my_data, output_dir = "plots")



cleanEx()
nameEx("create_ast_pivot")
### * create_ast_pivot

flush(stderr()); flush(stdout())

### Name: create_ast_pivot
### Title: Create Pivot tables of Antimicrobial Susceptibility Testing
###   (AST) by Isolate Type
### Aliases: create_ast_pivot

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
pivot <- create_ast_pivot(masterdata, output_dir)



cleanEx()
nameEx("create_resistance_upset_plots")
### * create_resistance_upset_plots

flush(stderr()); flush(stdout())

### Name: create_resistance_upset_plots
### Title: Create Upset Plots for Resistance Determinants, Faceted by
###   Isolate Type
### Aliases: create_resistance_upset_plots

### ** Examples

# create_virulence_upset_plots(data = my_data, output_dir = "plots")



cleanEx()
nameEx("create_sentinel_table")
### * create_sentinel_table

flush(stderr()); flush(stdout())

### Name: create_sentinel_table
### Title: Create Pivot Table of ST vs Carbapenem Genes
### Aliases: create_sentinel_table

### ** Examples

pivot <- create_ST_gene_pivot(masterdata, "~/git_repos/GenepiK/test_output/")
head(pivot)




cleanEx()
nameEx("create_virulence_upset_plots")
### * create_virulence_upset_plots

flush(stderr()); flush(stdout())

### Name: create_virulence_upset_plots
### Title: Create Upset Plots for Virulence Factors, Faceted by Isolate
###   Type
### Aliases: create_virulence_upset_plots

### ** Examples

# create_virulence_upset_plots(data = my_data, output_dir = "plots")



cleanEx()
nameEx("import_data")
### * import_data

flush(stderr()); flush(stdout())

### Name: import_data
### Title: Check Columns and Read CSV Data
### Aliases: import_data

### ** Examples

# Assuming 'data.csv' exists with the required columns
# and the appropriate packages (readr, dplyr, stringr) are loaded.
# import_data(file_path = "data.csv", output_dir = "my_output_folder")




cleanEx()
nameEx("topN_ST_counts_csv")
### * topN_ST_counts_csv

flush(stderr()); flush(stdout())

### Name: topN_ST_counts_csv
### Title: Create a table os top n ST Distribution
### Aliases: topN_ST_counts_csv

### ** Examples

# Assuming 'masterdata' is a data frame loaded in the environment
topN_ST_counts_csv(masterdata, "~/git_repos/GenepiK/test_output/", 10)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
