#-------------------------------------------------
# LaTex number plugging
# April 10, 2025
# Purpose: Generate all numbers for number 
# plugging a manuscript 
# Input: CSV file with two columns 
#  1. Variable 
#  2. Value
# Output: tex file with LaTex code to plug in the values

#-------------------------------------------------
# setup
rm(list = ls())
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr, readr)

#-------------------------------------------------
# load data
df <- read_csv(file.path("{your_csv}.csv"))
# these data should have 2 columns: 
# 1. variable name
#   - this will be the name of the variable in the tex file
#   - accordingly, it will be the name of the command in overleaf
#   - overleaf requires no underscores, so use dashes or CamelCase
# 2. value
#   - this will be the value to plug in. it can be numeric or character. 

#-------------------------------------------------
# sample dataframe
df <- data.frame(
  variable = c("var1", "var2", "var3"),
  value = c("1", "4.5", "text")
)
write.csv(df, "sample.csv", row.names = FALSE)

#-------------------------------------------------
# function to create objects and generate tex file
number_plug <- function(df, output_file = "analysis-values.tex") {
  # process each row in the dataframe to create global objects
  tex_commands <- vector()
  for(i in 1:nrow(df)) {
    var_name <- df$variable[i]
    var_value <- df$value[i]
    tex_commands[i] <- paste0("\\newcommand{\\", var_name, "}{", var_value, "}")
  }
  # write to tex file
  write_lines(tex_commands, output_file)
  
  # return the path to the file created
  return(output_file)
}

number_plug(df, "analysis-values.tex")
