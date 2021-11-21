library(usethis)
library(devtools)
rm(list=ls())
load_all()

# Read the reports from the /data folder in this R package; need to specify what file extension they have
reports <- get_reports("*.docx")

# Create the regular expressions
variable_regexs <- get_variable_regular_expressions()

# Read the reports
data <- extract_report_data(reports, variable_regexs)

