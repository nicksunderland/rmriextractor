% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_report_data.R
\name{extract_report_data}
\alias{extract_report_data}
\title{extract_report_data}
\usage{
extract_report_data(reports, variable_regexs)
}
\arguments{
\item{reports}{the data frame containing the text reports}

\item{variable_regexs}{the dataframe containing the regular expressions}
}
\value{
a dataframe of all the extracted numerical variables from each report
}
\description{
extract_report_data
}
\examples{
extract_report_data(data.frame(report1 = "hello world 23.0\%", report2 = "hello world 54.0\%"),
data.frame(percentage = "hello world (?<value>\\\\d+\\\\.?\\\\d*)(?<units>\\\\w*)"))

}
