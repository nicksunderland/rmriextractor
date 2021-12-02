#' extract_report_data
#'
#' @param reports the data frame containing the text reports
#' @param variable_regexs the dataframe containing the regular expressions
#'
#' @return a dataframe of all the extracted numerical variables from each report
#' @export
#'
#' @examples extract_report_data(data.frame(report1 = "hello world 23.0%", report2 = "hello world 54.0%"),
#' data.frame(percentage = "hello world (?<value>\\d+\\.?\\d*)(?<units>\\w*)"))
#'
extract_report_data <- function(reports, variable_regexs){

  # The base::outer function just helps do matrix multiplication; all it does is replicate the regexes so that each report gets exposed to each regex - e.g. if 10 regexes and 10 reports this just creates a [100 x 2] matrix to work on
  result <- outer(reports,
                  variable_regexs,
                  FUN = function(reports, variable_regexs){
                      return(purrr::map2_dbl(reports, variable_regexs, .f = inner_func)) # Apply the inner function to each text-report:regex pair, return as a double database
                  }
  )                                                   # the outer function nicely re-converts back to an appropriately sized matrix e.g. 10x10 in the example above
  return(as.data.frame(t(result)))
}


inner_func <- function(text , regex_list){        # Define this function to apply to each row (col1 = the report, col2 = the list of regex for a particular variable)
  value <- NA_real_                               # set to NA
  units <- NA_character_                          # set to NA
  for(pattern in regex_list){                # Cycle through each regex provided for each variable e.g. it might be c("find my variable this way (\d+)", "find my variable another way (\d+)"")
    # match_obj <- stringr::str_match(text, pattern) # Match the regex in the text report

    match_obj <- regexpr(pattern, text, perl=TRUE)
    value_str <- substr(text, attr(match_obj, "capture.start" )[,"value"],
                              attr(match_obj, "capture.start" )[,"value"] +
                              attr(match_obj, "capture.length")[,"value"] - 1)

    # If value not found "capture.start" will be 0 or -1
    if(attr(match_obj, "capture.start")[,"value"] > 0){
      try(value <- as.numeric(value_str))        # try to convert the result to a number
    }

    # Units don't have to be in the regular expression
    tryCatch(
      expr = {units_str <- substr(text, attr(match_obj, "capture.start" )[,"units"],
                                        attr(match_obj, "capture.start" )[,"units"] +
                                        attr(match_obj, "capture.length")[,"units"] - 1)
              units <- as.character(units_str)},
      error= function(e) {
        # Choose a return value in case of error
        return(NA)
      }
    )

    if(!is.na(value)) break                       # if successful at finding a numerical value exit the loop
  }
  return(value)
}
