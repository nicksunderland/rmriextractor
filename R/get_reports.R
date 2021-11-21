#' get_reports
#'
#' @param file_extension the file extension that the text reports are stored in (the files must be in /data project subdirectory)
#' @return the reports in a data frame
#' @export
#'
#'
get_reports <- function(file_extension){

  file_dir_path <- paste0(path.package(package = "rmriextractor"), "/data")

  files_list <- Sys.glob(file.path(file_dir_path, paste0("*", file_extension)))

  names(files_list) <- seq(length(files_list))

  reports <- purrr::map_dfc(.x = files_list,
                            .f = function(x) stringr::str_replace_all(                      # replace new lines with spaces
                                            stringi::stri_trans_general(                    # translate any non-ASCII characters to standard characters
                                                    stringr::str_to_lower(                  # to lower case letters
                                                         readtext::readtext(x)["text"]      # read in the text files and take just the "text" part
                                                                          ),
                                                                        "latin-ascii"),
                                                                      "\\r?\\n|\\r", " ")
                            )

  return(reports)
}


