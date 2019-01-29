#' Change the path windows ==> linux
#'
#' This function allows to change "\" into "/" from system paths
#' @input : string that is required to be inserted int console
#' windows_to_linux_path()

windows_to_linux_path <- function()
{

  path <- readline("Input File: ")
  new_path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  return(new_path)

}
