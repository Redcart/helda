#########################################
### Change the path windows ==> linux ###
#########################################

#' @title Convert windows path into linux path
#'
#' @description This function allows to make conversion of windows path into linux path
#'
#' @details
#' When the function is called, a prompt asks for the windows path to be converted
#' in the R console. Enter a windows path or copy paste one. Then type ENTER.
#' The Linux converted path appears.
#'
#' @author Simon CORDE
#' @keywords windows linux path
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export windows_to_linux_path

windows_to_linux_path <- function()
{

  path <- readline("Input File: ")
  new_path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  return(new_path)

}
