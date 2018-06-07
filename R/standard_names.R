#' @title Standard Column Names
#' @description Standardizes column names by converting the strings to lowercase, removing white space, and removing all non-alphanumeric characters and replacing them with a specified character.
#' @param x A data frame or tibble.
#' @param remove.non.alphanumeric.char If FALSE, all non-alphanumeric characters will be unaltered. If TRUE, all non-alphanumeric characters will be replaced by a specified character. Default: TRUE
#' @param replacement.char The character(s) used to replace all of the non-alphanumeric characters., Default: '_'
#' @return A data fram with standardized column names.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data("iris")
#'  test.df <- head(iris)
#'  standard_names(test.df)
#'  standard_names(test.df, replacement.char = "+")
#'  }
#' }
#' @rdname standard_names
#' @export 
standard_names <- function(x, remove.non.alphanumeric.char = TRUE, replacement.char = "_") {
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
  
  names(x) <- tolower(names(x))
  names(x) <- trimws(names(x))
  if (remove.non.alphanumeric.char == TRUE) {
    names(x) <- replace_non_alphanumeric_char(names(x), replacement.char)
  }
  
    
  return(x)
}
