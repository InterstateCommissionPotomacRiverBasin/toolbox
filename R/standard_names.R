#' @title Standard Column Names
#' @description Standardizes column names by converting the strings to lowercase, removing white space, and removing all non-alphanumeric characters and replacing them with a specified character.
#' @param x A data frame or tibble.
#' @inheritParams replace_non_alphanumeric_char
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
standard_names <- function(x, ...) {
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
  
  names(x) <- names(x) %>%
    # to lowercase
    tolower() %>% 
    # remove leading/trailing white space
    trimws() %>% 
    replace_non_alphanumeric_char(...)
  
  return(x)
}
