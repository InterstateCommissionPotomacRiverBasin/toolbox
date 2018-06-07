#' @title Standardize Character Column Strings
#' @description Standardizes all character column strings by converting the strings to lowercase, removing white space, and removing all non-alphanumeric characters and replacing them with a specified character.
#' @param x A data frame or tibble.
#' @param remove.non.alphanumeric.char If FALSE, all non-alphanumeric characters will be unaltered. If TRUE, all non-alphanumeric characters will be replaced by a specified character. Default: TRUE
#' @inheritParams replace_non_alphanumeric_char
#' @return  A data fram with standardized character columns.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data("starwars")
#'  test.df <- head(starwars)
#'  standard_char(test.df)
#'  standard_char(test.df, replacement.char = "+")
#'  }
#' }
#' @rdname standard_char
#' @export 
#' @importFrom dplyr mutate_if funs

standard_char <- function(x, remove.non.alphanumeric.char = TRUE, ...) {
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
    # All characters to lowercase and remove leading/trailing white space
    final.df <- dplyr::mutate_if(x,
                     is.character,
                     dplyr::funs(trimws(tolower(.))))  
    # remove instances of more than one space
    final.df <- dplyr::mutate_if(final.df,
                                 is.character,
                                 dplyr::funs(gsub("[' ']{2,}", " ", .)))  
  
  if (remove.non.alphanumeric.char == TRUE) {
    final.df <- dplyr::mutate_if(final.df,
                                 is.character,
                                 dplyr::funs(replace_non_alphanumeric_char(., ...)))

  }
  
  return(final.df)
}
