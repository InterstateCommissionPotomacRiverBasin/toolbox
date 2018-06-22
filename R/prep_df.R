#' @title Prepare Data Frame
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param standard.names Should column names be standardized?, Default: TRUE
#' @param standard.char Should character column strings be standardized?, Default: TRUE
#' @inheritParams standard_names
#' @return A standardized data frame.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data("starwars")
#'  test.df <- head(starwars)
#'  prep_df(test.df)
#'  prep_df(test.df, replacement.char = "+")
#'  }
#' }
#' @rdname prep_df
#' @export 

prep_df <- function(x, standard.names = TRUE, standard.char = TRUE, ...){
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
  final.df <- x
  if (standard.names == TRUE) final.df <- standard_names(final.df, ...)
  if (standard.char == TRUE) final.df <- standard_char(final.df, ...)
  # final.df[final.df == ""] <- NA
  return(final.df)
}

