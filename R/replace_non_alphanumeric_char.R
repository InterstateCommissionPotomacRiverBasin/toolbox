#' @title Replace Non-Alphanumeric Characters
#' @description FUNCTION_DESCRIPTION
#' @param x A characte vector.
#' @param replacement.char The character(s) used to replace all of the non-alphanumeric characters., Default: '_'
#' @return A vector
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' test.vec <- c("blah&*%^#blah", "blah*%   ()blah", "1234+==-:;56789")
#'  replace_non_alphanumeric_char(test.vec)
#'  replace_non_alphanumeric_char(test.vec, replacement.char = "+")
#'  }
#' }
#' @rdname replace_non_alphanumeric_char
#' @export 

replace_non_alphanumeric_char <- function(x, replacement.char = "_") {
  if (!is.vector(x)) stop("'x' must be a vetor")
    # replace special characters and spaces with "_"
    replace.vec <- gsub("[^[:alnum:]]", replacement.char, x)
    # replace any instances of more than one sequential "_" with a single "_"
    final.vec <- gsub(paste0("[", replacement.char, "]{2,}"), replacement.char, replace.vec)
    
    return(final.vec)
}


