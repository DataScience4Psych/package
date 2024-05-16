#' Quietly Check Packages
#'
#' Creates a wrapper around `devtools::check()` to execute package checking quietly.
#' This function captures and returns the output and errors without printing them directly to the console.
#'
#' @return A function that when executed will return a list containing elements `result`, `output`, `warnings`, and `messages`.
#' @seealso `purrr::quietly()` for details on the structure of the returned list.
#' @examples
#' result <- check_quietly("myPackage")
#' @export
#' @importFrom purrr quietly
#' @importFrom devtools check install

check_quietly <- purrr::quietly(devtools::check)

#' Quietly Install Packages
#'
#' Creates a wrapper around `devtools::install()` to execute package installation quietly.
#' This function captures and returns the output and errors without printing them directly to the console.
#'
#' @return A function that when executed will return a list containing elements `result`, `output`, `warnings`, and `messages`.
#' @seealso `purrr::quietly()` for details on the structure of the returned list.
#' @examples
#' result <- install_quietly("myPackage")
#' @export
install_quietly <- purrr::quietly(devtools::install)

#' Check and Install Packages Quietly
#'
#' Executes the check or install commands quietly, using purrr's quietly wrapper.
#' @param ... Arguments passed to devtools::check or devtools::install.
#' @param quiet Logical indicating whether to suppress output (default is TRUE for shhh_check).
#' @return Returns a list of the function results.
#' @examples
#' result <- shhh_check("myPackage")
#' install_results <- pretty_install("myPackage")
#' @export
shhh_check <- function(..., quiet = TRUE) {
  out <- check_quietly(..., quiet = quiet)
  out$result
}

#' Pretty Install
#'
#' Performs a quiet installation and cleans up the output, only displaying relevant information.
#' @param ... Arguments passed to devtools::install.
#' @return Returns a cleaned list of output and messages.
#' @examples
#' install_results <- pretty_install("myPackage")
#' @export
pretty_install <- function(...) {
  out <- install_quietly(...)
  output <- strsplit(out$output, split = "\n")[[1]]
  output <- grep("^(\\s*|[-|])$", output, value = TRUE, invert = TRUE)
  c(output, out$messages)
}
