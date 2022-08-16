#' @importFrom utils packageVersion

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion("stenR"), 
                        " of stenR package.\n",
                        "Visit https://github.com/statismike/stenR to report an issue",
                        " or contribute. If you like it - star it!")
}
