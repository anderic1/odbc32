#' Driver for Odbc32 database.
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
setClass("Odbc32Driver", contains = "DBIDriver")

#' @export
#' @rdname Odbc32-class
setMethod("dbUnloadDriver", "Odbc32Driver", function(drv, ...) {
  TRUE
})

setMethod("show", "Odbc32Driver", function(object) {
  cat("<Odbc32Driver>\n")
})

#' @export
odbc32 <- function() {
  new("Odbc32Driver")
}
