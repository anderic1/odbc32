

setOldClass("SOCKcluster")

#' odbc32 connection class.
#'
#' @export
#' @keywords internal
setClass("Odbc32Connection",
         contains = "DBIConnection",
         slots = list(
           cl = "SOCKcluster"
         )
)


#' @param drv An object created by \code{odbc32()}
#' @rdname Odbc32
#' @export
#' @examples
#' \dontrun{
#' db <- dbConnect(odbc32::odbc32())
#' dbWriteTable(db, "mtcars", mtcars)
#' dbGetQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
setMethod("dbConnect", "Odbc32Driver", function(drv, .connection_string="", ...,
                                                timezone = "UTC",
                                                timezone_out = "UTC",
                                                encoding = "",
                                                bigint = "integer64",
                                                timeout = 10) {
  # ...


  fp = file.path(R.home(), "bin", "i386", "rscript.exe")
  stopifnot("R must have a 32bit installation"=file.exists(fp))
  cl = parallel::makePSOCKcluster(1, rscript = fp)


  exports = c("timezone", ".connection_string", "timezone_out", "encoding", "bigint", "timeout")

  parallel::clusterExport(cl, exports, environment())

  parallel::clusterEvalQ(cl, {
    library(DBI)

    dbcon = dbConnect(odbc::odbc(), .connection_string=.connection_string, timezone=timezone, timezone_out=timezone_out,
                      encoding=encoding, bigint=bigint, timeout=timeout)
    respool = (function(){
      counter = 0L
      data = list()
      list(
        add = function(res) {
          counter <<- counter + 1L
          data <<- c(data, setNames(list(res), counter))
          as.character(counter)
        },
        remove = function(nn) {
          i = which(names(data) == nn)
          if (length(i)) {
            resi = data[[i]]
            data <<- data[-i]
            return(resi)
          }
          NULL
        },
        get = function(nn) {
          i = which(names(data) == nn)
          if (length(i)) {
            resi = data[[i]]
            return(resi)
          }
          NULL
        })
    })()
    NULL
  })


  new("Odbc32Connection", cl=cl)
})


setMethod("show", "Odbc32Connection", function(object) {
  cat("<Odbc32Connection>\n")
})


setMethod("dbDisconnect", "Odbc32Connection", function(conn, ...) {
  # ...
  parallel::clusterEvalQ(conn@cl, try(dbDisconnect(dbcon)))
  try(parallel::stopCluster(conn@cl))
  invisible(TRUE)
})



#' @export
setMethod("dbListTables", "Odbc32Connection", function(conn, ...) {

  parallel::clusterEvalQ(conn@cl, dbListTables(dbcon))[[1]]
})


#' @inherit DBI::dbListFields
#' @inheritParams DBI::dbListFields
#' @aliases dbListFields
#' @inheritParams dbListTables,Odbc32Connection-method
#' @param column_name The name of the column to return, the default returns all columns.
#' @inherit dbListTables,Odbc32Connection-method details
#' @export
setMethod(
  "dbListFields", c("Odbc32Connection", "character"), function(conn, name, ...) {
  parallel::clusterExport(cl, "name", environment())
  parallel::clusterEvalQ(conn@cl, dbListFields(dbcon, name))[[1]]
})


#' @export
setMethod("dbQuoteString", "Odbc32Connection", function(conn, x, ...) {
  parallel::clusterExport(conn@cl, "x", environment())
  parallel::clusterEvalQ(conn@cl, dbQuoteString(dbcon, x))[[1]]
})


#' @export
setMethod("dbQuoteIdentifier", "Odbc32Connection", function(conn, x, ...) {
  parallel::clusterExport(conn@cl, "x", environment())
  parallel::clusterEvalQ(conn@cl, dbQuoteIdentifier(dbcon, x))[[1]]
})


#' @export
setMethod("dbWriteTable", "Odbc32Connection", function(conn, name, value, ...) {

  .local <- function (conn, name, value, overwrite = FALSE,
                      append = FALSE, temporary = FALSE, row.names = NA, field.types = NULL,
                      batch_rows = getOption("odbc.batch_rows", NA),
                      ...) {

    exports = c("name", "value", "overwrite", "append", "temporary", "row.names", "field.types", "batch_rows")
    parallel::clusterExport(conn@cl, exports, environment())

    parallel::clusterEvalQ(conn@cl,
                           dbWriteTable(dbcon, name, value, overwrite,
                                        append, temporary, row.names, field.types,
                                        batch_rows))[[1]]

  }
  invisible(.local(conn, name, value, ...))
})



#' @rdname Odbc32Connection
#' @inheritParams DBI::dbRemoveTable
#' @export
setMethod("dbRemoveTable", c("Odbc32Connection", "character"), function(conn, name, ...) {

    exports = c("name")
    parallel::clusterExport(conn@cl, exports, environment())

    ret = parallel::clusterEvalQ(conn@cl,
                           dbRemoveTable(dbcon, name))[[1]]

    invisible(ret)
})



