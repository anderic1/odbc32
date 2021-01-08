#' Odbc32 results class.
#'
#' @keywords internal
#' @export
setClass("Odbc32Result",
         contains = "DBIResult",
         slots = list(nn = "character", conn = "Odbc32Connection")
)

#' @rdname Odbc32Connection
#' @inheritParams DBI::dbSendQuery
#' @param params Optional query parameters, passed on to [dbBind()]
#' @param immediate If `TRUE`, SQLExecDirect will be used instead of
#'   SQLPrepare, and the `params` argument is ignored
#' @export
setMethod(
  "dbSendQuery", c("Odbc32Connection", "character"),
  function(conn, statement, params = NULL, ..., immediate = FALSE) {
  # some code
  force(statement)
  exports = c("statement", "params", "immediate")
  parallel::clusterExport(conn@cl, exports, environment())
  nn = parallel::clusterEvalQ(conn@cl, {
    res = dbSendQuery(dbcon, statement, params, immediate)
    nn = respool$add(res)
    nn
  })[[1]]
  new("Odbc32Result", nn=nn, conn=conn)
})

#' @export
setMethod("dbClearResult", "Odbc32Result", function(res, ...) {
  # free resources
  nn = res@nn
  exports = c("nn")
  parallel::clusterExport(res@conn@cl, exports, environment())
  parallel::clusterEvalQ(res@conn@cl, {
    res = respool$get(nn)
    dbClearResult(res)
    respool$remove(nn)
  })
  invisible(TRUE)
})


#' Retrieve records from Odbc32 query
#' @export
setMethod("dbFetch", "Odbc32Result", function(res, n = -1, ...) {
  nn = res@nn
  force(n)
  exports = c("nn", "n")
  parallel::clusterExport(res@conn@cl, exports, environment())
  parallel::clusterEvalQ(res@conn@cl, {
    res = respool$get(nn)
    dbFetch(res, n)
  })[[1]]
})

#' @export
setMethod("dbHasCompleted", "Odbc32Result", function(res, ...) {
  nn = res@nn
  exports = c("nn")
  parallel::clusterExport(res@conn@cl, exports, environment())
  parallel::clusterEvalQ(res@conn@cl, {
    res = respool$get(nn)
    dbHasCompleted(res)
  })[[1]]
})


#' @export
setMethod("dbBind", "Odbc32Result", function(res, params, ...) {
  nn = res@nn
  force(params)
  exports = c("nn", "params")
  parallel::clusterExport(res@conn@cl, exports, environment())
  parallel::clusterEvalQ(res@conn@cl, {
    res = respool$get(nn)
    dbBind(res, params)
    NULL
  })
  invisible(res)
})


