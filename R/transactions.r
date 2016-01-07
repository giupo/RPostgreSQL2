#' Adds implementation of DBI::dbBegin for RPostgreSQL
#'
#' @name .dbBegin
#' @seealso dbBeginTransaction
#' @rdname dbBegin-internal
#' @importFrom RPostgreSQL dbGetQuery

.dbBegin <- function(conn) {
  dbGetQuery(conn, "START TRANSACTION")
  TRUE
}

#' Adds implementation of DBI::dbBegin for RPostgreSQL
#'
#' @name dbBegin
#' @seealso dbBeginTransaction
#' @exportMethod dbBegin

tryCatch(
  setMethod("dbBegin", signature("PostgreSQLConnection"), .dbBegin),
  error=function(err) { # pass, non servono su Jenkins
    setGeneric("dbBegin", function(conn) {
      standardGeneric("dbBegin")
    })
    setMethod("dbBegin", signature("PostgreSQLConnection"), .dbBegin)
  })
