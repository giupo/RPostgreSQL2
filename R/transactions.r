#' Adds implementation of DBI::dbBegin for RPostgreSQL
#'
#' @name dbBegin
#' @seealso dbBeginTransaction
#' @importFrom RPostgreSQL dbGetQuery
#' @export dbBegin

.dbBegin <- function(conn) {
  dbGetQuery(conn, "START TRANSACTION")
  TRUE
}

tryCatch(
  setMethod("dbBegin", signature("PostgreSQLConnection"), .dbBegin),
  error=function(err) { # pass, non servono su Jenkins
    setGeneric("dbBegin", function(conn) {
      standardGeneric("dbBegin")
    })
    setMethod("dbBegin", signature("PostgreSQLConnection"), .dbBegin)
  })
