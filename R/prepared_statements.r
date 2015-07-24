#' Adds prepared statements support to RPostgreSQL via SQL.
#'
#' Not efficient as I wanted via a libpq/C
#'
#' @name dbGetPreparedQuery
#' @usage dbGetPreparedQuery(conn, statement, bind.data)
#' @param conn a valid PostgreSQLConnection
#' @param statement a valid SQL statement
#' @param bind.data a dataframe with params to bind to the statement
#' @return I'll see it later
#' @import RPostgreSQL stringr digest methods

.dbGetPreparedQuery <- function(conn, statement, bind.data, ...) {
  ret <- data.frame()
  if(is.vector(bind.data) || is.list(bind.data) || is.matrix(bind.data)) {
    bind.data <- as.data.frame(bind.data)
  }
  
  if(!is.data.frame(bind.data)) {
    stop("bind.data should be a vector, matrix, list or data.frame")
  }
  
  pstatement_name <- paste0("p",digest(
    statement, algo='md5', serialize = FALSE))
  
  count_params <- str_count(statement, "\\?")
  for(I in seq(count_params)) {
    statement <- sub("\\?", paste0("$", I), statement)
  }
  sql <- gsub("--SQL--", statement, "PREPARE --NAME-- AS --SQL--")
  sql <- gsub("--NAME--", pstatement_name, sql)
  
  
  ## controllo se esiste e in caso lo creo.
  df <- dbGetQuery(
    conn,
    paste0(
      "select name from pg_prepared_statements where name='",
      pstatement_name,"'"))
  
  if(!nrow(df)) {
    dbSendQuery(conn, sql)
  }
  
  for(I in seq(nrow(bind.data))) {
    params <- list()
    for (J in seq(ncol(bind.data))) {
      token <- as.character(bind.data[I,J])
      params[[J]] <- if(!is.na(suppressWarnings(as.numeric(token)))) {
        token
      } else {
        paste0("'", gsub("'", "''", token),"'")
      }
    }
    
    params <- paste0(params, collapse=", ")
    execute_sql <- paste0("EXECUTE ", pstatement_name, "(", params, ")")
      results <- tryCatch(
        dbGetQuery(conn, execute_sql),
        error = function(e) {
          stop(e)
        },warning = function(w) {
          stop(w)
        })
    
    ret <- rbind(ret, results)
  }
  ret
}

tryCatch(
  setMethod(
    "dbBegin",
    signature("PostgreSQLConnection", "character", "ANY"),
    .dbGetPreparedQuery),
  error=function(err) {
    setGeneric("dbGetPreparedQuery", function(conn, statement, bind.data, ...) {
      standardGeneric("dbGetPreparedQuery")
    })
    setMethod(
      "dbGetPreparedQuery",
      signature("PostgreSQLConnection", "character", "ANY"),
      .dbGetPreparedQuery)
  })

