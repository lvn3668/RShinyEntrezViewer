#' Title
#'
#' @return
#' @export
#'
#' @examples
dbconnectionhs <- function() {
  homosapiensmongodbconnection <-
    mongo(
      collection = "entrezgene",
      db = "homosapiensgeneinfo",
      url = "mongodb://localhost:27017/?readPreference=primary&appname=MongoDB%20Compass&ssl=false",
      verbose = TRUE
    )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
dbconnectionphasing <- function() {
  musmusculusphasingdatadbconnection <-
    mongo(
      collection = "NA12878phasingdata",
      db = "homosapiensgeneinfo",
      url = "mongodb://localhost:27017/?readPreference=primary&appname=MongoDB%20Compass&ssl=false",
      verbose = TRUE
    )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
dbconnectionmm <- function() {
  musmusculusentrezgenedbconnection <-
    mongo(
      collection = "musmusculusentrezgene",
      db = "homosapiensgeneinfo",
      url = "mongodb://localhost:27017/?readPreference=primary&appname=MongoDB%20Compass&ssl=false",
      verbose = TRUE
    )
}
