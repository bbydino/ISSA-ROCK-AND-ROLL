#' Read a CSV File
#'
#' Given a csv file, reads the file from the current working directory. MATH 4753 Lab 2
#'
#' @param csv a csv file in the working directory to read in
#'
#' @usage
#' my.df = myread("MYCSV.csv")
#'
#' @export
myread <- function(csv) {
  dird = paste(getwd(), "/", sep = "")
  fl = paste(dird, csv, sep = "")
  read.table(fl, header = TRUE, sep = ",")
}
