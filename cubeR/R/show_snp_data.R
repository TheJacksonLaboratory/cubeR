#' SNP Grid Data
#'
#' This function allows you to access the snp grid data.
#' @param num_of_rows number of rows to print.
#' @keywords cube
#' @export
#' @examples
#' show_snp_data(5)
show_snp_data <- function(num_of_rows=5){
    file_name <- "snap_grid_sample.csv"
    file <- system.file("extdata", file_name, package="cube")
    data <- read.table(file, header=TRUE, sep=",")
    print( head(data, num_of_rows) )
}
