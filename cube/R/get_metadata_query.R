#' Get the get_metadata_query
#'
#' This function can return database query string based on the element id
#' passed in as a paramteter.
#'
#' @param dataset_id integer for the element id in database
#' @param accession_ids a vector of character of accession ids to filter on
#' @param max_number_of_row maximum number of rows returned.
#'        default is 100 and -1 for everything
#'
#' @return data.frame
#'
#' @export
get_metadata_query <- function(
  dataset_id,
  accession_ids = NULL,
  max_number_of_row = 100
  ){
  query <- paste("SELECT e.element_label, e.element_abbreviation,
      p.property_label, p.data_type, ie.accession_id, ip.property_value
      FROM metadata_definition_element e,
      metadata_definition_property p,
      metadata_repository_elementinstance ie,
      metadata_repository_propertyvalue ip
      WHERE e.id = ", dataset_id, sep = " ")

  if ( length(accession_ids) > 0 ) {
    id_with_single_quote <- lapply(accession_ids, function(v)
        return( paste("'", v, "'", sep = "")))
    query <- paste(query, "AND ie.accession_id in(",
        paste(id_with_single_quote, collapse=","), ")",  sep = " ")
  }

  query <- paste(query, "AND e.id = ie.element_id
      AND ie.id = ip.element_instance_id
      AND ip.property_id = p.id
      ORDER BY p.sort_order", sep = " ")

  if ( max_number_of_row > 0 ) {
    query <- paste(query, "LIMIT", max_number_of_row, sep = " ")
  }
  query
}





