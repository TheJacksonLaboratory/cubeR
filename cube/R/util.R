#' These are the util function shared by the package
#'


#' @description
#' to_json convert a HTTP json response \code{httr::Response} to R
#'    data \code{jsonlite::fromJSON}.
#' @param response \code{httr::Response} http response
#'
#' @return data \code{jsonlite::fromJSON}. The data could be list or data frame
#'   depend on the json
#'   https://httr.r-lib.org
#'
#' @export
response_json_to_data = function(
  response
) {
  content = content(response, "text")
  data = fromJSON(content)
}


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


#' Get the get_metadata_query_pivot
#'
#' This function can return database query string based on the element id
#' passed in as a parameter.  It pivots the property_label and property value,
#' grouping by instance id
#'
#' @param dataset_id integer for the element id in database
#' @param accession_ids a vector of character of accession ids to filter on
#' @param max_number_of_row maximum number of rows returned.
#'        default is 100 and -1 for everything
#' @param column_info data.frame of two columns, column name and data type
#'
#' @return data.frame
#'
#' @export
get_metadata_query_pivot <- function(
  dataset_id,
  accession_ids = NULL,
  max_number_of_row = 100,
  column_info
){
  query <- paste("SELECT *
    FROM crosstab( $$select  CAST(ie.accession_id as TEXT),
    CAST(p.property_label as TEXT), CAST(ip.property_value as TEXT)
    from metadata_definition_element e,
    metadata_definition_property p,
    metadata_repository_elementinstance ie,
    metadata_repository_propertyvalue ip
    WHERE e.id=", dataset_id, sep = " ")

  query <- paste(query, "AND e.id = ie.element_id
    AND ie.id = ip.element_instance_id
    AND ip.property_id = p.id", sep = " ")

  if ( length(accession_ids) > 0 ) {
    id_with_single_quote <- lapply(accession_ids, function(v)
      return( paste("'", v, "'", sep = "")))
    query <- paste(query, "AND ie.accession_id in(",
                   paste(id_with_single_quote, collapse=","), ")",  sep = " ")
  }

  # first column is column name
  column_names <- column_info[,1]
  # add single quote to the column names
  columns_with_single_quote <- lapply(column_names, function(v)
    return( paste("'", v, "'", sep = "")))
  query <- paste(query, "AND p.property_label in (", paste(columns_with_single_quote,
                                                           collapse=","), ")",  sep = " ")

  # prepare the data type column, only keep text and number type, and remove other
  # type such as HTML, Enumeration
  data_types <- lapply(column_info[,2], function(v)
    return (switch(v, "Text" = "TEXT", "Number" = "NUMERIC", "Text") ))

  # clean up column names, replace the space with underscore
  column_names_clean <- lapply(column_names, function(v)
    return( gsub(" ", "_", v)))

  # Merge to  to "Classification TEXT, Assay_Name TEXT, N50 Number"
  output_columns <- paste(column_names_clean, " ", data_types)

  query <- paste(query, "ORDER BY ie.accession_id, p.sort_order$$)
         AS final_result(accession_id TEXT,",
                 paste(output_columns, collapse=","), ")", sep = " ");

  if ( max_number_of_row > 0 ) {
    query <- paste(query, "LIMIT", max_number_of_row, sep = " ")
  }
  query
}


