#' A CubeR Function
#'
#' This is basic function to retrieve cube data
#' @param dataset dataset to access.
#' @param accession_ids accession_ids to access.
#' @keywords cubeR
#' @export
#' @examples
#' cubeR("Assay", "JAXAS00005")
cubeR <- function(dataset="Assay", accession_ids="JAXAS00005"){
    print( paste("dataset: ", dataset) )
    print( paste("accession_ids: ", accession_ids) )
   
    data = switch(   
        dataset,   
        "Assay" = assay,   
        "Disease" = disease,   
        "Drug_Treatment" = drug_treatment,   
        "Mouse_Strain"= mouse_strain 
    ) 
    return (data)
}
