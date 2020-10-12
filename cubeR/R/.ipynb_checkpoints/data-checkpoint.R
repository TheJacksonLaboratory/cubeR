#' @title Cube Metadata Assay 
#'
#' @name Cube Metadata Assay
#'
#' @description Metadata information for assay
#'
#' @format A assay metadata with 5 variables:
#' \describe{
#'   \item{accession_id}{accession id}
#'   \item{subclass}{Sub class}
#'   \item{target}{Target}
#'   \item{seqclass}{Seq class}
#'   \item{short}{Short}
#' }
"assay"

#' @title Cube Metadata Disease 
#'
#' @name Cube Metadata Disease
#'
#' @description Metadata information for Disease
#'
#' @format A Disease metadata with 3 variables:
#' \describe{
#'   \item{accession_id}{accession id}
#'   \item{hpo_disease_id}{HPO Disease Id }
#'   \item{hpo_disease_term}{HPO Disease Term}
#' }
"disease"

#' @title Cube Metadata Drug_Treatment 
#'
#' @name Cube Metadata Drug_Treatment
#'
#' @description Metadata information for Drug_Treatment
#'
#' @format A Disease metadata with 3 variables:
#' \describe{
#'   \item{accession_id}{accession id}
#'   \item{chebi_name}{Ch E B name }
#'   \item{chebi_id}{Ch E B Id}
#'   \item{short_name}{Short Name}
#' }
"drug_treatment"

#' @title Cube Metadata Mouse_Strain 
#'
#' @name Cube Metadata Mouse_Strain
#'
#' @description Metadata information for Mouse_Strain
#'
#' @format A Disease metadata with 3 variables:
#' \describe{
#'   \item{accession_id}{accession id}
#'   \item{strain_name}{strain_name }
#'   \item{classification}{classification}
#' }
"mouse_strain"

#' @source <https://www.github.com/TheJacksonLaboratory/cubeR>
"snp_data"

#' @title SNP Grid Data
#'
#' @name SNP Grid Data
#'
#' @description A sample data set for SNP Grid Data.
#'
#' @format A SNP data frame with 1000 rows and 10 variables:
#' \describe{
#'   \item{chr}{chromosome number, eg: 6}
#'   \item{start_position}{start position}
#'   \item{end_position}{end position}
#'   \item{rs}{rs}
#'   \item{strain_id}{strain id}
#'   \item{strain_name}{strain name}
#'   \item{genotyp}{genotype}
#'   \item{imputed_flag}{imputed flag}
#'   \item{percent_impluted}{percent impluted}
#'   \item{percent_missing}{percent missing}
#' }
#' @source <https://www.github.com/TheJacksonLaboratory/cube_r>
"snp_data"
