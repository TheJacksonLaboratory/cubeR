% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cube.R
\name{Cube-class}
\alias{Cube-class}
\alias{Cube}
\title{The Cube data object}
\description{
The Cube data object

The Cube data object
}
\details{
Class \code{cube} defines a Cube analysis object.
}
\section{Slots}{

\describe{
\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids

Example to use this classcube_obj = Cube$new()cube_obj$login()    # call the login to get url for verificationdo the verification in a browserresponse = cube_obj$get_element()json = cube_obj$get_metadata_json(element_id = 116, page_size = 5)metadata = cube_obj$get_metadata(element_id = 116, page_size = 5)}
}}

\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{cubeapi}}{\code{CubeAPI}, CubeAPI}

\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Cube$new()}}
\item \href{#method-login}{\code{Cube$login()}}
\item \href{#method-get_element}{\code{Cube$get_element()}}
\item \href{#method-get_data}{\code{Cube$get_data()}}
\item \href{#method-get_metadata_collection}{\code{Cube$get_metadata_collection()}}
\item \href{#method-get_metadata_json}{\code{Cube$get_metadata_json()}}
\item \href{#method-get_metadata}{\code{Cube$get_metadata()}}
\item \href{#method-get_metadata_from_database}{\code{Cube$get_metadata_from_database()}}
\item \href{#method-annotate}{\code{Cube$annotate()}}
\item \href{#method-save_annotated_data}{\code{Cube$save_annotated_data()}}
\item \href{#method-test}{\code{Cube$test()}}
\item \href{#method-finalize}{\code{Cube$finalize()}}
\item \href{#method-print}{\code{Cube$print()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
private fields


Initialization method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$new(element_id = NULL, accession_ids = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-login"></a>}}
\if{latex}{\out{\hypertarget{method-login}{}}}
\subsection{Method \code{login()}}{
login, call \code{CubeAPI} login
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$login()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
character, url for verification, and user will open it in a browser
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_element"></a>}}
\if{latex}{\out{\hypertarget{method-get_element}{}}}
\subsection{Method \code{get_element()}}{
get_element
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_element(element_id = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{element_id}}{integer, element id}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Response \code{httr::Response}  http response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_data}{}}}
\subsection{Method \code{get_data()}}{
get_data, get actual data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_data(page = 1, page_size = 100)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{page}}{integer, page number}

\item{\code{page_size}}{integer, number of items per page}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.frame
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_metadata_collection"></a>}}
\if{latex}{\out{\hypertarget{method-get_metadata_collection}{}}}
\subsection{Method \code{get_metadata_collection()}}{
get_metadata_collection  get metadata collection
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_metadata_collection()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Response \code{httr::Response}  http response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_metadata_json"></a>}}
\if{latex}{\out{\hypertarget{method-get_metadata_json}{}}}
\subsection{Method \code{get_metadata_json()}}{
get_metadata_json by calling cube app API and return json
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_metadata_json(
  element_id = NULL,
  accession_ids = NULL,
  page = 1,
  page_size = 100
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids}

\item{\code{page}}{integer, page number}

\item{\code{page_size}}{integer, number of items per page}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
json \code{jsonlite::fromJSON}cube_obj = Cube$new()cube_obj$login()json = cube_obj$get_metadata_json(element_id = 116, page_size = 5)json$count1 34class(json$results)1 "data.frame"head(json$results, 2)id element.id element.element_label element.element_description element.element_collection_label element.element_abbreviation  element.icon_name1 43465        116          Weight Study                        test                   Weight Studies                           WS stacked_line_chart2 43464        116          Weight Study                        test                   Weight Studies                           WS stacked_line_chartelement.is_primary element_id1               TRUE        1162               TRUE        116
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_metadata"></a>}}
\if{latex}{\out{\hypertarget{method-get_metadata}{}}}
\subsection{Method \code{get_metadata()}}{
get_metadata by calling cube app API
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_metadata(
  element_id = NULL,
  accession_ids = NULL,
  page = 1,
  page_size = 100
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids}

\item{\code{page}}{integer, page number}

\item{\code{page_size}}{integer, number of items per page}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Response \code{httr::Response}  http responsecube_obj = Cube$new()cube_obj$login()response = cube_obj$get_metadata(element_id = 116, page_size = 5)response$status_code          # get http status codecontent = content(response)   # get content of response, content is listnames(content)1 "count"    "next"     "previous" "results"> content$count1 34> content$results1[1][1]$id1 43465[1]$element[1]$element$id1 116[1]$element$element_label1 "Weight Study"[1]$element$element_description1 "test"
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_metadata_from_database"></a>}}
\if{latex}{\out{\hypertarget{method-get_metadata_from_database}{}}}
\subsection{Method \code{get_metadata_from_database()}}{
get_metadata directly from Postgresql database, prefer to use API call
keep it for now
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$get_metadata_from_database(
  element_id = NULL,
  accession_ids = NULL,
  page = 1,
  page_size = 100,
  is_pivot = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{element_id}}{integer, element id}

\item{\code{accession_ids}}{vector of characters, accession ids}

\item{\code{page}}{integer, page number}

\item{\code{page_size}}{integer, number of items per page}

\item{\code{is_pivot}}{boolean, to indicate if the return data
is pivoted by property_label}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.frame
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-annotate"></a>}}
\if{latex}{\out{\hypertarget{method-annotate}{}}}
\subsection{Method \code{annotate()}}{
annotate
annotate derived data objects with a metadata description
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$annotate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-save_annotated_data"></a>}}
\if{latex}{\out{\hypertarget{method-save_annotated_data}{}}}
\subsection{Method \code{save_annotated_data()}}{
save_annotated_data
save the annotated derived data object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$save_annotated_data()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-test"></a>}}
\if{latex}{\out{\hypertarget{method-test}{}}}
\subsection{Method \code{test()}}{
test method.
This is a test function
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$test()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
finalize
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
print
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cube$print()}\if{html}{\out{</div>}}
}

}
}
