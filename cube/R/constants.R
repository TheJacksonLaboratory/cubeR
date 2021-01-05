#' The constants used in the package
#'
#'
#' @export

CUBE_VERSION = "0.0.2"
APP_NAME = "cube"

HTTP_METHOD_GET = 1
HTTP_METHOD_POST = 2

HTTP_METHOD_LABELS = list("GET", "POST")

API_END_POINT_metadata_definition_element = "metadata_definition/element/"
API_END_POINT_metadata_repository_element_instance = "metadata_repository/element_instance/"
# leave the trial slash on, post will not work if no trial slash
API_END_POINT_metadata_repository_element_instance_filter = "metadata_repository/element_instance/filter/"
API_END_POINT_cube_about_disclaimer = "cube_about/disclaimer/"
API_END_POINT_data_store_files = "/data_store/files/"
API_END_POINT_metadata_repository_collection = "metadata_repository/collection/"

USER_ACCESS_TOKEN_FILE = "~/.cube_user_access_token.RData"
USER_ACCESS_KEY = "ACCESS_TOKEN"
