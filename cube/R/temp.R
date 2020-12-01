library(httr)

temp <- function() {
# Set Up
#   Create "Native" application
#   Enable "Device Code" grant under "Grant Types" in "Advanced Settings" on bottom of applciation settings page
TENANT_URL = 'thejacksonlaboratory.auth0.com'
DEVICE_URL = paste0('https://', TENANT_URL, '/oauth/device/code')
TOKEN_URL = paste0('https://', TENANT_URL, '/oauth/token')
HEADER = 'content-type: application/x-www-form-urlencoded'
CLIENT_ID = 'sE1muLJOxWHw9Ajed3biEngd1zOi7cOM'
SCOPES = 'profile email'
AUD = 'https://cube.jax.org'
# Get Device Code
device_code_resp = with_verbose(POST(DEVICE_URL,
                                     body = list(client_id = CLIENT_ID, audience = AUD, scopes = SCOPES),
                                     encode = "form"))
content = content(device_code_resp)

# Send user to verification uri
# While you are waiting for the user to activate the device, begin polling the token URL to request an Access Token.
# Using the extracted polling interval (interval) from the previous step, you will need to POST to the token URL sending along the device_code.
# TODO: Make this poll

verification_uri = content[["verification_uri"]]
device_code = content[["device_code"]]
user_code = content[["user_code"]]

log_debug(paste("verification_uri", verification_uri, sep = ": "))
log_debug(paste("device_code", device_code, sep = ": "))
log_debug(paste("user_code", user_code, sep = ": "))


token_resp = POST(TOKEN_URL, body = list(grant_type = 'urn:ietf:params:oauth:grant-type:device_code', device_code = device_code, client_id = CLIENT_ID),
                encode = "form")
token_content = content(token_resp)

access_token = token_content[["access_token"]]



response <- GET(
  url = "https://metadata-service-sqa.azurewebsites.net/metadata_repository/element_instance?format=json&limit=100",
  httr::add_headers("Authorization"=paste("Bearer", access_token, sep = " "), "Content-Type" = "application/json"))

response

content(response)

}


