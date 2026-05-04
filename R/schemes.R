#' Get a Color Scheme Definition
#'
#' Returns the full color scheme list for a given scheme name.
#'
#' @param name Character. One of `"clinical"`, `"basic"`, `"code"`,
#'   `"special"`, `"other"`.
#'
#' @return A named list containing color values, palette, label, and icon info.
#'
#' @examples
#' s <- reflow_scheme("clinical")
#' s$primary
#' s$palette
#'
#' @seealso [reflow_schemes()] for listing all available schemes.
#' @export
reflow_scheme <- function(name) {
  name <- match.arg(name, names(.schemes))
  .schemes[[name]]
}

# Internal scheme definitions
.schemes <- list(
  clinical = list(
    name          = "clinical",
    label         = "Clinical Research",
    navbar_icon   = "\U0001F3E5",
    primary       = "#C8102E",
    primary_light = "#E63946",
    accent        = "#A50021",
    secondary     = "#8B0000",
    yellow        = "#F4A261",
    orange        = "#E76F51",
    green         = "#2A9D8F",
    light_grey    = "#F2F2F2",
    grey          = "#6C757D",
    dark          = "#1A1A2E",
    white         = "#FFFFFF",
    gradient_from = "#C8102E",
    gradient_to   = "#8B0000",
    palette       = c("#C8102E", "#E63946", "#A50021", "#F4A261",
                      "#E76F51", "#2A9D8F", "#264653", "#6C757D")
  ),
  basic = list(
    name          = "basic",
    label         = "Basic Research",
    navbar_icon   = "\U0001F52C",
    primary       = "#4682B4",
    primary_light = "#5B9BD5",
    accent        = "#2C5F8A",
    secondary     = "#1B3A5C",
    yellow        = "#F1C40F",
    orange        = "#F39C11",
    red           = "#E84C3D",
    green         = "#27AE61",
    light_grey    = "#ECF0F1",
    grey          = "#7F8C8D",
    dark          = "#2C3E50",
    white         = "#FFFFFF",
    gradient_from = "#4682B4",
    gradient_to   = "#1B3A5C",
    palette       = c("#4682B4", "#5B9BD5", "#2C5F8A", "#F1C40F",
                      "#E84C3D", "#27AE61", "#F39C11", "#7F8C8D")
  ),
  code = list(
    name          = "code",
    label         = "Code & Statistics",
    navbar_icon   = "\U0001F4BB",
    primary       = "#228B22",
    primary_light = "#3CB371",
    accent        = "#1B6B1B",
    secondary     = "#0D3D0D",
    yellow        = "#DAA520",
    orange        = "#CC7722",
    red           = "#CD5C5C",
    blue          = "#4682B4",
    light_grey    = "#F0F4F0",
    grey          = "#6B8E6B",
    dark          = "#1A2E1A",
    white         = "#FFFFFF",
    gradient_from = "#228B22",
    gradient_to   = "#0D3D0D",
    palette       = c("#228B22", "#3CB371", "#1B6B1B", "#DAA520",
                      "#CD5C5C", "#4682B4", "#CC7722", "#6B8E6B")
  ),

special = list(
    name          = "special",
    label         = "Special",
    navbar_icon   = "\U0001F4DC",
    primary       = "#6A0DAD",
    primary_light = "#9B59B6",
    accent        = "#4A0078",
    secondary     = "#2E004F",
    yellow        = "#F4D03F",
    orange        = "#EB984E",
    red           = "#E74C3C",
    green         = "#1ABC9C",
    light_grey    = "#F4F0F8",
    grey          = "#8E7AA0",
    dark          = "#1A0A2E",
    white         = "#FFFFFF",
    gradient_from = "#6A0DAD",
    gradient_to   = "#2E004F",
    palette       = c("#6A0DAD", "#9B59B6", "#4A0078", "#F4D03F",
                      "#E74C3C", "#1ABC9C", "#EB984E", "#8E7AA0")
  ),
  other = list(
    name          = "other",
    label         = "Other",
    navbar_icon   = "\U0001F4C1",
    primary       = "#2C2C2C",
    primary_light = "#555555",
    accent        = "#444444",
    secondary     = "#1A1A1A",
    yellow        = "#BFBFBF",
    orange        = "#999999",
    red           = "#777777",
    green         = "#666666",
    light_grey    = "#F5F5F5",
    grey          = "#808080",
    dark          = "#1A1A1A",
    white         = "#FFFFFF",
    gradient_from = "#2C2C2C",
    gradient_to   = "#1A1A1A",
    palette       = c("#2C2C2C", "#555555", "#444444", "#808080",
                      "#999999", "#BFBFBF", "#666666", "#1A1A1A")
  )
)
