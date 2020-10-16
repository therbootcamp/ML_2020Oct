# Following https://baselrbootcamp.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# See https://github.com/therbootcamp/organisation/wiki/Color-schemes
#  for color palette


baselrbootcamp_colors <- c(
  `magenta`     = "#EA4B68",
  `grey`        = "#606060",
  `green`       = "#6ABA9A",
  `yellow`      = "#EACC48")

#' Function to extract baselrbootcamp colors as hex codes
#'
#' @param ... Character names of baselrbootcamp_colors 
#'
baselrbootcamp_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (baselrbootcamp_colors)
  
  baselrbootcamp_colors[cols]
}


baselrbootcamp_palettes <- list(
  
  `two`  = baselrbootcamp_cols("magenta", "grey"),
  
  `three`  = baselrbootcamp_cols("magenta", "grey", "green"),
  
  `four`  = baselrbootcamp_cols("magenta", "grey", "green", "yellow")
)

#' Return function to interpolate a baselrbootcamp color palette
#'
#' @param palette Character name of palette in baselrbootcamp_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
baselrbootcamp_pal <- function(palette = "three", reverse = FALSE, ...) {
  pal <- baselrbootcamp_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for baselrbootcamp colors
#'
#' @param palette Character name of palette in baselrbootcamp_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_baselrbootcamp <- function(palette = "three", discrete = TRUE, reverse = FALSE, ...) {
  pal <- baselrbootcamp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("baselrbootcamp_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for baselrbootcamp colors
#'
#' @param palette Character name of palette in baselrbootcamp_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_baselrbootcamp <- function(palette = "three", discrete = TRUE, reverse = FALSE, ...) {
  pal <- baselrbootcamp_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("baselrbootcamp_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
