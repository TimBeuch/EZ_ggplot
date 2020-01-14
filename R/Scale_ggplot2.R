#' Automatique manual scale
#'
#' @param primary first color
#' @param other second color
#' @param direction direction like ggplot2
#'
#' @return character vector
palette_couleur <- function(primary = "bleu",
                            other = "gris",
                            direction = 1) {

  palette = list( "bleu" = "#56B4E9",
                  "rouge" = "#A4243B",
                  "jaune" = "#EDAE49",
                  "vert" = "#66A182",
                  "rose" = "#FCC5C2",
                  "gris" = "#999999",
                  "sombre" = "#2E4057")

  stopifnot(primary %in% names(palette))

  function(n) {
    if (n > 7) warning("La pellette de couleur n'a que 7 couleur")

    if (n == 2) {
      other <- if (!other %in% names(palette)) {
        other
      } else {
        palette[other]
      }
      color_list <- c(other,
                         palette[primary])
    } else {
      color_list <- palette[1:n]
    }

    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}


#' Automatique manual scale
#'
#' @param primary first color
#' @param other second color
#' @param direction direction like ggplot2
#' @param ... other
#' @import ggplot2
#'
#' @return ggplot argument to add to a ggplot object
#' @export
EZ_colour_point <- function(primary = "bleu",
                            other = "gris",
                            direction = 1,
                            ...) {
  ggplot2::discrete_scale(
    "colour", "branded",
    palette_couleur(primary,
                    other,
                    direction),
    ...
  )
}

#' Automatique manual scale
#'
#' @param primary first color
#' @param other second color
#' @param direction direction like ggplot2
#' @param ... other
#' @import ggplot2
#'
#' @return a ggplot argument to add to a ggplot object
#' @export
EZ_colour_fill <- function(primary = "bleu",
                           other = "gris",
                           direction = 1,
                           ...) {
  ggplot2::discrete_scale(
    "fill", "branded",
    palette_couleur(primary,
                    other,
                    direction),
    ...
  )
}
