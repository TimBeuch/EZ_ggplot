#' ggplot theme
#'
#' @param supress permet de specifier si on veut supprimer la mise en forme pour un des axes x,y ou les deux.
#' @param legend permet de specifier si on veut la presence ou non de la legende, si bot alors la legende est en bas
#' @param ... other
#' @import ggplot2
#'
#' @return a ggplot object
#' @export
EZ_theme = function(legend = c("TRUE","bot","FALSE"),
                    supress = c("none","x","y","both"),
                    rotate = c("none","x","y","both"),
                    angle = 90,
                    ...) {

  legend <- match.arg(legend)
  supress <- match.arg(supress)
  rotate <- match.arg(rotate)

if(!supress == "none" &
   !rotate == "none"){
  if(supress =="both" &
     rotate %in% c("x","y","both")){
    message("Attention, vous demandez deux actions non compatibles pour un même axe")
}
  if(rotate =="both" &
     supress %in% c("x","y","both")){
    message("Attention, vous demandez deux actions non compatibles pour un même axe")
  }
  }else{}


  THEME = ggplot2::theme(plot.title = element_text(hjust = 0.5,
                                                   size = 11,
                                                   face = "bold",
                                                   colour = "#ad1c72"),
                         plot.subtitle = element_text( hjust = 0.5,
                                                       size = 8,
                                                       face = "italic",
                                                       colour = "black"),
                         legend.title = element_text(hjust = 0.5,
                                                     colour = "#ad1c72"),
                         axis.title.x = element_text(color = "#ad1c72"),
                         axis.title.y = element_text(color = "#ad1c72"),
                         axis.line = element_line(size = 0.5,
                                                  linetype = "solid",
                                                  colour = "#ad1c72"),
                         plot.caption = element_text(hjust = 0,
                                                     size = 6,
                                                     face = "italic"),
                         panel.background = element_rect(fill = "gray97"),
                         axis.text.x = element_text(size = 8,
                                                    color = "black",
                                                    angle = 0),
                         axis.text.y = element_text(size = 8,
                                                    color = "black",
                                                    angle = 0) )

  if (legend == "FALSE") {
    THEME = THEME +
      ggplot2::theme(legend.position = 'none')
  } else if(legend == "bot"){
    THEME = THEME +
      ggplot2::theme(legend.position = 'bottom')
    }else{
    THEME = THEME +
      ggplot2::theme(legend.text.align = 0.5,
                     legend.background = element_rect( fill = "gray97",
                                                       size = 0.5,
                                                       linetype = "solid",
                                                       colour = "#ad1c72"))
  }

  if (supress %in% c("x",
                     "y",
                     "both")){
    if(supress %in% c("x",
                      "y")){
      expr = substitute(paste0("THEME = THEME + ggplot2::theme(axis.text.",
                               V1,
                               " = element_blank(),axis.ticks.",
                               V1,
                               " = element_blank())"),
                        list(V1 = supress))
      chaine = eval(expr)
      expr_chaine = parse(text = chaine)
      eval(expr_chaine)
    }else {
      THEME = THEME +
      ggplot2::theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())
      }
  }else{}


  if(rotate %in% c("both",
                   "x",
                   "y")){

    angle = if (is.numeric(angle) || is.integer(angle)) {
      angle
    } else if (is.character(angle)) {
      stop("angle doit etre numerique")
    }

    if(rotate %in% c("x",
                      "y")){
      expr = substitute(paste0("THEME = THEME + ggplot2::theme(axis.text.",
                               V1,
                               " = element_text(angle = ",
                               V2,
                               "))"),
                        list(V1 = rotate,
                             V2 = angle))
      chaine = eval(expr)
      expr_chaine = parse(text = chaine)
      eval(expr_chaine)
    }else {
      THEME = THEME +
        ggplot2::theme(axis.text.x = element_text(angle = angle),
                       axis.text.y = element_text(angle = angle))
    }
  }else{}

  return(THEME)
}
