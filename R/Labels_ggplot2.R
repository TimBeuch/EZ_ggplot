#' label for ggplot
#'
#' @param plot a ggplot object
#' @param h1_title main title
#' @param y_title titre ordonnée
#' @param x_title titre abscisse
#' @param leg_title est un moyen facile d'associer un titre sans avoir à spécifier pour quel type de donnée il s'applique
#' @param leg_couleur est un moyen d'associer un titre pour les couleurs (color/colour)
#' @param leg_forme est un moyen d'associer un titre pour les formes (shape)
#' @param leg_remplissage est un moyen d'associer un titre pour les contenants (fill)
#' @param source est un moyen d'associer un texte en bas du graphique
#' @param champs est un moyen d'associer un texte en bas du graphique
#' @param subtitle sous-titre
#' @param ... other param
#' @import stringr
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @importFrom tidyr gather
#' @importFrom stats na.omit
#'
#' @return a ggplot object
#' @export
EZ_label <- function(plot,
                     h1_title = NULL,
                     y_title = NULL,
                     x_title = NULL,
                     leg_title = NULL,
                     leg_couleur = NULL,
                     leg_forme = NULL,
                     leg_taille = NULL,
                     leg_remplissage = NULL,
                     source = NULL,
                     champs = NULL,
                     subtitle = NULL,
                     ...){
#LEGEND TITLE
  liste_legende = list(leg_couleur,
                       leg_forme,
                       leg_remplissage,
                       leg_taille)

# CAPTION
if (!is.null(source) == TRUE){
  source = paste0("Source : ",
                  source)
}else {source = ""}


  if (!is.null(champs) == TRUE){
  champs = paste0("Champs : ",
                  champs)
}else{champs = ""}

# LAYOUT
if (is.null(h1_title) == TRUE){
  h1_title = suppressWarnings(glue::glue("Plot of {plot$mapping$y[[2]][[2]]} ~ {plot$mapping$x[[2]][[2]]}"))#warning <- use [[]]
  }

if (is.null(y_title) == TRUE){
  y_title = glue::glue("{plot$labels$y}")
  if( !is.na(str_match(y_title,
                       pattern = "\\(|\\)")) ==  TRUE){
    y_title = regmatches(y_title,
                         gregexpr("(?<=\\().*?(?=\\))",
                                  y_title,
                                  perl=T))[[1]]
  }
}

if(is.null(x_title) == TRUE){
  x_title = glue::glue("{plot$labels$x}")
  if( !is.na(str_match(x_title,
                       pattern = "\\(|\\)")) ==  TRUE){
    x_title = regmatches(x_title,
                         gregexpr("(?<=\\().*?(?=\\))",
                                  x_title,
                                  perl=T))[[1]]
  }
}

if(is.null(subtitle) == TRUE){
subtitle = glue::glue("Type : {class(plot$layers[[1]]$geom)[1]}")
}

  #LEGEND
if (is.null(leg_couleur) == TRUE){
  leg_couleur = glue::glue("{plot$labels$colour}")
  if(is_empty(leg_couleur) == TRUE){
    leg_couleur = NA_character_
    } else {
     leg_couleur = regmatches(leg_couleur,
                              gregexpr("(?<=\\().*?(?=\\))",
                                       leg_couleur,
                                       perl=T))[[1]]
     leg_couleur = paste0("Couleur : ",
                          leg_couleur)
      }
}

if (is.null(leg_forme) == TRUE){
  leg_forme = glue::glue("{plot$labels$shape}")
  if(is_empty(leg_forme) == TRUE){
    leg_forme = NA_character_
  }else {
    leg_forme = regmatches(leg_forme,
                           gregexpr("(?<=\\().*?(?=\\))",
                                    leg_forme,
                                    perl=T))[[1]]
    leg_forme = paste0("Forme : ",
                       leg_forme)
  }
  }

if (is.null(leg_remplissage) == TRUE){
  leg_remplissage = glue::glue("{plot$labels$fill}'s fill")
  if(is_empty(leg_remplissage) == TRUE){
    leg_remplissage = NA_character_
  }else {
    leg_remplissage =  regmatches(leg_remplissage,
                                  gregexpr("(?<=\\().*?(?=\\))",
                                           leg_remplissage,
                                           perl=T))[[1]]
    leg_remplissage = paste0("Couleur : ",
                             leg_remplissage)
  }
}

  if (is.null(leg_taille) == TRUE){
    leg_taille = glue::glue("{plot$labels$size}'s fill")
    if(is_empty(leg_taille) == TRUE){
      leg_taille = NA_character_
    }else {
      leg_taille =  regmatches(leg_taille,
                               gregexpr("(?<=\\().*?(?=\\))",
                                        leg_taille,
                                        perl=T))[[1]]
      leg_taille = paste0("Taille : ",
                          leg_taille)
    }
  }

if (!is.null(leg_title) == TRUE &
    is.null(liste_legende[["1"]]) == TRUE){
  leg_couleur = leg_title
}else if(!is.null(leg_title) == TRUE &
         is.null(liste_legende[["2"]]) == TRUE){
  leg_forme = leg_title
} else if (!is.null(leg_title) == TRUE &
           is.null(liste_legende[["3"]]) == TRUE){
  leg_remplissage = leg_title
}else if (!is.null(leg_title) == TRUE &
          is.null(liste_legende[["4"]]) == TRUE) {
  leg_taille = leg_title
}else {leg_title = NULL}


#SET DATAFRAMES
caption = tibble::tribble(~caption,
                          paste(source,
                                champs,
                                sep = "\n"))

legende = tibble::tribble( ~colour, ~shape, ~fill,~size,
                           leg_couleur, leg_forme, leg_remplissage,leg_taille)

layout = tibble::tribble(~title,  ~subtitle, ~y, ~x,
                         h1_title, subtitle, y_title, x_title)

# FUSION
test = cbind(caption,
             legende,
             layout) %>%
  gather(key = "name")

test = stats::na.omit(test)
#gglabels

plot$labels = split(x = test$value,
                    f = test$name)
  return(plot)
}
