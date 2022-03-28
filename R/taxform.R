
#' Title
#'
#' @param tabla
#' @param x_var
#' @param y_var
#' @param x_title
#' @param y_title
#' @param fill_var
#' @param title
#' @param descripcion
#'
#' @return
#' @export
#'
#' @examples
taxform_barras_100 <- function(tabla = tibble(),
                               x_var = character(),
                               y_var = character(),
                               x_title = "",
                               y_title = "",
                               fill_var = character(),
                               title = character(),
                               descripcion = character(),
                               paleta = "default"){

  if(is.factor(tabla[[fill_var]])){

    n_cat <- levels(tabla[[fill_var]])

  }else if(is.character(tabla[[fill_var]])){

    tabla[[fill_var]] <- factor(tabla[[fill_var]])

    n_cat <- levels(tabla[[fill_var]])
  }else{

    stop("Check your fill variable")

  }

  palette <- paletas_color(paleta,ncolor = length(n_cat))


  plot <- tabla %>%
    ggplot2::ggplot(mapping = aes_string(x=x_var,
                                         y = y_var,
                                         fill= fill_var))+
    ggplot2::geom_bar(stat = "identity",
                      position="fill")+
    ggplot2::labs(title = title,
                  subtitle = descripcion,
                  x = x_title,
                  y = y_title,
                  fill= "Categoría")+
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = palette)+
    ggplot2::theme_light()

  return(plot)

}





#' Title
#'
#' @param tabla
#' @param group_var
#' @param time_var
#' @param percent_var
#' @param paleta
#'
#' @return
#' @export
#'
#' @examples
taxform_smoothline <- function(tabla,
                               group_var,
                               # numeric_var,
                               time_var,
                               percent_var,
                               paleta,text_size = 30) {


  if(is.factor(tabla[[group_var]])){

    n_cat <- levels(tabla[[group_var]])

  }else if(is.character(tabla[[group_var]])){

    tabla[[group_var]] <- factor(tabla[[group_var]])

    n_cat <- levels(tabla[[group_var]])
  }else{

    stop("Check your fill variable")

  }

  custom_pal <- paletas_color(paleta,ncolor = length(n_cat))

  group_var <- dplyr::sym(group_var)

  time_var <- dplyr::sym(time_var)
  percent_var <- dplyr::sym(percent_var)

  plot <- tabla %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::mutate(
           !!group_var := stringr::str_to_sentence(!!group_var)) %>%
    # dplyr::filter(stringr::str_detect(group_var,"Total",negate = T)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = !!time_var,y = !!percent_var,color = !!group_var))+
    ggplot2::geom_point() +
    ggalt::geom_xspline() +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0))+
    ggplot2::scale_color_manual(values = custom_pal)+
    ggplot2::scale_y_continuous(labels = scales::percent) +
    # labs(title = "Evolución de las remesas por area de residencia:")+
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(title = ggplot2::element_text(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          # axis.text.y = element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "top",
          legend.title = ggplot2::element_blank(),
          legend.margin = ggplot2::margin(0,2,0,2,"cm"),
          legend.spacing.x = ggplot2::unit(0.5,"cm"),
          panel.grid.major = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          # plot.title = element_text(size = 20),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )

  return(plot)

}
