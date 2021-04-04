#' Title
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_phenomenon <- function(base_size = 9,
                             base_family = "sans") {

  PT_TO_MM <- 0.352778

  colorlist <- list(background = "#1b2021",
                    text = "#cfcec8",
                    axis = "#c9c4c4",
                    grid_prim = "#918f8f",
                    grid_secun = "#c7c6c6",
                    title = "#cfcec8",
                    subtitle = "#f5f6f1" ,
                    strip_back = "#3b3c39",
                    strip_border = "#999a94"
                    )

  ggplot2::update_geom_defaults("text",
                       list(colour = colorlist$subtitle,
                            fontface = "bold"))

  ggplot2::update_geom_defaults("segment",
                       list(colour = "white",size = 1))

  ggplot2::update_geom_defaults("line",
                                list(size = 1))



  ggplot2::theme_bw(base_family = base_family,
                    base_size = base_size) +

    ggplot2::theme(

      # Global elements:
      text = ggplot2::element_text(
        colour = colorlist$text,
        size = base_size,
        family = base_family,
        face = "bold"
      ),

      line = ggplot2::element_blank(),

      rect = ggplot2::element_rect(
        fill = colorlist$background,
        colour = colorlist$background
      ),

      # Panel

      panel.grid.major = ggplot2::element_line(
        linetype = "solid",
        colour = colorlist$grid_prim,
        size = 0.75 * PT_TO_MM
      ),

      panel.grid.minor = ggplot2::element_line(
        linetype = "solid",
        colour = colorlist$grid_secun
      ),

      panel.background = ggplot2::element_rect(
        fill = colorlist$background,
        colour = colorlist$background
      ),

      panel.border = ggplot2::element_rect(
        colour = colorlist$background
        ),

      # Axis:

      axis.title =  ggplot2::element_text(
        colour = colorlist$text,
        size = 12
      ),

      axis.text = ggplot2::element_text(
        colour = colorlist$text,
        size = 9
      ),

      axis.line = ggplot2::element_line(
        colour = colorlist$axis,
        size = 1.2
      ),

      axis.ticks = ggplot2::element_blank(),

      # Strips (title from facets)

      strip.background = ggplot2::element_rect(
        fill = colorlist$strip_back,
        colour = colorlist$strip_border
      ),

      strip.text = ggplot2::element_text(
        colour = colorlist$text,
        size = 9
      ),


      # Plot
      title = ggplot2::element_text(
        size = 48,
        hjust = 1
      ),

      plot.title = ggplot2::element_text(
        size = 48,
        hjust = 1
      ),

      plot.subtitle = ggplot2::element_text(
        size = 10,
        margin = ggplot2::margin(0,0,20,0),
        colour = colorlist$subtitle
      ),

      # Legend:

      legend.text = ggplot2::element_text(
        size = 9,
        colour = colorlist$text
      ),

      # legend.title = ggplot2::element_blank(),

      legend.key =  ggplot2::element_rect(
        colour = "transparent",
        fill = colorlist$background
        ),

      legend.box.background = ggplot2::element_rect(
        fill = colorlist$background,
        colour = colorlist$background
      )
    )
}
