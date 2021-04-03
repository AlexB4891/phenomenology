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

  ggplot2::update_geom_defaults("text",
                       list(colour = "#e9be54",
                            fontface = "bold"))

  ggplot2::update_geom_defaults("segment",
                       list(colour = "white",size = 1))

  ggplot2::update_geom_defaults("line",
                                list(size = 1))

  colorlist <- list(lt_gray = "#D9D9D9",
                    gray = "#BFBFBF",
                    dk_gray = "#595959")


  ggplot2::theme_bw(base_family = base_family,
                    base_size = base_size) +

    ggplot2::theme(

      # Global elements:
      text = ggplot2::element_text(
        colour = "white",
        size = base_size,
        family = base_family,
        face = "bold"
      ),

      line = ggplot2::element_blank(),

      rect = ggplot2::element_rect(
        fill = colorlist$dk_gray,
        colour = colorlist$lt_gray
      ),

      # Panel

      panel.grid.major = ggplot2::element_line(
        linetype = "solid",
        colour = "white",
        size = 0.75 * PT_TO_MM
      ),

      panel.grid.minor = ggplot2::element_line(
        linetype = "solid",
        colour = "#787878"
      ),

      panel.background = ggplot2::element_rect(
        fill = colorlist$dk_gray,
        colour = colorlist$lt_gray
      ),

      panel.border = ggplot2::element_rect(colour = colorlist$dk_gray),

      # Axis:

      axis.title =  ggplot2::element_text(
        colour = "white",
        size = 12
      ),

      axis.text = ggplot2::element_text(
        colour = "white",
        size = 9
      ),

      axis.line = ggplot2::element_line(
        colour = "white",size = 1.2
      ),

      axis.ticks = ggplot2::element_blank(),

      # Strips (title from facets)

      strip.background = ggplot2::element_rect(
        fill = "5b5b5b",
        colour = "5b5b5b"
      ),

      strip.text = ggplot2::element_text(
        colour = "white",
        size = 9
      ),


      # title = ggplot2::element_text(
      #   face = "plain",
      #   hjust = 0.5
      # ),

      # Plot

      plot.title = ggplot2::element_text(
        size = 20,
        hjust = 0.5
      ),

      plot.subtitle = ggplot2::element_text(
        size = 10,
        margin = ggplot2::margin(0,0,20,0),
        colour = "#e9be54"
      ),

      # Legend:

      legend.text = ggplot2::element_text(
        size = 9,
        colour = "white"
      ),

      # legend.title = ggplot2::element_blank(),

      legend.key =  ggplot2::element_rect(
        colour = "transparent",
        fill = colorlist$dk_gray
        ),

      legend.box.background = ggplot2::element_rect(
        fill = colorlist$lt_gray,
        colour = colorlist$dk_gray
      )
    )
}
