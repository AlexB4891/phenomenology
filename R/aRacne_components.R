rsc3 <- create_market(price_q0 = c(50,10),
                      slope= c(-1,1))

cuve3 <- linear_curve(market = rsc3,
                      market_name = "Market of food")

cuve3$market +  theme_phenomenon()



library(shiny)

ui <- shiny::fluidPage(
  shiny::titlePanel(shiny::HTML("<b>aRacne</b> A microeconomic analysis tool")),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput(
        inputId = "inter_1",
        label = "Demanda intercepto",
        value = 50

      ),
      shiny::numericInput(
        inputId = "slope_1",
        label = "Demanda inclinacion",
        value = 1
      ),
      shiny::numericInput(
        inputId = "inter_2",
        label = "Oferta intercepto",
        value = 10
      ),
      shiny::numericInput(
        inputId = "slope_2",
        label = "Oferta inclinacion",
        value = 1
      )

    ),
    shiny::mainPanel(
      shiny::plotOutput("mercado")
    )



  )
)

server <- function(input, output, session) {

  output$mercado <- shiny::renderPlot({

    rsc3 <- create_market(
      price_q0 = c(
        input$inter_1,
        input$inter_2
      ),
      slope= c(
        -input$slope_1,
        input$slope_2
      )

    )

    cuve3 <- linear_curve(market = rsc3,
                          market_name = "Mercado generico")

    cuve3$market +
      theme_phenomenon()

  })

}

shiny::shinyApp(ui, server)
