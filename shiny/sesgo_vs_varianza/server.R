#-------------------------------------------------------#
#   Árboles de Decisión, Bagging y Bosques Aleatorios   #
#   Preparado por:      Kael Huerta y Enrique Bonilla   #
#                                 06 de Julio de 2016   #
#-------------------------------------------------------#

library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

real <- data.table(x = seq(0, 1, .01)) %>%
  mutate(y = sin(2 * pi * x))

shinyServer(function(input, output){
  r <- reactive({
    set.seed(3)
    res <- data.table(x = seq(0, 1, 1 / input$n)) %>%
      mutate(y = sin(2 * pi * x) + rnorm(length(x), 0, 0.5))
    return(res)
  })

  output$plot <- renderPlot({
    pl <- r() %>% ggplot(aes(x = x, y = y))

    ## Simulaciones
    if(input$simul)
      pl <- pl + geom_point(colour = '#56B4E9', size = 2)

    ## Modelo real
    if(input$real)
      pl <- pl + geom_line(data = real,
        aes(x = x, y = y), colour = '#E69F00', alpha = .3, size = 1.5)

    ## Aproximación polinomial
    if(input$polinomio)
      pl <- pl + geom_smooth(method = 'lm', formula = y ~ poly(x, input$k),
        colour = 'salmon', alpha = .1, se = F)

    if(input$simul | input$real | input$polinomio)
      return(pl)
    else
      return(NULL)
  })

  output$table <- renderTable({
    r()
  })

})

