
library(tidyverse)
library(shiny)

ggplot2::theme_set(theme_bw(base_size = 20))

N <- 100000

set.seed(123)

poblacion <- rnorm(N, 0, 2)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Simulación de intervalos de confianza"),
  
  h3("Steven García Goñi"),
  h5("Los datos provienen de una población normal con media 0 y varianza 4 N(0, 4)"),
  h5("Se grafican 100 intervalos de confianza construidos a partir de 100 muestras de tamaño N"),
  h5("El nivel de confianza y el tamaño de muestra pueden ser variados en la aplicación"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      "Cambie los siguientes valores", 
      position = "right",
      sliderInput("conf",
                  "Nivel de confianza:",
                  min = 0.5,
                  max = 0.99,
                  value = 0.95, 
                  step = 0.01), 
      
      sliderInput("muestra",
                  "Tamaño de muestra (N):",
                  min = 10,
                  max = 200,
                  value = 30, 
                  step = 10), 
      
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("IC_plot")
    )
  )
)


server <- function(input, output) {
  
  output$IC_plot <- renderPlot({
    
    
    funcion_intervalos <- function(x, n){
      
      estadistico <- t.test(sample(poblacion, size = input$muestra), 
                            mu = 0,
                            conf.level = input$conf)
      
      IC_inf <- estadistico$conf.int[1]
      
      IC_sup <- estadistico$conf.int[2]
      
      data.frame(x = IC_inf, xend = IC_sup)
    }
    
    cantidad_ic <- 100
    
    set.seed(123)
    
    ejemplo <- list(c(1:cantidad_ic), 10) %>% 
      purrr::pmap_dfr(funcion_intervalos) %>% 
      dplyr::mutate(y = 1:cantidad_ic,
                    yend = 1:cantidad_ic,
                    cero = if_else(x<0, "No rechaza", "Rechaza"))
    
    ejemplo %>% 
      ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = cero)) +
      geom_segment(size = 1.0) +
      geom_vline(xintercept = 0, color = "darkred", size = 1) +
      labs(title = "Intervalos de confianza",
           y = "Número de intervalos",
           x = "Intervalo de confianza",
           color = "Hipótesis nula") +
      # scale_x_continuous(limits = c(-1, 3)) +
      scale_color_manual(breaks = c("No rechaza", "Rechaza"),
                         values = c("green", "blue")) +
      coord_flip() +
      theme(legend.position = "bottom")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)