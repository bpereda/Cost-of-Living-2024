# Cargar las bibliotecas necesarias
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(shinythemes)

# Paso 1: Añadir CSS personalizado para el estilo
custom_css <- "
body {
  background-color: #1e1e2f;
}
.sidebar {
  background-color: #2c2c44;
  border-color: #2c2c44;
}
.sidebar h3, .sidebar label, .control-label {
  color: white;  /* Texto blanco */
}
#top_n {
  background-color: #1e1e2f;
  color: white;
  border: 2px solid #ff00ff;  /* Acento fucsia */
}
.control-label {
  color: white;  /* Texto blanco */
}
.plotly .modebar .button, .plotly .modebar .button:hover {
  background-color: #2c2c44;
  color: #00FFFF;  /* Acento aqua */
}
"

# Paso 2: Diseño de la interfaz de usuario (UI) con tema personalizado y CSS
ui <- fluidPage(
  
  # Aplicar el tema oscuro e incluir CSS personalizado
  theme = shinytheme("cyborg"),
  tags$head(tags$style(HTML(custom_css))),
  
  titlePanel(h2("Análisis del Costo de Vida: Tema Oscuro Personalizado", style = "color: white;")),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "display_type", 
        label = h3("Selecciona el Tipo de Gráfico", style = "color: white"),  # Texto blanco
        choices = c("Top N Países por Costo de Vida", 
                    "Top N Países por Índice de Alquiler", 
                    "Top N Países por Costo de Comestibles",
                    "Top N Países por Costo de Restaurantes",
                    "Top N Países por Poder Adquisitivo",
                    "Costo de Vida vs Alquiler (Gráfico de Dispersión)", 
                    "Comestibles vs Restaurantes (Gráfico de Dispersión)",
                    "Poder Adquisitivo vs Costo de Vida (Gráfico de Dispersión)")
      ),
      
      # Mostrar el control deslizante solo para los gráficos "Top N"
      conditionalPanel(
        condition = "input.display_type.includes('Top N')",
        sliderInput(
          inputId = "top_n", 
          label = h3("Selecciona el Top N de Países", style = "color: white"),  # Texto blanco
          min = 5, 
          max = 30, 
          value = 10
        )
      )
    ),
    
    mainPanel(
      # Usar plotlyOutput para los gráficos interactivos
      plotlyOutput("mainPlot")
    )
  )
)

# Paso 3: Lógica del servidor con colores personalizados para los gráficos
server <- function(input, output, session) {
  
  # Cargar el conjunto de datos
  dataset <- reactive({
    read.csv("/Users/belupereda/Downloads/Cost_of_Living_Index_by_Country_2024.csv")
  })
  
  # Generar gráficos estáticos e interactivos basados en el tipo seleccionado
  output$mainPlot <- renderPlotly({
    req(dataset())  # Asegurarse de que los datos están disponibles
    
    data <- dataset()
    
    # Obtener el valor seleccionado por el usuario para el Top N de países
    top_n_value <- input$top_n
    
    # Gráficos de barras estáticos para Top N Países con acentos fucsia/aqua
    if (input$display_type == "Top N Países por Costo de Vida") {
      top_cost_living <- data %>%
        arrange(desc(Cost.of.Living.Index)) %>%
        head(top_n_value)
      
      p <- ggplot(top_cost_living, aes(x = reorder(Country, Cost.of.Living.Index), y = Cost.of.Living.Index)) +
        geom_bar(stat = "identity", fill = "#ff00ff") +  # Barras fucsia
        coord_flip() +
        labs(title = paste("Top", top_n_value, "Países por Costo de Vida"), x = "País", y = "Índice de Costo de Vida") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),  # Texto blanco
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p)
      
    } else if (input$display_type == "Top N Países por Índice de Alquiler") {
      top_rent <- data %>%
        arrange(desc(Rent.Index)) %>%
        head(top_n_value)
      
      p <- ggplot(top_rent, aes(x = reorder(Country, Rent.Index), y = Rent.Index)) +
        geom_bar(stat = "identity", fill = "#00FFFF") +  # Barras aqua
        coord_flip() +
        labs(title = paste("Top", top_n_value, "Países por Índice de Alquiler"), x = "País", y = "Índice de Alquiler") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),  # Texto blanco
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p)
      
    } else if (input$display_type == "Top N Países por Costo de Comestibles") {
      top_groceries <- data %>%
        arrange(desc(Groceries.Index)) %>%
        head(top_n_value)
      
      p <- ggplot(top_groceries, aes(x = reorder(Country, Groceries.Index), y = Groceries.Index)) +
        geom_bar(stat = "identity", fill = "#ff00ff") +  # Barras fucsia
        coord_flip() +
        labs(title = paste("Top", top_n_value, "Países por Costo de Comestibles"), x = "País", y = "Índice de Comestibles") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),  # Texto blanco
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p)
      
    } else if (input$display_type == "Top N Países por Costo de Restaurantes") {
      top_restaurants <- data %>%
        arrange(desc(Restaurant.Price.Index)) %>%
        head(top_n_value)
      
      p <- ggplot(top_restaurants, aes(x = reorder(Country, Restaurant.Price.Index), y = Restaurant.Price.Index)) +
        geom_bar(stat = "identity", fill = "#f39c12") +  # Barras naranjas
        coord_flip() +
        labs(title = paste("Top", top_n_value, "Países por Costo de Restaurantes"), x = "País", y = "Índice de Restaurantes") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),  # Texto blanco
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p)
      
    } else if (input$display_type == "Top N Países por Poder Adquisitivo") {
      top_purchasing_power <- data %>%
        arrange(desc(Local.Purchasing.Power.Index)) %>%
        head(top_n_value)
      
      p <- ggplot(top_purchasing_power, aes(x = reorder(Country, Local.Purchasing.Power.Index), y = Local.Purchasing.Power.Index)) +
        geom_bar(stat = "identity", fill = "#2ecc71") +  # Barras verdes
        coord_flip() +
        labs(title = paste("Top", top_n_value, "Países por Poder Adquisitivo"), x = "País", y = "Índice de Poder Adquisitivo") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),  # Texto blanco
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p)
      
      # Gráficos de dispersión interactivos
    } else if (input$display_type == "Costo de Vida vs Alquiler (Gráfico de Dispersión)") {
      p <- ggplot(data, aes(x = Cost.of.Living.Index, y = Rent.Index, text = Country)) +  # Usar 'text' para mostrar el país al pasar el cursor
        geom_point(size = 3, color = "#ff00ff") +  # Puntos fucsia
        labs(title = "Costo de Vida vs Alquiler", x = "Índice de Costo de Vida", y = "Índice de Alquiler") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p, tooltip = "text")  # Activar el tooltip para mostrar los nombres de los países
      
    } else if (input$display_type == "Comestibles vs Restaurantes (Gráfico de Dispersión)") {
      p <- ggplot(data, aes(x = Groceries.Index, y = Restaurant.Price.Index, text = Country)) +  # Usar 'text' para tooltip
        geom_point(size = 3, color = "#f39c12") +  # Puntos naranjas
        geom_smooth(method = "lm", color = "#ff00ff") +  # Línea de regresión fucsia
        labs(title = "Comestibles vs Restaurantes", x = "Índice de Comestibles", y = "Índice de Restaurantes") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p, tooltip = "text")  # Activar el tooltip para mostrar los nombres de los países
      
    } else if (input$display_type == "Poder Adquisitivo vs Costo de Vida (Gráfico de Dispersión)") {
      p <- ggplot(data, aes(x = Cost.of.Living.Index, y = Local.Purchasing.Power.Index, text = Country)) +  # Usar 'text' para tooltip
        geom_point(size = 3, color = "#2ecc71") +  # Puntos verdes
        labs(title = "Poder Adquisitivo vs Costo de Vida", x = "Índice de Costo de Vida", y = "Índice de Poder Adquisitivo") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "white"),
          axis.title = element_text(size = 16, color = "white"),
          axis.text = element_text(size = 14, color = "white"),
          panel.background = element_rect(fill = "#1e1e2f"),
          plot.background = element_rect(fill = "#1e1e2f"),
          panel.grid.major = element_line(color = "#2c2c44")
        )
      ggplotly(p, tooltip = "text")  # Activar el tooltip para mostrar los nombres de los países
    }
  })
}

# Paso 4: Ejecutar la aplicación
shinyApp(ui = ui, server = server)

                                    