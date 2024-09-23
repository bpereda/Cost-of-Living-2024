# Análisis del Costo de Vida 2024 - Aplicación Shiny Interactiva

## Descripción del Proyecto

Aplicación web interactiva desarrollada en R utilizando Shiny para visualizar y analizar datos del costo de vida por país en 2024. La aplicación ofrece múltiples visualizaciones interactivas con un diseño moderno de tema oscuro personalizado.

## Demo en Vivo

**👉 [Ver aplicación en vivo](https://bpereda.shinyapps.io/cost-of-living-2024/)**

La aplicación está desplegada y disponible públicamente en shinyapps.io

## Contexto Académico

**Seminario:** Visualización de datos, Reportes html y web interactivas en R  
**Institución:** Universidad de Montevideo  
**Evento:** Mes Internacional 2024  
**Profesor:** Dr. Ing. Ignacio Cassol  
*Profesor Titular de Bioinformática - Universidad Austral*

**Tipo de trabajo:** Tarea final del seminario  
**Calificación obtenida:** 12/12

## Características Principales

### Visualizaciones Disponibles

La aplicación ofrece 8 tipos diferentes de visualizaciones:

#### Gráficos de Barras (Top N Países)
- **Top N Países por Costo de Vida**: Ranking de países según su índice de costo de vida general
- **Top N Países por Índice de Alquiler**: Comparación de costos de alquiler entre países
- **Top N Países por Costo de Comestibles**: Análisis de los precios de alimentos y productos básicos
- **Top N Países por Costo de Restaurantes**: Evaluación de precios en restaurantes
- **Top N Países por Poder Adquisitivo**: Medición de la capacidad de compra local

#### Gráficos de Dispersión Interactivos
- **Costo de Vida vs Alquiler**: Correlación entre índices de vida general y alquiler
- **Comestibles vs Restaurantes**: Relación entre costos de alimentos y restaurantes (incluye línea de regresión)
- **Poder Adquisitivo vs Costo de Vida**: Análisis de la relación inversa entre costo y poder de compra

### Funcionalidades Interactivas

- **Control deslizante dinámico**: Permite seleccionar entre Top 5 y Top 30 países
- **Tooltips informativos**: Al pasar el cursor sobre puntos en gráficos de dispersión, se muestra el nombre del país
- **Gráficos interactivos**: Zoom, pan y exportación de imágenes mediante Plotly
- **Interfaz responsive**: Diseño adaptable con tema oscuro personalizado

## Tecnologías Utilizadas

### Bibliotecas de R
```r
library(shiny)         # Framework para aplicaciones web interactivas
library(dplyr)         # Manipulación de datos
library(ggplot2)       # Visualizaciones estáticas
library(ggrepel)       # Etiquetas optimizadas en gráficos
library(plotly)        # Gráficos interactivos
library(shinythemes)   # Temas prediseñados para Shiny
```

## Estructura del Proyecto

```
Cost-of-Living-2024/
│
├── entrega.R                      # Aplicación Shiny principal
├── Cost-of-Living-2024.Rproj      # Proyecto de RStudio
└── README.md                      # Documentación
```

## Instalación y Ejecución

### Requisitos Previos
- R (versión 4.0 o superior)
- RStudio (recomendado)

### Instalación de dependencias
```r
install.packages(c("shiny", "dplyr", "ggplot2", "ggrepel", "plotly", "shinythemes"))
```

### Ejecución de la aplicación

**Opción 1: Desde RStudio**
1. Abrir el archivo `entrega.R`
2. Hacer clic en "Run App" en la esquina superior derecha

**Opción 2: Desde la consola de R**
```r
shiny::runApp("entrega.R")
```

## Fuente de Datos

El proyecto utiliza el conjunto de datos **Cost of Living Index by Country 2024** obtenido de Kaggle:

**[Dataset en Kaggle](https://www.kaggle.com/datasets/myrios/cost-of-living-index-by-country-by-number-2024)**

El dataset incluye los siguientes índices por país:
- Cost of Living Index (Índice de Costo de Vida)
- Rent Index (Índice de Alquiler)
- Groceries Index (Índice de Comestibles)
- Restaurant Price Index (Índice de Precios de Restaurantes)
- Local Purchasing Power Index (Índice de Poder Adquisitivo Local)

## Autor

Belén Pereda  
Universidad de Montevideo - 2024

---

*Proyecto desarrollado como trabajo final del seminario de Visualización de datos en R*