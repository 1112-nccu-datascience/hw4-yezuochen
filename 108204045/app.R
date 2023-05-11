## Data Science hw4 Interactive web service of PCA and CA analysis by Shinyapp ####

library(shiny)
library(ggbiplot)
library(factoextra)
library(FactoMineR)
library(magrittr)

## PCA Plot ####
ui <- fluidPage(
  titlePanel("葉佐晨的HW4-108204045"),
  tabsetPanel(
     tabPanel("PCA",
        sidebarPanel(
          selectInput(inputId = "xaxis", label = "X axis",
                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)),
          
          selectInput(inputId = "yaxis", label = "Y axis",
                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4))
        ),
        mainPanel(
          plotOutput(outputId = "pcaPlot")
        )
      ),
     tabPanel("CA",
        sidebarPanel(
          selectInput(inputId = "pltType", label = "CA Plot Type",
                      colices <- c("CA Factor Map" = "factor map", 
                                   "CA BiPlot" = "biplot"))
          ),
        mainPanel(
          plotOutput(outputId = "caPlot")
        ))
      )
)


server <- function(input, output){
  
  
  output$pcaPlot <- renderPlot({
    
    PCs <- as.numeric(c(input$xaxis, input$yaxis))
    
    data(iris)
    
    log.ir <- log(iris[,1:4])
    ir.species <- iris[,5]
    
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    
    ggbiplot(ir.pca, choices = PCs, obs.scale = 1, var.scale = 1,
             groups = ir.species) +
      scale_color_discrete(name = "") +
      theme(legend.direction = "horizontal", legend.position = "top")
    
  })
  output$caPlot <- renderPlot({
    data(iris)
    
    if (input$pltType == "factor map"){
      CA(iris, quali.sup = 5) %>% 
        plot(invisible = "row")
    }else{
      CA(iris, graph = FALSE, quali.sup = 5) %>% 
        fviz_ca(repel = TRUE, col.row = iris$Species, geom.row = "point",
                arrows = c(FALSE, TRUE), palette = palette("default")) 
    }
    
  })
}

shinyApp(ui = ui, server = server)

## CA Plot ####









