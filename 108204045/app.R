## Data Science hw4 Interactive web service of PCA and CA analysis by Shinyapp ####

library(shiny)
library(ggbiplot)
library(factoextra)
library(FactoMineR)
library(magrittr)

## UI ####
ui <- fluidPage(
  titlePanel("NCCU1112DS-hw4 社四葉佐晨 108204045"),
  tabsetPanel(
    tabPanel("IRIS",
        navlistPanel(
          tabPanel("Data", 
                   h3("IRIS Row Data"),
                   tableOutput("data")),
          tabPanel("Summary",
                   h3("Descriptive Statistics of IRIS Data"),
                   verbatimTextOutput("summary"))
        )),
     tabPanel("PCA",
        sidebarPanel(
          selectInput(inputId = "xaxis", label = "X axis",
                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4)),
          
          selectInput(inputId = "yaxis", label = "Y axis", selected = 2,
                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4))
        ),
        mainPanel(
          h3("PCA Plot"),
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
          h3("CA Plot"),
          plotOutput(outputId = "caPlot")
        )),
    tabPanel("Kmeans",
             sidebarPanel(
               sliderInput(inputId = "k", "Cluster", 1, 10, 3)
             ),
             mainPanel(
               h3("Kmeans Cluster Plot"),
               plotOutput(outputId = "kmeansPlot")
             ))
      )
)

## Server ######
server <- function(input, output){
  
  data(iris)
  
  output$data <- renderTable(iris)
  
  output$summary <- renderPrint({
    summary(iris, digit = 3)
  })
  
  output$pcaPlot <- renderPlot({
    
    PCs <- as.numeric(c(input$xaxis, input$yaxis))
    


    log.ir <- log(iris[,1:4])
    ir.species <- iris[,5]
    
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    
    ggbiplot(ir.pca, choices = PCs, obs.scale = 1, var.scale = 1,
             groups = ir.species) +
      scale_color_discrete(name = "") +
      theme(legend.direction = "horizontal", legend.position = "top")
    
  })
  output$caPlot <- renderPlot({
    
    if (input$pltType == "factor map"){
      CA(iris, quali.sup = 5) %>% 
        plot(invisible = "row")
    }else{
      CA(iris, graph = FALSE, quali.sup = 5) %>% 
        fviz_ca(repel = TRUE, col.row = iris$Species, geom.row = "point",
                arrows = c(FALSE, TRUE), palette = palette("default")) 
    }
    
  })

  output$kmeansPlot <- renderPlot({
    
    iris.ca <- CA(iris[-5], graph = FALSE)
    
    iris.ca.coord <- iris.ca$row$coord[, c(1,2)]
    
    clusters <- kmeans(iris.ca.coord, input$k)
    
    plot(iris.ca.coord, col = clusters$cluster, 
         main = "Kmeans Plot with CA")
    points(clusters$centers, pch = 2, lwd = 2, cex = 2)
  })
}

# Run App #####
shinyApp(ui = ui, server = server)









