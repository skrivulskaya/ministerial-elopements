library(shiny)
library(plotly)
library(DT)
library(crosstalk)

# build the raw data table #
raw.data.tab <- read.csv("raw_data_tab.csv",stringsAsFactors = F)
rdt <- raw.data.tab %>%
  tibble::rownames_to_column()

#build Shiny app
ui <- fluidPage(
  wellPanel(DT::dataTableOutput("x1"),
            fluidRow(
              p(class = 'text-center')
            )) 
) #end fluidPage

server <- function(input, output) {
  
  #output raw data table
  #outputting table with raw data
  d <- SharedData$new(rdt, ~rowname)
  
  # highlight selected rows in the table
  output$x1 <- DT::renderDataTable(
    {rdt2 <- rdt[d$selection(),]
    dt <- DT::datatable(rdt, rownames = FALSE, 
                        options = list(
                        #hide the "rownames" column
                        columnDefs = list(list(visible=FALSE,targets=c(0))),
                        #define default number of rows displayed
                        pageLength = 20, 
                        #select view options for number of rows displayed
                        lengthMenu = c (20, 50, 100, 200, 266)))
    if (NROW(rdt2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "Rowname", target = "row",
                      color = DT::styleEqual(rdt2$rowname, rep("white", length(rdt2$rowname))),
                      backgroundColor = DT::styleEqual(rdt2$rowname, rep("black", length(rdt2$rowname))))  
      }
  })
}

shinyApp(ui, server)
