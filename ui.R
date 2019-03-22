library(colourpicker)
library(shinydashboard)
library(DT)
library(shiny)
library(plotly)
library(shinyWidgets)
library(shinyjqui)
library(formattable)







dashboardPage(
  dashboardHeader(title = "Volcano Plot"),
  dashboardSidebar(sidebarMenu(
    menuItem("File input", tabName = "input", icon = icon("cloud-upload"),
             
             fileInput('file1', 'Choose CSV or TXT File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
             fileInput('file2', 'Choose CSV or TXT File for gene highlight',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
             radioButtons('sep', 'Separator',c(Tab='\t',Comma=','),selected='\t')
    ),
    

    menuItem("Thresholds", tabName = "Thresholds", icon = icon("bar-chart"),
             textInput("hl", "P-Value Threshold:", value = "0.05", width = NULL),
             textInput("vl", "fold-change Threshold:", value = "3", width = NULL)
    ),
    menuItem("example", tabName = "example", icon = icon("table"),
             downloadButton("example", label="example data frame", class = "butt2"),
             br(),
             downloadButton("example.highlight", label="example highlight list", class = "butt2"),
             
             # making the font italics this time
             tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"))
             
    ),

    menuItem("downloads", tabName = "downloads", icon = icon("cloud-download"),
             downloadButton("download_Data", label="Download filtered list", class = "butt2"),
             br(),
             downloadButton("download_Plot", label="Download Volcano Plot (PDF)", class = "butt2"),
             
             # making the font italics this time
             tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"))
             
    ),
    h5("version 1.4.1") 
    
    
 
    
#h6("Note: The input file should be a ASCII text file (comma, tab, semicolon separated),containing three columns named ID, logFC and P.Value, respectivelly.")
  )),
  

  
  
  dashboardBody(# Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #080A0D;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #080A0D;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #080A0D;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #080A0D;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #080A0D;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #080A0D;
                              color: #FFFFFF;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #616262;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #080A0D;
                              }
                              '))),
    fluidRow(
      tabBox(width = 8,
        title = "Volcano Plot",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "900px",
                            tabPanel("volcano plot", 
                                     dropdownButton( #add menu directly to plot (shinyWidgets)
                                            tags$h3("List of Input"),
                                            checkboxInput("gene_names", "Show names", value = FALSE),
                                            checkboxInput("p.scale", "max. scaling of p-value", value = FALSE), tags$hr(),
                                            colourpicker::colourInput("col1", "Select colour DOWN:", "dodgerblue3",palette = "square", returnName = TRUE,showColour = c("background")),
                                            colourpicker::colourInput("col2", "Select colour UP:", "orangered3",palette = "square", returnName = TRUE,showColour = c("background")),
                                            colourpicker::colourInput("colhigh", "Select colour for highlight:", "black",palette = "square", returnName = TRUE,showColour = c("background")),
                                            sliderInput("text.size", "text size:", 0.1, 10, value = 4, step=0.1),
                                            sliderInput("cex.size", "highlight size:",  3, 10, value = 4, step=1),
                                            sliderInput("theme.cex", "basic theme text size:", 18, 60, value = 22, step=1),
                                            tags$hr(),
                                            sliderInput("lo", "-Log10(P-Value):",  0, 500, value = 6, step=1),
                                            sliderInput("lfcr", "Log2(Fold-Change) Range:", -10, 10, value = c(-4, 4), step=0.1, animate=FALSE),
                                            circle = TRUE, status = "danger", icon = icon("wrench"), width = "300px",
                                            tooltip = tooltipOptions(title = "Click to open plot adjustments !")
                                          ),
                                     jqui_resizabled(plotOutput("ggplot",width = 900,height = 900))
                            ),
                            tabPanel("table", DT::dataTableOutput("table_Out"))
      )
    
      )
    )
  )

    




#version: 1
# release of the app
#version: 1.1
# add gene highlighting with black dot in background

#version: 1.2
# add colour picker
# add automatic max p-value scaling check box
# add marked points in the examples !!! g08 etc. should never be in real data a gene or protein name
# add theme size slidebar
# transform UI to dashboard 

#version: 1.3
# add euclidean distance (ED) calculation for ranking genes/proteins
# add ED and ranking to table
# add table to app

#version: 1.4
# do shiny dashboard sideMenu implementation 
# !!! bug in download button ;; app duplication
#version: 1.4.1
# debugged in download button ;; app duplication // solution was update of shinydashboard;; refresh bug
# add shinyWidgets
# add shinyjqui