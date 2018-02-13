#Ui with dashboard
source("./DataDef.R", local = TRUE)

dashboardPage(
  dashboardHeader(title = "STCS - Data quality report tool"),
  dashboardSidebar(
   sidebarMenu(
         selectInput("organs","Select a organ:", choices = organs, selected = organs[1]),
         selectInput("forms","Select a form:", choices = forms, selected = forms[1]),
         selectInput("versions","Select a version:", choices = versions, selected = versions[1]),
         selectInput("variables","Variables:","")
       #column(2,downloadButton('downloadPlot', 'Download Plot')),
       #column(2,downloadButton('downloadPie', 'Download Pie Chart')),
       #column(2,verbatimTextOutput("current")),
       #column(2,verbatimTextOutput("selected_var")),
       #column(2,verbatimTextOutput("type"))
     
   ),
    menuItem("Misssing values", tabName = "missingvalues", icon = icon("dashboard")),
    menuItem("Other", tabName = "other", icon = icon("th"))
   ),
  dashboardBody(
    tabItems(
      tabItem(tabName="missingvalues",
              fluidPage(
                # h1(headerPanel("STCS - Data quality report tool",windowTitle = "STCS_Report")),
                # tags$style("h1{color: #990000;
                #            font-weight:bold;
                #            font-size: 30px;
                #            font-style: comic-sans;
                #            }"
                #          ),
                
                mainPanel(
                  h2(textOutput("titles")),
                  plotOutput("pieplot"),
                  plotOutput("missplot"),
                  tableOutput("prova"),
                  # textInput("comment","Your comment",value="write here"),
                  # verbatimTextOutput("usercomment"),
                  width = 12,
                  tags$style(type="text/css",
                             
                             ".shiny-output-error { visibility: hidden; }",
                             
                             ".shiny-output-error:before { visibility: hidden; }"
                             
                  ) 
                ),
                hr(),
                hr(),
                tags$style("h5{color: blue;
               font-style: comic-sans;
               }"
                ),
                h5(print("Data quality report application - Beta Version - Serena Bianco"))
                
                )
              )
    )
  )
)