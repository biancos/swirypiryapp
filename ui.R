#Ui with dashboard
source("./DataDef.R", local = TRUE)

dashboardPage( skin = "blue",
  dashboardHeader(title = "STCS - Data quality report tool" ,titleWidth = 450),
  dashboardSidebar(
   sidebarMenu(
         selectInput("organs","Select a organ:", choices = organs, selected = organs[1]),
         selectInput("forms","Select a form:", choices = forms, selected = forms[1]),
         selectInput("versions","Select a version:", choices = versions, selected = versions[1]),
         selectInput("variables","Variables:","")

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
              fluidRow(
                # h1(headerPanel("STCS - Data quality report tool",windowTitle = "STCS_Report")),
                # tags$style("h1{color: #990000;
                #            font-weight:bold;
                #            font-size: 30px;
                #            font-style: comic-sans;
                #            }"
                #          ),
                
                box( h2(textOutput("titles")))
              ),
              
              
              fluidRow(
                tabBox(
                  title = "Overall view",
                  height = "250px",
                  tabPanel("Chart",  plotOutput("pieplot"),downloadButton('downloadPie', 'Download Pie Chart')),
                  tabPanel("Table", tableOutput("prova"))
                ),

                box(title = "Missing values", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("missplot"),downloadButton('downloadPlot', 'Download Plot'))
               ),
                hr(),
                hr(),
                tags$style("h5{color: black;
               font-style: comic-sans;
               }"
                ),
                h5(print("Data quality report application - Beta Version - Serena Bianco"))
                
                )
    ),
    tags$body(tags$style(HTML('
        .skin-blue .main-body .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-blue .main-body .logo:hover {
                              background-color: #3c8dbc;
                              }
                              ')))
    )
)