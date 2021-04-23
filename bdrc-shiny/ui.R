suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(googleVis))

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #000000;
}"'
js_box <- '.box.box-primary {
    border-top-color: #000000;
}"'

js_button <- 'a.btn {
    color: #000000;
}"'

###00A65A

dashboardPage(skin = 'black',
            
              
              
    dashboardHeader(title='Bayesian Rating Curve', 
                    tags$li(a(href = 'https://github.com/sor16/bdrc',
                                                            icon("github"),
                                                            title = "Back to Apps Home"),
                                                          class = "dropdown"),tags$li(a(href = '',
                                                                                        tags$img(src = 'logo.png',
                                                                                           title = "Company Home", height = "30px"),
                                                                                       style = "padding-top:10px; padding-bottom:10px;"),
                                                                                     class = "dropdown")),
    dashboardSidebar(
        sidebarMenu(
        menuItem("App", icon = icon("line-chart"), tabName = "app"),
        menuItem("About", tabName = "about", icon = icon("info-circle")),
        menuItem("Tournament", tabName = "tournament", icon = icon("gamepad"))
        )
    ),
    dashboardBody(tags$style(js),tags$style(js_box),tags$style(js_button),
       
        tabItems(
            
            tabItem(tabName="app",
                fluidRow(
                    column(width=8, 
                           tabBox(
                               id = "tabset1",width=NULL,
                              
                               # h4("Data fitted"),
                               # htmlOutput('TableOfData1'),
                               # h4("Fit for unobserved stages"),
                               # htmlOutput('FitTable1'),
                               # h4("95% posterior predictive lower for unobserved stages"),
                               # htmlOutput('LowerTable1'),
                               # h4("95% posterior predictive upper for unobserved stages"),
                               # htmlOutput("UpperTable1")
                               
                               
                                tabPanel('plm0',uiOutput('plots2')),
                               tabPanel('plm',uiOutput('plots1')),
                                tabPanel('gplm0',uiOutput('plots3')),
                                tabPanel('gplm',uiOutput('plots4'))
                                         # h4("Data fitted"),
                                         # htmlOutput('TableOfData2'),
                                         # h4("Fit for unobserved stages"),
                                         # htmlOutput('FitTable2'),
                                         # h4("95% posterior predictive lower for unobserved stages"),
                                         # htmlOutput('LowerTable2'),
                                         # h4("95% posterior predictive upper for unobserved stages"),
                                         # htmlOutput('UpperTable2')
                                         
                                
                            ),
                          
            
                            tagList(
                                tags$head(
                                    tags$link(rel="stylesheet", type="text/css",href="style.css"),
                                    tags$script(type="text/javascript", src = "busy.js")
                                )
                            ),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                           div(class = "busy",
                               img(src="progress.gif") #,height="90%",align='left'
                           )
                        
                        
                    ),
                    column(width=4,#style = "color:#4d3a7d;",
                           
                           
                        box(status="primary", width = NULL,
                            #background="green",
                            title = "Controls",
                            #tags$a(href = 'V316.txt', target="_blank",class = "btn", icon("download"), 'Download txt test file'),
                            #br(),
                            tags$a(href = 'exceldata.xlsx', class = "btn", icon("download"), 'Download xlsx test file'),
                            br(),
                            br(),
                            #selectInput("select", label = "Choose Country", choices = list("Iceland" = 'Iceland'), 
                            #            selected = 'Iceland'),
                            textInput("name","Name of River"),
                            fileInput('file', 'Choose Excel File',
                            accept=c('application/vnd.ms-excel',
                                               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                               '.xls',
                                               '.xlsx')),
                            checkboxGroupInput("checkbox", label = "Output",
                                                choices=list("Rating curve"="rc","Rating curve transformed"="rc_tr",
                                                "Residuals"="res","Power law exponent"="f_h","Standard deviation (data level)" = "sigma_eps") ,selected = "raun"),
                            #checkboxInput("show_c", label="Set stage of zero discharge (c)", value=FALSE),
                             #conditionalPanel(condition="input.show_c == true", 
                                             textInput("c_parameter",label="Stage of zero discharge (c)"),
                            #),
                          #  checkboxInput("advanced", label="Advanced Settings", value=FALSE),
                          # conditionalPanel(condition="input.advanced == true", 
                          #                   radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                             
                          #                 textInput("Wmax",label="Maximum Stage (m)")
                          #                  
                          # ),
                            checkboxGroupInput("checkbox2", label = "Models",choices=list("Run plm0"='plm0',"Run plm"='plm', "Run gplm0" = 'gplm0', "Run gplm" = 'gplm'),selected=''),
                            actionButton('reset',label='Reset'),
                            actionButton("go", label="Submit"),
#                             actionButton("back", label="Back"),
#                             actionButton("forward", label="Forward"),
                            br(),br(),br(),
                            #downloadButton('downloadReport',label="Download Report"),
                            #br(),br(),
                            downloadButton('downloadImages',label='Download Images'),
                            br(),br(),
                            downloadButton('xlsxexport',label='Export Tables as xlsx')
                        )
                    )
                )
            ),
            
         
            tabItem(tabName="about",
                    includeMarkdown("About.md")
            ),
            tabItem(tabName="tournament",
                    h1("Tournament"), 
                    fluidRow(
                      #tags$head(
                      #  tags$style(HTML('#tournament_btn{background-color:blue}'))
                      #),
                      
                      column(width = 12, 
                             #box(width = NULL,
                               #checkboxGroupInput("checkbox_tournament", label = "Select models for tournament",choices=list("Run bplm"='bplm',"Run bplm0"='bplm0', "Run bgplm0" = 'bgplm0', "Run bgplm" = 'bgplm'),selected=''),
                              
                             #box(width = NULL,  title = "Tournament outcome",
                            # h2("Tournament output"), 
                            br(),
                            h4("Choose excel file and run tournament"),
                          
                            fileInput('file2', '',
                                      accept=c('application/vnd.ms-excel',
                                               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                               '.xls',
                                               '.xlsx')),
                            actionButton('tournament_btn',label='Run tournament',icon("paper-plane"), 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            #),
                            br(),br(),
                            # htmlOutput('TableTournament'),
                            # tags$style(type="text/css", "#text {white-space: pre-wrap;}"),
                             div(class = "busy",
                                 img(src="progress.gif") #,height="90%",align='left'
                             #)
                             )
                    
                    )
                    )
                     
            )

        )    
    )
)



