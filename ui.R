suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(shinyWidgets))




js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #1a3263;
}"'
js_box <- '.box.box-primary {
    border-top-color: #1a3263;
}"'
js_button <- 'a.btn {
    color: #000000;
}"'
js_background <- ".content {background-color: #FFFFFF;}"

fluidPage(
    dashboardPage(skin = 'black',
                  
        dashboardHeader(title = span(#span("P(",
                                     #     style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px'),
                                     span("Bayesian",
                                          style = 'font-family: "Georgia"; color: #1a3263; font-size: 28px'),
                                     span("|",
                                          style = 'font-family: "Times"; color: gray; font-size: 28px'),
                                     span("Rating Curves",
                                          style = 'font-family: "Georgia"; color: #913535; font-size: 28px')#,
                                     #span(")",
                                     #     style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px')
                                     ),
                        titleWidth = 335,
                        tags$li(a(href = 'https://github.com/sor16/bdrc',
                                  icon("github"),
                                  title = "Back to Apps Home"),
                                class = "dropdown"),
                        tags$li(a(href = 'https://sor16.github.io/bdrc/index.html',
                                  tags$img(src = 'logo.png',
                                           title = "Company Home", 
                                           height = "30px"),
                                  style = "padding-top:10px; padding-bottom:10px;"),
                                class = "dropdown")),
        dashboardSidebar(
            sidebarMenu(
            menuItem("Rating Curve Builder", icon = icon("water"), tabName = "app"),
            #menuItem("Optimized Rating Curve", tabName = "tournament", icon = icon("gamepad")),
            menuItem("Instructions", tabName = "instructions", icon = icon("life-ring")),
            menuItem("About Method", tabName = "about", icon = icon("book"))
            )
        ),
        dashboardBody(tags$style(js),tags$style(js_box),tags$style(js_button),tags$style(js_background),
           
            tabItems(
                
                tabItem(tabName="app",
                    fluidRow(
                        column(width=8, 
                               tabBox(
                                   id = "tabset1",width=NULL,
                                   
                                    
                                    tabPanel('Figures',
                                             #textOutput('debug'),
                                             plotOutput('rc_fig'),
                                             plotOutput('rc_panel')),
                                    tabPanel('Tables',
                                             #h4(textOutput("tab_header")), 
                                             plotOutput('param_sum'),
                                             plotOutput('rc_table')),
                                    tabPanel('Convergence diagnostics',
                                             plotOutput('conv_diag1'),
                                             plotOutput('conv_diag2'))),
                
                                tagList(
                                    tags$head(
                                        tags$link(rel="stylesheet", type="text/css",href="style.css"),
                                        tags$script(type="text/javascript", src = "busy.js")
                                    )
                                ),
                                br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               div(class = "busy",
                                   img(src="progress.gif") 
                               )
                            
                            
                        ),
                        column(width=4,
                               
                            box(status="primary", width = NULL,
                                title = "Controls",
                                tags$a(href = 'exceldata.xlsx', class = "btn", icon("download"), 'Download xlsx test file'),
                                br(),
                                br(),
                                fileInput('file', 'Upload Excel File',
                                          accept=c('application/vnd.ms-excel',
                                                   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                   '.xls',
                                                   '.xlsx')),
                                #checkboxGroupInput("checkbox", label = "Output",
                                #                    choices=list("Rating curve"="rc","Rating curve transformed"="rc_tr",
                                #                    "Residuals"="res","Power law exponent"="f_h","Standard deviation (data level)" = "sigma_eps") ,selected = "raun"),
                                radioButtons(inputId="checkbox2", label="Rating Curve Type",choices=list("Generalized Power-law"='gen',"Power-law"='trad'),selected="gen"),
                                radioButtons(inputId="checkbox3", label="Residual variance",choices=list("Stage varying" = 'vary',"Constant" = 'const'),selected='vary'),
                                textInput("c_parameter",label="Stage of zero discharge (c)",placeholder = 'Optional'),
                                actionButton("go", label="Create Rating Curve"),
                                br(),br(),br(),
                                #textInput("name","Name of River (optional)"),
                                downloadButton('downloadImages',label='Download Report')
                            )
                        )
                    )
                ),
                
             
                tabItem(tabName="about",
                        includeMarkdown("About.md")
                ),
                tabItem(tabName="tournament",
                        fluidRow(
                          
                          
                          column(width = 12, 
                             
                                 box(title = "Tournament",status = 'primary',
                                     
                              
                                fileInput('file2', '',
                                          accept=c('application/vnd.ms-excel',
                                                   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                   '.xls',
                                                   '.xlsx')),
                                actionButton('tournament_btn',label='Run tournament',icon("paper-plane"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                br(),
                                 div(class = "busy",
                                     img(src="progress.gif")
                                 ))
                        
                        )
                        )
                         
                )
    
            )    
        )
    )
)


