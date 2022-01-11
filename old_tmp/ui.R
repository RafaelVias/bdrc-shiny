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
tab1_style <- '#tab1_head {font-size:20px; color:black; ;display:block; }'
tab2_style <- '#tab2_head {font-size:20px; color:black; ;display:block; }'
rc_head_style <- '#rc_head {font-size:20px; color:black; ;display:block; }'
rhat_head_style <- '#rhat_head {font-size:20px; color:black; ;display:block; }'
auto_head_style <- '#auto_head {font-size:20px; color:black; ;display:block; }'
m_item <- ".sidebar-menu li a { font-size: 15px; }"

rmdfiles <- c('Method.Rmd','Instructions.Rmd','Bugs.Rmd')
sapply(rmdfiles, knit, quiet = T)

mdfiles <- gsub('.rmd','.md',tolower(rmdfiles)) 


fluidPage(
    withMathJax(),
    dashboardPage(skin = 'black',
                  
        dashboardHeader(title = span(#span("P(",
                                     #     style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px'),
                                     span("Bayesian",
                                          style = 'font-weight: bold; color: #1F65CC; font-size: 28px'),
                                     span("|",
                                          style = 'font-family: "Times"; color: gray; font-size: 28px'),
                                     span("Discharge Rating Curves",
                                          style = ' font-weight: bold; color: #4AA4DE; font-size: 28px')#,#1a3263
                                     #span(")",
                                     #     style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px')
                                     ),
                        titleWidth = 501,
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
            menuItem("Rating Curve Builder", tabName = "app", icon = icon("water")),
            menuItem("How to use the app?", tabName = "instructions", icon = icon("life-ring")),
            menuItem("Background", tabName = "about", icon = icon("book")),
            menuItem("Report a Bug", tabName = "bugs", icon = icon("bug"))
            )
        ),
        dashboardBody(tags$style(js),
                      tags$style(js_box),
                      tags$style(js_button),
                      tags$style(js_background),
                      tags$style(tab1_style),
                      tags$style(tab2_style),
                      tags$style(m_item),
                      tags$style(rhat_head_style),
                      tags$style(auto_head_style),
                      tags$style(rc_head_style),
            tabItems(
                
                tabItem(tabName="app",
                    fluidRow(
                        column(width=8, 
                               tabBox(
                                   id = "tabset1",width=NULL,
                                   
                                    
                                    tabPanel('Figures',
                                             textOutput('debug'),
                                             h4(textOutput("rc_head")), 
                                             plotOutput('rc_fig',click ='rc_fig_click',dblclick = dblclickOpts(id = 'rc_fig_dblclick'),
                                                        brush = brushOpts(id = 'rc_fig_brush',resetOnNew = TRUE)),
                                             plotOutput('rc_panel')),
                                    tabPanel('Tables',
                                             h4(textOutput("tab1_head")), 
                                             uiOutput('param_sum'),
                                             h4(textOutput("tab2_head")),
                                             plotOutput('rc_table')),
                                    tabPanel('Convergence diagnostics',
                                             h4(textOutput("rhat_head")),
                                             plotOutput('conv_diag1'),
                                             h4(textOutput("auto_head")),
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
                                radioButtons(inputId="checkbox2", label="Rating Curve Type",choices=list("Generalized Power-law"='gen',"Power-law"='trad'),selected="gen"),
                                radioButtons(inputId="checkbox3", label="Residual variance",choices=list("Stage varying" = 'vary',"Constant" = 'const'),selected='vary'),
                                checkboxInput("advanced", label=span('Advanced Settings',style='font-weight: bold;'), value=FALSE),
                                conditionalPanel(condition="input.advanced == true", 
                                                 radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                                 sliderInput("includeDates", label = "Date Range", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")),
                                                             value=c(1950,as.numeric(format(Sys.Date(), "%Y"))),sep=""),
                                                 checkboxInput("exclude", label=span("Exclude certain period",style='font-weight: bold;'), value=FALSE),
                                                 conditionalPanel(condition="input.exclude == true",
                                                                  dateRangeInput("excludeDates", label = "Date Range",start=Sys.Date()-1,end=Sys.Date()-1)),
                                                 textInput("Wmax",label="Maximum Stage (m)"),
                                                 textInput("c_parameter",label="Stage of zero discharge (c)"
                                                           #,placeholder = 'Optional'
                                                           ),
                                                 actionButton('reset',label='Reset'),
                                ),
                                br(),
                                actionButton("go", label="Create Rating Curve"),
                                br(),br(),br(),
                                #textInput("name","Name of River (optional)"),
                                downloadButton('downloadReport',label='Download Report')
                            )
                        )
                    )
                ),
                
                tabItem(tabName="about",
                        #includeMarkdown("About.md")
                        includeMarkdown("method.md")
                ),
                tabItem(tabName="instructions",
                        includeMarkdown("instructions.md")
                ),
                tabItem(tabName="bugs",
                        includeMarkdown("bugs.md")
                )
            )    
        )
    )
)


