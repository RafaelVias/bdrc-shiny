library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
# library(bdrc)
library(ggplot2)
library(devtools)
library(readxl)
library(grid)
library(gridExtra)
library(xtable)
library(shiny)
library(knitr)
library(writexl)
library(parallel)
library(markdown)

# devtools::install_github("sor16/bdrc",force=T)
devtools::load_all("bdrc")
source('shiny_help_functions.R')

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
m_item <- ".sidebar-menu li a { font-size: 15px; }"

rmdfiles <- c('Method.Rmd','Instructions.Rmd','Bugs.Rmd')
sapply(rmdfiles, knit, quiet = T)


downloadTableUI <- function(id) {
    ns <- NS(id)
    downloadButton(ns("table_download"), label = "Download excel tables")
}

# downloadReportUI <- function(id) {
#     ns <- NS(id)
#     downloadButton(ns("report_download"), label = "Download report")
# }


downloadTable <- function(input, output, session, data) {
    
    output$table_download <- downloadHandler(
        filename = function() {
            paste("bdrc_tables_", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(data(), path=file)
        }
    )
    
}

# downloadReport <- function(input, output, session, page_one, remaining_pages ) {
#     
#     output$report_download <- downloadHandler(
#         filename = function() {
#             paste("bdrc_report_", Sys.Date(), ".pdf", sep="")
#         },
#         content = function(file) {
#             pdf(file=file,paper='a4',width=9,height=11)
#             grid.arrange(page_one(),as.table=TRUE)
#             for(i in 2:length(remaining_pages()) ) grid.arrange(remaining_pages()[[i]],as.table=TRUE)
#             invisible(dev.off())
#         }
#     )
#     
# }

ui <- shinyUI(fluidPage(
        tags$head(tags$link(rel="shortcut icon", href="logo_nbg.png")),
        title='Bayesian Discharge Rating Curves - bdrc',
        useShinyjs(),
        withMathJax(),
        dashboardPage(skin = 'black',
                      dashboardHeader(
                          tags$li(class = "dropdown",
                                  tags$style(".main-header {max-height: 200px}"),
                                  tags$style(".main-header .logo {height: 54px}")
                          ),
                          title = span(
                              span("Bayesian",
                                   style = 'font-weight: bold; color: #1F65CC; font-size: 30px'),
                              span("|",
                                   style = 'font-family: "Times"; color: gray; font-size: 30px'),
                              span("Discharge Rating Curves",
                                   style = ' font-weight: bold; color: #4AA4DE; font-size: 30px')
                      ),
                      titleWidth = 550,
                      tags$li(a(href = 'https://github.com/RafaelVias/bdrc-shiny',
                                tags$img(src = 'github_icon.svg',
                                         title = "The bdrc Shiny app github page", 
                                         height = "24px"),
                                title = "Back to Apps Home"),
                              class = "dropdown"),
                      tags$li(a(href = 'https://sor16.github.io/bdrc/index.html',
                                tags$img(src = 'logo.png',
                                         title = "The bdrc package web page", 
                                         height = "34px"),
                                style = "padding-top:10px; padding-bottom:10px;"),
                              class = "dropdown")),
                      dashboardSidebar(
                          sidebarMenu(
                              menuItem("Rating curve app", tabName = "app", icon = icon("water")),
                              menuItem("Instructions", tabName = "instructions", icon = icon("life-ring")),
                              menuItem("Background", tabName = "about", icon = icon("book")),
                              menuItem("Report a bug", tabName = "bugs", icon = icon("bug"))
                          )
                      ),
                      dashboardBody(tags$style(js),
                                    tags$style(js_box),
                                    tags$style(js_button),
                                    tags$style(js_background),
                                    tabItems(
                                        tabItem(tabName="app",
                                                fluidRow(
                                                    column(width=8, 
                                                           tabBox(
                                                               id = "tabset1",width=NULL,
                                                               tabPanel(span('Figures',style='font-size: 16px;'),
                                                                        textOutput('debug'),
                                                                        h4("Interactive rating curve -",
                                                                            bsButton("ib1", label = "", icon = icon("info"),
                                                                                     style = "primary" ,size="extra-small"),
                                                                            bsPopover(id = "ib1", title = "",
                                                                                      content = paste0('An interactive plot showing the fitted rating curve. The observations are shown as black dots, the estimated rating curve as a solid line and the dotted lines depict the 95% posterior predictive interval. To interact with the rating curve engage the "Advanced settings" option in the "Controls" column.'),
                                                                                      placement = "bottom", 
                                                                                      trigger = "trigger", 
                                                                                      options = list(container = "body")
                                                                            )), 
                                                                        plotOutput('rc_fig',
                                                                                   click ='rc_fig_click',
                                                                                   dblclick = dblclickOpts(id = 'rc_fig_dblclick'),
                                                                                   hover = hoverOpts(id = "rc_hover",delay=100),
                                                                                   brush = brushOpts(id = 'rc_fig_brush',resetOnNew = TRUE)),
                                                                        h4("Rating curve panel plot -",
                                                                           bsButton("ib2", label = "", icon = icon("info"),
                                                                                    style = "primary" ,size="extra-small"),
                                                                           bsPopover(id = "ib2", title = "",
                                                                                     content = paste0('A panel plot containing four figures that summarize the rating curve model resuts. Top-left: The estimated rating curve shown on a log-scale. Top-right: A residual plot showing the difference between the estimated rating curve and the observations, on a log scale. Bottom-right: The estimated standard deviation of the error terms shown as a function of water elevation. Bottom-left: The estimated power-law exponent shown as a function of water elevation. For more details, see the ', 
                                                                                                      a("bdrc", 
                                                                                                        href = "https://sor16.github.io/bdrc/articles/introduction.html"),
                                                                                                      " web page."),
                                                                                     placement = "bottom", 
                                                                                     trigger = "trigger", 
                                                                                     options = list(container = "body")
                                                                           )), 
                                                                        uiOutput("rc_tooltip"),
                                                                        plotOutput('rc_panel')),
                                                               tabPanel(span('Tables',style='font-size: 16px;'),
                                                                        h4("Parameter summary table -",
                                                                           bsButton("ib3", label = "", icon = icon("info"),
                                                                                    style = "primary" ,size="extra-small"),
                                                                           bsPopover(id = "ib3", title = "",
                                                                                     content = paste0("A table showing summary statstics of the estimated model parameters. Shown are the 0.025, 0.500 and 0.975 quantiles of the posterior distributions of each parameter. For more details, see the ", 
                                                                                                      a("bdrc", 
                                                                                                        href = "https://sor16.github.io/bdrc/articles/introduction.html"),
                                                                                                      " web page."),
                                                                                     placement = "bottom", 
                                                                                     trigger = "trigger", 
                                                                                     options = list(container = "body")
                                                                           )), 
                                                                        uiOutput('param_sum_ui',height="auto"),
                                                                        h4("Tabular rating curve -",
                                                                            bsButton("ib4", label = "", icon = icon("info"),
                                                                                     style = "primary" ,size="extra-small"),
                                                                            bsPopover(id = "ib4", title = "",
                                                                                      content = paste0("The fitted rating curve shown in tabular form. The column- and rownames show the water elevation in centimeter [cm] and decimeter [dm] increments, respectively. The table cells show the water discharge in cubic meters per second [m^3/s]."),
                                                                                      placement = "bottom", 
                                                                                      trigger = "trigger", 
                                                                                      options = list(container = "body")
                                                                            )), 
                                                                        #plotOutput('rc_table')
                                                                        uiOutput('rc_table_ui',height="auto")
                                                                        ),
                                                               tabPanel(span('Convergence diagnostics',style='font-size: 16px;'),
                                                                        h4("Gelman-Rubin statistic -",
                                                                           bsButton("ib5", label = "", icon = icon("info"),
                                                                                    style = "primary" ,size="extra-small"),
                                                                           bsPopover(id = "ib5", title = "",
                                                                                     content = paste0('A plot showing the Gelman-Rubin statistic for the posterior samples of the model parameters. This statistic can be used to assess the mixing and convergence of the Markov-Chain Monte-Carlo used to sample from the posterior distributions of the model parameters. If the statistic decreases below the value 1.1 as a the number of iterations increases then that is an inciation that the chains have mixed and converged adequately well. For more details, see the ', 
                                                                                                      a("bdrc", 
                                                                                                        href = "https://sor16.github.io/bdrc/articles/introduction.html"),
                                                                                                      " web page."),
                                                                                     placement = "bottom", 
                                                                                     trigger = "trigger", 
                                                                                     options = list(container = "body")
                                                                           )), 
                                                                        plotOutput('conv_diag1'),
                                                                        h4("Autocorrelation in posterior draws -",
                                                                           bsButton("ib6", label = "", icon = icon("info"),
                                                                                    style = "primary" ,size="extra-small"),
                                                                           bsPopover(id = "ib6", title = "",
                                                                                     content = paste0('A plot showing the autocorrelation of the draws from the posterior distributions of the model parameters. The autocorrelation is a measure of how correlated the posterior draws are. For more details, see the ', 
                                                                                                      a("bdrc", 
                                                                                                        href = "https://sor16.github.io/bdrc/articles/introduction.html"),
                                                                                                      " web page."),
                                                                                     placement = "bottom", 
                                                                                     trigger = "trigger", 
                                                                                     options = list(container = "body")
                                                                           )), 
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
                                                           actionButton(inputId = "hide_controls", label = "Hide controls",icon('eye-slash') ),
                                                           br(),br(),
                                                           box(id = "controls",
                                                               status="primary", width = NULL,
                                                               title = "Controls",
                                                               tags$a(href = 'exceldata.xlsx', class = "btn", icon("download"), 'Download .xlsx test file', style = "border-style: solid; border-color: #D2D2D2;"),
                                                                   br(),
                                                                   br(),
                                                                   fileInput('file', 'Upload excel file (.xlsx)',
                                                                             accept=c('application/vnd.ms-excel',
                                                                                      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                                                      '.xls',
                                                                                      '.xlsx')),
                                                               actionButton("go", 
                                                                            label="Create rating curve",
                                                                            icon("play",style="color: dodgerblue")),
                                                               br(),br(),br(),
                                                               box(radioButtons(inputId="checkbox2", label="Rating curve type",choices=list("Generalized power law"='gen',"Power law"='trad'),selected="gen"),
                                                                   radioButtons(inputId="checkbox3", label="Error variance",choices=list("Varying with water elevation" = 'vary',"Constant" = 'const'),selected='vary'),
                                                                   checkboxInput("tournament", label=span('Select best model',style='font-weight: bold;'), value=FALSE),
                                                                   width = 12),
                                                               br(),
                                                               box(checkboxInput("advanced", label=span('Advanced settings',style='font-weight: bold;'), value=FALSE),
                                                                   conditionalPanel(condition="input.advanced == true", 
                                                                                    radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                                                                    sliderInput("includeDates", label = "Date range included", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")),
                                                                                                value=c(1950,as.numeric(format(Sys.Date(), "%Y"))),sep=""),
                                                                                    checkboxInput("exclude", label=span("Exclude certain period",style='font-weight: bold;'), value=FALSE),
                                                                                    conditionalPanel(condition="input.exclude == true",
                                                                                                     dateRangeInput("excludeDates", label = "Date range",start=Sys.Date()-1,end=Sys.Date()-1)),
                                                                                    textInput("h_max",label="Maximum water elevation (m)"),
                                                                                    textInput("c_parameter",label="Water elevation of zero discharge (m)"
                                                                                    ),
                                                                                    actionButton('reset',
                                                                                                 label='Remove changes',
                                                                                                 icon('eraser',style="color: dodgerblue")),
                                                                                    ),
                                                                   width=12),
                                                               #br(),br(),
                                                               #downloadReportUI(id = 'downloadReport'),
                                                               br(),br(),
                                                               downloadTableUI(id = "xlsxexport"),
                                                           )
                                                    )
                                                )
                                        ),
                                        
                                        tabItem(tabName="about",
                                                includeMarkdown("Method.md")
                                        ),
                                        tabItem(tabName="instructions",
                                                includeMarkdown("Instructions.md")
                                        ),
                                        tabItem(tabName="bugs",
                                                includeMarkdown("Bugs.md")
                                        )
                                    )    
                      )
        )
    )
)



################## SERVER ##################################################

vals <- reactiveValues(keeprows=NULL)
daterange <- reactiveValues(keeprows=NULL)
above_hmax <- reactiveValues(logical=NULL)
param_sum_height <- reactiveValues(height=100)
tabular_rating_curve_height <- reactiveValues(height=100)
ranges <- reactiveValues(x = NULL, y = NULL)
dummy <- reactiveValues(Q=NULL,W=NULL)
force <- reactiveValues(Q=NULL,W=NULL)
exclude_point <- reactiveValues(Q=NULL,W=NULL)
temp_exclude_point <- reactiveValues(Q=NULL,W=NULL)
input_h_max <- reactiveValues(W=NA)

server <- function(input, output, session) {
    
    # hide controls
    observeEvent(input$hide_controls, {
        shinyjs::toggle("controls")
    })
    
    observeEvent(input$go, {
        if(is.null(input$file)){
            message_fun(title="Whoops!",text="Please upload excel file in order to fit a rating curve!")
        }
    })
    
    ## data import ##
    data <- eventReactive(input$go,{
        
        validate(
            need(!is.null(input$file), "")
        )
        
        upd_pts <- update_interactive_points( reactiveValuesToList(input_h_max)$W, reactiveValuesToList(dummy), reactiveValuesToList(force), reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign( i, upd_pts[[i]] ) 
        h_max <- upd_pts$h_max
        
        cleandata=clean(input$file,dummy=dummy,force=force,keeprows=vals$keeprows,
                        advanced=input$advanced,exclude=input$exclude,
                        excludedates=input$excludeDates, includedates=input$includeDates,
                        h_max=as.numeric(input$h_max))
        if(length(vals$keeprows)==0 ){
            vals$keeprows= rep(TRUE,nrow(cleandata$observedData_before))
        }
        years <- as.numeric(format(cleandata$observedData_before$Date, "%Y"))
        includeindex <- years<=input$includeDates[2] & years >= input$includeDates[1]
        excludeindex <- cleandata$observedData_before$Date<=input$excludeDates[1] | cleandata$observedData_before$Date >= input$excludeDates[2]
        daterange$keeprows <- excludeindex & includeindex
        above_hmax$logical <- if(is.na(h_max)) rep(F,nrow(cleandata$observedData_before)) else ( cleandata$observedData_before$W > h_max )
        
        if(nrow(cleandata$wq) < 3){
            message_fun(title="Whoops!",text='There are less than 3 data points. Cannot fit rating curve.')
            stop()
        }
        return(cleandata)
        
    })
    
    observeEvent(input$go,{
    
        validate(
            need(!is.null(input$file), "")
        )
        
        input_h_max$W <- c(as.numeric(input$h_max))
        temp_exclude_point$W <- NULL
        temp_exclude_point$Q <- NULL
        param_sum_height$height <- nrow(rc_model()$param_summary)
        tabular_rating_curve_height$height <- max(ceiling(max(rc_model()$rating_curve$h)*10),input_h_max$W,na.rm=T) - floor(min(rc_model()$rating_curve$h)*10)
    
    })
    
    ## run Models ##
    rc_model <- eventReactive(input$go,{

        validate(
            need(!is.null(input$file), "")
        )
    
        if(input$tournament){
            m <- 'tournament'
        }else{
            m <- paste0(ifelse(input$checkbox2=='gen','gplm','plm'),
                        ifelse(input$checkbox3=='vary','','0'))
        }
        dat <- as.data.frame(data()$wq)
        upd_pts <- update_interactive_points( reactiveValuesToList(input_h_max)$W, reactiveValuesToList(dummy), reactiveValuesToList(force), reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign( i, as.data.frame( upd_pts[[i]] ) )
        h_max <- upd_pts$h_max
        
        if(is.na(h_max)){
            h_max <- max(dat$W,data()$observedData_before$W,dummy$W,force$W,exclude_point$W)
        }else{
            h_max <- max(h_max,dat$W,data()$observedData_before$W,dummy$W,force$W,exclude_point$W)
        }
        c_parameter <- as.numeric(input$c_parameter)
        if(is.na(c_parameter)){
            c_parameter <- NULL
        }
        rc_fun <- get(m)
        if(m=='tournament'){
            rc.fit <- rc_fun( Q~W, c_param=c_parameter, h_max=h_max, data=dat, forcepoint=data()$observedData$Quality=='forcepoint')
            rc.fit <- rc.fit$winner
            rc_type <- if(grepl('g',class(rc.fit))) 'gen' else 'trad'
            var_type <- if(grepl('0',class(rc.fit))) 'const' else 'vary'
            updateRadioButtons(session, inputId = "checkbox2", selected = rc_type)
            updateRadioButtons(session, inputId = "checkbox3", selected = var_type)
            updateCheckboxInput(session, 'tournament', value = FALSE)
        }else{
            rc.fit <- rc_fun( Q~W, c_param=c_parameter, h_max=h_max, data=dat, forcepoint=data()$observedData$Quality=='forcepoint')
        }
        return(rc.fit)
        
    })
    
    ### create rating curve plot ###
    create_rc_fig <- reactive({
        temp_exclude_point <- as.data.frame(reactiveValuesToList(temp_exclude_point))
        upd_pts <- update_interactive_points( reactiveValuesToList(input_h_max)$W, reactiveValuesToList(dummy), reactiveValuesToList(force), reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign( i, as.data.frame(upd_pts[[i]] ) )
        
        rc <- autoplot( rc_model(), title= '' ) + coord_cartesian( xlim = ranges$x, ylim = ranges$y )
        
        if( length(exclude_point$W)>0 | any(!daterange$keeprows) | any(above_hmax$logical) ){
            ex_dat <- data.frame( 'W'=c(exclude_point$W,data()$observedData_before$W[!daterange$keeprows | above_hmax$logical]), 
                                  'Q'=c(exclude_point$Q,data()$observedData_before$Q[!daterange$keeprows | above_hmax$logical]) )
            rc <- rc + 
                geom_point( data = ex_dat, aes(Q,W), shape=21, fill="white", col="black", alpha=0.3 ) +
                geom_point( data = as.data.frame(data()$wq[,c('W','Q')]), aes(Q,W), size=.9, shape=21, fill="gray60", color="black",alpha=0.95) +
                if(length(temp_exclude_point$W)>0) geom_point( data = temp_exclude_point, aes(Q,W), shape=21, fill="white", col="gray80" ) 
        }
        if( length(dummy$W)>0 ){
            rc <- rc + geom_point( data=dummy, aes(Q,W), shape=21, fill="green", col="black" ) 
        }
        if( length(force$W)>0 ){
            rc <- rc + geom_point( data=force, aes(Q,W), shape=21, fill="tomato", col="black" ) 
        }
        return(rc)
    })
    
    ### create panel plot figures ###
    create_rc_panel <- reactive({
        
        upd_pts <- update_interactive_points( reactiveValuesToList(input_h_max)$W, reactiveValuesToList(dummy), reactiveValuesToList(force), reactiveValuesToList(exclude_point) )
        for (i in c('dummy','force','exclude_point')) assign( i, as.data.frame( upd_pts[[i]] ) )
        
        m <- rc_model()
        d <- data()
        c <- ifelse(is.null(m$run_info$c_param),m$param_summary['c','median'],m$run_info$c_param)

        
        trans_rc <- autoplot( m, transformed=T) + theme(plot.title = element_text(size=15) )
        resid <- plot_resid( m )  
        f_h <- autoplot( m, type='f')  + theme(plot.title = element_text(size=15) )
        sigma_eps <- autoplot( m, type='sigma_eps')  + theme(plot.title = element_text(size=15) )
        
        
        if( length(dummy$W)>0 | length(force$W)>0 | length(exclude_point$W)>0 | any(!daterange$keeprows) ){
            int_h_min <- min(dummy$W,force$W,exclude_point$W,d$observedData_before$W[!daterange$keeprows])
            if( int_h_min < min(d$wq[,'W']) ){
                
                ext_seq <- seq( log(int_h_min), log(min(d$wq[,'W'])) , 0.001 )
                ext_pred <- predict( m, newdata=exp(ext_seq) )
                
                trans_rc <- trans_rc + 
                    geom_path( data=ext_pred, aes( log(h-c), log(median) ), alpha=0.95 ) +
                    geom_path( data=ext_pred, aes( log(h-c), log(lower) ), linetype='dashed', alpha=0.95 ) +
                    geom_path( data=ext_pred, aes( log(h-c), log(upper) ), linetype='dashed', alpha=0.95 ) 
                
                resid <- resid +
                    geom_hline(yintercept=0,size=0.8,alpha=.95) +
                    geom_path(data=ext_pred, aes( log(h-c), log(lower)-log(median) ),color='black',linetype='dashed',size=0.5,alpha=0.95) +
                    geom_path(data=ext_pred, aes( log(h-c), log(upper)-log(median) ),color='black',linetype='dashed',size=0.5,alpha=0.95) 
                
            }
        }    
        if( length(exclude_point$W)>0 | any(!daterange$keeprows) | any(above_hmax$logical) ){
            temp_exclude_point <- as.data.frame(reactiveValuesToList(temp_exclude_point))
            ex_dat <- data.frame( 'W'=c(exclude_point$W,data()$observedData_before$W[!daterange$keeprows | above_hmax$logical]), 
                                  'Q'=c(exclude_point$Q,data()$observedData_before$Q[!daterange$keeprows | above_hmax$logical]) )
            trans_rc <- trans_rc + 
                geom_point( data= ex_dat, aes(log(W-c),log(Q)), shape=21, fill="white", col="black", alpha=.3 ) +
                geom_point( data = as.data.frame(data()$wq[,c('W','Q')]), aes(log(W-c),log(Q)), size=.9, shape=21, fill="gray60", color="black",alpha=0.95) +
                if(length(temp_exclude_point$W)>0) geom_point( data = temp_exclude_point, aes(log(W-c),log(Q)), shape=21, fill="white", col="gray80" ) 
                
            resid <- resid + 
                geom_point( data=ex_dat, aes(log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="white", col="black", alpha=.3 ) +
                geom_blank( data=ex_dat, aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) +
                geom_point( data = as.data.frame(data()$wq[,c('W','Q')]), aes( log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), size=.9, shape=21, fill="gray60", color="black",alpha=0.95) +
                if(length(temp_exclude_point$W)>0) geom_point( data = temp_exclude_point, aes(log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="white", col="gray80" ) 
        }    
        if( length(dummy$W)>0 ){
            trans_rc <- trans_rc + 
                geom_point( data=dummy, aes(log(W-c),log(Q)), shape=21, fill="green", col="black" )
            resid <- resid + 
                geom_point( data=dummy, aes( log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="green", col="black" ) +
                geom_blank( data=dummy,aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) 
        }
        if( length(force$W)>0 ){
            trans_rc <- trans_rc + 
                geom_point( data=force, aes(log(W-c),log(Q) ), shape=21, fill="tomato", col="black" ) 
            resid <- resid + 
                geom_point( data=force, aes( log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="tomato", col="black" )  +
                geom_blank( data=force, aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) 
        } 
        return(list('trans_rc'=trans_rc,'resid'=resid,'f_h'=f_h,'sigma_eps'=sigma_eps))
    })
    
    
    # # ########## DEBUGGER ##########
    # output$debug <- renderPrint({
    #     print(class(rc_model()))
    #     print(rc_model()$run_info$c_param)
    #     print(get_param_names(class(rc_model()),rc_model()$run_info$c_param))
    #     print(parse(text=shiny_get_param_expression(get_param_names(class(rc_model()),rc_model()$run_info$c_param),latex=FALSE)))
    # })
    # # #############################
    
    
    ### Tournament ###
    observeEvent(input$tournament, {
        if(input$tournament){
            disable("checkbox2")
            disable("checkbox3")
            message_fun(title="Heads up!",text=span(span('We will find the appropriate rating curve model for your data by comparing all available model types. 
                     This is a great feature of our software, but might take a couple of minutes to complete. The optimal setting for the '),span('Rating curve 
                     type',style = 'font-weight: bold;'),span(' and the'),span('Error variance',style = 'font-weight: bold;'),span(' will be indicated in the '),span('Controls',style = 'font-weight: bold;'),
                     span(' panel on the right after running. For more details, see the ',span('Instructions',style = 'font-weight: bold;'),span(' in the left sidebar menu.'))))
        }else{
            enable("checkbox2")
            enable("checkbox3")
        }
    })

    
    #### TAB 1 - Figures
    output$rc_fig <- renderPlot({
        create_rc_fig()
    },height=400,width=550)
    
    output$rc_panel <- renderPlot({
        grid.arrange(create_rc_panel()$trans_rc,create_rc_panel()$resid,
                     create_rc_panel()$f_h,create_rc_panel()$sigma_eps+ylab(expression(sigma[phantom(x)*epsilon])), ncol=2)
    },height=500,width=550)
    
    #### TAB 2 - Tables
    output$param_sum <- renderPlot({
        m <- rc_model()
        param <- get_param_names(class(m),m$run_info$c_param)
        table <- m$param_summary[,c('lower','median','upper')]
        names(table) <- paste0(names(table),c('-2.5%','-50%','-97.5%'))
        row.names(table) <- sapply(param,function(x) paste0("italic(",shiny_get_param_expression(x,latex=FALSE),"*phantom(x))") )
        table <- format(round(table,digits=3),nsmall=3)
        tg <- list(tableGrob(table,
                             theme=ttheme_minimal(core=list(bg_params = list(fill = c("#F7FBFF","#DEEBF7"), col=NA),fg_params=list(fontface=3)),
                                                  colhead=list(fg_params=list(col="black",fontface=2L)),
                                                  rowhead=list(fg_params=list(col="black",fontface=2L,parse=TRUE)))))
        tgt <- lapply(tg, justify, vjust="top", hjust="left")
        grid.arrange(grobs=tgt, ncol=1)
    })
    
    output$param_sum_ui <- renderUI({
        plotOutput('param_sum',height=param_sum_height$height*27-0.25*param_sum_height$height^2) 
    })
    
    
    output$rc_table <- renderPlot({
        tg <- list(tableGrob(format(round(predict( rc_model() ,wide=T),digits=3),nsmall=3),
                             theme=ttheme_minimal(core=list(bg_params=list(fill = c("#F7FBFF","#DEEBF7"), col=NA),
                                                            fg_params=list(fontface=3)),
                                                  colhead=list(fg_params=list(col="black",fontface=2L)),
                                                  rowhead=list(fg_params=list(col="black",fontface=2L)))))
        tgt <- lapply(tg, justify, vjust="top", hjust="left")
        grid.arrange(grobs=tgt, ncol=1)
    })
    
    output$rc_table_ui <- renderUI({
        plotOutput('rc_table',height=tabular_rating_curve_height$height*22,width = 2000) 
    })
    
    
    #### TAB 3 - Convergence diagnostics
    color_palette <- c("green","red","slateblue1","hotpink","#56B4E9","#E69F00","#000000","#999999","#CC79A7","#D55E00","#0072B2","#009E73")
    
    output$conv_diag1 <- renderPlot({
        autoplot(rc_model(), type='r_hat', title='') + scale_color_manual(values=color_palette,name=class(rc_model()),labels = parse(text=shiny_get_param_expression(get_param_names(class(rc_model()),rc_model()$run_info$c_param),latex=FALSE)))
    },height = 400,width = 550)
    
    output$conv_diag2 <- renderPlot({
        autoplot(rc_model(), type='autocorrelation', title='') + scale_color_manual(values=color_palette,name=class(rc_model()),labels = parse(text=shiny_get_param_expression(get_param_names(class(rc_model()),rc_model()$run_info$c_param),latex=FALSE)))
    },height = 400,width = 550)
    
    
    # my_report_1 <- reactive({
    #     m <- rc_model()
    #     report_pages <- get_report_pages(m)
    #     rating_curve_plot <- create_rc_fig() + ggtitle('Rating curve')
    #     panel_plot <- arrangeGrob(rating_curve_plot,create_rc_panel()$resid,
    #                               create_rc_panel()$f_h,create_rc_panel()$sigma_eps)
    #     param <- get_param_names(class(m),m$run_info$c_param)
    #     table <- rbind(m$param_summary[,c('lower','median','upper')],c(m$Deviance_summary))
    #     names(table) <- paste0(names(table),c('-2.5%','-50%','-97.5%'))
    #     row.names(table) <- c(sapply(param,function(x) shiny_get_param_expression(x,latex=FALSE)),"Deviance")
    #     table <- format(round(table,digits=3),nsmall=3)
    #     table_grob <- tableGrob(table,theme=ttheme_minimal(rowhead=list(fg_params=list(parse=TRUE))))
    #     refined_first_page <- arrangeGrob(panel_plot,
    #                                       table_grob,
    #                                       nrow=2,
    #                                       as.table=TRUE,
    #                                       heights=c(5,3),
    #                                       top=textGrob(paste0('Model type: ',class(m)),gp=gpar(fontsize=18,facetype='bold')))
    #     refined_first_page
    # })
    # 
    # my_report_2 <- reactive({
    #     m <- rc_model()
    #     report_pages <- get_report_pages(m)
    #     report_pages
    # })
    # 
    # callModule(downloadReport, id = "downloadReport", page_one = my_report_1,  remaining_pages = my_report_2)
    
    
    my_table <- reactive({
        m <- rc_model()
        tablelist=list()
        tablelist$Rating_Curve_Mean <- m$rating_curve_mean
        tablelist$Rating_Curve_Predictive <- m$rating_curve
        tablelist$Parameter_summary <- data.frame('parameters'=rownames(m$param_summary),m$param_summary)
        tablelist$Data_and_Added_Points <- m$data
        tablelist
    })
    
    callModule(downloadTable, id = "xlsxexport", data = my_table)

    
    #######Interactivity#######
    
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$rc_fig_dblclick, {
        brush <- input$rc_fig_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    observeEvent(input$rc_fig_click,{
        observedData=as.data.frame(data()$observedData_before)
        res <- nearPoints(observedData, input$rc_fig_click, xvar = "Q", yvar = "W", allRows = TRUE, threshold=5)
        if(any(res$selected_) & input$clickopts=='exclude'){
            vals$keeprows=xor(vals$keeprows,res$selected_)
            exclude_point$W=c(exclude_point$W,as.numeric(observedData$W[res$selected_]))
            exclude_point$Q=c(exclude_point$Q,as.numeric(observedData$Q[res$selected_])) 
            temp_exclude_point$W=c(temp_exclude_point$W,as.numeric(observedData$W[res$selected_]))
            temp_exclude_point$Q=c(temp_exclude_point$Q,as.numeric(observedData$Q[res$selected_])) 
        }else if(input$clickopts=='force'){
            if( !is.null(input_h_max$W) & !is.na(input_h_max$W) ){
                if( input$rc_fig_click$y > input_h_max$W ){
                    updateTextInput(session, 'h_max', value = format(round(input$rc_fig_click$y,4)+1e-4,nsmall=4))
                    input_h_max$W <- round(input$rc_fig_click$y,4)+1e-4
                    message_fun(title="Heads up!",text="You added a forcepoint above your pre-selected maximum stage value. We have updated the maximum stage value to be the same as the newly added forcepoint.")
                }
            } 
            force$W=c(force$W,input$rc_fig_click$y)
            force$Q=c(force$Q,input$rc_fig_click$x)
        }else if(input$clickopts=='dummy'){
            if( !is.null(input_h_max$W) & !is.na(input_h_max$W) ){
                if( input$rc_fig_click$y > input_h_max$W ){
                    updateTextInput(session, 'h_max', value = format(round(input$rc_fig_click$y,4)+1e-4,nsmall=4))
                    input_h_max$W <- round(input$rc_fig_click$y,4)+1e-4
                    message_fun(title="Heads up!",text="You added a dummypoint above your pre-selected maximum stage value. We have updated the maximum stage value to be the same as the newly added dummypoint")
                }
            }
            dummy$W=c(dummy$W,input$rc_fig_click$y)
            dummy$Q=c(dummy$Q,input$rc_fig_click$x)
        }
    })
    
    output$rc_tooltip <- renderUI({
        dat <- data()$observedData
        hover <- input$rc_hover
        x_px <- 640
        y_px <- 400
        point <- nearPoints(dat, hover,xvar='Q',yvar='W', threshold = 5, maxpoints = 1)
        if (nrow(point) == 0) return(NULL)
        style <- paste0("position:absolute; z-index:100; pointer-events:none; background-color: rgba(255, 255, 255, 1); ",
                        "left:", x_px-0.42*x_px, "px; top:", y_px-0.28*y_px, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Date: </b>", point$Date, "<br/>",
                          "<b> Quality: </b>", point$Quality, "<br/>",
                          "<b> Water elevation: </b>", format(round(point$W,3),nsmall=3), " m <br/>",
                          "<b> Discharge: </b>", format(round(point$Q,3),nsmall=3), " m^3/s<br/>")))
        )
        
    })
    
    
    observeEvent(input$reset,{
        n=nrow(data()$observedData_before)
        vals$keeprows=rep(TRUE,n)
        dummy$W=NULL
        dummy$Q=NULL
        force$W=NULL
        force$Q=NULL
        exclude_point$W <- NULL
        exclude_point$Q <- NULL
        temp_exclude_point$W <- NULL
        temp_exclude_point$Q <- NULL
    })
    
}


shinyApp(ui, server)


