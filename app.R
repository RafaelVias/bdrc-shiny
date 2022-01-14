library(shinydashboard)
library(googleVis)
library(shinyWidgets)
library(bdrc)
library(ggplot2)
library(readxl)
library(Cairo)
library(grid)
library(gridExtra)
library(xtable)
library(shiny)
library(knitr)
library(shinyalert)

source('help_functions.R')
options(shiny.usecairo=T)


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

ui <- shinyUI(fluidPage(
        shinyjs::useShinyjs(),
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
                                                               
                                                               
                                                               tabPanel(span('Figures',style='font-size: 15px;'),
                                                                        textOutput('debug'),
                                                                        h4(textOutput("rc_head")), 
                                                                        plotOutput('rc_fig',click ='rc_fig_click',dblclick = dblclickOpts(id = 'rc_fig_dblclick'),
                                                                                   brush = brushOpts(id = 'rc_fig_brush',resetOnNew = TRUE)),
                                                                        plotOutput('rc_panel')),
                                                               tabPanel(span('Tables',style='font-size: 15px;'),
                                                                        h4(textOutput("tab1_head")), 
                                                                        uiOutput('param_sum'),
                                                                        h4(textOutput("tab2_head")),
                                                                        plotOutput('rc_table')),
                                                               tabPanel(span('Convergence diagnostics',style='font-size: 15px;'),
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
                                                               box(radioButtons(inputId="checkbox2", label="Rating Curve Type",choices=list("Generalized Power-law"='gen',"Power-law"='trad'),selected="gen"),
                                                                   radioButtons(inputId="checkbox3", label="Residual variance",choices=list("Stage varying" = 'vary',"Constant" = 'const'),selected='vary'),
                                                                   checkboxInput("tournament", label=span('Calculate Best Rating Curve',style='font-weight: bold;'), value=FALSE),
                                                                   width = 12),
                                                               box(checkboxInput("advanced", label=span('Advanced Settings',style='font-weight: bold;'), value=FALSE),
                                                                   conditionalPanel(condition="input.advanced == true", 
                                                                                    radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                                                                    sliderInput("includeDates", label = "Date Range Included", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")),
                                                                                                value=c(1950,as.numeric(format(Sys.Date(), "%Y"))),sep=""),
                                                                                    checkboxInput("exclude", label=span("Exclude certain period",style='font-weight: bold;'), value=FALSE),
                                                                                    conditionalPanel(condition="input.exclude == true",
                                                                                                     dateRangeInput("excludeDates", label = "Date Range",start=Sys.Date()-1,end=Sys.Date()-1)),
                                                                                    textInput("h_max",label="Maximum Stage (m)"),
                                                                                    textInput("c_parameter",label="Stage of zero discharge (c)"
                                                                                              #,placeholder = 'Optional'
                                                                                    ),
                                                                                    actionButton('reset',label='Reset'),
                                                                                    ),
                                                                   width=12),
                                                               br(),
                                                               actionButton("go", label="Create Rating Curve"),
                                                               br(),br(),br(),
                                                               downloadButton('downloadReport',label='Download Report')
                                                           )
                                                    )
                                                )
                                        ),
                                        
                                        tabItem(tabName="about",
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
)

################## SERVER ##################################################

vals<-reactiveValues(keeprows=NULL)
daterange=reactiveValues(keeprows=NULL)
ranges <- reactiveValues(x = NULL, y = NULL)
dummy <- reactiveValues(Q=NULL,W=NULL)
force <- reactiveValues(Q=NULL,W=NULL)
exclude_point <- reactiveValues(Q=NULL,W=NULL)
best_model <- reactiveValues(class=NULL)

server <- function(input, output, session) {
    
    ## data import ##
    data <- eventReactive(input$go,{
        if(is.null(input$file)){
            stop('Please upload data in order to fit a rating curve')
        }
        dummy=reactiveValuesToList(dummy)
        force=reactiveValuesToList(force)
        cleandata=clean(input$file,dummy=dummy,force=force,keeprows=vals$keeprows,
                        advanced=input$advanced,exclude=input$exclude,
                        excludedates=input$excludeDates, includedates=input$includeDates,
                        h_max=as.numeric(input$h_max))
        
        if(length(vals$keeprows)==0 ){
            vals$keeprows= rep(TRUE,nrow(cleandata$observedData_before))
        }
        years=as.numeric(format(cleandata$observedData_before$Date, "%Y"))
        includeindex=years<=input$includeDates[2] & years >= input$includeDates[1]
        excludeindex=cleandata$observedData_before$Date<=input$excludeDates[1] | cleandata$observedData_before$Date >= input$excludeDates[2]
        daterange$keeprows=excludeindex & includeindex
        return(cleandata)
    })
    
    ## run Models ##
    rc_model <- eventReactive(input$go,{
        if(input$tournament){
            m <- 'tournament'
        }else{
            m <- paste0(ifelse(input$checkbox2=='gen','gplm','plm'),
                        ifelse(input$checkbox3=='vary','','0'))
        }
        dat <- as.data.frame(data()$wq)
        if( input$h_max!='' ){
            if( as.numeric(input$h_max) > max(dat$W) ){
                h_max <- as.numeric(input$h_max)
            }else{
                h_max <- NULL
            }
        }else{
            h_max <- NULL
        }
        c_parameter <- as.numeric(input$c_parameter)
        if(is.na(c_parameter)){
            c_parameter <- NULL
        }
        rc_fun <- get(m)
        if(m=='tournament'){
            rc.fit <- rc_fun( Q~W, c_param=c_parameter, h_max=h_max, data=dat)
            rc.fit <- rc.fit$winner
            best_model$class <- class(rc.fit)
        }else{
            rc.fit <- rc_fun( Q~W, c_param=c_parameter, h_max=h_max, data=dat, forcepoint=data()$observedData$Quality=='forcepoint')
        }
        return(rc.fit)
    })
    
    ## create headers ##
    headers <- eventReactive(input$go,{
        head_list <- list()
        head_list$tab1_head <- paste('Parameter Summary Table')
        head_list$tab2_head <- paste('Tabular Rating Curve')
        head_list$rc_head <- paste('Rating Curve')
        head_list$rhat_head <- paste('Gelman-Rubin Statistic Plot')
        head_list$auto_head <- paste('Autocorrelation Plot')
        return(head_list)
    })
    
    
    output$tab1_head <- renderText({
        headers()$tab1_head
    })
    output$tab2_head <- renderText({
        headers()$tab2_head
    })
    output$rc_head <- renderText({
        headers()$rc_head
    })
    output$rhat_head <- renderText({
        headers()$rhat_head
    })
    output$auto_head <- renderText({
        headers()$auto_head
    })
    
    #### TAB 1 - Figures
    output$rc_fig <- renderPlot({
        dummy=as.data.frame(reactiveValuesToList(dummy))
        force=as.data.frame(reactiveValuesToList(force))
        exclude_point=as.data.frame(reactiveValuesToList(exclude_point))
        
        rc_fig <- autoplot( rc_model()) + coord_cartesian( xlim = ranges$x, ylim = ranges$y )
        
        if(any(dim(dummy))){
            rc_fig <- rc_fig + geom_point( data=dummy, aes(Q,W), fill="green", col="green" )
        }
        if(any(dim(force))){
            rc_fig <- rc_fig + geom_point( data=force, aes(Q,W), fill="blue", col="blue")
        }
        if(any(dim(exclude_point))){
            rc_fig <- rc_fig + geom_point( data=exclude_point, aes(Q,W), fill="red", col="red")
        }
        rc_fig
    },height=400,width=550)
    
    output$rc_panel <- renderPlot({
        trans_rc <- autoplot( rc_model(), transformed=T, title= 'Log-transformed Rating Curve')
        resid <- autoplot( rc_model(), type='residuals', title= 'Residual Plot')
        f_h <- autoplot( rc_model(), type='f', title= 'Power-law Exponent')
        sigma_eps <- autoplot( rc_model(), type='sigma_eps', title= 'Residual Standard Deviation')
        grid.arrange(trans_rc,resid,f_h,sigma_eps, ncol=2)
    },height=500,width=550)
    
    #### TAB 2 - Tables
    output$param_sum <- renderUI({
        m <- rc_model()
        param <- get_param_names(class(m),m$run_info$c_param)
        table <- m$param_summary[,c('lower','median','upper')]
        names(table) <- paste0(names(table),c('-2.5\\%','-50\\%','-97.5\\%'))
        row.names(table) <- sapply(1:length(param),get_param_expression)
        table <- format(round(table,digits=3),nsmall=3)
        table <- print(xtable::xtable(as.data.frame(table)),
                       floating=FALSE, 
                       tabular.environment="array",
                       include.rownames = TRUE,
                       comment=FALSE, 
                       print.results=FALSE, 
                       sanitize.rownames.function = function(x) x,
                       sanitize.colnames.function = function(x) x)
        tagList(
            withMathJax(),
            HTML(paste0("$$", table, "$$"))
        )
    })
    
    
    
    output$rc_table <- renderPlot({
        tg <- list(tableGrob(format(round(predict( rc_model() ,wide=T),digits=3),nsmall=3),
                             theme=ttheme_minimal(core=list(bg_params = list(fill = c("#F7FBFF","#DEEBF7"), col=NA),fg_params=list(fontface=3)),
                                                  colhead=list(fg_params=list(col="black",fontface=2L)),
                                                  rowhead=list(fg_params=list(col="black",fontface=2L)))))
        tgt <- lapply(tg, justify, vjust="top")
        grid.arrange(grobs=tgt, ncol=1)
    })
    
    
    #### TAB 3 - Convergence diagnostics
    output$conv_diag1 <- renderPlot({
        autoplot(rc_model(), type='r_hat') 
    },height = 400,width = 550)
    output$conv_diag2 <- renderPlot({
        autoplot(rc_model(), type='autocorrelation')
    },height = 400,width = 550)
    
    
    ### Download Report ###
    output$downloadReport <- downloadHandler(
        filename <- 'bdrc_report.pdf',
        content <- function(file) {
            filename <- 'bdrc_report.pdf'
            report_pages <- bdrc::get_report_pages( rc_model() )
            pdf(file=filename,paper='a4',width=9,height=11)
            for(i in 1:length(report_pages)){
                grid.arrange(report_pages[[i]],as.table=TRUE)
            }
            invisible(dev.off())
            file.copy("bdrc_report.pdf", file)
        }
    )
    
    ### Tournament ###
    
    observeEvent(input$tournament, {
        if(input$tournament){
            shinyjs::disable("checkbox2")
            shinyjs::disable("checkbox3")
            showModal(modalDialog(
                title = span("Heads up!",style = 'font-weight: bold; color: #1F65CC; font-size: 28px'),
                span("We will find the appropriate rating curve model for your data using model comparison of all available model types. 
                     This is a great feature of our software, but might take a couple of minutes to complete. The optimal rating curve 
                     type and residual variance will be indicated on the right after running.",style='font-size: 17px'),
                size='m',
                easyClose = TRUE,
                fade = TRUE,
                footer = tagList(
                    modalButton(span("Got it!",style='font-size: 17px'))
                )
            ))
            # shinyalert(
            #     title = "Heads up!",
            #     text = "Great choice!\n We will find the appropriate rating curve model for your data.\n This is great feature of our software,\n but might take a couple of minutes complete.",
            #     size = "s",
            #     closeOnEsc = TRUE,
            #     closeOnClickOutside = FALSE,
            #     html = FALSE,
            #     type = "success",
            #     showConfirmButton = TRUE,
            #     showCancelButton = FALSE,
            #     confirmButtonText = "Got it!",
            #     confirmButtonCol = "#AEDEF4",
            #     timer = 0,
            #     imageUrl = "",
            #     animation = TRUE
            # )
        }else{
            shinyjs::enable("checkbox2")
            shinyjs::enable("checkbox3")
        }
    })
    
    observeEvent(input$go, {
        if(input$tournament){
            rc_type <- ifelse(grepl('g',best_model$class),'gen','trad')
            var_type <- ifelse(grepl('0',best_model$class),'const','vary')
            updateRadioButtons(session, inputId = "checkbox2", selected = rc_type)
            updateRadioButtons(session, inputId = "checkbox3", selected = var_type)
            updateCheckboxInput(session, 'tournament', value = FALSE)
        }
    })
    
    
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
    
    # ########## DEBUGGER ##########
    # output$debug <- renderPrint({
    #     print(best_model$class)
    # })
    # ##############################
    
    observeEvent(input$rc_fig_click,{
        observedData=as.data.frame(data()$observedData_before)
        res <- nearPoints(observedData, input$rc_fig_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
        if(any(res$selected_) & input$clickopts=='exclude'){
            vals$keeprows=xor(vals$keeprows,res$selected_)
            exclude_point$W=c(exclude_point$W,as.numeric(observedData$W[res$selected_]))
            exclude_point$Q=c(exclude_point$Q,as.numeric(observedData$Q[res$selected_]))   # FIX !
        }else if(input$clickopts=='force'){
            force$W=c(force$W,input$rc_fig_click$y)
            force$Q=c(force$Q,input$rc_fig_click$x)
        }else if(input$clickopts=='dummy'){
            dummy$W=c(dummy$W,input$rc_fig_click$y)
            dummy$Q=c(dummy$Q,input$rc_fig_click$x)
        }
    })
    
    
    observeEvent(input$reset,{
        n=nrow(data()$observedData_before)
        vals$keeprows=rep(TRUE,n)
        dummy$W=NULL
        dummy$Q=NULL
        force$W=NULL
        force$Q=NULL
    })
}
shinyApp(ui, server)
