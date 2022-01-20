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
library(writexl)

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
                      
                      dashboardHeader(title = span(
                          span("P(",
                               style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px'),
                          span("Bayesian",
                               style = 'font-weight: bold; color: #1F65CC; font-size: 28px'),
                          span("|",
                               style = 'font-family: "Times"; color: gray; font-size: 28px'),
                          span("Discharge Rating Curves",
                               style = ' font-weight: bold; color: #4AA4DE; font-size: 28px'),
                          span(")",
                               style = 'font-family: "Brush Script MT"; color: gray; font-size: 28px')
                      ),
                      #titleWidth = 501,
                      titleWidth = 548,
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
                                                                        plotOutput('rc_fig',
                                                                                   click ='rc_fig_click',
                                                                                   dblclick = dblclickOpts(id = 'rc_fig_dblclick'),
                                                                                   hover = hoverOpts(id = "rc_hover",delay=100),
                                                                                   brush = brushOpts(id = 'rc_fig_brush',resetOnNew = TRUE)),
                                                                        uiOutput("rc_tooltip"),
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
                                                               br(),
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
                                                                                    actionButton('reset',
                                                                                                 label='Reset',
                                                                                                 icon('refresh',style="color: dodgerblue")),
                                                                                    ),
                                                                   width=12),
                                                               actionButton("go", 
                                                                            label="Create Rating Curve",
                                                                            icon("play",style="color: dodgerblue")),
                                                               br(),br(),
                                                               tags$a(href = 'downloadReport', class = "btn", icon("download"), 'Download Report'),
                                                               br(),
                                                               tags$a(href = 'xlsxexport', class = "btn", icon("download"), 'Download Tables as xlsx'),
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
input_h_max <- reactiveValues(W=NA)

server <- function(input, output, session) {
    
    ## data import ##
    data <- eventReactive(input$go,{
        if(is.null(input$file)){
            showModal(modalDialog(
                title = span("Whoops!",style = 'font-weight: bold; color: #1F65CC; font-size: 28px'),
                span("Please upload data in order to fit a rating curve.",style='font-size: 17px'),
                size='m',
                easyClose = TRUE,
                fade = TRUE,
                footer = tagList(
                    modalButton(span("Got it!",style='font-size: 17px'))
                )
            ))
            stop()
        }else{
            upd_pts <- update_points(reactiveValuesToList(input_h_max)$W,
                                     reactiveValuesToList(dummy),
                                     reactiveValuesToList(force),
                                     reactiveValuesToList(exclude_point))
            for (i in c('dummy','force','exclude_point')) assign(i,upd_pts[[i]]) 
            
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
            if(nrow(cleandata$wq) < 3){
                stop('There are less than 3 data points. Cannot fit rating curve')
            }
            return(cleandata)
        }
    })
    
    observeEvent(input$go,{
        input_h_max$W <- c(as.numeric(input$h_max))
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
        upd_pts <- update_points(reactiveValuesToList(input_h_max)$W,
                                 reactiveValuesToList(dummy),
                                 reactiveValuesToList(force),
                                 reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign(i,as.data.frame(upd_pts[[i]]))
        h_max <- upd_pts$h_max
        
        if(is.na(h_max)){
            h_max <- max(dat$W,dummy$W,force$W,exclude_point$W)
        }else{
            h_max <- max(h_max,dat$W,dummy$W,force$W,exclude_point$W)
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
    
    ### create rating curve plot ###
    create_rc_fig <- reactive({
        upd_pts <- update_points(reactiveValuesToList(input_h_max)$W,
                                 reactiveValuesToList(dummy),
                                 reactiveValuesToList(force),
                                 reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign(i,as.data.frame(upd_pts[[i]]))
        
        rc <- autoplot( rc_model(), title= 'Rating Curve' ) + coord_cartesian( xlim = ranges$x, ylim = ranges$y )
        
        if(any(dim(dummy))){
            rc <- rc + geom_point( data=dummy, aes(Q,W), shape=21, fill="green", col="black" ) 
        }
        if(any(dim(force))){
            rc <- rc + geom_point( data=force, aes(Q,W), shape=21, fill="steelblue1", col="black" ) 
        }
        if(any(dim(exclude_point))){
            rc <- rc + geom_point( data=exclude_point, aes(Q,W), shape=21, fill="white", col="black" ) 
        }
        return(rc)
    })
    
    ### create panel plot figures ###
    create_rc_panel <- reactive({
        upd_pts <- update_points(reactiveValuesToList(input_h_max)$W,
                                 reactiveValuesToList(dummy),
                                 reactiveValuesToList(force),
                                 reactiveValuesToList(exclude_point))
        for (i in c('dummy','force','exclude_point')) assign(i,as.data.frame(upd_pts[[i]]))
        
        m <- rc_model()
        d <- data()
        c <- ifelse(is.null(m$run_info$c_param),m$param_summary['c','median'],m$run_info$c_param)
        
        trans_rc <- autoplot( m, transformed=T, title= 'Log-transformed Rating Curve')   
        resid <- plot_resid( m )
        f_h <- autoplot( m, type='f', title= 'Power-law Exponent')
        sigma_eps <- autoplot( m, type='sigma_eps', title= 'Residual Standard Deviation')
        
        ########## DEBUGGER ##########
        # output$debug <- renderPrint({
        #     print(length(dummy$W))
        # })
        ##############################
        
        if( length(dummy$W)>0 | any(dim(force)) ){
            if( min(dummy$W,force$W)<min(d$observedData_before$W) ){
                
                w_min <- min( dummy$W, force$W ) 
                ext_seq <- seq( log(w_min), log(min(d$observedData_before$W)) , 0.001 )
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
        if( length(dummy$W)>0 ){
            trans_rc <- trans_rc + geom_point( data=dummy, aes(log(W-c),log(Q)), shape=21, fill="green", col="black" )
            resid <- resid + 
                geom_point( data=dummy, aes( log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="green", col="black" ) +
                geom_blank( data=dummy,aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) 
            
        }
        if( length(force$W)>0 ){
            trans_rc <- trans_rc + geom_point( data=force, aes(log(W-c),log(Q) ), shape=21, fill="steelblue1", col="black" ) 
            resid <- resid + 
                geom_point( data=force, aes( log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="steelblue1", col="black" )  +
                geom_blank( data=force,aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) 
        } 
        if( length(exclude_point$W)>0 ){
            trans_rc <- trans_rc + geom_point( data=exclude_point, aes(log(W-c),log(Q)), shape=21, fill="white", col="black" ) 
            resid <- resid + 
                geom_point( data=exclude_point, aes(log(W-c), log(Q)-log(predict(m,newdata=W)[,'median']) ), shape=21, fill="white", col="black" ) +
                geom_blank( data=exclude_point, aes( y = log(predict(m,newdata=W)[,'median'])-log(Q) ) ) 
        }    
        
        return(list('trans_rc'=trans_rc,'resid'=resid,'f_h'=f_h,'sigma_eps'=sigma_eps))
    })
    
    
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
    
    ## create headers ##
    headers <- eventReactive(input$go,{
        head_list <- list()
        head_list$tab1_head <- paste('Parameter Summary Table')
        head_list$tab2_head <- paste('Tabular Rating Curve')
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
    output$rhat_head <- renderText({
        headers()$rhat_head
    })
    output$auto_head <- renderText({
        headers()$auto_head
    })
    
    #### TAB 1 - Figures
    output$rc_fig <- renderPlot({
        create_rc_fig()
    },height=400,width=550)
    
    output$rc_panel <- renderPlot({
        grid.arrange(create_rc_panel()$trans_rc,create_rc_panel()$resid,
                     create_rc_panel()$f_h,create_rc_panel()$sigma_eps, ncol=2)
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
            m <- rc_model()
            report_pages <- bdrc::get_report_pages(m)
            panel_plot <- arrangeGrob(create_rc_fig(),create_rc_panel()$resid,
                                      create_rc_panel()$f_h,create_rc_panel()$sigma_eps)
            param <- get_param_names(class(m),m$run_info$c_param)
            table <- rbind(m$param_summary[,c('lower','median','upper')],c(m$Deviance_summary))
            names(table) <- paste0(names(table),c('-2.5%','-50%','-97.5%'))
            row.names(table) <- c(sapply(1:length(param),function(x) get_param_expression(x,latex=FALSE)),"Deviance")
            table <- format(round(table,digits=3),nsmall=3)
            table_grob <- tableGrob(table,theme=ttheme_minimal(rowhead=list(fg_params=list(parse=TRUE))))
            page1_revised <- arrangeGrob(panel_plot,
                                         table_grob,
                                         nrow=2,
                                         as.table=TRUE,
                                         heights=c(5,3),
                                         top=textGrob(class(m),gp=gpar(fontsize=22,facetype='bold')))
            pdf(file=filename,paper='a4',width=9,height=11)
            grid.arrange(page1_revised,as.table=TRUE)
            for(i in 2:length(report_pages) ) grid.arrange(report_pages[[i]],as.table=TRUE)
            invisible(dev.off())
            file.copy("bdrc_report.pdf", file)
        }
    )
    
    ### Download xlsx ###
    output$xlsxexport <- downloadHandler(
        filename= function(){
            paste0('bdrc_tables.xlsx')
        },    
        content = function(file){
            m <- rc_model()
            tablelist=list()
            tablelist$Rating_Curve_Mean <- m$rating_curve_mean
            tablelist$Rating_Curve_Predictive <- m$rating_curve
            tablelist$Parameter_summary <- data.frame('parameters'=rownames(m$param_summary),m$param_summary)
            tablelist$Data_and_Added_Points <- m$data
            write_xlsx(tablelist, path=file)
        }
    )
    

    ########## DEBUGGER ##########
    # output$debug <- renderPrint({

    # })
    ##############################
    
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
        res <- nearPoints(observedData, input$rc_fig_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
        if(any(res$selected_) & input$clickopts=='exclude'){
            vals$keeprows=xor(vals$keeprows,res$selected_)
            exclude_point$W=c(exclude_point$W,as.numeric(observedData$W[res$selected_]))
            exclude_point$Q=c(exclude_point$Q,as.numeric(observedData$Q[res$selected_]))   
        }else if(input$clickopts=='force'){
            force$W=c(force$W,input$rc_fig_click$y)
            force$Q=c(force$Q,input$rc_fig_click$x)
        }else if(input$clickopts=='dummy'){
            dummy$W=c(dummy$W,input$rc_fig_click$y)
            dummy$Q=c(dummy$Q,input$rc_fig_click$x)
        }
    })
    
    output$rc_tooltip <- renderUI({
        
        hover <- input$rc_hover
        mod <- rc_model()
        dat <- data()$observedData
        c <- ifelse(is.null(mod$run_info$c_param),mod$param_summary['c','median'],mod$run_info$c_param)
        
        point <- nearPoints(dat, hover,xvar='Q',yvar='W', threshold = 10, maxpoints = 1)
        if (nrow(point) == 0) return(NULL)
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- hover$x / max(dat$Q)
        top_pct <- (max(dat$W) - hover$y) / (max(dat$W) - c)
        
        # calculate distance from left and bottom side of the picture in pixels
        x_px <- 400
        y_px <- 550
        x_shift <- 10
        y_shift <- 20
        left_px <- hover$range$left + x_shift + left_pct * (x_px-x_shift)
        top_px <- hover$range$top + y_shift + top_pct * (y_px-y_shift)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100;pointer-events:none; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px, "px; top:", top_px, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Date: </b>", point$Date, "<br/>",
                          "<b> Water elevation: </b>", point$W, " m <br/>",
                          "<b> Discharge: </b>", point$Q, " m^3/s<br/>")))
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
    })
    

    
}
shinyApp(ui, server)
