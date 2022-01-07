suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(googleVis))
library(bdrc)
library(ggplot2)
library(readxl)
library(Cairo)
library(rmarkdown)


options(shiny.usecairo=T)


vals<-reactiveValues(keeprows=NULL)
daterange=reactiveValues(keeprows=NULL)
ranges1 <- reactiveValues(x = NULL, y = NULL)
ranges2 <- reactiveValues(x = NULL, y = NULL)
dummy <- reactiveValues(Q=NULL,W=NULL)
force <- reactiveValues(Q=NULL,W=NULL)

shinyServer(function(input, output) {
    
    data <- eventReactive(input$go,{
        if(is.null(input$file)){
            return(NULL)
            }
        dummy=reactiveValuesToList(dummy)
        force=reactiveValuesToList(force)
        inFile <- input$file
        cleandata = readxl::read_xlsx(path = inFile$datapath, sheet = 1)
        wq=cleandata[,c("W","Q")]
        wq$W=0.01*wq$W
        return(wq)
    })
    
    ## run Models ##
    rc_model <- eventReactive(input$go,{
      m <- paste0(ifelse(input$checkbox2=='gen','gplm','plm'),
                  ifelse(input$checkbox3=='vary','','0'))
        withProgress(message = 'Making plot', value = 0, {
          rc_fun <- get(m)
          if(isTruthy(input$show_c)){
            rc.fit <- rc_fun(Q~W,c_param = as.numeric(input$c_parameter),data())
          }else{
            rc.fit <- rc_fun(Q~W,data())
          }
          return(rc.fit)
        })
    })

    fig_list <- reactive({
      
      model <- rc_model()
      output_fig <- list()
      dummy=as.data.frame(reactiveValuesToList(dummy))
      force=as.data.frame(reactiveValuesToList(force))

      rc_fig <- autoplot( model ) #+ coord_cartesian( xlim = ranges2$x, ylim = ranges2$y )
      # if(any(dim(dummy))){
      #   rc_fig <- rc_fig + geom_point( data=dummy, aes(Q,W), fill="red", col="red" )
      # }
      # if(any(dim(force))){
      #   rc_fig <- rc_fig + geom_point( data=force, aes(Q,W), fill="blue", col="blue")
      # }
      output_fig$rc_fig <- rc_fig
      
      panel_fig <- plot( model , type = 'panel', transformed = T )
      output_fig$panel_fig <- panel_fig
      
      return(output_fig)
    })
    
    output$debug <- renderPrint({
      class(rc_model())
    })
    
    output$rc_fig <- renderPlot({
      fig_list()$rc_fig
    })
    
    output$rc_panel <- renderPlot({
      fig_list()$panel_fig
    })
    
    
    # 
    # ## MODEL1 ##  Begin
    # model1 <- eventReactive(input$go,{
    #     if("gen" %in% input$checkbox2){
    #       # m <- paste0(ifelse(input$checkbox2=='gen','gplm','plm'),
    #       #             ifelse(input$checkbox3=='vary','','0'))
    #       # rc_fun <- get(m)
    #       # print(class(rc_fun))
    #         #if(!is.null(data())){
    #             withProgress(message = 'Making plot', value = 0, {
    #                 f <- Q~W
    #                # plm.fit <- plm(f,V316_river)
    #                 if(isTruthy(input$show_c)){
    #                   plm.fit <- plm(f,c_param = as.numeric(input$c_parameter),data())
    #                 }else{
    #                   plm.fit <- plm(f,data())
    #               }                    
    #                   #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
    #                 return(plm.fit)
    #                 
    #             })
    #         #}
    #     }
    # })
    # 
    # 
    # plotratingcurve1 <- reactive({
    #   model=model1()
    #   rclog=NULL
    #   rcraun=NULL
    #   rcleiflog=NULL
    #   rcleifraun=NULL
    #   #tafla=NULL
    #   outputlist=list()
    #   simdata = model$rating_curve_mean
    #   data = model$data
    #   
    #     #plotlist=model1()
    #     cleandata=data()
    #     dummy=as.data.frame(reactiveValuesToList(dummy))
    #     force=as.data.frame(reactiveValuesToList(force))
    #     
    #     outputlist=list()
    #     
    #     if(!is.null(model$data)) {
    #         #observedPrediction=plotlist$observedPrediction
    #         observedPrediction=model$rating_curve_mean$h
    #         #completePrediction=plotlist$completePrediction
    #         observedPrediction=model$rating_curve$h
    #         
    #         # keeprows=vals$keeprows & daterange$keeprows
    #         # 
    #         # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
    #         # 
    #         # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
    #         # 
    #         # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
    #         
    #         name=input$name
    #         
    #         if(nchar(name)==0){
    #             name=gsub("\\.[^.]*$","",input$file$name)
    #         }
    #         
    #         if ("rc" %in% input$checkbox){
    #           rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #           if(any(dim(dummy))){
    #             rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
    #           }
    #           if(any(dim(force))){
    #             rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
    #           }
    #           outputlist$rcraun=rcraun
    #         }
    #         
    #         if("rc_tr" %in% input$checkbox){
    #           rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #           
    #           outputlist$rclog=rclog
    #         }
    #         
    #         if ("res" %in% input$checkbox){
    #           rcleifraun=plot(model,type='residuals')
    #           
    #           outputlist$rcleifraun=rcleifraun
    #         } 
    #         
    #      
    #         if("f_h" %in% input$checkbox){
    #           smoothbeta=plot(model,type='f')
    #           outputlist$smoothbeta=smoothbeta
    #         }
    #         
    #         if("sigma_eps" %in% input$checkbox){
    #         sigma_eps = plot(model,type='sigma_eps')
    #         outputlist$sigma_eps=sigma_eps
    #         }
    #        # smoothbeta=plot(model,type='f')
    #         #outputlist$smoothbeta=smoothbeta
    #         
    #         
    #         tafla=model$rating_curve
    #         # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
    #         # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
    #         # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
    #         # tafla$diffQ=tafla$Q-tafla$Qfit
    #         # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
    #         # tafla=tafla[with(tafla,order(Date)),]
    #         outputlist$tafla=tafla
    #         
    #         
    #     return(outputlist)
    #     }
    #     
    # })
    # 
    # 
    # 
    # model2 <- eventReactive(input$go,{
    #     if("trad" %in% input$checkbox2){
    #         #if(!is.null(data())){
    #             withProgress(message = 'Making plot', value = 0, {
    #               f <- Q~W
    #               if(isTruthy(input$c_parameter)){
    #                 plm0.fit <- plm0(f,c_param = as.numeric(input$c_parameter),data())
    #               }else{
    #                 plm0.fit <- plm0(f,data())
    #               }                    
    #               return(plm0.fit)
    #                 
    #             })
    #         #}
    #     }
    # })
    # 
    # plotratingcurve2 <- reactive({
    #   
    #     model=model2()
    #     rclog=NULL
    #     rcraun=NULL
    #     rcleiflog=NULL
    #     rcleifraun=NULL
    #     #tafla=NULL
    #     outputlist=list()
    #     simdata = model$rating_curve_mean
    #     data = model$data
    #   
    #     #plotlist=model2()
    #     cleandata=data()
    #     dummy=as.data.frame(reactiveValuesToList(dummy))
    #     force=as.data.frame(reactiveValuesToList(force))
    #     TableOfData=NULL
    #     outputlist=list()
    #     
    #     if(!is.null(model$data)) {
    #       
    #         #observedPrediction=plotlist$observedPrediction
    #         observedPrediction=model$rating_curve_mean$h
    #         #completePrediction=plotlist$completePrediction
    #         observedPrediction=model$rating_curve$h
    #         
    #         #betaData=plotlist$betaData
    #         betaData=model$beta_summary$h
    #           
    #         # keeprows=vals$keeprows & daterange$keeprows
    #         # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
    #         # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
    #         # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
    #         filename=input$name
    #         name=input$name
    #         
    #         if(nchar(name)==0){
    #             name=gsub("\\.[^.]*$","",input$file$name)
    #         }
    #         
    #         
    #         if ("rc" %in% input$checkbox){
    #             rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #             if(any(dim(dummy))){
    #                 rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
    #             }
    #             if(any(dim(force))){
    #                 rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
    #             }
    #             outputlist$rcraun=rcraun
    #         }
    #         if("rc_tr" %in% input$checkbox){
    #             rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #             
    #             outputlist$rclog=rclog
    #         }
    #         
    #         
    #         if ("res" %in% input$checkbox){
    #             rcleifraun=plot(model,type='residuals')
    #             
    #             outputlist$rcleifraun=rcleifraun
    #         } 
    #         # if("leiflog" %in% input$checkbox){
    #         #     # max=max(abs(observedPrediction$standardResiduals))
    #         #     # if(max>4){
    #         #     #     ylim=c(-(max+0.2),max+0.2)
    #         #     # }else{
    #         #     #     ylim=c(-4,4)
    #         #     # }
    #         #     rcleiflog=plot(model,type='residuals',transformed = T)
    #         #     
    #         #     
    #         #     outputlist$rcleiflog=rcleiflog
    #         # }
    #         
    #         if("f_h" %in% input$checkbox){
    #           smoothbeta=plot(model,type='f')
    #           outputlist$smoothbeta=smoothbeta
    #         }
    #         
    #         if("sigma_eps" %in% input$checkbox){
    #           sigma_eps = plot(model,type='sigma_eps')
    #           outputlist$sigma_eps=sigma_eps
    #         }
    #         
    #         
    #         tafla=model$rating_curve
    #         # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
    #         # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
    #         # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
    #         # tafla$diffQ=tafla$Q-tafla$Qfit
    #         # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
    #         # tafla=tafla[with(tafla,order(Date)),]
    #         outputlist$tafla=tafla
    #         
    #         return(outputlist)
    #     } 
    #     
    # })
    # 
    # 
    # 
    # ## gplm0 ##  Begin
    # 
    # 
    # model3 <-eventReactive(input$go,{
    #   if("const" %in% input$checkbox2){
    #     #if(!is.null(data())){
    #     withProgress(message = 'Making plot', value = 0, {
    #       f <- Q~W
    #       if(isTruthy(input$c_parameter)){
    #       gplm0.fit <- gplm0(f,c_param = as.numeric(input$c_parameter),data())
    #       }else{
    #       gplm0.fit <- gplm0(f,data())
    #       }
    #       #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
    #       return(gplm0.fit)
    #       
    #     })
    #     #}
    #   }
    # })
    # 
    # 
    # plotratingcurve3 <- reactive({
    #   
    #   model=model3()
    #   rclog=NULL
    #   rcraun=NULL
    #   rcleiflog=NULL
    #   rcleifraun=NULL
    #   #tafla=NULL
    #   outputlist=list()
    #   simdata = model$rating_curve_mean
    #   data = model$data
    #   
    #   #plotlist=model1()
    #   cleandata=data()
    #   dummy=as.data.frame(reactiveValuesToList(dummy))
    #   force=as.data.frame(reactiveValuesToList(force))
    #   
    #   outputlist=list()
    #   
    #   if(!is.null(model$data)) {
    #     #observedPrediction=plotlist$observedPrediction
    #     observedPrediction=model$rating_curve_mean$h
    #     #completePrediction=plotlist$completePrediction
    #     observedPrediction=model$rating_curve$h
    #     
    #     # keeprows=vals$keeprows & daterange$keeprows
    #     # 
    #     # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
    #     # 
    #     # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
    #     # 
    #     # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
    #     
    #     name=input$name
    #     
    #     if(nchar(name)==0){
    #       name=gsub("\\.[^.]*$","",input$file$name)
    #     }
    #     
    #     if ("rc" %in% input$checkbox){
    #       rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #       if(any(dim(dummy))){
    #         rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
    #       }
    #       if(any(dim(force))){
    #         rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
    #       }
    #       outputlist$rcraun=rcraun
    #     }
    #     if("rc_tr" %in% input$checkbox){
    #       rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #       
    #       outputlist$rclog=rclog
    #     }
    #     
    #     
    #     if ("res" %in% input$checkbox){
    #       rcleifraun=plot(model,type='residuals')
    #       
    #       outputlist$rcleifraun=rcleifraun
    #     } 
    # 
    #     if("f_h" %in% input$checkbox){
    #     smoothbeta=plot(model,type='f')
    #     outputlist$smoothbeta=smoothbeta
    #     }
    #     
    #     if("sigma_eps" %in% input$checkbox){
    #       sigma_eps = plot(model,type='sigma_eps')
    #       outputlist$sigma_eps=sigma_eps
    #     }
    #     
    #     
    #     tafla=model$rating_curve
    #     # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
    #     # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
    #     # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
    #     # tafla$diffQ=tafla$Q-tafla$Qfit
    #     # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
    #     # tafla=tafla[with(tafla,order(Date)),]
    #     outputlist$tafla=tafla
    #     
    #     return(outputlist)
    #   }
    #   
    #   
    # })   ## gplm ##  Begin
    # model4 <-eventReactive(input$go,{
    #   if("vary" %in% input$checkbox2){
    #     #if(!is.null(data())){
    #     withProgress(message = 'Making plot', value = 0, {
    #       f <- Q~W
    #       if(isTruthy(input$c_parameter)){
    #         gplm.fit <- gplm(f,c_param = as.numeric(input$c_parameter),data())
    #       }else{
    #         gplm.fit <- gplm(f,data())
    #       }
    #       #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
    #       return(gplm.fit)
    #       
    #     })
    #     #}
    #   }
    # })
    # 
    # 
    # plotratingcurve4 <- reactive({
    #   
    #   model=model4()
    #   rclog=NULL
    #   rcraun=NULL
    #   rcleiflog=NULL
    #   rcleifraun=NULL
    #   #tafla=NULL
    #   outputlist=list()
    #   simdata = model$rating_curve_mean
    #   data = model$data
    #   
    #   #plotlist=model1()
    #   cleandata=data()
    #   dummy=as.data.frame(reactiveValuesToList(dummy))
    #   force=as.data.frame(reactiveValuesToList(force))
    #   
    #   outputlist=list()
    #   
    #   if(!is.null(model$data)) {
    #     #observedPrediction=plotlist$observedPrediction
    #     observedPrediction=model$rating_curve_mean$h
    #     #completePrediction=plotlist$completePrediction
    #     observedPrediction=model$rating_curve$h
    #     
    #     # keeprows=vals$keeprows & daterange$keeprows
    #     # 
    #     # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
    #     # 
    #     # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
    #     # 
    #     # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
    #     
    #     name=input$name
    #     
    #     if(nchar(name)==0){
    #       name=gsub("\\.[^.]*$","",input$file$name)
    #     }
    #     
    #     if ("rc" %in% input$checkbox){
    #       rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #       if(any(dim(dummy))){
    #         rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
    #       }
    #       if(any(dim(force))){
    #         rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
    #       }
    #       outputlist$rcraun=rcraun
    #     }
    #     if("rc_tr" %in% input$checkbox){
    #       rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
    #       
    #       outputlist$rclog=rclog
    #     }
    #     
    #     
    #     if ("res" %in% input$checkbox){
    #       rcleifraun=plot(model,type='residuals')
    #       
    #       outputlist$rcleifraun=rcleifraun
    #     } 
    #   
    #     if("f_h" %in% input$checkbox){
    #       smoothbeta=plot(model,type='f')
    #       outputlist$smoothbeta=smoothbeta
    #     }
    #     
    #     if("sigma_eps" %in% input$checkbox){
    #       sigma_eps = plot(model,type='sigma_eps')
    #       outputlist$sigma_eps=sigma_eps
    #     }
    #     
    #     tafla=model$rating_curve
    #     # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
    #     # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
    #     # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
    #     # tafla$diffQ=tafla$Q-tafla$Qfit
    #     # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
    #     # tafla=tafla[with(tafla,order(Date)),]
    #     outputlist$tafla=tafla
    #     
    #     
    #     return(outputlist)
    #   }
    #   
    # })
#'     
#'     
#'   
#'     output$plots1 <- renderUI({
#'         if(length(plotratingcurve1())!=0){
#'             plot_output_list <- lapply(1:(length(plotratingcurve1())), function(i) {
#'                 plotname=paste("plot", i, sep="")
#'                 clickname=paste("plot",i,"_click",sep="")
#'                 dblclickname=paste("plot",i,"_dblclick",sep="")
#'                 brushname=paste("plot",i,"_brush",sep="")
#'                 plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
#'                     id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
#'             })
#'             plotOutputlist$textoutput <- textOutput('debug')
#'             do.call(tagList, plot_output_list)
#'         }
#'     })
#'     
#'     output$debug <- renderPrint({
#'       
#'     })
#'     
#'     
#'     output$plot1<-renderPlot({
#'         if(length(plotratingcurve1())!=0)
#' #             sessionkeepermodel1=reactiveValuesToList(sessionkeepermodel1)
#' #             if(counter$i<length(sessionkeepermodel1)){
#' #             counter=reactiveValuesToList(counter)
#' #             i=as.character(counter$i)
#' #             sessionkeepermodel1[[i]][[1]]
#' #         }else{
#'             plotratingcurve1()[[1]]
#'         # }
#'     },height=400,width=600)
#'     
#'     
#'     output$plot2<-renderPlot({
#'         if(length(plotratingcurve1()) >= 2)
#'             plotratingcurve1()[[2]]   
#'     },height=400,width=600)
#'     
#'     
#'     output$plot3<-renderPlot({
#'         if(length(plotratingcurve1())>=3)
#'             plotratingcurve1()[[3]]    
#'     },height=400,width=600)
#'     
#'     
#'     output$plot4<-renderPlot({
#'         if(length(plotratingcurve1())>=4)
#'             plotratingcurve1()[[4]]     
#'     },height=400,width=600)
#'     
#'     
#'     output$plot5<-renderPlot({
#'       if(length(plotratingcurve1())>=5)
#'         plotratingcurve1()[[5]]     
#'     },height=400,width=600)
#'     
#'     
#'     
#'     
#'     
#'     output$TableOfData1 <- renderGvis({
#'         if(!is.null(model1())){
#'             table=model1()$TableOfData
#'             gvisTable(table,options=list(
#'                                 page='enable',
#'                                 pageSize=30,
#'                                 width=550
#'                             ))
#'         }
#'         
#'     })
#'   
#'     
#'     # output$Table1 <- renderGvis({
#'     #     if(!is.null(plotratingcurve1())){
#'     #         Table1=as.data.frame(plotratingcurve1()$tafla)
#'     #         gvisTable(Table1,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     }
#'     # 
#'     # })
#'     
#'     # output$FitTable1 <- renderGvis({
#'     #     if(!is.null(model1())){
#'     #         FitTable1=as.data.frame(model1()$FitTable)
#'     #         gvisTable(FitTable1,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     }
#'     #     
#'     # })
#'     
#'     
#'     # output$LowerTable1 <- renderGvis({
#'     #     if(!is.null(model1())){
#'     #         LowerTable1=as.data.frame(model1()$LowerTable)
#'     #         gvisTable(LowerTable1,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     }
#'     #     
#'     # })
#'     
#'     # output$UpperTable1 <- renderGvis({
#'     #     if(!is.null(model1())){
#'     #         UpperTable1=as.data.frame(model1()$UpperTable)
#'     #         gvisTable(UpperTable1,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     }
#'     # 
#'     # })
#'     
#'     output$plots2 <- renderUI({
#'         if(length(plotratingcurve2())!=0){
#'             plot_output_list <- lapply(1:(length(plotratingcurve2())-1), function(i) {
#'                 plotname=paste("plot", 5+i, sep="")
#'                 clickname=paste("plot",5+i,"_click",sep="")
#'                 dblclickname=paste("plot",5+i,"_dblclick",sep="")
#'                 brushname=paste("plot",5+i,"_brush",sep="")
#'                 plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
#'                     id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
#'             })
#'             #plot_output_list$Beta=plotOutput('Beta',click ='Beta_click',dblclick = dblclickOpts(
#'             #    id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
#'             
#'             do.call(tagList, plot_output_list)
#'         }
#'     })
#'     
#'     
#'     
#'     
#'     
#' 
#'     
#'     output$plot6<-renderPlot({
#'         if(length(plotratingcurve2())!=0)
#'             plotratingcurve2()[[1]]
#'     },height=400,width=600)
#'     
#'     
#'     output$plot7<-renderPlot({
#'         if(length(plotratingcurve2()) >= 2)
#'             plotratingcurve2()[[2]]   
#'     },height=400,width=600)
#'     
#'     
#'     output$plot8<-renderPlot({
#'         if(length(plotratingcurve2())>=3)
#'             plotratingcurve2()[[3]]    
#'     },height=400,width=600)
#'     
#'     
#'     output$plot9<-renderPlot({
#'         if(length(plotratingcurve2())>=4)
#'             plotratingcurve2()[[4]]     
#'     },height=400,width=600)
#'     
#'     output$plot10<-renderPlot({
#'       if(length(plotratingcurve2())>=5)
#'         plotratingcurve2()[[5]]     
#'     },height=400,width=600)
#'     
#' 
#'     
#'     
#'     
#'     
#'     
#'     # output$Beta <- renderPlot({
#'     #     if(!is.null(plotratingcurve2()))
#'     #         plotratingcurve2()$smoothbeta
#'     # },height=400,width=600)
#'     # 
#'     
#'     
#'     # output$TableOfData2 <- renderGvis({
#'     #     if(!is.null(model2())){
#'     #         table=as.data.frame(model2()$TableOfData)
#'     #         gvisTable(table,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     }
#'     #     
#'     #     
#'     # })
#'     
#'     
#'     # output$FitTable2 <- renderGvis({
#'     #     if(!is.null(model2())){
#'     #         FitTable2=as.data.frame(model2()$FitTable)
#'     #         gvisTable(FitTable2,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #     
#'     #     } 
#'     # })
#'     
#'     
#'     # output$LowerTable2 <- renderGvis({
#'     #     if(!is.null(model2())){
#'     #         LowerTable2=as.data.frame(model2()$LowerTable)
#'     #         gvisTable(LowerTable2,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #         
#'     #     } 
#'     # })
#'     
#'     # output$UpperTable2 <- renderGvis({
#'     #     if(!is.null(model2())){
#'     #         UpperTable2=as.data.frame(model2()$UpperTable)
#'     #         gvisTable(UpperTable2,options=list(
#'     #             page='enable',
#'     #             pageSize=30,
#'     #             width=550
#'     #         ))
#'     #         
#'     #     } 
#'     # })
#'     # 
#'     
#'     
#'     
#'     
#'     output$plots3 <- renderUI({
#'       if(length(plotratingcurve3())!=0){
#'         plot_output_list <- lapply(1:(length(plotratingcurve3())-1), function(i) {
#'           plotname=paste("plot", 10+i, sep="")
#'           clickname=paste("plot",10+i,"_click",sep="")
#'           dblclickname=paste("plot",10+i,"_dblclick",sep="")
#'           brushname=paste("plot",10+i,"_brush",sep="")
#'           plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
#'             id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
#'         })
#'         # plot_output_list$Beta2=plotOutput('Beta2',click ='Beta_click',dblclick = dblclickOpts(
#'         #   id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
#'         
#'         do.call(tagList, plot_output_list)
#'       }
#'     })
#'     
#'     
#'     output$plot11<-renderPlot({
#'       if(length(plotratingcurve3())!=0)
#'         plotratingcurve3()[[1]]
#'     },height=400,width=600)
#'     
#'     
#'     output$plot12<-renderPlot({
#'       if(length(plotratingcurve3()) >= 2)
#'         plotratingcurve3()[[2]]   
#'     },height=400,width=600)
#'     
#'     
#'     output$plot13<-renderPlot({
#'       if(length(plotratingcurve3())>=3)
#'         plotratingcurve3()[[3]]    
#'     },height=400,width=600)
#'     
#'     
#'     output$plot14<-renderPlot({
#'       if(length(plotratingcurve3())>=4)
#'         plotratingcurve3()[[4]]     
#'     },height=400,width=600)
#'     
#'     
#'     output$plot15<-renderPlot({
#'       if(length(plotratingcurve3())>=5)
#'         plotratingcurve3()[[5]]     
#'     },height=400,width=600)
#'     
#'     
#'     # output$Beta2 <- renderPlot({
#'     #   if(!is.null(plotratingcurve3()))
#'     #     plotratingcurve3()$smoothbeta
#'     # },height=400,width=600)
#'     # 
#'     
#'     
#'     
#'     output$plots4 <- renderUI({
#'       if(length(plotratingcurve4())!=0){
#'         plot_output_list <- lapply(1:(length(plotratingcurve4())-1), function(i) {
#'           plotname=paste("plot", 15+i, sep="")
#'           clickname=paste("plot",15+i,"_click",sep="")
#'           dblclickname=paste("plot",15+i,"_dblclick",sep="")
#'           brushname=paste("plot",15+i,"_brush",sep="")
#'           plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
#'             id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
#'         })
#'         # plot_output_list$Beta3=plotOutput('Beta3',click ='Beta_click',dblclick = dblclickOpts(
#'         #   id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
#'         
#'         do.call(tagList, plot_output_list)
#'       }
#'     })
#'     
#'     
#'     
#'     
#'     
#'     output$plot16<-renderPlot({
#'       if(length(plotratingcurve4())!=0)
#'         plotratingcurve4()[[1]]
#'     },height=400,width=600)
#'     
#'     
#'     output$plot17<-renderPlot({
#'       if(length(plotratingcurve4()) >= 2)
#'         plotratingcurve4()[[2]]   
#'     },height=400,width=600)
#'     
#'     
#'     output$plot18<-renderPlot({
#'       if(length(plotratingcurve4())>=3)
#'         plotratingcurve4()[[3]]    
#'     },height=400,width=600)
#'     
#'     
#'     output$plot19<-renderPlot({
#'       if(length(plotratingcurve4())>=4)
#'         plotratingcurve4()[[4]]     
#'     },height=400,width=600)
#'     
#'     output$plot20<-renderPlot({
#'       if(length(plotratingcurve4())>=5)
#'         plotratingcurve4()[[5]]     
#'     },height=400,width=600)
#'     
#'     
#'     # output$Beta3 <- renderPlot({
#'     #   if(!is.null(plotratingcurve4()))
#'     #     plotratingcurve4()$smoothbeta
#'     # },height=400,width=600)
#'     # 
#'     
#'     
#'     
#'     #######Interactivity#######
#'     
#'     # If so, zoom to the brush bounds; if not, reset the zoom.
#'     
#' # 
#' #     observeEvent(input$plot1_dblclick, {
#' #         brush <- input$plot1_brush
#' #         if (!is.null(brush)) {
#' #             ranges1$x <- c(brush$xmin, brush$xmax)
#' #             ranges1$y <- c(brush$ymin, brush$ymax)
#' # 
#' #         } else {
#' #             ranges1$x <- NULL
#' #             ranges1$y <- NULL
#' #         }
#' #     })
#' # 
#' #     observeEvent(input$plot5_dblclick, {
#' #         brush <- input$plot5_brush
#' #         if (!is.null(brush)) {
#' #             ranges2$x <- c(brush$xmin, brush$xmax)
#' #             ranges2$y <- c(brush$ymin, brush$ymax)
#' # 
#' #         } else {
#' #             ranges2$x <- NULL
#' #             ranges2$y <- NULL
#' #         }
#' #     })
#' # 
#' # 
#' # 
#' #     observeEvent(input$plot1_click,{
#' #         if("raun"%in% input$checkbox){
#' #             observedData=as.data.frame(data()$observedData_before)
#' #             res <- nearPoints(observedData, input$plot1_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
#' #             if(any(res$selected_) & input$clickopts=='exclude'){
#' #                 vals$keeprows=xor(vals$keeprows,res$selected_)
#' #             }else if(input$clickopts=='force'){
#' #                 force$W=c(force$W,input$plot1_click$y)
#' #                 force$Q=c(force$Q,input$plot1_click$x)
#' #             }else if(input$clickopts=='dummy'){
#' #                 dummy$W=c(dummy$W,input$plot1_click$y)
#' #                 dummy$Q=c(dummy$Q,input$plot1_click$x)
#' #             }
#' #         }
#' # 
#' #     })
#' #     observeEvent(input$plot5_click,{
#' #         if("raun"%in% input$checkbox){
#' #             observedData=data()$observedData_before
#' #             res <- nearPoints(observedData, input$plot5_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
#' #             if(any(res$selected_) & input$clickopts=='exclude'){
#' #                 vals$keeprows=xor(vals$keeprows,res$selected_)
#' #             }else if(input$clickopts=='force'){
#' #                 force$W=c(force$W,input$plot5_click$y)
#' #                 force$Q=c(force$Q,input$plot5_click$x)
#' #             }else if(input$clickopts=='dummy'){
#' #                 dummy$W=c(dummy$W,input$plot5_click$y)
#' #                 dummy$Q=c(dummy$Q,input$plot5_click$x)
#' #             }
#' #         }
#' # 
#' #     })
#' #      counter=reactiveValues(i=1)
#' #     sessionkeepermodel1=reactiveValues()
#' #     sessionkeepermodel2=reactiveValues()
#' #     
#' #      observeEvent(input$go,{
#' #          i=reactiveValuesToList(counter)
#' #          i=as.character(counter$i)
#' #          # if(is.null(sessionkeepermodel1[[i]])){
#' #          sessionkeepermodel1[[i]]=plotratingcurve1()
#' #          #}
#' #          counter$i=counter$i+1
#' #          
#' #      })
#' #      observeEvent(input$go,{
#' #          
#' #          counter=reactiveValuesToList(counter)
#' #          i=as.character(counter$i)
#' #          sessionkeepermodel2[[i]]=plotratingcurve2()
#' #          
#' #      })
#' #     observeEvent(input$forward,{
#' #         counter$i=counter$i+1
#' #         
#' #     })
#' #     observeEvent(input$back,{
#' #         counter$i=counter$i-1
#' #     })
#'     # observeEvent(input$reset,{
#'     #     n=nrow(data()$observedData_before)
#'     #     vals$keeprows=rep(TRUE,n)
#'     #     dummy$W=NULL
#'     #     dummy$Q=NULL
#'     #     force$W=NULL
#'     #     force$Q=NULL
#'     # })
#'     
#'      # output$xlsxexport <- downloadHandler(
#'      #     filename= function(){
#'      #         name=input$name
#'      #         if(nchar(name)==0){
#'      #             name=gsub("\\.[^.]*$","",input$file$name)
#'      #         }
#'      #         paste(name,'xlsx',sep=".")
#'      #     },
#'      #     content = function(file){
#'      #         wb <- createWorkbook()
#'      #         saveWorkbook(wb,file)
#'      #         tablelist=list()
#'      #         if(!is.null(plotratingcurve1())){
#'      #             tablelist$TableOfData1=plotratingcurve1()$tafla
#'      #             # tablelist$FitTable1=model1()$FitTable
#'      #             # tablelist$LowerTable1=model1()$LowerTable
#'      #             # tablelist$UpperTable1=model1()$UpperTable
#'      #             # tablelist$plotTable1=model1()$plotTable
#'      #         }
#'      #         if(!is.null(plotratingcurve2)){
#'      #           tablelist$TableOfData1=plotratingcurve2()$tafla
#'      #            #  tablelist$FitTable2=model2()$FitTable
#'      #            # tablelist$LowerTable2=model2()$LowerTable
#'      #            #  tablelist$UpperTable2=model2()$UpperTable
#'      #            #  tablelist$plotTable2=model2()$plotTable
#'      #         }
#'      #         if(!is.null(plotratingcurve3)){
#'      #           tablelist$TableOfData1=plotratingcurve3()$tafla
#'      #           #  tablelist$FitTable2=model2()$FitTable
#'      #           # tablelist$LowerTable2=model2()$LowerTable
#'      #           #  tablelist$UpperTable2=model2()$UpperTable
#'      #           #  tablelist$plotTable2=model2()$plotTable
#'      #         }
#'      #         if(!is.null(plotratingcurve4)){
#'      #           tablelist$TableOfData1=plotratingcurve4()$tafla
#'      #           #  tablelist$FitTable2=model2()$FitTable
#'      #           # tablelist$LowerTable2=model2()$LowerTable
#'      #           #  tablelist$UpperTable2=model2()$UpperTable
#'      #           #  tablelist$plotTable2=model2()$plotTable
#'      #         }
#'      #         lapply(names(tablelist),function(x) write.xlsx(tablelist[[x]],file,sheetName=x,append=TRUE,row.names=FALSE))
#'      #     }
#'      # )
#'      # # 
#'      # 
#'      # 
#'      output$downloadImages <- downloadHandler(
#'          filename = function() {
#'              filename=input$name
#'              if(nchar(filename)==0){
#'                  filename=gsub("\\.[^.]*$","",input$file$name)
#'              }
#'              paste(filename,'html', sep=".")
#'          },
#'          content <- function(file) {
#'              owd <- setwd(tempdir())
#'              on.exit(setwd(owd))
#'              setwd(owd)
#' 
#'                  src <- normalizePath('images.Rmd')
#'                  file.copy(src, 'images.Rmd')
#'                  out <- render('images.Rmd',html_document())
#'                  file.rename(out, file)
#'          }
#' 
#'      )
#' 
#'      # 
#'      # output$downloadReport <- downloadHandler(
#'      #         filename = function() {
#'      #             filename=input$name
#'      #             if(nchar(filename)==0){
#'      #             filename=gsub("\\.[^.]*$","",input$file$name)
#'      #             }
#'      #             paste(filename,'pdf', sep=".")
#'      #         },
#'      #         content <- function(file) {
#'      #             owd <- setwd(tempdir())
#'      #             on.exit(setwd(owd))
#'      #             setwd(owd)
#'      #             if("mdl1" %in% input$checkbox2 & ("mdl2" %in% input$checkbox2)==FALSE ){
#'      #                 src <- normalizePath('myreport1.Rmd')
#'      #                 file.copy(src, 'myreport1.Rmd')
#'      #                 out <- render('myreport1.Rmd',pdf_document())
#'      #                 }
#'      # 
#'      #             else if("mdl2" %in% input$checkbox2 & ("mdl1" %in% input$checkbox2)==FALSE ){
#'      #                 src <- normalizePath('myreport2.Rmd')
#'      #                 file.copy(src, 'myreport2.Rmd')
#'      #                 out <- render('myreport2.Rmd',pdf_document())
#'      #                 }
#'      #             else if("mdl1" %in% input$checkbox2 & "mdl2" %in% input$checkbox2 ){
#'      #                 src <- normalizePath('myreport.Rmd')
#'      #                 file.copy(src, 'myreport.Rmd')
#'      #                 out <- render('myreport.Rmd',pdf_document())
#'      #             }
#'      #             file.rename(out, file)
#'      # 
#'      #         }
#'      # )
#'     
#'     #'Data cleaning
#'     #'
#'     #'Takes in stage-discharge data and cleans it, and subsets it according to user inputs.
#'     #'@param file A string that contains the name of a txt file that contains stage and flow data from a certain river
#'     #'@param advanced Logical,depending if you want to use the advanced settings
#'     #'@param includedates A vector with two integers. The integers represent the year range the user wants to extract from the data.
#'     #'Every datapoint that is not inside that date range will be discarded. Parameter advanced needs to be TRUE.
#'     #'@param dummy A list with information on the dummy point, with elements W and Q, stage and discharge respectively. Parameter advanced needs to be TRUE.
#'     #'@param keeprows A logical vector of the same length as the data which indicates whether to keep a datapoint or not. Parameter advanced needs to be TRUE.
#'     #'@param force A list with information on the force point, with elements W and Q, stage and discharge respectively. Parameter advanced needs to be TRUE.
#'     #'@param shiny Logical, whether the function should read the data as done in shiny or not. Only TRUE when used in a shiny interface.
#'     #'@param Wmin Numeric, minimum stage of rating curve.
#'     #'@param Wmax Numeric, maximum stage of rating curve.
#'     #'@param exclude Logical depending on whether the user wants to exclude a date range from the data or not.
#'     #'@param excludedates Vector with two Date values of the form %Y-%m-%d. The dates span the range which to exclude from the data. Parameters advanced and exclude need to be true.
#'     #'@return Returns a list of objects that ar the input to model1BH and model1BH, especially the cleaned input data. Other outputs are a matrix of stage and discharge values
#'     #' and the cleaned data before it changes due to the interactive elements such as dummypoint,forcepoint,keeprows etc.
#'     #'@references Birgir Hrafnkelsson, Helgi Sigurdarson and Sigurdur M. Gardarson (2015) \emph{Bayesian Generalized Rating Curves}
#'     
#'     
#'     clean <- function(file,advanced=TRUE,includedates=c(1950,as.numeric(format(Sys.Date(), "%Y"))),dummy=NULL,keeprows=NULL,force=NULL,shiny=FALSE,Wmin=NA,Wmax=NA, exclude=TRUE){
#'       suppressPackageStartupMessages(require(readxl))
#'       
#'       if (is.null(file)){
#'         return(NULL)
#'       }
#'       name=file
#'       if(shiny==TRUE){
#'         list2env(file,envir=environment())
#'         
#'         if(type=='text/plain'){
#'           observedData=read.table(datapath,skip=2,sep="|",dec=",")
#'           observedData=observedData[,c(2,3,5,7,4)]
#'           names(observedData)=c("Date","Time","Quality","W","Q")
#'           observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
#'           #else if(type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
#'           #TODO skoða hvernig read_excel breytti clean fallinu
#'           #Notum readxl pakkann
#'         }else if(type=="list"){
#'           observedData=readxl::read_xlsx( path = datapath ,sheet = 1 )
#'           names(observedData)=c("Date","Time","Quality","W","Q")
#'           
#'         }else{return(NULL)}
#'         
#'       }else{
#'         if(gsub(".*\\.", "",file)=='txt'){
#'           observedData=read.table(file,skip=2,sep="|",dec=",")
#'           observedData=observedData[,c(2,3,5,7,4)]
#'           #added
#'           names(observedData)=c("Date","Time","Quality","W","Q")
#'           observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
#'         }else if(gsub(".*\\.", "",file)=='xlsx'){
#'           observedData=readxl::read_xlsx( path = file ,sheet = 1 )
#'           #added
#'           names(observedData)=c("Date","Time","Quality","W","Q")
#'           observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
#'         }else{return(NULL)}
#'       }
#'       observedData$Time=as.character(observedData$Time)
#'       observedData$Q=gsub('\\s+', '',observedData$Q)
#'       observedData=observedData[observedData$W!=0,]
#'       observedData$Q=as.numeric(as.character(gsub(",",".",observedData$Q)))
#'       observedData$W=0.01*observedData$W
#'       observedData=observedData[with(observedData,order(W)),]
#'       observedData_before=observedData
#'       
#'       if(advanced==TRUE){
#'         if(length(keeprows)!=0){
#'           observedData=observedData[keeprows,]
#'         }
#' 
#'         
#'         if(sum(unlist(lapply(dummy,length)))!=0){
#'           dummydata=as.data.frame(dummy)
#'           dummydata=round(dummydata,3)
#'           dummydata$Date=Sys.Date()
#'           dummydata$Time=format(Sys.time(),"%H:%M:%S")
#'           dummydata$Quality="dummy"
#'           dummydata=dummydata[,c("Date","Time","Quality","W","Q")]
#'           observedData=rbind(observedData,dummydata)
#'           
#'         }
#'         if(sum(unlist(lapply(force,length)))!=0){
#'           forcedata=as.data.frame(force)
#'           forcedata=round(forcedata,3)
#'           forcedata$Date=Sys.Date()
#'           forcedata$Time=format(Sys.time(),"%H:%M:%S")
#'           forcedata$Quality="forcepoint"
#'           forcedata=forcedata[,c("Date","Time","Quality","W","Q")]
#'           observedData=rbind(observedData,forcedata)
#'         }
#'       }
#'       #order again with new data from dummy or force
#'       observedData=observedData[with(observedData,order(W)),]
#'       if(is.na(Wmin)) Wmin=min(observedData$W)
#'       if(is.na(Wmax)) Wmax=max(observedData$W)
#'       observedData=subset(observedData,W >= Wmin & W <=Wmax )
#'       wq=as.matrix(observedData[,c("W","Q")])
#'       
#'       return(list("wq"=wq,"observedData"=observedData,"observedData_before"=observedData_before))
#'     }
#'     
#'     
#'     # data_tournament <- eventReactive(input$tournament_btn,{
#'        
#'        
#'         #output$text
#'     # TableTour=tournament(Q~W,data())$summary
#'     # #   #tournament <- tournament(Q~W,V316_river)
#'     # #   if(isTruthy(input$file)){
#'     # # text_out <- capture.output(tournament(Q~W,data())$summary)
#'     # #   }else{
#'     # #     text_out = 'Input File!'
#'     # #   }
#'     # return(TableTour)
#'     # # 
#'     #  })
#'     
#'     # output$TableTournament <- renderGvis({
#'     #       if(isTruthy(input$file2)){
#'     #           gvisTable(data_tournament(),options=list(
#'     #               page='enable',
#'     #               pageSize=30,
#'     #               width=550
#'     #           ))
#'     #       }
#'     # 
#'     #   })
#'     
#'     
#'     outputOptions(output, "plots1", suspendWhenHidden = FALSE)
#'     outputOptions(output, "plots2", suspendWhenHidden = FALSE)
#'     outputOptions(output, "plots3", suspendWhenHidden = FALSE)
#'     outputOptions(output, "plots4", suspendWhenHidden = FALSE)

})


