suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(googleVis))
library(bdrc)
library(ggplot2)
library(xlsx)
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
        #cleandata=clean(input$file,dummy=dummy,force=force,keeprows=vals$keeprows,shiny=TRUE,advanced=input$advanced)
        inFile <- input$file
        cleandata = read.xlsx(file = inFile$datapath, sheetIndex = 1)
        wq=cleandata[,c("W","Q")]
        wq$W=0.01*wq$W
        #if(length(vals$keeprows)==0 ){
        #vals$keeprows= rep(TRUE,nrow(cleandata$observedData_before))
        #}
               # years=as.numeric(format(cleandata$observedData_before$Date, "%Y"))
               # includeindex=years<=input$includeDates[2] & years >= input$includeDates[1]
               # excludeindex=cleandata$observedData_before$Date<=input$excludeDates[1] | cleandata$ observedData_before$Date >= input$excludeDates[2]
               # daterange$keeprows=excludeindex & includeindex
        return(wq)
         
    })
    
    ## MODEL1 ##  Begin
    model1 <-eventReactive(input$go,{
        if("plm" %in% input$checkbox2){
            #if(!is.null(data())){
                withProgress(message = 'Making plot', value = 0, {
                    f <- Q~W
                   # plm.fit <- plm(f,V316_river)
                    if(isTruthy(input$show_c)){
                      plm.fit <- plm(f,c_param = as.numeric(input$c_parameter),data())
                    }else{
                      plm.fit <- plm(f,data())
                  }                    
                      #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
                    return(plm.fit)
                    
                })
            #}
        }
    })


    plotratingcurve1 <- reactive({
      
      model=model1()
      rclog=NULL
      rcraun=NULL
      rcleiflog=NULL
      rcleifraun=NULL
      #tafla=NULL
      outputlist=list()
      simdata = model$rating_curve_mean
      data = model$data
      
        #plotlist=model1()
        cleandata=data()
        dummy=as.data.frame(reactiveValuesToList(dummy))
        force=as.data.frame(reactiveValuesToList(force))
        
        outputlist=list()
        
        if(!is.null(model$data)) {
            #observedPrediction=plotlist$observedPrediction
            observedPrediction=model$rating_curve_mean$h
            #completePrediction=plotlist$completePrediction
            observedPrediction=model$rating_curve$h
            
            # keeprows=vals$keeprows & daterange$keeprows
            # 
            # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
            # 
            # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
            # 
            # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
            
            name=input$name
            
            if(nchar(name)==0){
                name=gsub("\\.[^.]*$","",input$file$name)
            }
            
            if ("rc" %in% input$checkbox){
              rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
              if(any(dim(dummy))){
                rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
              }
              if(any(dim(force))){
                rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
              }
              outputlist$rcraun=rcraun
            }
            
            if("rc_tr" %in% input$checkbox){
              rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
              
              outputlist$rclog=rclog
            }
            
            if ("res" %in% input$checkbox){
              rcleifraun=plot(model,type='residuals')
              
              outputlist$rcleifraun=rcleifraun
            } 
            
         
            if("f_h" %in% input$checkbox){
              smoothbeta=plot(model,type='f')
              outputlist$smoothbeta=smoothbeta
            }
            
            if("sigma_eps" %in% input$checkbox){
            sigma_eps = plot(model,type='sigma_eps')
            outputlist$sigma_eps=sigma_eps
            }
           # smoothbeta=plot(model,type='f')
            #outputlist$smoothbeta=smoothbeta
            
            
            tafla=model$rating_curve
            # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
            # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
            # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
            # tafla$diffQ=tafla$Q-tafla$Qfit
            # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
            # tafla=tafla[with(tafla,order(Date)),]
            outputlist$tafla=tafla
            
            
        return(outputlist)
        }
        
    })
    
    
    
    model2 <- eventReactive(input$go,{
        if("plm0" %in% input$checkbox2){
            #if(!is.null(data())){
                withProgress(message = 'Making plot', value = 0, {
                  f <- Q~W
                  if(isTruthy(input$c_parameter)){
                    plm0.fit <- plm0(f,c_param = as.numeric(input$c_parameter),data())
                  }else{
                    plm0.fit <- plm0(f,data())
                  }                    
                  return(plm0.fit)
                    
                })
            #}
        }
    })
    
    plotratingcurve2 <- reactive({
      
        model=model2()
        rclog=NULL
        rcraun=NULL
        rcleiflog=NULL
        rcleifraun=NULL
        #tafla=NULL
        outputlist=list()
        simdata = model$rating_curve_mean
        data = model$data
      
        #plotlist=model2()
        cleandata=data()
        dummy=as.data.frame(reactiveValuesToList(dummy))
        force=as.data.frame(reactiveValuesToList(force))
        TableOfData=NULL
        outputlist=list()
        
        if(!is.null(model$data)) {
          
            #observedPrediction=plotlist$observedPrediction
            observedPrediction=model$rating_curve_mean$h
            #completePrediction=plotlist$completePrediction
            observedPrediction=model$rating_curve$h
            
            #betaData=plotlist$betaData
            betaData=model$beta_summary$h
              
            # keeprows=vals$keeprows & daterange$keeprows
            # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
            # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
            # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
            filename=input$name
            name=input$name
            
            if(nchar(name)==0){
                name=gsub("\\.[^.]*$","",input$file$name)
            }
            
            
            if ("rc" %in% input$checkbox){
                rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
                if(any(dim(dummy))){
                    rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
                }
                if(any(dim(force))){
                    rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
                }
                outputlist$rcraun=rcraun
            }
            if("rc_tr" %in% input$checkbox){
                rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
                
                outputlist$rclog=rclog
            }
            
            
            if ("res" %in% input$checkbox){
                rcleifraun=plot(model,type='residuals')
                
                outputlist$rcleifraun=rcleifraun
            } 
            # if("leiflog" %in% input$checkbox){
            #     # max=max(abs(observedPrediction$standardResiduals))
            #     # if(max>4){
            #     #     ylim=c(-(max+0.2),max+0.2)
            #     # }else{
            #     #     ylim=c(-4,4)
            #     # }
            #     rcleiflog=plot(model,type='residuals',transformed = T)
            #     
            #     
            #     outputlist$rcleiflog=rcleiflog
            # }
            
            if("f_h" %in% input$checkbox){
              smoothbeta=plot(model,type='f')
              outputlist$smoothbeta=smoothbeta
            }
            
            if("sigma_eps" %in% input$checkbox){
              sigma_eps = plot(model,type='sigma_eps')
              outputlist$sigma_eps=sigma_eps
            }
            
            
            tafla=model$rating_curve
            # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
            # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
            # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
            # tafla$diffQ=tafla$Q-tafla$Qfit
            # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
            # tafla=tafla[with(tafla,order(Date)),]
            outputlist$tafla=tafla
            
            return(outputlist)
        } 
        
    })
    
    
    
    ## gplm0 ##  Begin
    
    
    model3 <-eventReactive(input$go,{
      if("gplm0" %in% input$checkbox2){
        #if(!is.null(data())){
        withProgress(message = 'Making plot', value = 0, {
          f <- Q~W
          if(isTruthy(input$c_parameter)){
          gplm0.fit <- gplm0(f,c_param = as.numeric(input$c_parameter),data())
          }else{
          gplm0.fit <- gplm0(f,data())
          }
          #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
          return(gplm0.fit)
          
        })
        #}
      }
    })
    
    
    plotratingcurve3 <- reactive({
      
      model=model3()
      rclog=NULL
      rcraun=NULL
      rcleiflog=NULL
      rcleifraun=NULL
      #tafla=NULL
      outputlist=list()
      simdata = model$rating_curve_mean
      data = model$data
      
      #plotlist=model1()
      cleandata=data()
      dummy=as.data.frame(reactiveValuesToList(dummy))
      force=as.data.frame(reactiveValuesToList(force))
      
      outputlist=list()
      
      if(!is.null(model$data)) {
        #observedPrediction=plotlist$observedPrediction
        observedPrediction=model$rating_curve_mean$h
        #completePrediction=plotlist$completePrediction
        observedPrediction=model$rating_curve$h
        
        # keeprows=vals$keeprows & daterange$keeprows
        # 
        # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
        # 
        # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
        # 
        # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
        
        name=input$name
        
        if(nchar(name)==0){
          name=gsub("\\.[^.]*$","",input$file$name)
        }
        
        if ("rc" %in% input$checkbox){
          rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
          if(any(dim(dummy))){
            rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
          }
          if(any(dim(force))){
            rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
          }
          outputlist$rcraun=rcraun
        }
        if("rc_tr" %in% input$checkbox){
          rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
          
          outputlist$rclog=rclog
        }
        
        
        if ("res" %in% input$checkbox){
          rcleifraun=plot(model,type='residuals')
          
          outputlist$rcleifraun=rcleifraun
        } 

        if("f_h" %in% input$checkbox){
        smoothbeta=plot(model,type='f')
        outputlist$smoothbeta=smoothbeta
        }
        
        if("sigma_eps" %in% input$checkbox){
          sigma_eps = plot(model,type='sigma_eps')
          outputlist$sigma_eps=sigma_eps
        }
        
        
        tafla=model$rating_curve
        # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
        # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
        # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
        # tafla$diffQ=tafla$Q-tafla$Qfit
        # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
        # tafla=tafla[with(tafla,order(Date)),]
        outputlist$tafla=tafla
        
        return(outputlist)
      }
      
      
    })   ## gplm ##  Begin
    model4 <-eventReactive(input$go,{
      if("gplm" %in% input$checkbox2){
        #if(!is.null(data())){
        withProgress(message = 'Making plot', value = 0, {
          f <- Q~W
          if(isTruthy(input$c_parameter)){
            gplm.fit <- gplm(f,c_param = as.numeric(input$c_parameter),data())
          }else{
            gplm.fit <- gplm(f,data())
          }
          #model1BH(data(),country=input$select,Wmin="",Wmax=input$Wmax)
          return(gplm.fit)
          
        })
        #}
      }
    })
    
    
    plotratingcurve4 <- reactive({
      
      model=model4()
      rclog=NULL
      rcraun=NULL
      rcleiflog=NULL
      rcleifraun=NULL
      #tafla=NULL
      outputlist=list()
      simdata = model$rating_curve_mean
      data = model$data
      
      #plotlist=model1()
      cleandata=data()
      dummy=as.data.frame(reactiveValuesToList(dummy))
      force=as.data.frame(reactiveValuesToList(force))
      
      outputlist=list()
      
      if(!is.null(model$data)) {
        #observedPrediction=plotlist$observedPrediction
        observedPrediction=model$rating_curve_mean$h
        #completePrediction=plotlist$completePrediction
        observedPrediction=model$rating_curve$h
        
        # keeprows=vals$keeprows & daterange$keeprows
        # 
        # keep=cleandata$observedData_before[keeprows, ,drop=FALSE]
        # 
        # excludeManual=cleandata$observedData_before[!vals$keeprows, ,drop=FALSE]
        # 
        # excludeYears=cleandata$observedData_before[!daterange$keeprows, ,drop=FALSE]
        
        name=input$name
        
        if(nchar(name)==0){
          name=gsub("\\.[^.]*$","",input$file$name)
        }
        
        if ("rc" %in% input$checkbox){
          rcraun=plot(model,type = 'rating_curve')+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
          if(any(dim(dummy))){
            rcraun=rcraun+geom_point(data=dummy,aes(Q,W),fill="red",col="red")
          }
          if(any(dim(force))){
            rcraun=rcraun+geom_point(data=force,aes(Q,W),fill="blue",col="blue")
          }
          outputlist$rcraun=rcraun
        }
        if("rc_tr" %in% input$checkbox){
          rclog=plot(model,type = 'rating_curve',transformed=T)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
          
          outputlist$rclog=rclog
        }
        
        
        if ("res" %in% input$checkbox){
          rcleifraun=plot(model,type='residuals')
          
          outputlist$rcleifraun=rcleifraun
        } 
      
        if("f_h" %in% input$checkbox){
          smoothbeta=plot(model,type='f')
          outputlist$smoothbeta=smoothbeta
        }
        
        if("sigma_eps" %in% input$checkbox){
          sigma_eps = plot(model,type='sigma_eps')
          outputlist$sigma_eps=sigma_eps
        }
        
        tafla=model$rating_curve
        # tafla$Qfit=as.numeric(format(round(as.vector(exp(data$fit)),3)))
        # tafla$lower=as.numeric(format(round(exp(data$lower),3)))
        # tafla$upper=as.numeric(format(round(exp(data$upper),3)))
        # tafla$diffQ=tafla$Q-tafla$Qfit
        # names(tafla)=c("Date","Time","W","Q", "Q fit","Lower", "Upper","Q diff")
        # tafla=tafla[with(tafla,order(Date)),]
        outputlist$tafla=tafla
        
        
        return(outputlist)
      }
      
    })
    
    
    
    
    output$plots1 <- renderUI({
        if(length(plotratingcurve1())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve1())), function(i) {
                plotname=paste("plot", i, sep="")
                clickname=paste("plot",i,"_click",sep="")
                dblclickname=paste("plot",i,"_dblclick",sep="")
                brushname=paste("plot",i,"_brush",sep="")
                plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
                    id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
            })
            
            do.call(tagList, plot_output_list)
        }
    })
    
    
    output$plot1<-renderPlot({
        if(length(plotratingcurve1())!=0)
#             sessionkeepermodel1=reactiveValuesToList(sessionkeepermodel1)
#             if(counter$i<length(sessionkeepermodel1)){
#             counter=reactiveValuesToList(counter)
#             i=as.character(counter$i)
#             sessionkeepermodel1[[i]][[1]]
#         }else{
            plotratingcurve1()[[1]]
        # }
    },height=400,width=600)
    
    
    output$plot2<-renderPlot({
        if(length(plotratingcurve1()) >= 2)
            plotratingcurve1()[[2]]   
    },height=400,width=600)
    
    
    output$plot3<-renderPlot({
        if(length(plotratingcurve1())>=3)
            plotratingcurve1()[[3]]    
    },height=400,width=600)
    
    
    output$plot4<-renderPlot({
        if(length(plotratingcurve1())>=4)
            plotratingcurve1()[[4]]     
    },height=400,width=600)
    
    
    output$plot5<-renderPlot({
      if(length(plotratingcurve1())>=5)
        plotratingcurve1()[[5]]     
    },height=400,width=600)
    
    
    
    
    
    output$TableOfData1 <- renderGvis({
        if(!is.null(model1())){
            table=model1()$TableOfData
            gvisTable(table,options=list(
                                page='enable',
                                pageSize=30,
                                width=550
                            ))
        }
        
    })
  
    
    # output$Table1 <- renderGvis({
    #     if(!is.null(plotratingcurve1())){
    #         Table1=as.data.frame(plotratingcurve1()$tafla)
    #         gvisTable(Table1,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     }
    # 
    # })
    
    # output$FitTable1 <- renderGvis({
    #     if(!is.null(model1())){
    #         FitTable1=as.data.frame(model1()$FitTable)
    #         gvisTable(FitTable1,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     }
    #     
    # })
    
    
    # output$LowerTable1 <- renderGvis({
    #     if(!is.null(model1())){
    #         LowerTable1=as.data.frame(model1()$LowerTable)
    #         gvisTable(LowerTable1,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     }
    #     
    # })
    
    # output$UpperTable1 <- renderGvis({
    #     if(!is.null(model1())){
    #         UpperTable1=as.data.frame(model1()$UpperTable)
    #         gvisTable(UpperTable1,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     }
    # 
    # })
    
    output$plots2 <- renderUI({
        if(length(plotratingcurve2())!=0){
            plot_output_list <- lapply(1:(length(plotratingcurve2())-1), function(i) {
                plotname=paste("plot", 5+i, sep="")
                clickname=paste("plot",5+i,"_click",sep="")
                dblclickname=paste("plot",5+i,"_dblclick",sep="")
                brushname=paste("plot",5+i,"_brush",sep="")
                plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
                    id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
            })
            #plot_output_list$Beta=plotOutput('Beta',click ='Beta_click',dblclick = dblclickOpts(
            #    id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
            
            do.call(tagList, plot_output_list)
        }
    })
    
    
    
    
    

    
    output$plot6<-renderPlot({
        if(length(plotratingcurve2())!=0)
            plotratingcurve2()[[1]]
    },height=400,width=600)
    
    
    output$plot7<-renderPlot({
        if(length(plotratingcurve2()) >= 2)
            plotratingcurve2()[[2]]   
    },height=400,width=600)
    
    
    output$plot8<-renderPlot({
        if(length(plotratingcurve2())>=3)
            plotratingcurve2()[[3]]    
    },height=400,width=600)
    
    
    output$plot9<-renderPlot({
        if(length(plotratingcurve2())>=4)
            plotratingcurve2()[[4]]     
    },height=400,width=600)
    
    output$plot10<-renderPlot({
      if(length(plotratingcurve2())>=5)
        plotratingcurve2()[[5]]     
    },height=400,width=600)
    

    
    
    
    
    
    # output$Beta <- renderPlot({
    #     if(!is.null(plotratingcurve2()))
    #         plotratingcurve2()$smoothbeta
    # },height=400,width=600)
    # 
    
    
    # output$TableOfData2 <- renderGvis({
    #     if(!is.null(model2())){
    #         table=as.data.frame(model2()$TableOfData)
    #         gvisTable(table,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     }
    #     
    #     
    # })
    
    
    # output$FitTable2 <- renderGvis({
    #     if(!is.null(model2())){
    #         FitTable2=as.data.frame(model2()$FitTable)
    #         gvisTable(FitTable2,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #     
    #     } 
    # })
    
    
    # output$LowerTable2 <- renderGvis({
    #     if(!is.null(model2())){
    #         LowerTable2=as.data.frame(model2()$LowerTable)
    #         gvisTable(LowerTable2,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #         
    #     } 
    # })
    
    # output$UpperTable2 <- renderGvis({
    #     if(!is.null(model2())){
    #         UpperTable2=as.data.frame(model2()$UpperTable)
    #         gvisTable(UpperTable2,options=list(
    #             page='enable',
    #             pageSize=30,
    #             width=550
    #         ))
    #         
    #     } 
    # })
    # 
    
    
    
    
    output$plots3 <- renderUI({
      if(length(plotratingcurve3())!=0){
        plot_output_list <- lapply(1:(length(plotratingcurve3())-1), function(i) {
          plotname=paste("plot", 10+i, sep="")
          clickname=paste("plot",10+i,"_click",sep="")
          dblclickname=paste("plot",10+i,"_dblclick",sep="")
          brushname=paste("plot",10+i,"_brush",sep="")
          plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
            id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
        })
        # plot_output_list$Beta2=plotOutput('Beta2',click ='Beta_click',dblclick = dblclickOpts(
        #   id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
        
        do.call(tagList, plot_output_list)
      }
    })
    
    
    output$plot11<-renderPlot({
      if(length(plotratingcurve3())!=0)
        plotratingcurve3()[[1]]
    },height=400,width=600)
    
    
    output$plot12<-renderPlot({
      if(length(plotratingcurve3()) >= 2)
        plotratingcurve3()[[2]]   
    },height=400,width=600)
    
    
    output$plot13<-renderPlot({
      if(length(plotratingcurve3())>=3)
        plotratingcurve3()[[3]]    
    },height=400,width=600)
    
    
    output$plot14<-renderPlot({
      if(length(plotratingcurve3())>=4)
        plotratingcurve3()[[4]]     
    },height=400,width=600)
    
    
    output$plot15<-renderPlot({
      if(length(plotratingcurve3())>=5)
        plotratingcurve3()[[5]]     
    },height=400,width=600)
    
    
    # output$Beta2 <- renderPlot({
    #   if(!is.null(plotratingcurve3()))
    #     plotratingcurve3()$smoothbeta
    # },height=400,width=600)
    # 
    
    
    
    
    
    
    
    
    output$plots4 <- renderUI({
      if(length(plotratingcurve4())!=0){
        plot_output_list <- lapply(1:(length(plotratingcurve4())-1), function(i) {
          plotname=paste("plot", 15+i, sep="")
          clickname=paste("plot",15+i,"_click",sep="")
          dblclickname=paste("plot",15+i,"_dblclick",sep="")
          brushname=paste("plot",15+i,"_brush",sep="")
          plotOutput(plotname,click =clickname,dblclick = dblclickOpts(
            id = dblclickname),brush = brushOpts(id = brushname,resetOnNew = TRUE))
        })
        # plot_output_list$Beta3=plotOutput('Beta3',click ='Beta_click',dblclick = dblclickOpts(
        #   id = 'Beta_dblclick'),brush = brushOpts(id = 'Beta_brush',resetOnNew = TRUE))
        
        do.call(tagList, plot_output_list)
      }
    })
    
    
    
    
    
    output$plot16<-renderPlot({
      if(length(plotratingcurve4())!=0)
        plotratingcurve4()[[1]]
    },height=400,width=600)
    
    
    output$plot17<-renderPlot({
      if(length(plotratingcurve4()) >= 2)
        plotratingcurve4()[[2]]   
    },height=400,width=600)
    
    
    output$plot18<-renderPlot({
      if(length(plotratingcurve4())>=3)
        plotratingcurve4()[[3]]    
    },height=400,width=600)
    
    
    output$plot19<-renderPlot({
      if(length(plotratingcurve4())>=4)
        plotratingcurve4()[[4]]     
    },height=400,width=600)
    
    output$plot20<-renderPlot({
      if(length(plotratingcurve4())>=5)
        plotratingcurve4()[[5]]     
    },height=400,width=600)
    
    
    # output$Beta3 <- renderPlot({
    #   if(!is.null(plotratingcurve4()))
    #     plotratingcurve4()$smoothbeta
    # },height=400,width=600)
    # 
    
    
    
    #######Interactivity#######
    
    # If so, zoom to the brush bounds; if not, reset the zoom.
    
    
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges1$x <- c(brush$xmin, brush$xmax)
            ranges1$y <- c(brush$ymin, brush$ymax)

        } else {
            ranges1$x <- NULL
            ranges1$y <- NULL
        }
    })
    
    observeEvent(input$plot5_dblclick, {
        brush <- input$plot5_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)

        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })

    
    
    observeEvent(input$plot1_click,{
        if("raun"%in% input$checkbox){
            observedData=as.data.frame(data()$observedData_before)
            res <- nearPoints(observedData, input$plot1_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
            if(any(res$selected_) & input$clickopts=='exclude'){
                vals$keeprows=xor(vals$keeprows,res$selected_)
            }else if(input$clickopts=='force'){
                force$W=c(force$W,input$plot1_click$y)
                force$Q=c(force$Q,input$plot1_click$x)
            }else if(input$clickopts=='dummy'){
                dummy$W=c(dummy$W,input$plot1_click$y)
                dummy$Q=c(dummy$Q,input$plot1_click$x)
            }
        }
        
    })
    observeEvent(input$plot5_click,{
        if("raun"%in% input$checkbox){
            observedData=data()$observedData_before
            res <- nearPoints(observedData, input$plot5_click,xvar = "Q", yvar = "W", allRows = TRUE,threshold=5)
            if(any(res$selected_) & input$clickopts=='exclude'){
                vals$keeprows=xor(vals$keeprows,res$selected_)
            }else if(input$clickopts=='force'){
                force$W=c(force$W,input$plot5_click$y)
                force$Q=c(force$Q,input$plot5_click$x)
            }else if(input$clickopts=='dummy'){
                dummy$W=c(dummy$W,input$plot5_click$y)
                dummy$Q=c(dummy$Q,input$plot5_click$x)
            }
        }
        
    })
#      counter=reactiveValues(i=1)
#     sessionkeepermodel1=reactiveValues()
#     sessionkeepermodel2=reactiveValues()
#     
#      observeEvent(input$go,{
#          i=reactiveValuesToList(counter)
#          i=as.character(counter$i)
#          # if(is.null(sessionkeepermodel1[[i]])){
#          sessionkeepermodel1[[i]]=plotratingcurve1()
#          #}
#          counter$i=counter$i+1
#          
#      })
#      observeEvent(input$go,{
#          
#          counter=reactiveValuesToList(counter)
#          i=as.character(counter$i)
#          sessionkeepermodel2[[i]]=plotratingcurve2()
#          
#      })
#     observeEvent(input$forward,{
#         counter$i=counter$i+1
#         
#     })
#     observeEvent(input$back,{
#         counter$i=counter$i-1
#     })
    observeEvent(input$reset,{
        n=nrow(data()$observedData_before)
        vals$keeprows=rep(TRUE,n)
        dummy$W=NULL
        dummy$Q=NULL
        force$W=NULL
        force$Q=NULL
    })
    
     output$xlsxexport <- downloadHandler(
         filename= function(){
             name=input$name
             if(nchar(name)==0){
                 name=gsub("\\.[^.]*$","",input$file$name)
             }
             paste(name,'xlsx',sep=".")
         },
         content = function(file){
             wb <- createWorkbook()
             saveWorkbook(wb,file)
             tablelist=list()
             if(!is.null(plotratingcurve1())){
                 tablelist$TableOfData1=plotratingcurve1()$tafla
                 # tablelist$FitTable1=model1()$FitTable
                 # tablelist$LowerTable1=model1()$LowerTable
                 # tablelist$UpperTable1=model1()$UpperTable
                 # tablelist$plotTable1=model1()$plotTable
             }
             if(!is.null(plotratingcurve2)){
               tablelist$TableOfData1=plotratingcurve2()$tafla
                #  tablelist$FitTable2=model2()$FitTable
                # tablelist$LowerTable2=model2()$LowerTable
                #  tablelist$UpperTable2=model2()$UpperTable
                #  tablelist$plotTable2=model2()$plotTable
             }
             if(!is.null(plotratingcurve3)){
               tablelist$TableOfData1=plotratingcurve3()$tafla
               #  tablelist$FitTable2=model2()$FitTable
               # tablelist$LowerTable2=model2()$LowerTable
               #  tablelist$UpperTable2=model2()$UpperTable
               #  tablelist$plotTable2=model2()$plotTable
             }
             if(!is.null(plotratingcurve4)){
               tablelist$TableOfData1=plotratingcurve4()$tafla
               #  tablelist$FitTable2=model2()$FitTable
               # tablelist$LowerTable2=model2()$LowerTable
               #  tablelist$UpperTable2=model2()$UpperTable
               #  tablelist$plotTable2=model2()$plotTable
             }
             lapply(names(tablelist),function(x) write.xlsx(tablelist[[x]],file,sheetName=x,append=TRUE,row.names=FALSE))
         }
     )
     # 
     # 
     # 
     output$downloadImages <- downloadHandler(
         filename = function() {
             filename=input$name
             if(nchar(filename)==0){
                 filename=gsub("\\.[^.]*$","",input$file$name)
             }
             paste(filename,'html', sep=".")
         },
         content <- function(file) {
             owd <- setwd(tempdir())
             on.exit(setwd(owd))
             setwd(owd)

                 src <- normalizePath('images.Rmd')
                 file.copy(src, 'images.Rmd')
                 out <- render('images.Rmd',html_document())
                 file.rename(out, file)
         }

     )

     # 
     # output$downloadReport <- downloadHandler(
     #         filename = function() {
     #             filename=input$name
     #             if(nchar(filename)==0){
     #             filename=gsub("\\.[^.]*$","",input$file$name)
     #             }
     #             paste(filename,'pdf', sep=".")
     #         },
     #         content <- function(file) {
     #             owd <- setwd(tempdir())
     #             on.exit(setwd(owd))
     #             setwd(owd)
     #             if("mdl1" %in% input$checkbox2 & ("mdl2" %in% input$checkbox2)==FALSE ){
     #                 src <- normalizePath('myreport1.Rmd')
     #                 file.copy(src, 'myreport1.Rmd')
     #                 out <- render('myreport1.Rmd',pdf_document())
     #                 }
     # 
     #             else if("mdl2" %in% input$checkbox2 & ("mdl1" %in% input$checkbox2)==FALSE ){
     #                 src <- normalizePath('myreport2.Rmd')
     #                 file.copy(src, 'myreport2.Rmd')
     #                 out <- render('myreport2.Rmd',pdf_document())
     #                 }
     #             else if("mdl1" %in% input$checkbox2 & "mdl2" %in% input$checkbox2 ){
     #                 src <- normalizePath('myreport.Rmd')
     #                 file.copy(src, 'myreport.Rmd')
     #                 out <- render('myreport.Rmd',pdf_document())
     #             }
     #             file.rename(out, file)
     # 
     #         }
     # )
    
    #'Data cleaning
    #'
    #'Takes in stage-discharge data and cleans it, and subsets it according to user inputs.
    #'@param file A string that contains the name of a txt file that contains stage and flow data from a certain river
    #'@param advanced Logical,depending if you want to use the advanced settings
    #'@param includedates A vector with two integers. The integers represent the year range the user wants to extract from the data.
    #'Every datapoint that is not inside that date range will be discarded. Parameter advanced needs to be TRUE.
    #'@param dummy A list with information on the dummy point, with elements W and Q, stage and discharge respectively. Parameter advanced needs to be TRUE.
    #'@param keeprows A logical vector of the same length as the data which indicates whether to keep a datapoint or not. Parameter advanced needs to be TRUE.
    #'@param force A list with information on the force point, with elements W and Q, stage and discharge respectively. Parameter advanced needs to be TRUE.
    #'@param shiny Logical, whether the function should read the data as done in shiny or not. Only TRUE when used in a shiny interface.
    #'@param Wmin Numeric, minimum stage of rating curve.
    #'@param Wmax Numeric, maximum stage of rating curve.
    #'@param exclude Logical depending on whether the user wants to exclude a date range from the data or not.
    #'@param excludedates Vector with two Date values of the form %Y-%m-%d. The dates span the range which to exclude from the data. Parameters advanced and exclude need to be true.
    #'@return Returns a list of objects that ar the input to model1BH and model1BH, especially the cleaned input data. Other outputs are a matrix of stage and discharge values
    #' and the cleaned data before it changes due to the interactive elements such as dummypoint,forcepoint,keeprows etc.
    #'@references Birgir Hrafnkelsson, Helgi Sigurdarson and Sigurdur M. Gardarson (2015) \emph{Bayesian Generalized Rating Curves}
    
    
    clean <- function(file,advanced=TRUE,includedates=c(1950,as.numeric(format(Sys.Date(), "%Y"))),dummy=NULL,keeprows=NULL,force=NULL,shiny=FALSE,Wmin=NA,Wmax=NA, exclude=TRUE){
      suppressPackageStartupMessages(require(xlsx))
      
      if (is.null(file)){
        return(NULL)
      }
      name=file
      if(shiny==TRUE){
        list2env(file,envir=environment())
        
        if(type=='text/plain'){
          observedData=read.table(datapath,skip=2,sep="|",dec=",")
          observedData=observedData[,c(2,3,5,7,4)]
          names(observedData)=c("Date","Time","Quality","W","Q")
          observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
          #else if(type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
          #TODO skoða hvernig read_excel breytti clean fallinu
          #Notum readxl pakkann
        }else if(type=="list"){
          observedData=read.xlsx(datapath,sheetIndex=1)
          names(observedData)=c("Date","Time","Quality","W","Q")
          
        }else{return(NULL)}
        
      }else{
        if(gsub(".*\\.", "",file)=='txt'){
          observedData=read.table(file,skip=2,sep="|",dec=",")
          observedData=observedData[,c(2,3,5,7,4)]
          #added
          names(observedData)=c("Date","Time","Quality","W","Q")
          observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
        }else if(gsub(".*\\.", "",file)=='xlsx'){
          observedData=read.xlsx(file,sheetIndex=1)
          #added
          names(observedData)=c("Date","Time","Quality","W","Q")
          observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
        }else{return(NULL)}
      }
      observedData$Time=as.character(observedData$Time)
      observedData$Q=gsub('\\s+', '',observedData$Q)
      observedData=observedData[observedData$W!=0,]
      observedData$Q=as.numeric(as.character(gsub(",",".",observedData$Q)))
      observedData$W=0.01*observedData$W
      observedData=observedData[with(observedData,order(W)),]
      observedData_before=observedData
      
      if(advanced==TRUE){
        if(length(keeprows)!=0){
          observedData=observedData[keeprows,]
        }

        
        if(sum(unlist(lapply(dummy,length)))!=0){
          dummydata=as.data.frame(dummy)
          dummydata=round(dummydata,3)
          dummydata$Date=Sys.Date()
          dummydata$Time=format(Sys.time(),"%H:%M:%S")
          dummydata$Quality="dummy"
          dummydata=dummydata[,c("Date","Time","Quality","W","Q")]
          observedData=rbind(observedData,dummydata)
          
        }
        if(sum(unlist(lapply(force,length)))!=0){
          forcedata=as.data.frame(force)
          forcedata=round(forcedata,3)
          forcedata$Date=Sys.Date()
          forcedata$Time=format(Sys.time(),"%H:%M:%S")
          forcedata$Quality="forcepoint"
          forcedata=forcedata[,c("Date","Time","Quality","W","Q")]
          observedData=rbind(observedData,forcedata)
        }
      }
      #order again with new data from dummy or force
      observedData=observedData[with(observedData,order(W)),]
      if(is.na(Wmin)) Wmin=min(observedData$W)
      if(is.na(Wmax)) Wmax=max(observedData$W)
      observedData=subset(observedData,W >= Wmin & W <=Wmax )
      wq=as.matrix(observedData[,c("W","Q")])
      
      return(list("wq"=wq,"observedData"=observedData,"observedData_before"=observedData_before))
    }
    
    
    # data_tournament <- eventReactive(input$tournament_btn,{
       
       
        #output$text
    # TableTour=tournament(Q~W,data())$summary
    # #   #tournament <- tournament(Q~W,V316_river)
    # #   if(isTruthy(input$file)){
    # # text_out <- capture.output(tournament(Q~W,data())$summary)
    # #   }else{
    # #     text_out = 'Input File!'
    # #   }
    # return(TableTour)
    # # 
    #  })
    
    # output$TableTournament <- renderGvis({
    #       if(isTruthy(input$file2)){
    #           gvisTable(data_tournament(),options=list(
    #               page='enable',
    #               pageSize=30,
    #               width=550
    #           ))
    #       }
    # 
    #   })
    
    
    
    
    
    ####### catalog
    
    catalog <- function(formula,data,c_values=NULL,directory,type=1){
      begin <- Sys.time()
      #argument checking
      error_msg1 <- 'Please provide a formula and data.'
      error_msg2 <- 'The data should have 3 variables: (1) Water stage (2) Water discharge (3) Name variable for seperating the different data sets.'
      error_msg3 <- 'The data should only have 3 variables: (1) Water stage (2) Water discharge (3) Name variable for seperating the different data sets.'
      error_msg4 <- 'Directory does not exist. Please enter a valid directory.'
      error_msg5 <- 'Invalid catalog type. Only two possible catalog types: 1 or 2.'
      error_msg6 <- 'The known c values should be input as a data frame with 2 variables: (1) A name variable and (2) a corresponding c value.'
      if(is.null(formula) | is.null(data)){
        stop(error_msg1)  
      }else if(ncol(data)<3){
        stop(error_msg2)
      }else if(ncol(data)>3){
        stop(error_msg3)
      }else if(!dir.exists(directory)){
        stop(error_msg4)
      }else if(type!=1 & type!=2 & type!=3){
        stop(error_msg5)
      }else if(!is.null(c_values)&ncol(c_values)!=2){
        stop(error_msg6)
      }
      stopifnot('formula' %in% class(formula))
      stopifnot('data.frame' %in% class(data))
      stopifnot('data.frame' %in% class(c_values))
      stopifnot('character' %in% class(directory))
      formula_args <- all.vars(formula)
      stopifnot(length(formula_args)==2 & all(formula_args %in% names(data)))
      #start seting up the data
      main_list_output <- main_list(formula,data)
      snames <- main_list_output[[1]]
      main <- main_list_output[[2]]
      log_file <- main_list_output[[3]]
      if(!is.null(c_values)){
        c_values <- c_list(c_values,snames)
      }
      #run the models
      fits <- list()
      print_load <- T
      b <- 0
      for (i in 1:length(snames)) {
        rc_dat <- data.frame(main$sets[i])
        set.seed(3004)
        fits$gplm[[i]] <- gplm(formula,rc_dat)
        set.seed(3004)
        fits$gplm0[[i]] <- gplm0(formula,rc_dat)
        set.seed(3004)
        fits$plm[[i]] <- plm(formula,rc_dat)
        set.seed(3004)
        fits$plm0[[i]] <- plm0(formula,rc_dat)
        b <- b+1
        if(print_load==T) print(paste(100*round(b/(length(snames)+nrow(c_values)),digits = 3),"%",sep = ""))
      }
      if(!is.null(c_values)){
        fits_c <- list()
        for (i in 1:nrow(c_values)) {
          j <- which(snames %in% c_values$name[i])
          rc_dat <- data.frame(main$sets[j])
          set.seed(3004)
          fits_c$gplm[[j]] <- gplm(formula,rc_dat,c_param = c_values$c[i])
          set.seed(3004)
          fits_c$gplm0[[j]] <- gplm0(formula,rc_dat,c_param = c_values$c[i])
          set.seed(3004)
          fits_c$plm[[j]] <- plm(formula,rc_dat,c_param = c_values$c[i])
          set.seed(3004)
          fits_c$plm0[[j]] <- plm0(formula,rc_dat,c_param = c_values$c[i])
          b <- b+1
          if(print_load==T) print(paste(100*round(b/(length(snames)+nrow(c_values)),digits = 3),"%",sep = ""))
        }
        kc <- which(sapply(1:length(fits_c$gplm),function(x) !is.null(fits_c$gplm[[x]])))
        fits_com <- list()
        for (i in 1:length(snames)) {
          if(i %in% kc){
            fits_com$gplm[[i]] <- fits_c[[1]][[i]]
            fits_com$gplm0[[i]] <- fits_c[[2]][[i]]
            fits_com$plm[[i]] <- fits_c[[3]][[i]]
            fits_com$plm0[[i]] <- fits_c[[4]][[i]]
          }else{
            fits_com$gplm[[i]] <- fits[[1]][[i]]
            fits_com$gplm0[[i]] <- fits[[2]][[i]]
            fits_com$plm[[i]] <- fits[[3]][[i]]
            fits_com$plm0[[i]] <- fits[[4]][[i]]
          }
        }
      }else{ 
        fits_com <- fits
      }
      #start creating the objects to be presented in the pdf
      pmo <- list()
      for (i in 1:length(snames)) {
        fit1 <- fits_com$gplm[[i]]
        fit2 <- fits_com$gplm0[[i]]
        fit3 <- fits_com$plm[[i]]
        fit4 <- fits_com$plm0[[i]]
        if(snames[[i]] %in% c_values$name){c_value <- c_values$c[which(c_values$name %in% snames[[i]])]}else{c_value <- NULL}
        ppo <- page_prep(fit1,fit2,fit3,fit4)
        DICt <- data.frame(gplm=round(fit1$DIC,digits=2),gplm0=round(fit2$DIC,digits=2),plm=round(fit3$DIC,digits=2),plm0=round(fit4$DIC,digits=2))
        pmo$gplm[[i]] <- gplm_page_maker(fit1,snames[i],ppo[[1]],ppo[[2]],ppo[[3]],ppo[[4]],ppo[[5]],ppo[[6]],ppo[[7]],c_value)
        pmo$gplm0[[i]] <- gplm0_page_maker(fit2,snames[i],ppo[[1]],ppo[[2]],ppo[[3]],ppo[[4]],ppo[[5]],ppo[[6]],ppo[[7]],c_value)
        pmo$plm[[i]] <- plm_page_maker(fit3,snames[i],ppo[[1]],ppo[[2]],ppo[[3]],ppo[[4]],ppo[[5]],ppo[[6]],ppo[[7]],c_value)
        pmo$plm0[[i]] <- plm0_page_maker(fit4,snames[i],ppo[[1]],ppo[[2]],ppo[[3]],ppo[[4]],ppo[[5]],ppo[[6]],ppo[[7]],DICt,c_value)
      }
      if(type==1) {
        c_tables <- comparison_table(fits_com,snames)
        p_tables <- predict_tables(fits_com,snames,c_tables)
      }else if(type==2){
        c_tables <- comparison_table(fits_com,snames)           
        mcmc_tables <- mcmc_diag_table(fits_com,snames)
        d_plots <- dev_boxplot(fits_com,snames)
        c_plots <- plot_c_posterior(fits,snames,c_values)   
      }else if(type==3){
        h_plots <- mcmc_plots(fits_com,snames)
        c_tables <- comparison_table(fits_com,snames)
        mcmc_tables <- mcmc_diag_table(fits_com,snames)
        d_plots <- dev_boxplot(fits_com,snames)
        c_plots <- plot_c_posterior(fits,snames,c_values) 
      }
      #start printing the pdf
      pdf(file=paste(directory,'/catalog.pdf',sep = ''),paper = 'a4',width=8,height=11)
      for (i in 1:length(snames)) {
        if(type==1){
          winner <- as.character(c_tables$tour[[i]]$model[5:6][c_tables$tour[[i]]$winner[5:6]])
          grid.arrange(pmo[[winner]][[i]]$plots,pmo[[winner]][[i]]$table,nrow=2,as.table=TRUE,heights=c(5,3))
          p_table <- tableGrob(p_tables[[i]],theme=ttheme_minimal(base_family = "Times"),rows = NULL)
          grid.arrange(p_table,nrow=1,as.table=TRUE,heights=c(1),
                       top=textGrob(paste0('Rating curve predictions for ',as.character(snames[i])),gp=gpar(fontsize=22,facetype='bold',fontfamily="Times")))
        }else if(type==2){
          grid.arrange(pmo$gplm[[i]]$plots, pmo$gplm[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$gplm0[[i]]$plots, pmo$gplm0[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$plm[[i]]$plots, pmo$plm[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$plm0[[i]]$plots, pmo$plm0[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          c_table <- tableGrob(c_tables$tour[[i]], theme=ttheme_minimal(base_family = "Times"),rows = NULL)
          mcmc_table <- tableGrob(mcmc_tables[[i]], theme=ttheme_minimal(base_family = "Times"))
          if(snames[[i]] %in% c_values$name){
            j <- which(c_values$name %in% snames[[i]])
            grid.arrange(c_table,arrangeGrob(d_plots[[i]],arrangeGrob( c_plots$c_plots[[j]], mcmc_table,nrow=2) ,ncol = 2),nrow=2,as.table=TRUE,heights=c(1,2),
                         top=textGrob(paste0('Model comparison for ',as.character(snames[i])),gp=gpar(fontsize=22,facetype='bold',fontfamily="Times")))
          }else{
            grid.arrange(c_table,arrangeGrob(d_plots[[i]],mcmc_table,ncol = 2),nrow=2,as.table=TRUE,heights=c(1,2),
                         top=textGrob(paste0('Model comparison for ',as.character(snames[i])),gp=gpar(fontsize=22,facetype='bold',fontfamily="Times")))
          }
        }else if(type==3){
          grid.arrange(pmo$gplm[[i]]$plots, pmo$gplm[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$gplm0[[i]]$plots, pmo$gplm0[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$plm[[i]]$plots, pmo$plm[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          grid.arrange(pmo$plm0[[i]]$plots, pmo$plm0[[i]]$table, nrow=2,as.table=TRUE,heights=c(5,3))
          c_table <- tableGrob(c_tables$tour[[i]], theme=ttheme_minimal(base_family = "Times"),rows = NULL)
          mcmc_table <- tableGrob(mcmc_tables[[i]], theme=ttheme_minimal(base_family = "Times"))
          if(snames[[i]] %in% c_values$name){
            j <- which(c_values$name %in% snames[[i]])
            grid.arrange(c_table,arrangeGrob(d_plots[[i]],arrangeGrob( c_plots$c_plots[[j]], mcmc_table,nrow=2) ,ncol = 2),nrow=2,as.table=TRUE,heights=c(1,2),
                         top=textGrob(paste0('Model comparison for ',as.character(snames[i])),gp=gpar(fontsize=22,facetype='bold',fontfamily="Times")))
          }else{
            grid.arrange(c_table,arrangeGrob(d_plots[[i]],mcmc_table,ncol = 2),nrow=2,as.table=TRUE,heights=c(1,2),
                         top=textGrob(paste0('Model comparison for ',as.character(snames[i])),gp=gpar(fontsize=22,facetype='bold',fontfamily="Times")))
          }
          grid.arrange(h_plots$gplm[[i]],nrow=2,as.table=TRUE,heights=c(3,1),
                       top=textGrob(paste0('gplm MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
          grid.arrange(h_plots$gplm0[[i]],nrow=2,as.table=TRUE,heights=c(1,1),
                       top=textGrob(paste0('gplm0 MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
          if(snames[[i]] %in% c_values$name){
            grid.arrange(h_plots$plm[[i]],nrow=2,as.table=TRUE,heights=c(3,1),
                         top=textGrob(paste0('plm MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
            grid.arrange(h_plots$plm0[[i]],nrow=2,as.table=TRUE,heights=c(1,3),
                         top=textGrob(paste0('plm0 MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
          }else{
            grid.arrange(h_plots$plm[[i]],nrow=2,as.table=TRUE,heights=c(3,1),
                         top=textGrob(paste0('plm MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
            grid.arrange(h_plots$plm0[[i]],nrow=2,as.table=TRUE,heights=c(2,2),
                         top=textGrob(paste0('plm0 MCMC sampling for ',as.character(snames[i])),gp=gpar(fontsize=20,facetype='bold',fontfamily="Times")))
          }
        }
      }
      dev.off()
      end <- Sys.time()
      t_diff <- end-begin
      print(t_diff)
    }
    
    
    
    ###### catalog_functions
    
    
    R_hat <- function(posterior,num_ch=4){ 
      l <- length(posterior)/(num_ch)
      r <- 1
      s <- l
      #if(length(posterior)%%2!=0){posterior <- posterior[-1]}
      sims <- c()
      for (i in 1:num_ch) {
        sims <- cbind(sims,posterior[r:s])
        r <- r+l
        s <- s+l
      }
      if(length(sims[,1])%%2!=0){sims <- sims[-1,]}
      l <- length(sims[,i])/2
      r <- 1
      s <- l
      psi <- c()
      for (i in 1:num_ch) {
        psi <- cbind(psi,sims[(r:l),i],sims[((r+l):(s+l)),i])
      }
      m <- num_ch*2
      n <- l
      psi_bar_dj <- sapply(1:m, function(x) mean(psi[,x]))
      psi_bar_dd <- mean(psi_bar_dj)
      B <- ((n)/(m-1))*sum((psi_bar_dj-psi_bar_dd)^2)
      s_sqrd <- sapply(1:8, function(x) ((1)/(n-1))*sum((psi[,x]-psi_bar_dj[x])^2))
      W <- mean(s_sqrd)
      var_hat_p <- ((n-1)/(n))*W+((1)/(n))*B
      rhat <- sqrt(var_hat_p/W)
      return(rhat)
    }
    
    
    rhat_vector <- function(fit){
      if(is.null(fit$c_posterior)){
        if(is.null(fit$phi_beta_posterior)){
          if(is.null(fit$sigma_eta_posterior)){
            rhat_vec <- sapply(1:3,function(x) R_hat(fit[[x]]))
          }else{
            rhat_vec <- sapply(1:9,function(x) R_hat(fit[[x]]))
          }
        }else{
          if(is.null(fit$sigma_eta_posterior)){
            rhat_vec <- sapply(1:5,function(x) R_hat(fit[[x]]))
          }else{
            rhat_vec <- sapply(1:11,function(x) R_hat(fit[[x]]))
          }
        }
      }else{
        if(is.null(fit$phi_beta_posterior)){
          if(is.null(fit$sigma_eta_posterior)){
            rhat_vec <- sapply(1:4,function(x) R_hat(fit[[x]]))
          }else{
            rhat_vec <- sapply(1:10,function(x) R_hat(fit[[x]]))
          }
        }else{
          if(is.null(fit$sigma_eta_posterior)){
            rhat_vec <- sapply(1:6,function(x) R_hat(fit[[x]]))
          }else{
            rhat_vec <- sapply(1:12,function(x) R_hat(fit[[x]]))
          }
        }
      }
      return(rhat_vec)
    }
    
    
    main_list <- function(formula, data, min_obs=10){
      id_var <- colnames(data)[-which(colnames(data) %in% formula_args)]     
      if(class(data[[all_of(id_var)]])!='factor') data[[all_of(id_var)]] <- as.factor(data[[all_of(id_var)]])
      data <- data %>% arrange(data[[all_of(id_var)]]) 
      data <- select(data, id_var, formula_args[1],formula_args[2])
      snames <- unique(data[[all_of(id_var)]])
      main <- list()
      log_file <- list()
      n <- 1
      #create the mainlist
      for (i in 1:length(snames)){
        if(nrow(filter(data, data[[all_of(id_var)]] == snames[i]))<min_obs){
          log_file <- rbind(log_file,paste0(Sys.time(),'  [WARN]  Data set ',snames[i],' was excluded. Minimum requirement of ',min_obs,' observations was not fulfilled.' ))
        }else{
          main$sets[n] <- list(filter(data, data[[all_of(id_var)]] == snames[i])) 
          n <- n+1
        }
      }  
      error_msg7 <- 'No data sets have 10 or more observations. Minimum number of observations required per data set is 10.'
      if(n==1) stop(error_msg7) 
      #new sets vector
      snames <- snames[1:(n-1)]
      for (i in 1:(n-1))  snames[i] <- main$sets[[i]][[all_of(id_var)]][[1]] 
      return(list('snames'=snames,'main'=main,'log_file'=log_file))
    }
    
    
    c_list <- function(c_values,snames){
      if(!any(c_values[,1] %in% snames | c_values[,2] %in% snames)){
        c_values <- NULL
        log_file <- rbind(log_file,paste0(Sys.time(),'  [WARN]  All known c values from the "c_values" data frame were excluded. No names, corresponding to these c values, were found in the main data frame of paired stage and discharge observations.' )) 
      }else{
        n_0 <- nrow(c_values)
        if(any(c_values[,1] %in% snames)){name_col <- 1}else{name_col <- 2}
        c_values <- data.frame(name=c_values[[name_col]],c=c_values[[1+name_col%%2]])
        error_msg7 <- 'The c values need to be numerical.'
        if(!any(sapply(1:length(c_values$c),function(x) is.numeric(c_values$c[x])))){
          stop(error_msg7)
        }
        c_values <- c_values[which(c_values$name %in% snames),]
        c_values <- c_values[sapply(1:length(c_values$c),function(x) is.numeric(c_values$c[x])&!is.na(c_values$c[x])),]           
        n_1 <- nrow(c_values)
        if(n_0!=n_1){
          N <- n_0-n_1
          log_file <- rbind(log_file,paste0(Sys.time(),'  [WARN]  ',N,' known c values from the "c_values" data frame were excluded. Either the names, corresponding to these c values, were not found in the main data frame of paired stage and discharge observations or the actual values were not numerical.' )) 
        }
      }
      return(c_values)
    }
    
    
    
    plot_c_posterior <- function(fits,snames,c_values){
      c_plots <- list()
      for (i in 1:length(c_values$name)) {
        j <- which(snames %in% c_values$name[[i]])
        fit <- fits$gplm[[j]]
        # if(all(c_tables$tour[[j]]$winner[c(1,5)] %in% T)){
        #   fit <- fits$gplm[[j]]
        # }else if(all(c_tables$tour[[j]]$winner[c(2,5)] %in% T)){
        #   fit <- fits$gplm0[[j]]
        # }else if(all(c_tables$tour[[j]]$winner[c(3,6)] %in% T)){
        #   fit <- fits$plm[[j]]
        # }else{
        #   fit <- fits$plm0[[j]]
        # }
        d <- data.frame(c=fit$c_posterior,name=rep('c',length(fit$c_posterior)))
        p <- ggplot(data=d,aes(x=c))+
          geom_density(fill='gray70',color='black',alpha=0.5) +
          geom_vline(xintercept =  c_values$c[[i]], color='red',size=1.5,linetype='solid')+
          theme_bw() +
          theme(axis.title.x = element_blank(), 
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 13),
                plot.title = element_text(size = 14,face = "bold",hjust = 0.45),
                text = element_text(family="Times", face="plain"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                panel.border = element_rect(colour = "black")) +
          ggtitle(paste0('P(c|Data) and the known c (red line).'))+
          ylab('')
        c_plots[[i]] <- grid.arrange(p)
      }  
      return(list('c_plots'=c_plots))
    }
    
    predict_tables <- function(fits,snames,c_tables){
      p_tables <- list()
      for (i in 1:length(snames)) {
        if(all(c_tables$tour[[i]]$winner[c(1,5)] %in% T)){
          fit <- fits$gplm[[i]]
        }else if(all(c_tables$tour[[i]]$winner[c(2,5)] %in% T)){
          fit <- fits$gplm0[[i]]
        }else if(all(c_tables$tour[[i]]$winner[c(3,6)] %in% T)){
          fit <- fits$plm[[i]]
        }else{
          fit <- fits$plm0[[i]]
        }
        h <- fit$rating_curve[[1]]
        step <- (max(h)-min(h))/2^5
        h_grid <- seq(min(h),max(h),by=step)
        rating_curve_h_grid <- predict(fit,newdata=h_grid)
        p_tables[[i]] <- format(round(rating_curve_h_grid,digits = 3),nsmall=3)
      }
      return(p_tables)
    }
    
    
    comparison_table <- function(fits,snames){
      c_tables <- list()
      for (i in 1:length(snames)) {
        tour <- tournament(fits$gplm[[i]],fits$gplm0[[i]],fits$plm[[i]],fits$plm0[[i]])
        c_tables$tour[[i]] <- tour$summary
      } 
      return(c_tables)
    }
    
    
    mcmc_diag_table <- function(fits,snames){
      mcmc_tables <- list()
      for (i in 1:length(snames)) {
        mcmc_table <- data.frame()
        mcmc_table[1,1:4] <- c(fits$gplm[[i]]$run_info$nr_iter,fits$gplm0[[i]]$run_info$nr_iter,fits$plm[[i]]$run_info$nr_iter,fits$plm0[[i]]$run_info$nr_iter)
        mcmc_table[2,1:4] <- c(fits$gplm[[i]]$run_info$num_chains,fitsgplm0[[i]]$run_info$num_chains,fits$plm[[i]]$run_info$num_chains,fits$plm0[[i]]$run_info$num_chains)
        mcmc_table[3,1:4] <- c(fits$gplm[[i]]$run_info$burnin,fits$gplm0[[i]]$run_info$burnin,fits$plm[[i]]$run_info$burnin,fits$plm0[[i]]$run_info$burnin)
        mcmc_table[4,1:4] <- c(fits$gplm[[i]]$run_info$thin,fits$gplm0[[i]]$run_info$thin,fits$plm[[i]]$run_info$thin,fits$plm0[[i]]$run_info$thin)
        mcmc_table[5,1:4] <- c(fits$gplm[[i]]$num_effective_param,fits$gplm0[[i]]$num_effective_param,fits$plm[[i]]$num_effective_param,fits$plm0[[i]]$num_effective_param)
        mcmc_table[6,1:4] <- c(fits$gplm[[i]]$acceptance_rate,fits$gplm0[[i]]$acceptance_rate,fits$plm[[i]]$acceptance_rate,fits$plm0[[i]]$acceptance_rate)
        mcmc_table[5:6,] <- round(mcmc_table[5:6,],digits = 3)
        mcmc_table[1:4,] <- format(mcmc_table[1:4,],nsmall = 0)
        colnames(mcmc_table) <- c('gplm','gplm0','plm','plm0')
        rownames(mcmc_table) <- c('nr_iter','nr_chains','burnin','thin','nr_eff_param','accept_rate')
        mcmc_tables[[i]] <- mcmc_table
      }
      return(mcmc_tables)
    }
    
    
    dev_boxplot <- function(fits,snames){
      d_plots <- list()
      for (i in 1:length(snames)) {
        val_gplm <- fits$gplm[[i]]$Deviance_posterior
        d1 <- cbind(rep("gplm", length(val_gplm)),val_gplm)
        val_gplm0 <- fits$gplm0[[i]]$Deviance_posterior
        d2 <- cbind(rep("gplm0", length(val_gplm0)),val_gplm0)
        val_plm <- fits$plm[[i]]$Deviance_posterior
        d3 <- cbind(rep("plm", length(val_plm)),val_plm)
        val_plm0 <- fits$plm0[[i]]$Deviance_posterior
        d4 <- cbind(rep("plm0", length(val_plm0)),val_plm0)
        dev_df <- data.frame(rbind(d1,d2,d3,d4))
        colnames(dev_df) <- c("model","values")
        dev_df$values <- as.numeric(dev_df$values)
        DIC_p <- data.frame(model=c('gplm','gplm0','plm','plm0'), DIC=c(fits$gplm[[i]]$DIC,fits$gplm0[[i]]$DIC,fits$plm[[i]]$DIC,fits$plm0[[i]]$DIC))
        p <- ggplot(data = dev_df, aes(x=model, y=values)) +
          geom_boxplot(size=.4,color="black",outlier.size=0.1,outlier.shape=21,outlier.fill="gray90",fill="gray90") +
          stat_boxplot(geom ='errorbar') +
          geom_line(data = DIC_p, aes(x=model, y=DIC, group = 1),color='gray30') +
          geom_point(data = DIC_p, aes(x=model, y=DIC), size=3,shape=23,fill='red2',color='black') +
          theme_bw() +
          theme(axis.title.x = element_blank(), 
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = 13),
                axis.text.x = element_text(size = 13),
                plot.title = element_text(size = 14,face = "bold",hjust = 0.45),
                text = element_text(family="Times", face="plain"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                panel.border = element_rect(colour = "black")) +
          ggtitle('Deviance posterior and DIC ')
        d_plots[[i]] <- p
      }
      return(d_plots)
    }
    
    
    histogram_plots <- function(fit){
      if(!is.null(fit$c_posterior)){
        if(is.null(fit$sigma_eta_posterior)&is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','c','sigma_eps',transformed=F)
        }else if(!is.null(fit$sigma_eta_posterior)&is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','c','sigma_eta','eta_1','eta_2','eta_3','eta_4','eta_5','eta_6',transformed=F)
        }else if(is.null(fit$sigma_eta_posterior)&!is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','c','sigma_eps','sigma_beta','phi_beta',transformed=F)
        }else{
          p <- plot(fit,type='histogram','a','b','c','sigma_eta','eta_1','eta_2','eta_3','eta_4','eta_5','eta_6','sigma_beta','phi_beta',transformed=F)
        } 
      }else{
        if(is.null(fit$sigma_eta_posterior)&is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','sigma_eps',transformed=F)
        }else if(!is.null(fit$sigma_eta_posterior)&is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','sigma_eta','eta_1','eta_2','eta_3','eta_4','eta_5','eta_6',transformed=F)
        }else if(is.null(fit$sigma_eta_posterior)&!is.null(fit$phi_beta_posterior)){
          p <- plot(fit,type='histogram','a','b','sigma_eps','sigma_beta','phi_beta',transformed=F)
        }else{
          p <- plot(fit,type='histogram','a','b','sigma_eta','eta_1','eta_2','eta_3','eta_4','eta_5','eta_6','sigma_beta','phi_beta',transformed=F)
        } 
      }
      return(p)
    }
    
    
    mcmc_plots <- function(fits,snames){
      hist_plots <- list()
      for (i in 1:length(snames)) {
        hist_plots$gplm[[i]] <- histogram_plots(fits$gplm[[i]])
        hist_plots$gplm0[[i]] <- histogram_plots(fits$gplm0[[i]])
        hist_plots$plm[[i]] <- histogram_plots(fits$plm[[i]])
        hist_plots$plm0[[i]] <- histogram_plots(fits$plm0[[i]])
      }
      return(hist_plots)
    }
    
    
    multiplier <- function (Q_median, h_i) {
      Q_median_temp1 <- Q_median[0,]
      Q_median_temp2 <- Q_median
      account <- data.frame(table(h_i[which( h_i %in% h_i[duplicated(h_i)])]))
      if (nrow(account)!=0) {
        b <- 0
        for (i in 1:nrow(account)) {
          row_num <- as.numeric(rownames(Q_median[which(Q_median[[2]] %in% account[[1]][i] ),]))
          row_num <- row_num + b
          Q_median_temp1 <- slice(Q_median,rep(row_num-b,each=account[[2]][i])) 
          if (row_num==1) {
            Q_median_temp2  <- rbind(Q_median_temp1,Q_median[Q_median[[2]]>Q_median_temp1[1,2],])
          } else {
            Q_median_temp2  <- rbind(Q_median_temp2[(1:(row_num-1)),],Q_median_temp1,Q_median[Q_median[[2]]>Q_median_temp1[1,2],])
          }
          row.names(Q_median_temp2) <- NULL
          b <- b+(account$Freq[i]-1)
        }
      }
      return(Q_median_temp2)
    }
    
    
    logQmed <- function(fit){
      df1 <- as.data.frame(fit$data[[2]])
      df2 <- as.data.frame(fit$rating_curve$median)
      df3 <- as.data.frame(fit$rating_curve$h)
      df2and3 <- cbind(df2,df3)
      Q_median <- df2and3 %>% filter(df2and3[[2]] %in% df1[[1]]) 
      Q_median <- multiplier(Q_median,fit$data[[2]])
      logQ_median <- log(Q_median[[1]])
      return(logQ_median)
    }
    
    
    
    max_res <- function(fit) {
      logQ_i <- log(fit$data[[1]])
      logQ_median <- logQmed(fit)                    
      max_value <- max(abs(logQ_i - logQ_median))
      return(max_value)
    }
    
    
    
    page_prep <- function(fit1,fit2,fit3,fit4) {
      rate_upr_lim <- min(fit1$data[[2]]) + abs(min(fit1$data[[2]])-max(fit1$data[[2]]))*1.01
      rate_lwr_lim <- min(fit1$data[[2]])
      rate_lim <- c(rate_lwr_lim,rate_upr_lim)
      rate_upr_lim_x <- max(fit1$data[[1]])*1.2
      rate_lwr_lim_x <- min(min(fit1$rating_curve$lower),
                            min(fit2$rating_curve$lower),
                            min(fit3$rating_curve$lower),
                            min(fit4$rating_curve$lower))
      rate_lim_x <- c(rate_lwr_lim_x,rate_upr_lim_x)
      maxRes <- max(max_res(fit1),max_res(fit2),max_res(fit3),max_res(fit4))
      res_upr_lim <- max(max(fit1$sigma_eps_summary$median*1.96*1.1),
                         fit2$param_summary['sigma_eps',]$median*1.96*1.1,
                         max(fit3$sigma_eps_summary$median*1.96*1.1),
                         fit4$param_summary['sigma_eps',]$median*1.96*1.1,
                         maxRes*1.1)
      res_lwr_lim <- min(min(fit1$sigma_eps_summary$median*(-1.96)*1.1),
                         fit2$param_summary['sigma_eps',]$median*(-1.96)*1.1,
                         min(fit3$sigma_eps_summary$median*(-1.96)*1.1),
                         fit4$param_summary['sigma_eps',]$median*(-1.96)*1.1,
                         (-maxRes)*1.1)
      res_lim <- c(res_lwr_lim,res_upr_lim)
      beta_upr_lim <- max(max(fit1$f_summary$upper)*1.01,
                          max(fit2$f_summary$upper)*1.01,
                          fit3$param_summary['b',]$upper*1.01,
                          fit4$param_summary['b',]$upper*1.01,
                          3.5)
      beta_lwr_lim <- min(min(fit1$f_summary$lower)*0.99,
                          min(fit2$f_summary$lower)*0.99,
                          fit3$param_summary['b',]$lower*0.99,
                          fit4$param_summary['b',]$lower*0.99,
                          1)
      beta_lim <- c(beta_lwr_lim,beta_upr_lim)
      beta_upr_lim_x <- max(max(fit1$f_summary$h),
                            max(fit2$f_summary$h))
      beta_lwr_lim_x <- min(min(fit1$f_summary$h),
                            min(fit2$f_summary$h))
      beta_lim_x <- c(beta_lwr_lim_x,beta_upr_lim_x)
      sig_upr_lim <- max(max(fit1$sigma_eps_summary$upper),
                         fit2$param_summary['sigma_eps',]$upper,
                         max(fit3$sigma_eps_summary$upper),
                         fit4$param_summary['sigma_eps',]$upper)
      sig_lwr_lim <- 0
      sig_lim <- c(sig_lwr_lim,sig_upr_lim)
      sig_upr_lim_x <- max(fit1$data[[2]])
      sig_lwr_lim_x <- min(fit1$data[[2]])
      sig_lim_x <- c(sig_lwr_lim_x,sig_upr_lim_x)
      return(list('res_lim'=res_lim,'rate_lim'=rate_lim,'rate_lim_x'=rate_lim_x,'beta_lim'=beta_lim,'beta_lim_x'=beta_lim_x,'sig_lim'=sig_lim,'sig_lim_x'=sig_lim_x))
    }
    
    
    
    rc_plotter <- function(fit,rclim,rclimx){
      p <- ggplot(data=fit$rating_curve) +
        geom_point(data=fit$data,aes(fit$data[[1]],fit$data[[2]]), size=.9, shape=21, fill="gray60", color="black") +
        geom_line(aes(median,h)) +
        geom_path(aes(lower,h),linetype='dashed') +
        geom_path(aes(upper,h),linetype='dashed') +
        xlab(expression(paste('Q[',m^{3},'/s]'))) + 
        ylab("h[m]") +
        scale_y_continuous(expand = c(0,0), limits = c(rclim[1],rclim[2]) ) +
        scale_x_continuous(expand = c(0,0), limits = c(rclimx[1],rclimx[2]) ) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))
      return(p)
    }
    
    res_plotter <- function(fit,rlim,c_value){
      if(is.null(c_value)){
        c <- fit$param_summary['c',]$median
        c_exp <- expression(paste('log(h - ',hat(c),')'))
      }else{
        c <- c_value
        c_exp <- expression(paste('log(h - c)'))
      }
      h <- fit$rating_curve$h
      h_i <- fit$data[[2]]
      logQ_i <- log(fit$data[[1]])
      logQ_median <- logQmed(fit)
      if(is.null(fit$sigma_eps_summary)){
        upr_r <- rep(fit$param_summary['sigma_eps',]$median*1.96, length(h))
        lwr_r <- rep(fit$param_summary['sigma_eps',]$median*(-1.96), length(h))
        res_2sd <- data.frame(upr_r,lwr_r,h)
      }else{
        upr_r <- fit$sigma_eps_summary$median*1.96
        lwr_r <- fit$sigma_eps_summary$median*(-1.96)
        res_2sd <- data.frame(upr_r,lwr_r,h)
      }
      p <- ggplot(fit$data, aes( log( h_i - c ) )) + 
        geom_point(aes(y=( logQ_i - logQ_median ) ), size=.9, shape=21, fill="gray60", color="black") +
        geom_hline(yintercept = 0, linetype="solid") +
        geom_line(data=filter(res_2sd,h>=min(fit$data[[2]]),h<=max(fit$data[[2]])),aes(log(h-c),upr_r),linetype='dashed') +
        geom_line(data=filter(res_2sd,h>=min(fit$data[[2]]),h<=max(fit$data[[2]])),aes(log(h-c),lwr_r),linetype='dashed') +
        ylim( rlim[1],rlim[2] ) +
        labs(x = c_exp, y = expression(paste('log(Q)-log(',hat(Q),')'))) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black")) 
      return(p)
    }
    
    b_plus_beta_plotter <- function(fit,blim,blimx){
      p <- ggplot(data=fit$f_summary)  +
        geom_line(aes(h, median)) +
        geom_line(aes(h, lower),linetype='dashed') +
        geom_line(aes(h, upper),linetype='dashed') +
        ylim( blim[1],blim[2] ) +
        xlim( blimx[1],blimx[2] ) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black")) +
        labs( x = "h[m]",
              y = expression(paste('b + ',beta,'(h)')) ) 
      return(p)
    }
    
    
    b_plotter <- function(fit,blim,blimx){
      h <- fit$rating_curve$h
      lwr_b <- rep(fit$param_summary['b',]$lower, length(h))
      med_b <- rep(fit$param_summary['b',]$median, length(h))
      upr_b <- rep(fit$param_summary['b',]$upper, length(h))
      b_df <- data.frame(h, upr_b, med_b, lwr_b)
      p <- ggplot(data=b_df)  +
        geom_line(aes(h, med_b)) +
        geom_line(aes(h, lwr_b),linetype='dashed') +
        geom_line(aes(h, upr_b),linetype='dashed') +
        ylim( blim[1],blim[2] ) +
        xlim( blimx[1],blimx[2] ) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black")) +
        labs( x = 'h[m]', y = 'b' ) 
      return(p)
    }
    
    sigma_eps_h_plotter <- function(fit,slim,slimx){
      p <- ggplot(data=fit$sigma_eps_summary)  +
        geom_line(aes(h, median)) +
        geom_line(aes(h, lower),linetype='dashed') +
        geom_line(aes(h, upper),linetype='dashed') +
        labs(x = "h[m]", y = expression(paste(sigma[epsilon],'(h)'))) +
        scale_y_continuous(expand = c(0, 0), limits = c(slim[1],slim[2]*1.05)) +
        xlim( slimx[1],slimx[2] ) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black")) 
      return(p)
    }
    
    
    sigma_eps_plotter <- function(fit,slim,slimx){
      h <- fit$rating_curve$h
      lwr_s <- rep(fit$param_summary['sigma_eps',]$lower, length(h))
      med_s <- rep(fit$param_summary['sigma_eps',]$median, length(h))
      upr_s <- rep(fit$param_summary['sigma_eps',]$upper, length(h))
      sigma_eps_df <- data.frame(h, upr_s, med_s, lwr_s)
      p <- ggplot(data=sigma_eps_df)  +
        geom_line(aes(h, med_s)) +
        geom_line(aes(h, lwr_s),linetype='dashed') +
        geom_line(aes(h, upr_s),linetype='dashed') +
        labs(x = 'h[m]', y = expression(sigma[epsilon])) +
        scale_y_continuous(expand = c(0, 0), limits = c(slim[1],slim[2]*1.05)) +
        xlim( slimx[1],slimx[2] ) +
        theme_bw() +
        theme( text = element_text(family="Times", face="plain"),
               axis.text = element_text(face="plain"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black")) 
      return(p)
    }
    
    
    
    
    gplm_page_maker <- function(fit,snames,rlim,rclim,rclimx,blim,blimx,slim,slimx,c_value) {
      p1 <- rc_plotter(fit,rclim,rclimx)
      p2 <- res_plotter(fit,rlim,c_value)
      p3 <- b_plus_beta_plotter(fit,blim,blimx)
      p4 <- sigma_eps_h_plotter(fit,slim,slimx)
      plots <- grid.arrange(p1,p2,p3,p4,top=textGrob(paste(as.character(snames),as.character("   -   "),as.character("gplm"),sep=""),gp=gpar(fontfamily="Times",fontsize=19,facetype='bold')))
      DEV <- as.data.frame(fit$Deviance_summary)
      row.names(DEV) <- "Deviance"
      table <- as.data.frame(fit$param_summary)
      rownames(table) <- sapply(1:nrow(table), function(x) get_param_expression( rownames(table)[[x]] )[[1]])
      table <- rbind(table, DEV)
      rhat_col <- data.frame(Rhat=c(rhat_vector(fit),NA))
      table <- cbind(table,rhat_col)
      table <- format(round(table,digits = 3),nsmall=3)
      table[length(table[,4]),4] <- ''
      table <- tableGrob(table, theme=ttheme_minimal(base_family = "Times",rowhead=list(fg_params = list(parse=TRUE))))
      return(list("plots" = plots, "table" = table))
    }
    
    
    gplm0_page_maker <- function(fit,snames,rlim,rclim,rclimx,blim,blimx,slim,slimx,c_value) {
      p1 <- rc_plotter(fit,rclim,rclimx)
      p2 <- res_plotter(fit,rlim,c_value)
      p3 <- b_plus_beta_plotter(fit,blim,blimx)
      p4 <- sigma_eps_plotter(fit,slim,slimx)
      plots <- grid.arrange(p1,p2,p3,p4,top=textGrob(paste(as.character(snames),as.character("   -   "),as.character("gplm0"),sep=""),gp=gpar(fontfamily="Times",fontsize=19,facetype='bold')))
      DEV <- as.data.frame(fit$Deviance_summary)
      row.names(DEV) <- "Deviance"
      table <- as.data.frame(fit$param_summary)
      rownames(table) <- sapply(1:nrow(table), function(x) get_param_expression( rownames(table)[[x]] )[[1]])
      table <- rbind(table, DEV)
      rhat_col <- data.frame(Rhat=c(rhat_vector(fit),NA))
      table <- cbind(table,rhat_col)
      table <- format(round(table,digits = 3),nsmall=3)
      table[length(table[,4]),4] <- ''
      table <- tableGrob(table, theme=ttheme_minimal(base_family = "Times",rowhead=list(fg_params = list(parse=TRUE))))
      return(list("plots" = plots, "table" = table))
    }
    
    
    plm_page_maker <- function(fit,snames,rlim,rclim,rclimx,blim,blimx,slim,slimx,c_value) {
      p1 <- rc_plotter(fit,rclim,rclimx)
      p2 <- res_plotter(fit,rlim,c_value)
      p3 <- b_plotter(fit,blim,blimx)
      p4 <- sigma_eps_h_plotter(fit,slim,slimx)
      plots <- grid.arrange(p1,p2,p3,p4,top=textGrob(paste(as.character(snames),as.character("   -   "),as.character("plm"),sep=""),gp=gpar(fontfamily="Times",fontsize=19,facetype='bold')))
      DEV <- as.data.frame(fit$Deviance_summary)
      row.names(DEV) <- "Deviance"
      table <- as.data.frame(fit$param_summary)
      rownames(table) <- sapply(1:nrow(table), function(x) get_param_expression( rownames(table)[[x]] )[[1]])
      table <- rbind(table, DEV)
      rhat_col <- data.frame(Rhat=c(rhat_vector(fit),NA))
      table <- cbind(table,rhat_col)
      table <- format(round(table,digits = 3),nsmall=3)
      table[length(table[,4]),4] <- ''
      table <- tableGrob(table, theme=ttheme_minimal(base_family = "Times",rowhead=list(fg_params = list(parse=TRUE))))
      return(list("plots" = plots, "table" = table))
    }
    
    
    
    plm0_page_maker <- function(fit,snames,rlim,rclim,rclimx,blim,blimx,slim,slimx,DICt,c_value) {
      p1 <- rc_plotter(fit,rclim,rclimx)
      p2 <- res_plotter(fit,rlim,c_value)
      p3 <- b_plotter(fit,blim,blimx)
      p4 <- sigma_eps_plotter(fit,slim,slimx)
      plots <- grid.arrange(p1,p2,p3,p4,top=textGrob(paste( as.character(snames),as.character("   -   "),as.character("plm0"),sep=""),gp=gpar(fontfamily="Times",fontsize=19,facetype='bold')))
      DEV <- as.data.frame(fit$Deviance_summary)
      row.names(DEV) <- "Deviance"
      table <- as.data.frame(fit$param_summary)
      rownames(table) <- sapply(1:nrow(table), function(x) get_param_expression( rownames(table)[[x]] )[[1]])
      table <- rbind(table, DEV)
      rhat_col <- data.frame(Rhat=c(rhat_vector(fit),NA))
      table <- cbind(table,rhat_col)
      table <- format(round(table,digits = 3),nsmall=3)
      table[length(table[,4]),4] <- ''
      table <- tableGrob(table, theme=ttheme_minimal(base_family = "Times",rowhead=list(fg_params = list(parse=TRUE))))
      return(list("plots" = plots, "table" = table))
    }
    
    ##### catalog_functions
    
    
    
    
    outputOptions(output, "plots1", suspendWhenHidden = FALSE)
    outputOptions(output, "plots2", suspendWhenHidden = FALSE)
    outputOptions(output, "plots3", suspendWhenHidden = FALSE)
    outputOptions(output, "plots4", suspendWhenHidden = FALSE)

})


