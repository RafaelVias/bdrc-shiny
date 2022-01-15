### Help functions ###
get_param_names <- function(model,c_param){
    if(model=='plm0'){
        hyper_param <- 'sigma_eps'
    }else if(model=='plm'){
        hyper_param <- c('sigma_eta',paste('eta',1:6,sep='_'))
    }else if(model=='gplm0'){
        hyper_param <- c('sigma_eps','sigma_beta','phi_beta')
    }else if(model=='gplm'){
        hyper_param <- c('sigma_beta','phi_beta','sigma_eta',paste('eta',1:6,sep='_'))
    }
    if(is.null(c_param)){
        hyper_param <- c('c',hyper_param)
    }
    latent_param <- c('a','b')
    return(c(latent_param,hyper_param))
}


get_param_expression <- function(param,latex=TRUE){
    if(latex){
        expr_vec <- c('a'='a','b'='b','c'='c','sigma_eps'='\\sigma_{\\epsilon}',
                      'sigma_beta'='\\sigma_{\\beta}','phi_beta'='\\phi_{\\beta}',
                      'sigma_eta'='\\sigma_{\\eta}','eta_1'='\\eta_{1}','eta_2'='\\eta_{2}',
                      'eta_3'='\\eta_{3}','eta_4'='\\eta_{4}','eta_5'='\\eta_{5}',
                      'eta_6'='\\eta_{6}','log(a)'='\\log(a)','log(h_min-c)'='\\log(h_{min}-c)',
                      '2log(sigma_eps)'='\\log(\\sigma_{\\epsilon}^2)',
                      'log(sigma_beta)'='\\log(\\sigma_{\\beta})',
                      'log(phi_beta)'='\\log(\\phi_{\\beta})',
                      'log(sigma_eta)'='\\log(\\sigma_[\\eta})',
                      'z_1'='z_{1}','z_2'='z_{2}','z_3'='z_{3}',
                      'z_4'='z_{4}','z_5'='z_{5}','z_6'='z_{6}')
    }else{
        expr_vec <- c('a'='a','b'='b','c'='c','sigma_eps'='sigma[epsilon]',
                      'sigma_beta'='sigma[beta]','phi_beta'='phi[beta]',
                      'sigma_eta'='sigma[eta]','eta_1'='eta[1]','eta_2'='eta[2]',
                      'eta_3'='eta[3]','eta_4'='eta[4]','eta_5'='eta[5]',
                      'eta_6'='eta[6]','log(a)'='log(a)','log(h_min-c)'='log(h[min]-c)',
                      '2log(sigma_eps)'='log(sigma[epsilon]^2)',
                      'log(sigma_beta)'='log(sigma[beta])',
                      'log(phi_beta)'='log(phi[beta])',
                      'log(sigma_eta)'='log(sigma[eta])',
                      'z_1'='z[1]','z_2'='z[2]','z_3'='z[3]',
                      'z_4'='z[4]','z_5'='z[5]','z_6'='z[6]')
    }
    param_expr <- expr_vec[param]
    if(any(is.na(param_expr))){
        stop('param not found')
    }
    return(param_expr)
}


justify <- function(x, hjust="center", vjust="center"){
    w <- sum(x$widths)
    h <- sum(x$heights)
    xj <- switch(hjust,
                 center = 0.5,
                 left = 0.5*w,
                 right=unit(1,"npc") - 0.5*w)
    yj <- switch(vjust,
                 center = 0.5,
                 bottom = 0.5*h,
                 top=unit(1,"npc") - 0.5*h)
    x$vp <- viewport(x=xj, y=yj)
    return(x)
}

clean <- function(file,advanced=TRUE,includedates=c(1950,as.numeric(format(Sys.Date(), "%Y"))),
                  dummy=NULL,keeprows=NULL,force=NULL,h_min=NA,h_max=NA, exclude=TRUE,
                  excludedates=c(Sys.Date()-1,Sys.Date()-1)){
    
    if (is.null(file)){
        return(NULL)
    }
    if(file$type=='text/plain'){
        observedData=read.table(file$datapath,skip=2,sep="|",dec=",")
        observedData=observedData[,c(2,3,5,7,4)]
        names(observedData)=c("Date","Time","Quality","W","Q")
        observedData$Date=as.Date(gsub("\\.","-",observedData$Date),"%d-%m-%Y")
    }else if(file$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        observedData = readxl::read_xlsx(path = file$datapath, sheet = 1)
        names(observedData)=c("Date","Time","Quality","W","Q")    
    }else{
        return(NULL)
    }
    observedData$Date <- as.Date(observedData$Date)
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
        years=as.numeric(format(observedData$Date, "%Y"))
        observedData=observedData[which(years<=includedates[2] & years >= includedates[1]),]
        if(exclude==TRUE){
            observedData=observedData[which(observedData$Date<=excludedates[1] | observedData$Date >= excludedates[2]),]
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
    if(is.na(h_min)) h_min=min(observedData$W)
    if(is.na(h_max)) h_max=max(observedData$W)
    observedData=subset(observedData,W >= h_min & W <=h_max )
    wq=as.matrix(observedData[,c("W","Q")])
    
    return(list("wq"=wq,"observedData"=observedData,"observedData_before"=observedData_before))
}

get_residuals_dat <- function(m){
    h_min <- min(m$data[[all.vars(m$formula)[2]]])
    rc_dat <- merge(m$rating_curve_mean[,c('h','median','lower','upper')],m$rating_curve[,c('h','median','lower','upper')],by.x='h',by.y='h')
    resid_dat <- merge(rc_dat[rc_dat$h>=h_min,],m$data,by.x='h',by.y=all.vars(m$formula)[2],all.x=TRUE)
    colnames(resid_dat) <- c('h','rcm_median','rcm_lower','rcm_upper','rc_median','rc_lower','rc_upper','Q')
    c_hat <- if(is.null(m$run_info$c_param))  median(m$c_posterior)  else  m$run_info$c_param 
    resid_dat[,'log(h-c_hat)'] <- log(resid_dat$h-c_hat)
    resid_dat$r_median <- log(resid_dat$Q)-log(resid_dat$rc_median)
    # resid_dat$m_lower <- log(resid_dat$rcm_lower)-log(resid_dat$rcm_median)
    # resid_dat$m_upper <- log(resid_dat$rcm_upper)-log(resid_dat$rcm_median)
    resid_dat$r_lower <- log(resid_dat$rc_lower)-log(resid_dat$rc_median)
    resid_dat$r_upper <- log(resid_dat$rc_upper)-log(resid_dat$rc_median)
    return(resid_dat)
}

theme_bdrc <- function(...,scaling=1){
    title_size <- scaling*12
    text_size <- scaling*10
    plot_title_size=scaling*15
    theme_classic() %+replace%
        theme( #text = element_text(family="Times", face="plain"),
            strip.background = element_blank(),
            strip.text.x = element_text(size = title_size),
            axis.title.x = element_text(size=title_size),
            axis.title.y = element_text(size=title_size,angle=90),
            axis.text.x = element_text(size=text_size),
            axis.text.y = element_text(size=text_size),
            legend.text = element_text(size=text_size),
            legend.title = element_text(size=text_size),
            plot.title = element_text(size=plot_title_size),
            panel.border = element_rect(colour="black",fill=NA),
            ...)
}

plot_resid <- function(m){
    resid_dat <- get_residuals_dat(m)
    y_lab <- "paste('','log','(','',italic(paste('Q')),')','','-log','(','',italic(paste('',hat(paste('Q')))),')','','')"
    x_lab <- "paste('','log','(','',italic(paste('h',phantom() - phantom(),'',hat(paste('c')))),')','','')"
    method <- 'loess'
    span <- 0.3
    p <- ggplot(data=resid_dat) +
        geom_hline(yintercept=0,size=0.8,alpha=.95) +
        geom_point(data=resid_dat[!is.na(resid_dat$Q),],aes(.data$`log(h-c_hat)`,.data$r_median), size=.9, shape=21, fill="gray60", color="black",alpha=0.95) +
        geom_smooth(aes(x=.data$`log(h-c_hat)`,y=.data$r_upper),span=span,se=FALSE,stat = "smooth",color='black',linetype='dashed',size=0.5,alpha=0.95,method=method,formula='y~x') +
        geom_smooth(aes(x=.data$`log(h-c_hat)`,y=.data$r_lower),span=span,se=FALSE,stat = "smooth",color='black',linetype='dashed',size=0.5,alpha=0.95,method=method,formula='y~x') +
        xlab(parse(text=x_lab)) +
        ylab(parse(text=y_lab)) +
        scale_x_continuous(limits= c(NA,NA),expand=expansion(mult=rep(.01,2))) +
        scale_y_continuous(limits= c(NA,NA),expand=expansion(mult=rep(.05,2))) +
        ggtitle('Residual Plot') +
        theme_bdrc()
    return(p)
}
