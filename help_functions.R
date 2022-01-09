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


get_param_expression <- function(param){
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
    param_expr <- expr_vec[param]
    if(any(is.na(param_expr))){
        stop('param not found')
    }
    return(param_expr)
}


justify <- function(x, hjust="center", vjust="center", draw=TRUE){
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
    if(draw) grid.draw(x)
    return(x)
}
