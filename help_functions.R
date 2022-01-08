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
    param_expr <- expr_vec[param]
    if(any(is.na(param_expr))){
        stop('param not found')
    }
    return(param_expr)
}
