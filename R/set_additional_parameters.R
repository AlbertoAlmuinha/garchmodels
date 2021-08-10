
#' @export
set_additional_parameters <- function(model, ...){
    
    args <- data.table::as.data.table(list(...))
    
    model_class <- class(model)[1]
    
    model <- cbind(model, args)
    
    data.table::setattr(model, "class", union(model_class, class(model)))
    
    return(model)
    
}

#' @export
engine_arima <- function(args){
    
    args[["order"]] <- c(args$non_seasonal_ar, args$non_seasonal_differences, args$non_seasonal_ma)
    args[["seasonal"]] <- c(args$seasonal_ar, args$seasonal_differences, args$seasonal_ma)
    
    args<-within(args, rm(non_seasonal_ar, non_seasonal_differences, non_seasonal_ma, seasonal_ar, seasonal_differences, seasonal_ma, seasonal_period))
    
    return(args)
    
}

#' @export
engine_auto_arima <- function(args){
    
    args[["max.p"]] <- args$non_seasonal_ar
    args[["max.d"]] <- args$non_seasonal_differences
    args[["max.q"]] <- args$non_seasonal_ma
    args[["max.P"]] <- args$seasonal_ar
    args[["max.D"]] <- args$seasonal_differences
    args[["max.Q"]] <- args$seasonal_ma
    
    args<-within(args, rm(non_seasonal_ar, non_seasonal_differences, non_seasonal_ma, seasonal_ar, seasonal_differences, seasonal_ma, seasonal_period))
    
    return(args)
    
}



