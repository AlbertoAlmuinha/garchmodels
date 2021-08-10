#' @export
arima_reg <- function(non_seasonal_ar = 0,
                      non_seasonal_differences = 0,
                      non_seasonal_ma = 0,
                      seasonal_ar = 0,
                      seasonal_differences = 0,
                      seasonal_ma = 0,
                      seasonal_period = "auto"){

    res <- data.table::data.table(non_seasonal_ar = non_seasonal_ar,
                                  non_seasonal_differences = non_seasonal_differences,
                                  non_seasonal_ma = non_seasonal_ma,
                                  seasonal_ar = seasonal_ar,
                                  seasonal_differences = seasonal_differences,
                                  seasonal_ma = seasonal_ma,
                                  seasonal_period = seasonal_period)

    data.table::setattr(res, "class", union("f_arima_reg", class(res)))

    return(res)

}

#' @export
print.f_arima_reg <- function(x, ...){
    
    tibble_args <- tibble::tibble(Arguments = names(x),
                                  Value = x[, unname(unlist(lapply(.SD, as.character)))])
    
    pretty_print_model_spec("ARIMA Regression Specification", tibble_args)
}


#' @export
fit.f_arima_reg <- function(model, formula, data, ...){
    
    if (!rlang::inherits_any(data, "data.table")){
        data <- data.table::as.data.table(data)
    }
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    
    args <- as.list(model)
    
    index_tbl <- parse_index(data)
    idx       <- index_tbl[[1]]
    
    if (is.numeric(args$seasonal_period)){
        period <- args$seasonal_period
    } else {
        period <- parse_frequency(idx, args$seasonal_period)
    }
    
    idx_col   <- names(index_tbl)
    
    period <- args$seasonal_period
    
    args <- engine_arima(args)
    
    outcome <- data[[y]]
    predictors <- data[, x, with = FALSE]
    
    #period <- forecast::findfrequency(outcome)
    
    args[["y"]] <- stats::ts(outcome, frequency = period)
    
    predictors <- predictors[, .SD, .SDcols = !which(sapply(predictors, class) %chin% c("Date", "POSIXt"))] |>
        zero_variance() |>
        one_hot() |>
        fast_to_matrix()
    
    predictors <- NULL
    
    if (!is.null(predictors)){
        
        args[["xreg"]] <- predictors
        
        arima_call <- parsnip::make_call(fun  = "Arima",
                                         ns   = "forecast",
                                         args = args)
        
        arima_fit <- rlang::eval_tidy(arima_call)
    
    } else {
        
        arima_call <- parsnip::make_call(fun  = "Arima",
                                         ns   = "forecast",
                                         args = args)

        arima_fit <- rlang::eval_tidy(arima_call)

    }
    
    new_fastime_bridge(
        class = "f_arima_fitted",
        
        # Models
        models = list(
            model_1 = arima_fit
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !!idx_col      := idx,
            .actual      =  as.numeric(arima_fit$x),
            .fitted      =  as.numeric(arima_fit$fitted),
            .residuals   =  as.numeric(arima_fit$residuals)
        ) |> data.table::as.data.table(),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            x = x
        ),
        
        # Description - Convert arima model parameters to short description
        desc = modeltime::get_arima_description(arima_fit)
    )
    
}


#' @export
predict.f_arima_fitted <- function(model, new_data, ...){
    
    if (!rlang::inherits_any(new_data, "data.table")){
        new_data <- as.data.table(new_data)
    }
    
    f_model <- model$models$model_1
    x       <- model$extras$x
    h       <- nrow(new_data)
    
    predictors <- new_data[, x, with = FALSE]
    
    predictors <- predictors[, .SD, .SDcols = !which(sapply(predictors, class) %chin% c("Date", "POSIXt"))] |> 
        zero_variance() |>
        one_hot() |> 
        fast_to_matrix()
    
    if (!is.null(predictors)){
        
        preds_forecast <- data.table::data.table(.pred = forecast::forecast(f_model, h = h, xreg = predictors, ...)$mean)
        
    } else {
        
        preds_forecast <- data.table::data.table(.pred = forecast::forecast(f_model, h = h, ...)$mean)
    }
    
    return(preds_forecast)
    
}


#' @export
auto_arima_reg <- function(non_seasonal_ar = 5,
                           non_seasonal_differences = 2,
                           non_seasonal_ma = 5,
                           seasonal_ar = 2,
                           seasonal_differences = 1,
                           seasonal_ma = 2,
                           seasonal_period = "auto"){
    
    res <- data.table::data.table(non_seasonal_ar = non_seasonal_ar,
                                  non_seasonal_differences = non_seasonal_differences,
                                  non_seasonal_ma = non_seasonal_ma,
                                  seasonal_ar = seasonal_ar,
                                  seasonal_differences = seasonal_differences,
                                  seasonal_ma = seasonal_ma,
                                  seasonal_period = seasonal_period)
    
    data.table::setattr(res, "class", union("f_auto_arima_reg", class(res)))
    
    return(res)
    
}

#' @export
print.f_auto_arima_reg <- function(x, ...){
    
    tibble_args <- tibble::tibble(Arguments = names(x),
                                  Value = x[, unname(unlist(lapply(.SD, as.character)))])
    
    pretty_print_model_spec("Auto ARIMA Regression Specification", tibble_args)
}



#' @export
fit.f_auto_arima_reg <- function(model, formula, data, ...){
    
    if (!rlang::inherits_any(data, "data.table")){
        data <- data.table::as.data.table(data)
    }
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    
    args <- as.list(model)
    
    index_tbl <- parse_index(data)
    idx       <- index_tbl[[1]]
    
    if (is.numeric(args$seasonal_period)){
        period <- args$seasonal_period
    } else {
        period <- parse_frequency(idx, args$seasonal_period)
    }
    
    idx_col   <- names(index_tbl)
    
    period <- args$seasonal_period
    
    args <- engine_auto_arima(args)
    
    outcome <- data[[y]]
    predictors <- data[, x, with = FALSE]
    
    #period <- forecast::findfrequency(outcome)
    
    args[["y"]] <- stats::ts(outcome, frequency = period)
    
    predictors <- predictors[, .SD, .SDcols = !which(sapply(predictors, class) %chin% c("Date", "POSIXt"))] |> 
        zero_variance() |>
        one_hot() |> 
        fast_to_matrix()
    
    if (!is.null(predictors)){
        
        args[["xreg"]] <- predictors
        
        arima_call <- parsnip::make_call(fun  = "auto.arima",
                                         ns   = "forecast",
                                         args = args)
        
        arima_fit <- rlang::eval_tidy(arima_call)
        
    } else {
        
        arima_call <- parsnip::make_call(fun  = "auto.arima",
                                         ns   = "forecast",
                                         args = args)
        
        arima_fit <- rlang::eval_tidy(arima_call)
    }
    
    new_fastime_bridge(
        class = "f_auto_arima_fitted",
        
        # Models
        models = list(
            model_1 = arima_fit
        ),
        
        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !!idx_col      := idx,
            .actual      =  as.numeric(arima_fit$x),
            .fitted      =  as.numeric(arima_fit$fitted),
            .residuals   =  as.numeric(arima_fit$residuals)
        ) |> data.table::as.data.table(),
        
        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            x = x
        ),
        
        # Description - Convert arima model parameters to short description
        desc = modeltime::get_arima_description(arima_fit)
    )
    
}


#' @export
predict.f_auto_arima_fitted <- function(model, new_data, ...){
    
    if (!rlang::inherits_any(new_data, "data.table")){
        new_data <- as.data.table(new_data)
    }
    
    f_model <- model$models$model_1
    x       <- model$extras$x
    h       <- nrow(new_data)
    
    predictors <- new_data[, x, with = FALSE]
    
    predictors <- predictors[, .SD, .SDcols = !which(sapply(predictors, class) %chin% c("Date", "POSIXt"))] |> 
        zero_variance() |>
        one_hot() |> 
        fast_to_matrix()
    
    if (!is.null(predictors)){
        
        preds_forecast <- data.table::data.table(.pred = forecast::forecast(f_model, h = h, xreg = predictors, ...)$mean)
        
    } else {
        
        preds_forecast <- data.table::data.table(.pred = forecast::forecast(f_model, h = h, ...)$mean)
    }
    
    return(preds_forecast)
    
}
