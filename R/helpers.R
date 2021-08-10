
#' @export
pretty_print_model_spec <- function(model_desc, tibble_args){
    
    if (!is.character(model_desc)){
        rlang::abort("`model_desc` should be a string with the model specification")
    }
    
    if (!any(names(tibble_args) %in% c("Arguments", "Default"))){
        rlang::abort("`tibble_args` names should be Arguments and Default")
    }
    
    cli::cli_h1("Model Specification")
    cat("\n")
    cli::cli_alert_success(model_desc)
    cli::cli_h2("Main Arguments:")
    print(tibble_args)
    cat("\n")
    cli::cli_h1("End Report")
}

#' @export
parse_index <- function(dt){
    
    if(!rlang::inherits_any(dt, "data.table")){
        rlang::abort("`dt` should be a data.table")
    }
    
    val <- dt[, .SD, .SDcols = collapse::ffirst(which(sapply(dt, class) %chin% c("Date", "POSIXt")))]
    
    return(val)
    
}

#' @export
get_period_statistic <- function(idx, period = "1 day") {
    
    if (!is_date_class(idx)) rlang::abort("idx must be date or date-time class.")
    if (!is.character(period)) rlang::abort("period must be character data.")
    
    data <- data.table::data.table(index = idx)
    
    data[, index := lubridate::floor_date(index, unit = period)]
    
    data <- data[, .N, by = "index"][, collapse::fmedian(.SD), .SDcols = 2][[1]]
    
    return(data)
    
}


#' @export
parse_frequency <- function(idx, period = "auto"){
    
    idx <- sort(unique(idx))
    
    timetk:::check_weeks(period)
    
    # Setup inputs
    template <- time_scale_template()
    
    # Get timeseries summary attributes
    ts_summary <- timetk::tk_get_timeseries_summary(idx)
    ts_nobs    <- ts_summary$n.obs
    ts_scale   <- ts_summary$scale
    
    # Frequency Calculation
    if (is.numeric(period)) {
        # 1. Numeric Periods
        freq <- period
        
    } else if (tolower(period) != "auto") {
        
        # 2. Text (e.g. period = "2 Weeks")
        freq <- get_period_statistic(idx, period = period)
        
    } else {
        # 3. period = "auto"
        periodicity_target <- template[time_scale == ts_scale, "frequency"][[1]]
        
        freq <- get_period_statistic(idx, period = periodicity_target)
        
        # Insufficient observations: nobs-to-freq should be at least 3-1
        
        if (ts_nobs < 3*freq) {
            
            periodicity_target <- template[time_scale == ts_scale, "frequency"][[1]]
            
            freq <- get_period_statistic(idx, period = periodicity_target)
        }
        
        if (ts_nobs < 3*freq) {
            freq <- 1
        }
    }
    
    return(freq)
    
}

#' @export
one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=TRUE){
    
    check_factor <- function(dt){
        v<-which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))
        return(v)
    }
    
    safe_check_factor <- purrr::safely(check_factor, otherwise = NULL, quiet = TRUE)
    
    res <- safe_check_factor(dt)
    
    if (is.null(res$error)){
        
        # Automatically get the unordered factor columns
        if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
        
        # Build tempDT containing and ID column and 'cols' columns
        tempDT <- dt[, cols, with=FALSE]
        tempDT[, ID := .I]
        setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
        for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
        
        # One-hot-encode
        p <- data.table::melt(tempDT, id = 'ID', value.factor = T)
        data.table::setkey(p, ID)
        newCols <- data.table::dcast(p, ID ~ value, drop = dropUnusedLevels, fun = length)
        
        if (ncol(newCols)<3){
            newCols <- newCols[, c(2:ncol(newCols)), with=FALSE]
        } else{
            newCols <- newCols[, c(3:ncol(newCols)), with=FALSE]
        }
        
        # Combine binarized columns with the original dataset
        result <- cbind(dt, newCols)
        
        # If dropCols = TRUE, remove the original factor columns
        if(dropCols == TRUE){
            result <- result[, !cols, with=FALSE]
        } 
        
    } else {
        
        result <- NULL
        
    }
    
    return(result)
}

#' @export
zero_variance <- function(dt, percentage = 1){
    
    n <- nrow(dt)/(nrow(dt)*percentage)
    
    dt<-dt[, lapply(.SD, function(v) if(data.table::uniqueN(v, na.rm = TRUE) > n) v)]
    
    return(dt)
    
}


#' @export
fast_to_matrix <- function(dt){
    
    if (is.null(dt)){
        res <- NULL
    } else {
        res <- collapse::qM(dt)
    }
    
    return(res)
    
}












