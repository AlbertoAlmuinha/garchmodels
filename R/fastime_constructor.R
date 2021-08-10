#' Constructor for creating fastime models
#'
#' These functions are used to construct new `fastime` bridge functions
#'
#' @param class A class name that is used for creating custom printing messages
#' @param models A list containing one or more models
#' @param data A data.table containing 4 columns:
#'  (date column with name that matches input data), .actual, .fitted, and .residuals.
#' @param extras An optional list.
#' @param desc An optional model description to appear when printing your fastime objects
#'
#' @export
new_fastime_bridge <- function(class, models, data, extras = NULL, desc = NULL) {
    
    if (missing(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    if (!is.character(class)) rlang::abort("'class' must be a character vector. This is used to define a print method.")
    
    msg <- "'models' should be a list:\n 1. The first model should named 'model_1'.\n 2. Subsequent models should be named 'model_2' and so on."
    if (missing(models)) rlang::abort(paste0("'models' is missing.\n\n", msg))
    if (!is.list(models)) rlang::abort(paste0("'models' is not a list().\n\n", msg))
    if (!all(stringr::str_detect(names(models), pattern = "^model_"))) rlang::abort(paste0("'model' has bad list names. Try naming 'model_1'.\n\n", msg))
    
    msg <- "'data' should be a data table containing 4 columns:\n 1. date column (with name that matches input data)\n 2. .actual (these are the original values your model trains from)\n 3. .fitted (these are your model's in-sample training results)\n 4. .residuals (these are your model's in-sample errors)"
    if (missing(data)) rlang::abort(paste0("'data' is missing.\n\n", msg))
    if (!data.table::is.data.table(data)) rlang::abort(paste0("'data' is not a data.table\n\n", msg))
    if (ncol(data) != 4) rlang::abort(paste0("'data' does not have 4 columns\n\n", msg))
    if (!all(c(".actual", ".fitted", ".residuals") %in% names(data))) {
        rlang::abort(paste0("Column names don't contain: .actual, .fitted, and .residuals.\n\n", msg))
    }
    
    msg <- "'extras' should be a list."
    if (!is.null(extras)) {
        if (!is.list(extras)) rlang::abort(msg)
    }
    
    msg <- "'desc' should be a single character value. It's often used for printing a description of your model using a print method."
    if (!is.null(desc)) {
        if (!is.character(desc)) rlang::abort(paste0("'desc' is not of class character.\n", msg))
        if (length(desc) != 1) rlang::abort(paste0("'desc' length is not 1.\n", msg))
    }
    
    # CONSTRUCTOR
    ret <- list(
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
    
    class(ret) <- c(class, "fastime_bridge")
    
    return(ret)
    
}

#' @export
print.fastime_bridge <- function(x, ...){
    
    cli::cli_h1(stringr::str_glue("{x$desc} Fitted Model"))
    cat("\n")
    cli::cli_alert_success("Parameters Report:")
    cli::cli_h2("Parameters Table:")
    
    param_table <- broom::tidy(x$models$model_1) %>%
        dplyr::bind_cols(
            stats::confint(x$models$model_1) %>%
                tibble::as_tibble() %>%
                purrr::set_names(c("conf.low", "conf.high"))
        )
    
    print(param_table)
    
    cat("\n")
    cli::cli_alert_success("Stats Report:")
    cli::cli_h2("Stats Table:")
    
    print(broom::glance(x$models$model_1))
    
    cat("\n")
    cli::cli_h1("End Report")
    invisible(x)
    
}