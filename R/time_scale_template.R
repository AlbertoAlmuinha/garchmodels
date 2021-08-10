#' Get and modify the Time Scale Template
#'
#' @param .data A `tibble` with a "time_scale", "frequency", and "trend" columns.
#'
#' @details
#'
#' Used to get and set the time scale template, which is used by `tk_get_frequency()`
#' and `tk_get_trend()` when `period = "auto"`.
#'
#' The predefined template is stored in a function `tk_time_scale_template()`.
#' This is the default used by `timetk`.
#'
#' __Changing the Default Template__
#' - You can access the current template with `get_tk_time_scale_template()`.
#' - You can modify the current template with `set_tk_time_scale_template()`.
#'
#'
#' @examples
#'
#' get_tk_time_scale_template()
#'
#' set_time_scale_template(time_scale_template())
#'



#' @export
#' @rdname time_scale_template
set_time_scale_template <- function(.data) {
    if (!missing(.data)) {
        if (!rlang::inherits_any(.data, "data.table")){
            options(time_scale_template = data.table::as.data.table(.data))
        }
        
    }
    #getOption('time_scale_template')
}

#' @export
#' @rdname time_scale_template
get_time_scale_template <- function() {
    getOption('time_scale_template')
}

#' @export
#' @rdname time_scale_template
time_scale_template <- function() {
    
    data <- data.table::data.table(
        time_scale = c("second", "minute", "hour", "day", "week", "month", "quarter", "year"),
        frequency  = c("1 hour", "1 day", "1 day",  "1 week", "1 quarter", "1 year", "1 year", "5 years")
    )
    
    return(data)
    
}