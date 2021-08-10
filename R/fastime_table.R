

#' @export
fastime_table <- function(..., .data, .split, .future_horizon, .id = NULL){
    
    l <- tibble::tibble(.model = list(...)) %>%
        tibble::rowid_to_column(var = ".model_id") %>%
        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))
    
    idx <- .data %>% data.table::as.data.table() %>% parse_index() %>% names()
    
    .data <- .data %>% timetk::future_frame(.date_var = {{idx}}, .length_out = .future_horizon, .bind_data = TRUE)
    
    if (!is.null(.id)){
        
        id_var_expr    <- enquo(.id)
        
        # SPLIT FUTURE AND ACTUAL DATA
        
        future_data_tbl <- .data %>%
            modeltime::panel_tail(id = !!id_var_expr, n = .future_horizon) %>%
            data.table::as.data.table()
        
        groups <- future_data_tbl$id %>% unique() %>% length()
        
        n_group <- .data %>%
            group_by(!!id_var_expr) %>%
            summarise(n = n() - (dim(future_data_tbl)[1]/groups))
        
        actual_data_tbl <- .data %>%
            inner_join(n_group, by = rlang::quo_name(id_var_expr)) %>%
            group_by(!!id_var_expr) %>%
            slice(seq(first(n))) %>%
            ungroup() %>%
            select(-n) %>%
            data.table::as.data.table()
        
    } else {
        
        future_data_tbl <- .data %>% dplyr::slice_tail(n = .future_horizon) %>% data.table::as.data.table()
        
        n <- dim(.data)[1]-.future_horizon
        
        actual_data_tbl <- .data %>% dplyr::slice_head(n = n) %>% data.table::as.data.table()
        
    }
    
    if (rlang::inherits_all(.split, c("ts_cv_split", "rsplit"))){
        
        .split <- data.table::data.table(.train = list(rsample::training(.split)),
                                         .test  = list(rsample::testing(.split)))
        
        data.table::setattr(.split, "class", union("f_ts_split", class(.split)))
        
    } else {
        
        if (rlang::inherits_any(.split, "time_series_cv")){
            
            .split <- .split %>% purrr::pluck("splits")
            
            .train <- list(); .test <- list()
            for(j in 1:length(.split)){
                .train[[j]] <- rsample::training(.split[[j]])
                .test[[j]] <- rsample::testing(.split[[j]])
            }
            
            .split <- data.table::data.table(.train = .train, .test = .test)
            
            data.table::setattr(.split, "class", union("f_ts_splits", class(.split)))
            
        } else {
            rlang::abort("`.split` should be of class `ts_cv_split` (created with timetk::time_series_split()) or `time_series_cv`
                     (created with timetk::time_series_cv() function).")
        }
    }
    
    # Atributos
    
    attr(l, ".actual") <- actual_data_tbl
    attr(l, ".future") <- future_data_tbl
    attr(l, ".split")  <- .split
    attr(l, ".status") <- "Specification"
    
    if (!is.null(.id)){
        class(l) <- c("fastime_grouped_tbl", class(l))
    } else {
        class(l) <- c("fastime_tbl", class(l))
    }
    
    
    return(l)
    
}

# Methods
#' @export
future_data <- function(fastime_tbl, ...){
    UseMethod("future_data")
}

#' @export
actual_data <- function(fastime_tbl, ...){
    UseMethod("actual_data")
}

#' @export
split_data <- function(fastime_tbl, ...){
    UseMethod("split_data")
}

#' @export
fastime_status <- function(fastime_tbl, ...){
    UseMethod("fastime_status")
}

## Simple Fastime Table
#' @export
future_data.fastime_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".future"))
}

#' @export
actual_data.fastime_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".actual"))
}

#' @export
split_data.fastime_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".split"))
}

#' @export
fastime_status.fastime_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".status"))
}

## Grouped Fastime Table
#' @export
future_data.fastime_grouped_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".future"))
}

#' @export
actual_data.fastime_grouped_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".actual"))
}

#' @export
split_data.fastime_grouped_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".split"))
}

#' @export
fastime_status.fastime_grouped_tbl <- function(fastime_tbl, ...){
    tibble::as_tibble(attr(fastime_tbl, ".status"))
}


