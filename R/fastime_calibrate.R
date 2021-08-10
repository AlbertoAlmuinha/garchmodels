
#' @export
fastime_calibrate <- function(object, new_data, ...){
    
    # Checks
    if (rlang::is_missing(new_data)) {
        rlang::abort("Missing 'new_data'. Try adding a test data set using rsample::testing(splits). See help for more info: ?modeltime_calibrate ")
    }
    
    if (!is_fastime_table(object)){
        rlang::abort("'object' should be a fastime_tbl created with the fastime_table() function.")
    }
    
    UseMethod("fastime_calibrate")
}

fastime_calibrate.default <- function(object, new_data, ...){
    
    rlang::abort(stringr::str_glue("Received an object of class: {class(object)[1]}. Only allowed fastime_tbl"))
}

fastime_calibrate.fastime_tbl <- function(object, new_data, ...){
    
    
    
}



dt <- data.table::data.table(.model = as.list(pluck(tbl$.model)),
                       .test  = as.list(list(split_data(tbl)$.test[[1]], split_data(tbl)$.test[[1]], split_data(tbl)$.test[[1]]))
                       )

dt[, eval(".pred") := purrr::map2(.model, .test, predict)][., eval(".actual") := function(x) ]

.model <- dt %>% pluck(".model")
.test <- dt %>% pluck(".test")

microbenchmark::microbenchmark(
    map2(.model, .test, predict),
    dt[, pred := purrr::map2(.model, .test, predict)]
)

for(j in 1:length(.model)){
    
}

tictoc::tic()
map2(.model, .test, predict)
tictoc::toc()

tictoc::tic()
dt[, pred = purrr::map2(.model, .test, predict)]
tictoc::toc()



