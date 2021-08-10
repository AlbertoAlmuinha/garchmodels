
#' @export
is_fastime_table <- function(object){
    
    if (rlang::inherits_any(object, c("fastime_tbl", "fastime_grouped_tbl"))){
        res<-TRUE
    } else {
        res<-FALSE
    }
    
    return(res)
}


#' @export
get_model_description <- function(object){
    UseMethod("get_model_description")
}

#' @export
get_model_description.fastime_bridge <- function(object){
    return(object$desc)
}