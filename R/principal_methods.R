

#' @export
fit <- function(model, formula, data, ...){
    UseMethod("fit")
}

#' @export
fit.default <- function(model, formula, data, ...){
    rlang::abort(stringr::str_glue("Received an object of class: {class(model)[1]}. Check available models."))
}

#' @export
predict <- function(model, new_data, ...){
    UseMethod("predict")
}

#' @export
predict.default <- function(model, new_data, ...){
    rlang::abort(stringr::str_glue("Received an object of class: {class(model)[1]}. Check available models."))
}