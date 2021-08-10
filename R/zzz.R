

# IMPORTS ----

#' @import data.table
#' @import modeltime
#' @import collapse
#' @import patchwork
 
# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    
    options(
        time_scale_template = time_scale_template()
    )
    
}

