
#' @export
fastime_check_residuals <- function(model, lag = 12, ci = 0.95, ...){
    UseMethod("fastime_check_residuals")
}

#' @export
fastime_check_residuals.default <- function(model, lag = 12, ci = 0.95, ...){
    rlang::abort(stringr::str_glue("Received an object of class: {class(model)[1]}. Check available models."))
}

#' @export
fastime_check_residuals.fastime_bridge <- function(model, lag = 12, ci = 0.95, ...){
    
    n_lags <- lag
    
    g1 <- model$data %>% timetk::plot_time_series(
        .date_var    = date,
        .value       = .residuals,
        .interactive = FALSE,
        .line_size   = 0.8,
        .smooth      = FALSE,
        .title       = "Model Residuals" 
    ) 
    
    ci_value <- stats::qnorm((1 + ci)/2)/sqrt(nrow(model$data))
    
    df_acf <- stats::acf(model$data$.residuals, lag.max = n_lags, plot = FALSE, na.action = stats::na.pass) 
    
    df_acf <- tibble::tibble(Lag   = df_acf$lag / (length(df_acf$lag)-1),
                             ACF   = df_acf$acf,
                             lower = -ci_value,
                             upper = ci_value) %>%
                dplyr::mutate(ymax = ifelse(ACF > 0, ACF, 0), 
                              ymin = ifelse(ACF < 0, ACF, 0))

    
    g2 <- ggplot2::ggplot(data = df_acf, mapping = ggplot2::aes_string(x = "Lag")) + 
        ggplot2::geom_linerange(mapping = ggplot2::aes_string(ymin = "ymin", 
                                                              ymax = "ymax"), 
                                colour = "#000000", linetype = "solid", size = 1) +
        ggplot2::geom_line(ggplot2::aes(y = ci_value),
                           linetype = "dashed",
                           size = 0.8,
                           color    = "red") +
        ggplot2::geom_line(ggplot2::aes(y = -ci_value),
                           linetype = "dashed",
                           size = 0.8,
                           color = "red") + 
        ggplot2::ggtitle("ACF of Residuals") +
        ggplot2::ylab("Correlation") 
    
    bx <- vector(length = n_lags, mode = "numeric")
    for(i in 1:n_lags){
        bx[i]<-stats::Box.test(model$data$.residuals, lag = i, type = "Ljung-Box")$p.value
    }
    
    
    df <- data.frame(Lag = 1L:n_lags, `p value` = bx, lower = -0.05, upper = 0.05)
    colnames(df) <- c("Lag", "p value", "lower", "upper")
    
    g3 <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x = "Lag")) + 
        ggplot2::geom_point(mapping = ggplot2::aes_string(y = "`p value`")) + 
        ggplot2::scale_y_continuous(limits = c(0, 1)) + ggplot2::ggtitle("Ljung-Box Statistic") +
        ggplot2::scale_x_continuous(breaks = seq(0, n_lags + 0.5, 2.5)) +
        ggplot2::geom_line(ggplot2::aes(y = 0.05),
                           linetype = 1,
                           size = 1,
                           color    = "red") +
        ggplot2::geom_line(ggplot2::aes(y = -0,05),
                           linetype = 1,
                           size = 2,
                           color = "red") 
    
    df <- data.frame(residuals = model$data$.residuals)
    
    bins <- min(500, grDevices::nclass.FD(na.exclude(model$data$.residuals)))
    
    g4 <- ggplot2::ggplot(data = df) + 
        ggplot2::geom_histogram(mapping = aes(x = residuals, y=..density..), 
                                fill="cornflowerblue", 
                                colour="black", 
                                bins = bins) +
        ggplot2::ggtitle("Normality") +
        ggplot2::stat_function(fun = dnorm, args = list(mean = mean(df$residuals, na.rm = TRUE), 
                                                        sd = sd(df$residuals, na.rm = TRUE)), 
                               colour = "red", 
                               size = 0.75)
        
    
    p1<- ((g1 + g2) / (g3 + g4)) 
    
    print(p1 & ggthemes::theme_wsj(base_size = 8))
    
    return(invisible(model))
}





    











