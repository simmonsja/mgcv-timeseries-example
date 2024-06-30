################################################################################
################################################################################
# Make predictions from a fitted gam and plot against data

plot_model_prediction <- function(model, data, exp_bool = FALSE, ci = 0.89) {
    # make preds and get mean and sd - follow code of Pederson HGAM paper
    preds <- predict(model, se.fit = T, type = "response") %>% as.data.frame() %>%
        mutate(
            lower = if_else(rep(exp_bool,length(se.fit)), exp(fit - 2*se.fit), fit - 2*se.fit),
            upper = if_else(rep(exp_bool,length(se.fit)), exp(fit + 2*se.fit), fit + 2*se.fit),
            fit = if_else(rep(exp_bool,length(fit)), exp(fit), fit),
            .row = 1:nrow(.)
        )
        # exponentiate all the values except .row if exp = TRUE
        # mutate(.fitted = if_else(rep(exp_bool,length(.fitted)), exp(.fitted), .fitted)) %>%
        # pivot_wider(names_from = .draw, values_from = .fitted) %>% 
        # mutate(
        #     mean = rowMeans(across(-.row)), 
        #     lower = apply(across(-.row), 1, quantile, (1-ci)/2), 
        #     upper = apply(across(-.row), 1, quantile, 1-(1-ci)/2)
        # )
        
    # not plot against data
    ggplot(data %>% 
               mutate(obs_num = 1:length(flow)) %>% 
               left_join(preds, by = c("obs_num" = ".row")),
           aes(x = month, y = flow)) +
        geom_line() + 
        geom_line(aes(y = fit), color = "red") +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "red") +
        labs(
            x = "Year",
            y = "Streamflow (ML)"
        )
}


################################################################################
################################################################################


################################################################################
################################################################################
