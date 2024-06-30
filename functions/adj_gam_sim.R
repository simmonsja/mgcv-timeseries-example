
################################################################################
################################################################################
# Underlying functions for generating synthetic timeseries data

# these funtions are borrowed from mgcv::gamSim() and adjusted to suit
f0 <- function(x) {
    out <- 1.5 * sin(pi * x) + 0.5 * exp(2 * x)
    out <- out - mean(out)
    return(out)
}
f1 <- function(x) {
    exp(2 * x)
}
f2 <- function(x) {
    out <- 0.2*x^11*(10*(1-x))^6+6*(10*x)^3*(1-x)^10
    out <- 0.3*(out - mean(out))
    return(out)
}
f3 <- function(x) {
    0*x
}
f4 <- function(x) {
    1.2*(x * 5 + 1 * sin(pi * x) + exp(x) - 1)
}

################################################################################
################################################################################
# Generate a combined synthetic timeseries using the functions outlined above

tsSim <- function(data_in, scale = 2) {
    f_scale <- 0.7 # scale factor for the functions
    err <- rnorm(nrow(data_in), 0, scale) # iid error
    # generate some autoregressive residuals
    ar_err <- arima.sim(list(order = c(1, 0, 0), ar = 0.8), nrow(data_in)) * scale
    tmp <- data_in %>% mutate(ar_err = ar_err, err = err)
    print(ggplot(
        tmp, aes(x = month, y = ar_err)
    ) + geom_line() +
    theme_bw())
    print(
        wrap_plots(
            ggplot(
                data.frame(time_plot = data_in$month, moy = f2(data_in$moy), time = f3(data_in$time), rain = f4(data_in$rain), SOI = f0(data_in$SOI)),
            ) + 
            # geom_bar(stat = "identity", aes(x = time, y = rain)) +
            geom_line(aes(x = time_plot, y = SOI, color = "SOI")) +
            geom_line(aes(x = time_plot, y = moy, color = "moy")) +
            geom_line(aes(x = time_plot, y = rain, color = "rain")) +
            scale_color_manual(values = c("SOI" = "black", "moy" = "red", "rain" = "blue")) +
            # add legend
            theme(legend.position = "bottom"),
            ggplot(data_in, aes(x=month, y=rain)) +
                geom_bar(stat="identity"),
            ncol=1
    ))
    # return the synthetic data
    data_out <- data_in %>%
        mutate(
            log_flow = f_scale*(f2(moy) + f4(rain) + f0(SOI) + f3(time)) + ar_err,
            flow = exp(log_flow)
        )
    return(data_out)
}

################################################################################
################################################################################

# generate ground truth function data for later comparison to spline fits
generate_gtdata <- function() {
    f_scale <- 0.7 # scale factor for the functions
    # generate a curve from 100 uniformly sampled points for each variable
    gt_data <- data.frame(
        moy = sort(runif(100, 0, 1)),
        time = sort(runif(100, 0, 1)),
        rain = sort(runif(100, 0, 1)),
        SOI = sort(runif(100, 0, 1))
    ) %>% mutate(
        f_moy = f_scale*f2(moy),
        f_time = f_scale*f3(time),
        f_rain = f_scale*f4(rain),
        f_SOI = f_scale*f0(SOI)
    )
    return(gt_data)
}

################################################################################
################################################################################