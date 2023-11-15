library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(maddTools)
library(cowplot)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Simple Logit Model
load(here("models", "model_mnl.RData"))



# -------------
## Uncertainty

coefs_mnl <- coef(model_mnl)
cov_mnl <- vcov(model_mnl)

coef_draws_mnl <- as.data.frame(mvrnorm(10^4, coefs_mnl, cov_mnl))

coef_ci_mnl <- ci(coef_draws_mnl, ci = 0.95)
coef_ci_mnl

coef_ci_mnl$par <- row.names(coef_ci_mnl)
coef_price <- coef_ci_mnl %>% filter(par == 'price')
coef_opacity <- coef_ci_mnl %>% filter(par == 'opacity_yes')
coef_quantity <- coef_ci_mnl %>% filter(par == 'quantity')
coef_plastic_sustainable <- coef_ci_mnl %>% filter(par == 'plastic_sustainable')
coef_logo <- coef_ci_mnl %>% filter(par == 'logo_yes')
coef_volume16 <- coef_ci_mnl %>% filter(par == 'volume16')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)


df_price <- data.frame(level = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_price$mean,
        lower = diff*coef_price$lower,
        upper = diff*coef_price$upper)

df_opacity <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_opacity$mean,
        lower = diff*coef_opacity$lower,
        upper = diff*coef_opacity$upper)

df_quantity <- data.frame(level = c(25, 50, 75, 100)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_quantity$mean,
        lower = diff*coef_quantity$lower,
        upper = diff*coef_quantity$upper)

df_plastic_sustainable <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_plastic_sustainable$mean,
        lower = diff*coef_plastic_sustainable$lower,
        upper = diff*coef_plastic_sustainable$upper)

df_logo <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_logo$mean,
        lower = diff*coef_logo$lower,
        upper = diff*coef_logo$upper)

df_volume16 <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_volume16$mean,
        lower = diff*coef_volume16$lower,
        upper = diff*coef_volume16$upper)


# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_price$lower, df_opacity$lower, df_quantity$lower, 
    df_plastic_sustainable$lower, df_logo$lower, df_volume16$lower)))
ymax <- ceiling(max(c(
    df_price$upper, df_opacity$upper, df_quantity$upper, 
    df_plastic_sustainable$upper, df_logo$upper, df_volume16$upper)))

# Plot the utility for each attribute *with 95% CI*
plot_price <- df_price %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($)', y = 'Utility') +
    theme_bw()


plot_opacity <- df_opacity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Opacity', y = 'Utility') +
    theme_bw()

plot_quantity <- df_quantity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (no. Cups)', y = 'Utility') +
    theme_bw()

plot_plastic_sustainable <- df_plastic_sustainable %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sustainable Plastic', y = 'Utility') +
    theme_bw()

plot_logo <- df_logo %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Logo Included', y = 'Utility') +
    theme_bw()

plot_volume16 <- df_volume16 %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Volume (16 oz)', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
plot_mnl_linear_unc <- plot_grid(
    plot_price, plot_opacity, plot_quantity, plot_plastic_sustainable, plot_logo, plot_volume16,
    nrow = 1
)

# Save plots 
ggsave(
    filename = here('figs', 'mnl_simple_logit.png'), 
    plot = plot_mnl_linear_unc, 
    width = 10, height = 2.3
)


# -------------
## WTP

wtp(model_mnl, "price")

wtp_mnl <- coefs_mnl / (-1*coefs_mnl[1])
wtp_mnl

coefs_mnl <- coef(model_mnl)
cov_mnl <- vcov(model_mnl)

coef_draws_mnl <- as.data.frame(mvrnorm(10^4, coefs_mnl, cov_mnl))

coef_ci_mnl$par <- row.names(coef_ci_mnl)


wtp_mnl_draws = -1*(coef_draws_mnl[,2:6] / coef_draws_mnl[,1])
wtp_mnl_ci <- ci(wtp_mnl_draws)
wtp_mnl_ci

wtp_mnl_ci$par <- row.names(wtp_mnl_ci)
wtp_opacity <- wtp_mnl_ci %>% filter(par == 'opacity_yes')
wtp_quantity <- wtp_mnl_ci %>% filter(par == 'quantity')
wtp_plastic_sustainable <- wtp_mnl_ci %>% filter(par == 'plastic_sustainable')
wtp_logo <- wtp_mnl_ci %>% filter(par == 'logo_yes')
wtp_volume16 <- wtp_mnl_ci %>% filter(par == 'volume16')


df_opacity <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_opacity$mean,
        lower = diff*wtp_opacity$lower,
        upper = diff*wtp_opacity$upper)

df_quantity <- data.frame(level = c(25, 50, 75, 100)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_quantity$mean,
        lower = diff*wtp_quantity$lower,
        upper = diff*wtp_quantity$upper)

df_plastic_sustainable <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_plastic_sustainable$mean,
        lower = diff*wtp_plastic_sustainable$lower,
        upper = diff*wtp_plastic_sustainable$upper)

df_logo <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_logo$mean,
        lower = diff*wtp_logo$lower,
        upper = diff*wtp_logo$upper)

df_volume16 <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_volume16$mean,
        lower = diff*wtp_volume16$lower,
        upper = diff*wtp_volume16$upper)

ymin <- floor(min(c(
    df_opacity$lower, df_quantity$lower, 
    df_plastic_sustainable$lower, df_logo$lower, df_volume16$lower)))
ymax <- ceiling(max(c(
    df_opacity$upper, df_quantity$upper, 
    df_plastic_sustainable$upper, df_logo$upper, df_volume16$upper)))
ymin <- ymin + 0.8
ymax <- ymax - 0.8

plot_opacity <- df_opacity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Opacity', y = 'WTP') +
    theme_bw()

plot_quantity <- df_quantity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.7, fill = 'red') + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (no. Cups)', y = 'WTP') +
    theme_bw()

plot_plastic_sustainable <- df_plastic_sustainable %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sustainable Plastic', y = 'WTP') +
    theme_bw()

plot_logo <- df_logo %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Logo Included', y = 'WTP') +
    theme_bw()

plot_volume16 <- df_volume16 %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Volume (16 oz)', y = 'WTP') +
    theme_bw()


plot_mnl_wtp <- plot_grid(
    plot_opacity, plot_quantity, plot_plastic_sustainable, plot_logo, plot_volume16,
    nrow = 1
)

# Save plots 
ggsave(
    filename = here('figs', 'wtp_mnl_simple.png'), 
    plot = plot_mnl_wtp, 
    width = 10, height = 2.3
)

# -------------
## Market Simulation

scenarios_all <- read_csv(here('data', 'allMarket.csv'))
head(scenarios_all)

# Use the predict() function to compute the probabilities
sim_mnl_all <- predict(
    model_mnl,
    newdata = scenarios_all, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE
)

sim_mnl_all


sim_all_plot <- sim_mnl_all %>% 
    mutate(label = c("OMAO", "Eco Products (9 oz)", "Eco Products (16 oz)", 
                     "World Centric (9 oz)", "World Centric (16 oz)", 
                     "Solo (9 oz)", "Solo (16 oz)",
                     "Great Value (9 oz)", "Great Value (16 oz)")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    geom_text(aes(label = round(predicted_prob*100, 1), x = label, y = predicted_prob), vjust = -4.2) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Product', y = 'Market Share (%)') +
    theme_bw()

ggsave(
    filename = here('figs', 'sims_price_20.png'), 
    plot = sim_all_plot, 
    width = 12, height = 4
)

#### 15 cents
scenarios_all_rec <- scenarios_all
scenarios_all_rec[1,3] = 0.15

sim_mnl_all_rec <- predict(
    model_mnl,
    newdata = scenarios_all_rec, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE
)

sim_mnl_all_rec

sim_15_plot <- sim_mnl_all_rec %>% 
    mutate(label = c("OMAO", "Eco Products (9 oz)", "Eco Products (16 oz)", 
                     "World Centric (9 oz)", "World Centric (16 oz)", 
                     "Solo (9 oz)", "Solo (16 oz)",
                     "Great Value (9 oz)", "Great Value (16 oz)")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) + 
    geom_text(aes(label = round(predicted_prob*100, 1), x = label, y = predicted_prob), vjust = -4.2) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Product', y = 'Market Share') +
    theme_bw()

ggsave(
    filename = here('figs', 'sims_price_15.png'), 
    plot = sim_15_plot, 
    width = 12, height = 4
)

#### Appearance

scenarios_all_rec_app <- scenarios_all_rec
scenarios_all_rec_app[1,4] = 0
scenarios_all_rec_app[1,7] = 0


sim_mnl_all_rec_app <- predict(
    model_mnl,
    newdata = scenarios_all_rec_app, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE
)

sim_mnl_all_rec_app

sim_app_plot <- sim_mnl_all_rec_app %>% 
    mutate(label = c("OMAO", "Eco Products (9 oz)", "Eco Products (16 oz)", 
                     "World Centric (9 oz)", "World Centric (16 oz)", 
                     "Solo (9 oz)", "Solo (16 oz)",
                     "Great Value (9 oz)", "Great Value (16 oz)")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) + 
    geom_text(aes(label = round(predicted_prob*100, 1), x = label, y = predicted_prob), vjust = -4.2) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Product', y = 'Market Share') +
    theme_bw()

ggsave(
    filename = here('figs', 'sims_price_app.png'), 
    plot = sim_app_plot, 
    width = 12, height = 4
)


# -------------
## Sensitivity Analysis


prices <- (seq(5, 30))/100 # Define sensitivity price levels
n <- length(prices) # Number of simulations (21)
scenarios_price <- rep_df(scenarios_all, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 9) # Reset obsIDs

scenarios_price$price[which(scenarios_price$altID == 1)] <- prices 
head(scenarios_price)

sens_price <- predict(
    model_mnl,
    newdata = scenarios_price, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(price, starts_with("predicted_")) 

sens_price

share_price_plot <- 
    sens_price %>% 
    ggplot(aes(
        x = price, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_price %>% filter(price <= 0.25, price >= 0.1), 
        linetype = "solid") +
    expand_limits(x = c(0.05, 0.30), y = c(0, 0.5)) +
    labs(x = 'Price ($)', y = 'Market Share') +
    theme_bw()

share_price_plot

ggsave(
    filename = here('figs', 'share_price_plot.png'), 
    plot = share_price_plot,
    width = 5, height = 4
)

### Revenue
marketSize <- 100000000
rev_data <- sens_price %>%
    mutate(
        rev_mean = price*marketSize*predicted_prob / 10^6, # Convert to thousands
        rev_low  = price*marketSize*predicted_prob_lower / 10^6,
        rev_high = price*marketSize*predicted_prob_upper / 10^6)

rev_price_plot <- rev_data %>% 
    ggplot(aes(x = price, y = rev_mean, ymin = rev_low, ymax = rev_high)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = rev_data %>% filter(price <= 0.25, price >= 0.1), 
        linetype = "solid") +
    # expand_limits(x = c(10, 30), y = c(0, 1)) +
    labs(x = 'Price ($ Dollars)', y = 'Revenue ($ Millions)') +
    theme_bw()

rev_price_plot


ggsave(
    filename = here('figs', 'rev.png'), 
    plot = rev_price_plot, 
    width = 8, height = 5
)

## Quantity

quantities <- seq(25, 100, by = 5) # Define sensitivity price levels
n_q <- length(quantities) # Number of simulations (21)
scenarios_quant <- rep_df(scenarios_all, n_q) # Repeat the baseline data frame n times
scenarios_quant$obsID <- rep(seq(n_q), each = 9) # Reset obsIDs

scenarios_quant$quantity[which(scenarios_quant$altID == 1)] <- quantities 
head(scenarios_quant)

sens_quant <- predict(
    model_mnl,
    newdata = scenarios_quant, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(quantity, starts_with("predicted_")) 

sens_quant

share_quant_plot <- 
    sens_quant %>% 
    ggplot(aes(
        x = quantity, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_quant %>% filter(quantity <= 90, quantity >= 35), 
        linetype = "solid") +
    expand_limits(x = c(25, 100), y = c(0, 0.5)) +
    labs(x = 'Quantity', y = 'Market Share') +
    theme_bw()

share_quant_plot

## Opacity

opacity <- c(0,1) # Define sensitivity price levels
n_o <- length(opacity) # Number of simulations (21)
scenarios_op <- rep_df(scenarios_all, n_o) # Repeat the baseline data frame n times
scenarios_op$obsID <- rep(seq(n_o), each = 9) # Reset obsIDs

scenarios_op$opacity_yes[which(scenarios_op$altID == 1)] <- opacity 
head(scenarios_op)

sens_op <- predict(
    model_mnl,
    newdata = scenarios_op, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(opacity_yes, starts_with("predicted_")) 

sens_op

share_op_plot <- 
    sens_op %>% 
    ggplot(aes(
        x = opacity_yes, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    expand_limits(x = c(0,1), y = c(0, 0.5)) +
    labs(x = 'Opacity', y = 'Market Share') +
    theme_bw()

share_op_plot


## Sustainability

sus <- c(0,1) # Define sensitivity price levels
n_s <- length(sus) # Number of simulations (21)
scenarios_sus <- rep_df(scenarios_all, n_s) # Repeat the baseline data frame n times
scenarios_sus$obsID <- rep(seq(n_s), each = 9) # Reset obsIDs

scenarios_sus$plastic_sustainable[which(scenarios_sus$altID == 1)] <- sus 
head(scenarios_sus)

sens_sus <- predict(
    model_mnl,
    newdata = scenarios_sus, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(plastic_sustainable, starts_with("predicted_")) 

sens_sus

share_sus_plot <- 
    sens_sus %>% 
    ggplot(aes(
        x = plastic_sustainable, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    expand_limits(x = c(0,1), y = c(0, 0.5)) +
    labs(x = 'Sustainable', y = 'Market Share') +
    theme_bw()

share_sus_plot


## Logo

logo <- c(0,1) # Define sensitivity price levels
n_l <- length(logo) # Number of simulations (21)
scenarios_logo <- rep_df(scenarios_all, n_l) # Repeat the baseline data frame n times
scenarios_logo$obsID <- rep(seq(n_l), each = 9) # Reset obsIDs

scenarios_logo$logo_yes[which(scenarios_logo$altID == 1)] <- logo
head(scenarios_logo)

sens_logo <- predict(
    model_mnl,
    newdata = scenarios_logo, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(logo_yes, starts_with("predicted_")) 

sens_logo

share_logo_plot <- 
    sens_logo %>% 
    ggplot(aes(
        x = logo_yes, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    expand_limits(x = c(0,1), y = c(0, 0.5)) +
    labs(x = 'Logo', y = 'Market Share') +
    theme_bw()

share_logo_plot

## Volume

vol <- c(0,1) # Define sensitivity price levels
n_v <- length(vol) # Number of simulations (21)
scenarios_vol <- rep_df(scenarios_all, n_v) # Repeat the baseline data frame n times
scenarios_vol$obsID <- rep(seq(n_v), each = 9) # Reset obsIDs

scenarios_vol$volume16[which(scenarios_vol$altID == 1)] <- vol
head(scenarios_logo)

sens_vol <- predict(
    model_mnl,
    newdata = scenarios_vol, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(volume16, starts_with("predicted_")) 

sens_vol

share_vol_plot <- 
    sens_vol %>% 
    ggplot(aes(
        x = volume16, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    expand_limits(x = c(0,1), y = c(0, 0.5)) +
    labs(x = 'Volume (16 oz)', y = 'Market Share') +
    theme_bw()

share_vol_plot

plot_sens <- plot_grid(
    share_price_plot, share_op_plot, share_quant_plot, 
    share_sus_plot, share_logo_plot, share_vol_plot,
    nrow = 2
)

# Save plots 
ggsave(
    filename = here('figs', 'sens.png'), 
    plot = plot_sens, 
    width = 10, height = 4.6
)


## Tornado


cases <- tribble(
    ~obsID, ~altID, ~attribute,              ~case,  ~value,
    2,      1,     'price',                  'high',  0.2*0.7,
    3,      1,     'price',                  'low',   0.2*1.3,
    4,      1,     'quantity',               'high',  50*1.3,
    5,      1,     'quantity',               'low',   50*0.7,
    6,      1,     'opacity_yes',            'high',  0,
    7,      1,     'opacity_yes',            'low',   1,
    8,      1,     'logo_yes',               'high',  0,
    9,      1,     'logo_yes',               'low',   1,
    10,     1,     'volume16',               'high',  1,
    11,     1,     'volume16',               'low',   0,
    12,     1,    'plastic_sustainable',     'high',  1,
    13,     1,    'plastic_sustainable',     'low',   0
)

cases

# Define scenarios
n <- 13 # baseline + high & low for each attribute
scenarios_atts <- rep_df(scenarios_all, n) 
scenarios_atts$obsID <- rep(seq(n), each = 9) # Reset obsIDs

# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
    left_join(cases, by = c("altID", "obsID")) %>% 
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        price = ifelse(attribute == 'price', value, price),
        quantity = ifelse(attribute == 'quantity', value, quantity),
        opacity = ifelse(attribute == 'opacity_yes', value, opacity_yes),
        logo = ifelse(attribute == 'logo_yes', value, logo_yes),
        volume = ifelse(attribute == 'volume16', value, volume16),
        sustainable = ifelse(attribute == 'plastic_sustainable', value, plastic_sustainable),
        
    )

view(scenarios_atts)

# For each case, simulate the market share predictions
sens_atts <- predict(
    model_mnl,
    newdata = scenarios_atts, 
    obsID = 'obsID', 
    ci = 0.95, 
    returnData = TRUE) %>%
    filter(altID == 1) %>% 
    # Keep only attributes and predictions
    select(attribute, case, value, predicted_prob)

sens_atts


labels <- data.frame( 
    attribute = c('price', 'quantity', 'opacity_yes', 'logo_yes', 'volume16', 'plastic_sustainable'), 
    label = c('Price ($)', 'Quantity', 'Opaque', 'Logo', 'Volume (16 oz)', 'Sustainable Plastic')
)

tornado_data <- sens_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')

tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'value', 
    result = 'predicted_prob'
) 

# -----------------------------------------------------------------------------
# Mixed Logit
load(here("models", "model_mxl.RData"))
model_mxl$coefficients


# -----------------------------------------------------------------------------
# Group Model

load(here("models", "model_mnl_group.RData"))

coefs_g <- coef(model_mnl_group)
cov_g <- vcov(model_mnl_group)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs_g, cov_g))
coef_draws_A <- coef_draws %>%
    select(price, opacity_yes, quantity, plastic_sustainable, logo_yes, volume16)
coef_draws_B <- coef_draws %>%
    mutate(
        price       = price + price_B,
        opacity_yes = opacity_yes + opacity_yes_B,
        quantity   = quantity + quantity_B,
        plastic_sustainable = plastic_sustainable + plastic_sustainable_B,
        logo_yes = logo_yes + logo_yes_B,
        volume16 = volume16 + volume16_B) %>%
    select(price, opacity_yes, quantity, plastic_sustainable, logo_yes, volume16)


# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1* coef_draws_A$price)
wtp_B <- coef_draws_B / (-1* coef_draws_B$price)
wtp_A_ci <- ci(wtp_A)
wtp_B_ci <- ci(wtp_B)


#coef_ci_g$par <- row.names(coef_ci_mnl)

### Group A
wtp_A_ci$par <- row.names(wtp_A_ci)
wtp_A_opacity <- wtp_A_ci %>% filter(par == 'opacity_yes')
wtp_A_quantity <- wtp_A_ci %>% filter(par == 'quantity')
wtp_A_plastic_sustainable <- wtp_A_ci %>% filter(par == 'plastic_sustainable')
wtp_A_logo <- wtp_A_ci %>% filter(par == 'logo_yes')
wtp_A_volume16 <- wtp_A_ci %>% filter(par == 'volume16')


df_A_opacity <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_A_opacity$mean,
        lower = diff*wtp_A_opacity$lower,
        upper = diff*wtp_A_opacity$upper)

df_A_quantity <- data.frame(level = c(25, 50, 75, 100)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_A_quantity$mean,
        lower = diff*wtp_A_quantity$lower,
        upper = diff*wtp_A_quantity$upper)

df_A_plastic_sustainable <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_A_plastic_sustainable$mean,
        lower = diff*wtp_A_plastic_sustainable$lower,
        upper = diff*wtp_A_plastic_sustainable$upper)

df_A_logo <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_A_logo$mean,
        lower = diff*wtp_A_logo$lower,
        upper = diff*wtp_A_logo$upper)

df_A_volume16 <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_A_volume16$mean,
        lower = diff*wtp_A_volume16$lower,
        upper = diff*wtp_A_volume16$upper)

ymin <- floor(min(c(
    df_A_opacity$lower, df_A_quantity$lower, 
    df_A_plastic_sustainable$lower, df_A_logo$lower, df_A_volume16$lower)))
ymax<- ceiling(max(c(
    df_A_opacity$upper, df_A_quantity$upper, 
    df_A_plastic_sustainable$upper, df_A_logo$upper, df_A_volume16$upper)))
ymin <- ymin + 0.8
ymax <- ymax - 0.8

plot_A_opacity <- df_A_opacity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Opacity', y = 'WTP') +
    theme_bw()

plot_A_quantity <- df_A_quantity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.7, fill = 'red') + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (no. Cups)', y = 'WTP') +
    theme_bw()

plot_A_plastic_sustainable <- df_A_plastic_sustainable %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sustainable Plastic', y = 'WTP') +
    theme_bw()

plot_A_logo <- df_A_logo %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Logo Included', y = 'WTP') +
    theme_bw()

plot_A_volume16 <- df_A_volume16 %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Volume (16 oz)', y = 'WTP') +
    theme_bw()


plot_A_wtp <- plot_grid(
    plot_A_opacity, plot_A_quantity, plot_A_plastic_sustainable, plot_A_logo, plot_A_volume16,
    nrow = 1)

# Save plots 
ggsave(
    filename = here('figs', 'wtp_A.png'), 
    plot = plot_A_wtp, 
    width = 10, height = 2.3
)

### Group B
wtp_B_ci$par <- row.names(wtp_B_ci)
wtp_B_opacity <- wtp_B_ci %>% filter(par == 'opacity_yes')
wtp_B_quantity <- wtp_B_ci %>% filter(par == 'quantity')
wtp_B_plastic_sustainable <- wtp_B_ci %>% filter(par == 'plastic_sustainable')
wtp_B_logo <- wtp_B_ci %>% filter(par == 'logo_yes')
wtp_B_volume16 <- wtp_B_ci %>% filter(par == 'volume16')


df_B_opacity <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_B_opacity$mean,
        lower = diff*wtp_B_opacity$lower,
        upper = diff*wtp_B_opacity$upper)

df_B_quantity <- data.frame(level = c(25, 50, 75, 100)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_B_quantity$mean,
        lower = diff*wtp_B_quantity$lower,
        upper = diff*wtp_B_quantity$upper)

df_B_plastic_sustainable <- data.frame(level = c(0,1)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_B_plastic_sustainable$mean,
        lower = diff*wtp_B_plastic_sustainable$lower,
        upper = diff*wtp_B_plastic_sustainable$upper)

df_B_logo <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_B_logo$mean,
        lower = diff*wtp_B_logo$lower,
        upper = diff*wtp_B_logo$upper)

df_B_volume16 <- data.frame(level = c(0,1)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_B_volume16$mean,
        lower = diff*wtp_B_volume16$lower,
        upper = diff*wtp_B_volume16$upper)

ymin <- floor(min(c(
    df_B_opacity$lower, df_B_quantity$lower, 
    df_B_plastic_sustainable$lower, df_B_logo$lower, df_B_volume16$lower)))
ymax<- ceiling(max(c(
    df_B_opacity$upper, df_B_quantity$upper, 
    df_B_plastic_sustainable$upper, df_B_logo$upper, df_B_volume16$upper)))
ymin <- ymin + 0.8
ymax <- ymax - 0.8

plot_B_opacity <- df_B_opacity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Opacity', y = 'WTP') +
    theme_bw()

plot_B_quantity <- df_B_quantity %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.7, fill = 'red') + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Quantity (no. Cups)', y = 'WTP') +
    theme_bw()

plot_B_plastic_sustainable <- df_B_plastic_sustainable %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sustainable Plastic', y = 'WTP') +
    theme_bw()

plot_B_logo <- df_B_logo %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Logo Included', y = 'WTP') +
    theme_bw()

plot_B_volume16 <- df_B_volume16 %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Volume (16 oz)', y = 'WTP') +
    theme_bw()


plot_B_wtp <- plot_grid(
    plot_B_opacity, plot_B_quantity, plot_B_plastic_sustainable, plot_B_logo, plot_B_volume16,
    nrow = 1)

# Save plots 
ggsave(
    filename = here('figs', 'wtp_B.png'), 
    plot = plot_B_wtp, 
    width = 10, height = 2.3
)

