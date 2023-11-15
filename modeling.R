library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(maddTools)
library(cowplot)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here("data", "choiceData.csv"))
head(data)

# Estimate MNL model

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c('PlasticMaterial', 'Logo', 'Volume', 'Opacity', 'group'))
head(data)
# Clean up names of created variables
data <- clean_names(data) %>% 
    rename(
        plastic_regular = plastic_material_regular,
        plastic_sustainable = plastic_material_sustainable,
        volume9 = volume_9,
        volume16 = volume_16)

# -----------------------------------------------------------------------------
# Simple Logit Model

# Estimate the model
model_mnl <- logitr(
    data    = data,
    outcome = "choice",
    obsID   = "obs_id",
    pars = c("price", "opacity_yes", "quantity", "plastic_sustainable", "logo_yes", "volume16")
)

# View summary of results
summary(model_mnl)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl$hessian)$values


coefs_mnl <- coef(model_mnl)
cov_mnl <- vcov(model_mnl)

coef_draws_mnl <- as.data.frame(mvrnorm(10^4, coefs_mnl, cov_mnl))

# For each coefficient, get the mean and 95% confidence interval:
coef_ci_mnl <- ci(coef_draws_mnl, ci = 0.95)
coef_ci_mnl


save(
    model_mnl,
    file = here("models", "model_mnl.RData")
)

# -----------------------------------------------------------------------------
# Mixed Logit Model

model_mxl <- logitr(
    data    = data,
    outcome = "choice",
    obsID   = "obs_id",
    pars = c("price","opacity_yes", "quantity", "plastic_sustainable", "logo_yes", "volume16"),
    randPars = c(price = 'n', opacity_yes = 'n', plastic_sustainable = 'n', logo_yes = 'n', volume16 = 'n'),
    numMultiStarts = 10
)

save(
    model_mxl,
    file = here("models", "model_mxl.RData")
)
# -----------------------------------------------------------------------------
# Logit Model with Sub Groups

data_group <- data %>%
    mutate(
        price_B       = price*group_b,
        opacity_yes_B = opacity_yes*group_b,
        quantity_B      = quantity*group_b,
        plastic_sustainable_B = plastic_sustainable*group_b,
        logo_yes_B = logo_yes*group_b,
        volume16_B = volume16*group_b
    )
head(data_group)

model_mnl_group <- logitr(
    data    = data_group,
    outcome = "choice",
    obsID   = "obs_id",
    pars = c("price", "opacity_yes", "quantity", "plastic_sustainable", "logo_yes", "volume16",
             "price_B", "opacity_yes_B", "quantity_B", "plastic_sustainable_B", "logo_yes_B", "volume16_B")
)


# View summary of results
summary(model_mnl_group)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_group$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_group$hessian)$values


save(
    model_mnl_group,
    file = here("models", "model_mnl_group.RData")
)





## Data Tables

data_table <- data %>% 
    select(gender, yearOfBirth, group, income, employment, purchase, cups) %>% 
    kable()



