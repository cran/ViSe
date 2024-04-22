## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ViSe)
library(cowplot)
library(ggplot2)
library(plotly)

## -----------------------------------------------------------------------------
internal_unadjusted <- list(
  mean_diff = 3.68,
  lower_diff = 1.73,
  upper_diff = 5.62,
  sd = 8.29
)

internal_adjusted <- list(
  mean_diff = 2.73,
  lower_diff = 0.77,
  upper_diff = 4.69,
  sd = 8.28
)

external_unadjusted <- list(
  mean_diff = 3.72,
  lower_diff = 2.13,
  upper_diff = 5.32,
  sd = 6.81
)

external_adjusted <- list(
  mean_diff = 3.10,
  lower_diff = 1.49,
  upper_diff = 4.71,
  sd = 6.81
)

list_values <- list(internal_unadjusted, internal_adjusted, 
                 external_unadjusted, external_adjusted)
list_names <- c("internal_unadjusted", "internal_adjusted", 
                 "external_unadjusted", "external_adjusted")
names(list_values) <- list_names

for (i in list_names){
  list_values[[i]][["d"]] <- list_values[[i]][["mean_diff"]] / list_values[[i]][["sd"]]
  list_values[[i]][["lower_d"]] <- list_values[[i]][["lower_diff"]] / list_values[[i]][["sd"]]
  list_values[[i]][["upper_d"]] <- list_values[[i]][["upper_diff"]] / list_values[[i]][["sd"]]
}

unlist(list_values)

## -----------------------------------------------------------------------------
internal_unadj_output <- calculate_d(
  d = list_values$internal_adjusted$d, # d value
  a = .05, # alpha for confidence interval
  lower = TRUE, # you expect d to be positive 
  n1 = 71, # sample size group 1 
  n2 = 3653  # sample size group 2
)

internal_unadj_output$dlow_central
internal_unadj_output$done_low_central

# note, the program also provide noncentral t confidence intervals
# in this case, they are unusable because d has been calculated from 
# mean difference / control rather than mean difference / spooled
# therefore the approximation of t and the noncentral 
# limits is not appropriate 

## -----------------------------------------------------------------------------
# from dataframe
calculate_d(
  df = mtcars,
  x_col = "am",
  y_col = "hp",
  a = .05,
  lower = TRUE
)

# from two columns
x <- mtcars$am
y <- mtcars$hp

calculate_d(
  x_col = x,
  y_col = y,
  a = .05,
  lower = TRUE
)

# from summary statistics 
calculate_d(m1 = 14.37, # neglect mean
   sd1 = 10.716, # neglect sd
   n1 = 71, # neglect n
   m2 = 10.69, # none mean
   sd2 = 8.219, # none sd
   n2 = 3653, # none n
   a = .05, # alpha/confidence interval
   lower = TRUE) # lower or upper bound

# from t-test model
output <- t.test(mtcars$hp ~ mtcars$am, var.equal = TRUE)
n_values <- tapply(mtcars$hp, mtcars$am, length)
calculate_d(
  model = output, 
  n1 = unname(n_values[1]),
  n2 = unname(n_values[2]),
  a = .05, 
  lower = TRUE
)

# from t-values 
calculate_d(
  t = 1.37,
  n1 = unname(n_values[1]),
  n2 = unname(n_values[2]),
  a = .05, 
  lower = TRUE
)


## -----------------------------------------------------------------------------
other_to_d(nnt = 35)

## -----------------------------------------------------------------------------
visualize_effects(d = list_values$internal_adjusted$d)

## -----------------------------------------------------------------------------
d_to_r(d = list_values$internal_adjusted$d)
d_to_f2(d = list_values$internal_adjusted$d)
d_to_nnt(d = list_values$internal_adjusted$d)
probability_superiority(d = list_values$internal_adjusted$d)
proportion_overlap(d = list_values$internal_adjusted$d)

## -----------------------------------------------------------------------------
estimate_d(d = .09)$graph

## -----------------------------------------------------------------------------
estimate_r(r = .30)$graph

## -----------------------------------------------------------------------------
visual_c_mapped <-
  # your lower confidence limit required
  visualize_c_map(dlow = list_values$internal_adjusted$lower_d, 
                  # correlation values required
                  r_values = c(.1, .4, .3),
                  # other effect sizes you want to plot
                  d_values = c(.2, .8),
                  nnt_values = c(60),
                  # if you think d will be positive 
                  lower = TRUE)

visual_c_mapped$graph

ggsave(filename = "visualize_c_map.png", width = 8)

# ggplotly(visual_c_mapped$graph)

