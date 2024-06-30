#' Calculate Win Probability and Other Metrics
#' R package written by Pavel S Roshanov June 29 2024
#' Based on idea and SAS code in Zou G, Zou L, Q S-F. Parametric and nonparametric methods for confidence intervals and sample size planning for win probability in parallel-group randomized trials with Likert item and Likert scale data. Pharmaceutical Statistics. 2023 May 1;22(3):418–39.
#'https://onlinelibrary.wiley.com/doi/10.1002/pst.2280

The workhorse function is calculate_winP and takes inputs data, group, values pre baseline (not required but can increase power and precision), and values post baseline (required).
