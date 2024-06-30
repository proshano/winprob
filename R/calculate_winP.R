#' Calculate Win Probability and Other Metrics
#' R package written by Pavel S Roshanov June 29 2024
#' Based on idea and SAS code in Zou G, Zou L, Qiu S Fang. Parametric and nonparametric methods for confidence intervals and sample size planning for win probability in parallel-group randomized trials with Likert item and Likert scale data. Pharmaceutical Statistics. 2023 May 1;22(3):418–39.
#'
#' This function calculates the Win Probability (WinP) and its confidence interval
#' along with other related metrics.
#'
#' @param data A data frame containing the data.
#' @param group_var A string specifying the name of the grouping variable.
#' @param post_var A string specifying the name of the post-baseline variable.
#' @param pre_var An optional string specifying the name of the pre-baseline variable (not required).
#'
#' @return A data frame containing the calculated metrics.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:61,
#'   group = c(rep(0, 27), rep(1, 34)),
#'   pre = c(18, 27, 16, 17, 15, 20, 16, 28, 28, 25, 24, 16, 26, 21, 21, 22, 26, 19, 22, 16, 21, 20, 17, 22, 19, 21, 18, 21, 27, 15, 24, 15, 17, 20, 18, 28, 21, 18, 27.46, 19, 20, 16, 21, 23, 23, 24, 25, 22, 20, 20, 25, 18, 26, 20, 17, 22, 22, 23, 17, 22, 26),
#'   post = c(17, 26, 17, 14, 12, 19, 13, 26, 26, 9, 14, 19, 13, 7, 18, 18, 19, 19, 20, 7, 19, 16, 15, 20, 16, 7, 19, 13, 8, 8, 14, 15, 9, 7, 8, 11, 7, 8, 22, 14, 13, 17, 19, 11, 16, 16, 20, 15, 7, 12.13, 15, 17, 1, 27, 20, 12, 15.38, 11, 15, 7, 24)
#' )
#' result_with_pre <- calculate_winP(data, group_var = "group", pre_var = "pre", post_var = "post")
#' print(result_with_pre)
#' result_without_pre <- calculate_winP(data, group_var = "group", post_var = "post")
#' print(result_without_pre)
#'
#' @export

calculate_winP <- function(data, group_var, post_var, pre_var = NULL) {
  # List of required packages
  required_packages <- c("dplyr", "sandwich", "lmtest", "broom")
  # Install missing packages
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # Calculate overall rank
  data <- data %>%
    arrange(.data[[group_var]], desc(.data[[post_var]])) %>%
    mutate(rpost = rank(-.data[[post_var]], ties.method = "average"))

  if (!is.null(pre_var)) {
    data <- data %>%
      mutate(rpre = rank(-.data[[pre_var]], ties.method = "average"))
  }

  # Calculate group-specific rank
  data <- data %>%
    group_by(.data[[group_var]]) %>%
    mutate(grpost = rank(-.data[[post_var]], ties.method = "average"))

  if (!is.null(pre_var)) {
    data <- data %>%
      mutate(grpre = rank(-.data[[pre_var]], ties.method = "average"))
  }

  data <- data %>%
    ungroup()

  # Calculate win fractions
  freqcnt <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(count = n()) %>%
    mutate(across(all_of(group_var), ~ 1 - .)) %>%
    arrange(.data[[group_var]])

  data <- data %>%
    left_join(freqcnt, by = group_var) %>%
    mutate(winf = (rpost - grpost) / count)

  if (!is.null(pre_var)) {
    data <- data %>%
      mutate(winf_pre = (rpre - grpre) / count)
  }

  # Perform regression on win fractions
  formula <- if (!is.null(pre_var)) {
    as.formula(paste("winf ~ winf_pre +", group_var))
  } else {
    as.formula(paste("winf ~", group_var))
  }

  model2 <- lm(formula, data = data)

  # Extract coefficients and standard errors using HC2 standard errors
  hc_se <- coeftest(model2, vcov = vcovHC(model2, type = "HC2"))

  # Extract the coefficient and standard error for the group variable
  estMW <- tidy(hc_se) %>%
    filter(term == group_var)

  # Check if estMW is empty
  if (nrow(estMW) == 0) {
    stop("No coefficients found for the specified group variable. Check your input data.")
  }

  # Calculate Win Probability and Other Metrics
  WinP <- estMW$estimate / 2 + 0.5
  lgt <- log(WinP / (1 - WinP))
  lgtSe <- estMW$std.error / (WinP * (1 - WinP))
  ln_Odds_l <- lgt - 1.96 * lgtSe
  ln_Odds_u <- lgt + 1.96 * lgtSe

  Odds <- exp(lgt)
  Odds_l <- exp(ln_Odds_l)
  Odds_u <- exp(ln_Odds_u)

  WinP_l <- 1 / (1 + exp(-ln_Odds_l))  # logistic function
  WinP_u <- 1 / (1 + exp(-ln_Odds_u))  # logistic function
  test <- (lgt - 0) / lgtSe
  pvalue <- 2 * (1 - pnorm(abs(test)))
  SomersD <- 2 * WinP - 1
  D_L <- 2 * WinP_l - 1  # lower limit for Somers D
  D_U <- 2 * WinP_u - 1  # upper limit for Somers D

  # Create a data frame to store the results
  WinP_result <- data.frame(
    WinP = WinP,
    WinP_l = WinP_l,
    WinP_u = WinP_u,
    test = test,
    pvalue = pvalue,
    Odds = Odds,
    Odds_l = Odds_l,
    Odds_u = Odds_u,
    SomersD = SomersD,
    D_L = D_L,
    D_U = D_U
  )

  return(WinP_result)
}
