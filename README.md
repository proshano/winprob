`winprob` is an R package that calculates win probabilities and related statistics. It provides tools for analyzing grouped data and computing various probability metrics.

#' R package written by Pavel S Roshanov June 29 2024
#' Based on idea and SAS code in Zou G, Zou L, Q S-F. Parametric and nonparametric methods for confidence intervals and sample size planning for win probability in parallel-group randomized trials with Likert item and Likert scale data. Pharmaceutical Statistics. 2023 May 1;22(3):418–39.
#'https://onlinelibrary.wiley.com/doi/10.1002/pst.2280

The workhorse function is calculate_winP and takes inputs data, group, values pre baseline (not required but can increase power and precision), and values post baseline (required).


## Installation

You can install and run the development version of winprob from GitHub with:

```R
install.packages("devtools") 
devtools::install_github("proshano/winprob")
```
## Usage
After installation, load the package
```R
library(winprob)
```

You can then use the calculate_winP() function as follows:

```R
sample.data <- data.frame(
  id = 1:61,
  group = c(rep(0, 27), rep(1, 34)),
  pre = c(18, 27, 16, 17, 15, 20, 16, 28, 28, 25, 24, 16, 26, 21, 21, 22, 26, 19, 22, 16, 21, 20, 17, 22, 19, 21, 18, 21, 27, 15, 24, 15, 17, 20, 18, 28, 21, 18, 27.46, 19, 20, 16, 21, 23, 23, 24, 25, 22, 20, 20, 25, 18, 26, 20, 17, 22, 22, 23, 17, 22, 26),
  post = c(17, 26, 17, 14, 12, 19, 13, 26, 26, 9, 14, 19, 13, 7, 18, 18, 19, 19, 20, 7, 19, 16, 15, 20, 16, 7, 19, 13, 8, 8, 14, 15, 9, 7, 8, 11, 7, 8, 22, 14, 13, 17, 19, 11, 16, 16, 20, 15, 7, 12.13, 15, 17, 1, 27, 20, 12, 15.38, 11, 15, 7, 24)
)


result <- calculate_winP(data = sample.data, group_var = "group", post_var= "post",  pre_var= "pre")
print(result)
```

Note that pre_var is not required - it is just needed if you want to adjust to post-baseline between-group comparison for the pre-baseline values. This improves power and precision but is not available in all studies.

## Functions
calculate_winP()

Calculates win probability and related statistics.

### Arguments:

data: A data frame containing the variables for analysis
group_var: The name of the grouping variable
post_var: The name of the post-treatment variable
pre_var: The name of the pre-treatment variable (optional)

### Returns:
A data frame containing Win Probability and related statistics

## Dependencies
This package depends on the following R packages:

dplyr
stats
sandwich
lmtest
broom

These will be automatically installed when you install the package.

## Contributing
If you'd like to contribute to this package, please fork the repository and use a feature branch. Pull requests are warmly welcome.

## Issues
If you encounter any issues or have feature suggestions, please file an issue on the GitHub repository.
