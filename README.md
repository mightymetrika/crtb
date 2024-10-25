
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Complementary Resampling of Tags in Blocks (CRTB)

<!-- badges: start -->
<!-- badges: end -->

The crtb resampling method is inspired by complementary pairs
subsampling (Shah & Samworth, 2013). The method creates pairs of
resampled datasets with complementary properties.

## Installation

You can install the development version of crtb from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mightymetrika/crtb")
```

## Implementation Details

When working with multiple groups and pooled resampling, CRTB follows
these steps:

1.  **Tag Assignment**

- Each observation receives a unique integer tag
- For multiple groups, tagging can be done row-wise or column-wise

2.  **Initial Resampling**

- Tags are resampled using one of three methods
  - With replacement (default)
  - Without replacement
  - Custom resampling function
- This creates the “original resample”
- Process halts if the proportion of resamples falls below the tie
  threshold

3.  **Block Creation**

- Block length is set to half the length of initial tags
- First block:
  - Form initial block stem from unique tags in original sample
  - If block stem is undersized:
    1.  Take the set difference between all tags and block stem
    2.  Sample without replacement to fill block to target size
- Subsequent blocks:
  - Form new block stem from unique remaining tags
  - If block stem is undersized:
    1.  Take the set difference between all tags and block stem
    2.  Sample without replacement to fill block to target size
- Continue until all tags from original sample are assigned to blocks

4.  **Complementary Sampling**

- For each block:
  - Find complement (all tags not in block)
  - Sample from complement to match block stem size
- Combined complementary samples form the “complementary resample”

5.  **Output Generation**

- Map tags back to original observations
- Return two datasets:
  - Original resample
  - Complementary resample

``` r
library(crtb)

# Create sample data
data <- data.frame(
  group1 = stats::rnorm(10),
  group2 = stats::rnorm(10)
)

# Basic usage with default settings
result <- crtb(data)

# Access results
result$ordat
#>         group1      group2
#> 1    0.3066096   -1.005656
#> 2   -0.6602836  -0.1824224
#> 3   -0.1824224   0.1075436
#> 4    0.4067197   -1.976019
#> 5    -1.855841   0.1075436
#> 6   -0.1824224 -0.01792544
#> 7   0.09052154 -0.01792544
#> 8    -1.976019   0.8712189
#> 9  -0.01792544   0.3066096
#> 10   -1.005656  -0.3849311
result$crdat
#>        group1     group2
#> 1   0.7189153 -0.3600608
#> 2  -0.1121349 -0.1121349
#> 3  -0.3849311  0.5143101
#> 4  -0.2297448 0.09661478
#> 5   0.8712189 0.09052154
#> 6   0.3288312  0.4067197
#> 7  -0.3600608  0.7189153
#> 8   0.5143101 -0.2297448
#> 9  0.09661478 -0.1121349
#> 10  0.4301553  0.7189153
```

Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error
Control: Another Look at Stability Selection. Journal of the Royal
Statistical Society: Series B, 75(1), 55-80.
