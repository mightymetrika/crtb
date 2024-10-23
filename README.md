
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

1.  Tag Assignment

- Each observation receives a unique integer tag
- For multiple groups, tagging can be done row-wise or column-wise

2.  Initial Resampling

- Tags are resampled using one of three methods
- With replacement (default)
- Without replacement
- Custom resampling function
- This creates the “original resample”
- Process halts if the proportion of resamples falls below the tie
  threshold

3.  Block Creation

- Block length is set to half the length of initial tags
- First block:
- Formed from unique tags in original sample
- Subsequent blocks:
- Start with unique remaining tags (block stem)
- If block stem is undersized:

1.  Take the set difference between all tags and block stem
2.  Sample without replacement to fill block to target size

- Continue until all tags from original sample are assigned to blocks

4.  Complementary Sampling

- For each block:
- Find complement (all tags not in block)
- Sample from complement to match block stem size
- Combined complementary samples form the “complementary resample”

5.  Output Generation

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
#>       group1     group2
#> 1  0.6097446   0.376169
#> 2  0.6097446 -0.1281819
#> 3   -1.14996   1.629106
#> 4  -1.706804  -1.477292
#> 5  0.2242456   1.267569
#> 6   1.629106  -1.477292
#> 7  0.6097446   3.061794
#> 8  0.7098258  0.4625728
#> 9  0.3892787   -1.14996
#> 10  1.041136  0.5403194
result$crdat
#>        group1     group2
#> 1      1.8123  0.2242456
#> 2   -1.477292     1.8123
#> 3   0.6645448  0.3892787
#> 4   0.5403194  0.7098258
#> 5   -1.605467  0.6645448
#> 6   0.2384114 -0.5825956
#> 7    1.267569   1.041136
#> 8  -0.5825956  -1.605467
#> 9   0.4625728   1.041136
#> 10   3.061794   1.629106
```

Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error
Control: Another Look at Stability Selection. Journal of the Royal
Statistical Society: Series B, 75(1), 55-80.
