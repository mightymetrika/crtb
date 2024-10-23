
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
#>         group1      group2
#> 1   -0.9516754  -0.1822092
#> 2    -0.515937    1.126638
#> 3  -0.04955159 -0.04955159
#> 4    0.1611364  -0.9516754
#> 5   -0.8100763   0.2877215
#> 6     1.771701  -0.8202653
#> 7   -0.8202653   0.2877215
#> 8    0.1680735   0.1680735
#> 9   -0.9148794 -0.03766213
#> 10  -0.9148794 -0.03766213
result$crdat
#>         group1      group2
#> 1    -0.906561  -0.2299969
#> 2     1.279905   -0.515937
#> 3     2.205633  -0.1822092
#> 4     1.126638   0.1611364
#> 5   -0.2299969    1.279905
#> 6    0.9291673   0.9291673
#> 7  -0.03766213 -0.06317255
#> 8   -0.2851652  -0.2851652
#> 9    0.2877215   -0.906561
#> 10 -0.06317255 -0.06317255
```

Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error
Control: Another Look at Stability Selection. Journal of the Royal
Statistical Society: Series B, 75(1), 55-80.
