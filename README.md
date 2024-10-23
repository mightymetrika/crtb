
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
  - Formed from unique tags in original sample
- Subsequent blocks:
  - Start with unique remaining tags (block stem)
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
#>       group1     group2
#> 1  0.4582403   1.217752
#> 2  0.4861044 -0.8390638
#> 3   1.615581 -0.2999852
#> 4  0.3947707  0.4861044
#> 5  0.1023458  0.6444523
#> 6  0.4008284   1.685475
#> 7  0.4008284  0.3947707
#> 8  0.4008284 -0.6795774
#> 9   1.615581   1.685475
#> 10 0.9834048   1.177102
result$crdat
#>        group1     group2
#> 1  -0.6795774  -0.752682
#> 2    1.177102   1.417757
#> 3    0.420008  0.1023458
#> 4    1.265133  0.9834048
#> 5    1.685475 -0.2999852
#> 6    1.040568  0.2299534
#> 7    1.417757   1.265133
#> 8   0.6444523  0.4582403
#> 9   0.2299534  -0.752682
#> 10  -0.752682  0.6444523
```

Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error
Control: Another Look at Stability Selection. Journal of the Royal
Statistical Society: Series B, 75(1), 55-80.
