
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crtb

<!-- badges: start -->
<!-- badges: end -->

The goal of crtb is to complementary resampling of tags in blocks.

## Installation

You can install the development version of crtb from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mightymetrika/crtb")
```

## Complementary Resampling of Tags in Blocks

With multiple groups and pooled resampling, crtb is implemented as
follows:

- Tagging: assign each observation a unique integer.
- Resample the tags (with replacement, without replacement, or custom).
  This is the original resample.
- Quit if resample has more repeated tags than allowed by tie threshold.
- Create blocks (note: block size is 1/2 the length of initial tags)
- Get first block of unique tags
- Get unique remaining tags as the block stem for the second block
- If the block stem forms the full block size then the process is
  complete
- If the block stem is shorter then the block size, fill out the block
  by taking the set difference between a resample without replacement of
  all tags and the block stem.
- Repeat the previous step until all tags in the original resample are
  included in a block.
- Get the complement of the tags in each block
- In each complement, only keep the number of elements from the
  corresponding block’s block stem. The retained collection of tags
  corresponds to the complementary resample.
- Map the tags back onto the original observations
- Return the datasets corresponding to the original resample and to the
  complementary resample.

``` r
library(crtb)
## basic example code
```