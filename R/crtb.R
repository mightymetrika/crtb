crtb <- function(dat, pooled = TRUE, rowwise = TRUE, tie_thresh = 0.5){
  # Step 1: Get original data
  # this is dat

  # Step 2: Check for multiple groups
  # use ncol(dat)

  # Step 3: Decide on tagging method
  # (use rowwise)

  # Step 4: Tag values
  if (ncol(dat) > 1){
    # handle multiple groups

    grps <- names(dat) # get group names
    totobs <- ncol(dat)*nrow(dat) #get number of observations

    # tag values
    if (rowwise) {
      # handle rowwise tagging

      obscount <- 1 #initialize observation counter
      for(i in grps){
        dat[[paste0(grps[i], "_tag")]] <- obscount:nrow(dat)
        obscount <- obscount + nrow(dat)
      }

    } else {
      # handle column wise version
    }

  } else {
    # handle ncol == 1 version
  }


  # Step 5: Resample tagged values
  if (tagging_dimension == "row") {
    resampled_tags <- sample(dat$tag)
  } else {
    resampled_tags <- sample(tags)
  }

  # Step 6: Decide if too many ties to use method
  if (length(unique(resampled_tags)) < length(resampled_tags) * tie_thresh) {
    warning("Too many ties to use the method")
    return(NULL)
  }

  # Step 7: Increment usage counter
  used <- 0
  used <- used + 1

  # Step 8: Rearrange into blocks with complements
  block_size <- 5  # Example block size
  blocks <- split(resampled_tags, ceiling(seq_along(resampled_tags)/block_size))

  # Steps 9-13: Process blocks
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    # Get complement of the block
    complement <- unlist(blocks[-i])

    # Step 9: Get order within block
    block_order <- order(block)

    # Step 10: Get order within complement
    complement_order <- order(complement)

    # Step 11: Replace block resample tags with complement tags
    blocks[[i]] <- complement[complement_order]

    # Steps 12-13: Handle fragments
    if (i == length(blocks) && length(block) < block_size) {
      # Use adjacent block to complete the complement
      adjacent_block <- blocks[[i - 1]]
      complement <- c(unlist(blocks[-c(i)]), adjacent_block)
      blocks[[i]] <- complement[order(complement)]
    }
  }

  # Combine blocks back into resampled tags
  final_resampled_tags <- unlist(blocks)

  # Map resampled tags back to data
  if (tagging_dimension == "row") {
    resampled_data <- dat[match(final_resampled_tags, dat$tag), ]
  } else {
    resampled_data <- dat[, names(final_resampled_tags)]
  }

  return(resampled_data)
}


# crtb <- function(dat, pooled = TRUE, rowwise = TRUE){
#   # Basic Algorithm
#
#   # 1: create values
#   # this is just dat
#
#
#   # 2: If more than one group, decide if resampling will be pooled
#
#   # 3: If pooled, decide if tagging will take place rowwise or column wise
#
#   # 4: tag values (create a dictionary where each entry is given a unique value)
#   if (ncol(dat) == 1){
#
#
#   }
#   # 5: resample tagged values
#   # 6: decide if too many ties to use method
#   # 7: add one to "used" if using method
#   # 8: rearrange into blocks with compliments
#   # 9: within block get order of tags
#   # 10: within complement of block get order of tags
#   # 11: replace block resample tags with block complement tags
#   # 12: repeat for other blocks with tags
#   # 13: for fragments of blocks with incomplete complements, reorder to top and use piece of adjacent block as needed to determine complements
# }
