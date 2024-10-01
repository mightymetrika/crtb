crtb <- function(dat, pooled = TRUE, rowwise = TRUE, tie_thresh = 0.5,
                 replace = TRUE){

  # Original Algorithm (The steps in the code below have been renumbered.)
  # 1: create values
  # 2: If more than one group, decide if resampling will be pooled
  # 3: If pooled, decide if tagging will take place rowwise or column wise
  # 4: tag values (create a dictionary where each entry is given a unique value)
  # 5: resample tagged values
  # 6: decide if too many ties to use method
  # 7: add one to "used" if using method
  # 8: rearrange into blocks with compliments
  # 9: within block get order of tags
  # 10: within complement of block get order of tags
  # 11: replace block resample tags with block complement tags
  # 12: repeat for other blocks with tags
  # 13: for fragments of blocks with incomplete complements, reorder to top and use piece of adjacent block as needed to determine complements

  # Step 1: Tag values
  grps <- names(dat) # Get group names

  if (ncol(dat) > 1){
    # Initialize observation counter
    obscount <- 1

    # Tag values
    if (rowwise) {
      # Row-wise tagging (same as before)
      for(i in grps){
        dat[[paste0(i, "_tag")]] <- obscount:(obscount + nrow(dat) - 1)
        obscount <- obscount + nrow(dat)
      }
    } else {
      # Column-wise tagging
      for(i in 1:nrow(dat)){
        for(j in grps){
          # Assign unique tags to each value across columns for each row
          dat[[paste0(j, "_tag")]][i] <- obscount
          obscount <- obscount + 1
        }
      }
    }

  } else {
    # Handle single column case
    dat[[paste0(grps, "_tag")]] <- 1:nrow(dat)
  }

  # Collect all tags into a single vector
  tag_cols <- grep("_tag$", names(dat), value = TRUE)
  all_tags <- unlist(dat[, tag_cols])

  if(!rowwise){
    all_tags <- sort(all_tags)
  }

  # Step 2: Resample tagged values
  resampled_tags <- sample(all_tags, replace = replace)

  # Step 3: Decide if too many ties to use method
  if (length(unique(resampled_tags)) < length(resampled_tags) * tie_thresh) {
    warning("Too many ties to use the method")
    return(NULL)
  }

  # Step 4: Increment usage counter
  used <- 0
  used <- used + 1

  # Step 5: Rearrange into blocks with complements

  block_list <- list() # initialize list
  # get block1 size
  if (length(all_tags) %% 2 == 0){
    block_size <- length(all_tags) / 2
  } else {
    block_size <- (length(all_tags) / 2)
  }

  i <- 1 # initialize iterator (for naming blocks)
  temp_rtags <- block_loop <- resampled_tags # get resampled tags
  BLOCK_1_SIZE <- block_size # get initial block size (constant)

  # build blocks
  while(length(temp_rtags) > 1){
    if (i == 1){
      # Get block1 (first block with a resample in resampled_tags)
      block <- unique(block_loop) |> utils::head(block_size)
      block_stem = block

      # Save block
      block_list[[paste0("block",i)]] <- list(block = block,
                                              block_size = block_size)
    } else {
      # Get block_loop not represented in block 1
      temp_rtags <- temp_rtags[-match(block_stem, temp_rtags)]
      if (length(temp_rtags) == 0) break

      # Get remaining unique tags from temp_rtags
      block_stem <- unique(temp_rtags)
      block_size <- length(block_stem)

      # Get remainder of resampled tags
      ## note you still need to determine how to handle updating the loop.
      ## There are many possible methods (much more than 2)
      # block_loop <- c(temp_rtags, block_loop) # method 1
      block_loop <- c(block_stem, block_loop) # method 2
      block <- block_loop |> unique() |> utils::head(BLOCK_1_SIZE)

      # Save block
      block_list[[paste0("block",i)]] <- list(block = block,
                                              block_size = block_size)
    }

    i = i + 1
  }

  return(block_list)

  block_size <- 5  # Example block size
  blocks <- split(resampled_tags, ceiling(seq_along(resampled_tags)/block_size))

  # Steps 6-10: Process blocks
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    # Get complement of the block
    complement <- unlist(blocks[-i])

    # Step 6: Get order within block
    block_order <- order(block)

    # Step 7: Get order within complement
    complement_order <- order(complement)

    # Step 8: Replace block resample tags with complement tags
    blocks[[i]] <- complement[complement_order]

    # Steps 9-10: Handle fragments
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
