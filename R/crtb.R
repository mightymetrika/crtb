crtb <- function(dat, pooled = TRUE, rowwise = TRUE, tie_thresh = 0.5,
                 replace = TRUE){

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
  BLOCK_SIZE <- block_size <- floor(length(all_tags) / 2) # get block size (CONSTANT and variable)
  remaining_rtags <- resampled_tags # get resampled tags
  i <- 1 # initialize iterator (for naming blocks)

  # build blocks
  while(length(remaining_rtags) > 1){
    if (i == 1){
      # Get block1 (first block with a resample in resampled_tags)
      block <- unique(remaining_rtags) |> utils::head(block_size)
      block_stem = block

      # Save block
      block_list[[paste0("block",i)]] <- list(block = block,
                                              block_size = block_size)
    } else {
      # Get block_loop not represented in block 1
      remaining_rtags <- remaining_rtags[-match(block_stem, remaining_rtags)]
      if (length(remaining_rtags) == 0) break

      # Get remaining unique tags from remaining_rtags
      block_stem <- unique(remaining_rtags)
      block_size <- length(block_stem)

      # Get remainder of resampled tags
      # combine the block_stem with enough elements from resampled_tags to form a full block
      block <- c(block_stem,
                 setdiff(sample(resampled_tags, length(resampled_tags), replace = FALSE),
                         block_stem) |> utils::head(BLOCK_SIZE - block_size))

      # Save block
      block_list[[paste0("block",i)]] <- list(block = block,
                                              block_size = block_size)
    }

    i = i + 1
  }

  # Steps 6: Get complement of each block
  complement_list <- vector(mode = "list", length = length(block_list)) # initialize list
  for (i in seq_along(block_list)) {
    complement_list[[i]] <- setdiff(all_tags, block_list[[i]]$block) |> # complement
      sample(BLOCK_SIZE, replace = FALSE) |> # sample correct number of elements
      utils::head(block_list[[i]]$block_size) # get number of elements needed.
  }

  # Combine blocks back into resampled tags
  final_resampled_tags <- unlist(complement_list)
  if (ncol(dat) > 1){

    # Tag values
    if (rowwise) {
      # Row-wise tagging (same as before)
      for(i in grps){
        frtags <-
        dat[[paste0(i, "_rtag")]] <- final_resampled_tags |> utils::head(nrow(dat))
        final_resampled_tags <- final_resampled_tags |> utils::tail(-nrow(dat))
      }
    } else {
      # Column-wise tagging
      for(i in 1:nrow(dat)){
        for(j in grps){
          # Assign unique tags to each value across columns for each row
          dat[[paste0(j, "_rtag")]][i] <- final_resampled_tags |> utils::head(1)
          final_resampled_tags <- final_resampled_tags |> utils::tail(-1)
        }
      }
    }

  } else {
    # Handle single column case
    dat[[paste0(grps, "_rtag")]] <- final_resampled_tags
  }

  return(list(dat = dat,
         blocks = block_list,
         complements = complement_list))
}



# crtb <- function(dat, pooled = TRUE, rowwise = TRUE, tie_thresh = 0.5,
#                  replace = TRUE){
#
#   # Original Algorithm (The steps in the code below have been renumbered.)
#   # 1: create values
#   # 2: If more than one group, decide if resampling will be pooled
#   # 3: If pooled, decide if tagging will take place rowwise or column wise
#   # 4: tag values (create a dictionary where each entry is given a unique value)
#   # 5: resample tagged values
#   # 6: decide if too many ties to use method
#   # 7: add one to "used" if using method
#   # 8: rearrange into blocks with compliments
#   # 9: within block get order of tags
#   # 10: within complement of block get order of tags
#   # 11: replace block resample tags with block complement tags
#   # 12: repeat for other blocks with tags
#   # 13: for fragments of blocks with incomplete complements, reorder to top and use piece of adjacent block as needed to determine complements
#
#   # Step 1: Tag values
#   grps <- names(dat) # Get group names
#
#   if (ncol(dat) > 1){
#     # Initialize observation counter
#     obscount <- 1
#
#     # Tag values
#     if (rowwise) {
#       # Row-wise tagging (same as before)
#       for(i in grps){
#         dat[[paste0(i, "_tag")]] <- obscount:(obscount + nrow(dat) - 1)
#         obscount <- obscount + nrow(dat)
#       }
#     } else {
#       # Column-wise tagging
#       for(i in 1:nrow(dat)){
#         for(j in grps){
#           # Assign unique tags to each value across columns for each row
#           dat[[paste0(j, "_tag")]][i] <- obscount
#           obscount <- obscount + 1
#         }
#       }
#     }
#
#   } else {
#     # Handle single column case
#     dat[[paste0(grps, "_tag")]] <- 1:nrow(dat)
#   }
#
#   # Collect all tags into a single vector
#   tag_cols <- grep("_tag$", names(dat), value = TRUE)
#   all_tags <- unlist(dat[, tag_cols])
#
#   if(!rowwise){
#     all_tags <- sort(all_tags)
#   }
#
#   # Step 2: Resample tagged values
#   resampled_tags <- sample(all_tags, replace = replace)
#
#   # Step 3: Decide if too many ties to use method
#   if (length(unique(resampled_tags)) < length(resampled_tags) * tie_thresh) {
#     warning("Too many ties to use the method")
#     return(NULL)
#   }
#
#   # Step 4: Increment usage counter
#   used <- 0
#   used <- used + 1
#
#   # Step 5: Rearrange into blocks with complements
#   block_list <- list() # initialize list
#   BLOCK_SIZE <- block_size <- floor(length(all_tags) / 2) # get block size (CONSTANT and variable)
#   remaining_rtags <- loop_rtags <- resampled_tags # get resampled tags
#   i <- 1 # initialize iterator (for naming blocks)
#
#   # build blocks
#   while(length(remaining_rtags) > 1){
#     if (i == 1){
#       # Get block1 (first block with a resample in resampled_tags)
#       block <- unique(loop_rtags) |> utils::head(block_size)
#       block_stem = block
#
#       # Save block
#       block_list[[paste0("block",i)]] <- list(block = block,
#                                               block_size = block_size)
#     } else {
#       # Get block_loop not represented in block 1
#       remaining_rtags <- remaining_rtags[-match(block_stem, remaining_rtags)]
#       if (length(remaining_rtags) == 0) break
#
#       # Get remaining unique tags from remaining_rtags
#       block_stem <- unique(remaining_rtags)
#       block_size <- length(block_stem)
#
#       # Get remainder of resampled tags
#       # add unique remaining tags (block_stem) onto loop_rtags
#       loop_rtags <- c(block_stem, loop_rtags)
#       # combine the block_stem with enough elements of loop_rtags to form a full block
#       block <- loop_rtags |> unique() |> utils::head(BLOCK_SIZE)
#       # remove elements from loop_rtags which were used to help fill out the block
#       loop_rtags <- loop_rtags[-match(block |> utils::tail(BLOCK_SIZE - block_size), loop_rtags)]
#
#       # Save block
#       block_list[[paste0("block",i)]] <- list(block = block,
#                                               block_size = block_size)
#     }
#
#     i = i + 1
#   }
#
#   # return(block_list)
#   #
#   # block_size <- 5  # Example block size
#   # blocks <- split(resampled_tags, ceiling(seq_along(resampled_tags)/block_size))
#
#   # Get complement of each block
#
#   # Steps 6: Process blocks
#   complement_list <- vector(mode = "list", length = length(block_list)) # initialize list
#   # in_complements <- vector(mode = "numeric")
#   for (i in seq_along(block_list)) {
#     # get complement of the block
#     # complement <- unlist(blocks[-i])
#     # complement_list[[i]] <- setdiff(all_tags, block_list[[i]]$block) |> #complement
#     #   sample(BLOCK_SIZE, replace = FALSE) |> # sample correct number of elements
#     #   utils::head(block_list[[i]]$block_size) # get number of elements needed.
#
#     complement <- setdiff(all_tags, block_list[[i]]$block) #|> #complement
#       #sample(BLOCK_SIZE, replace = FALSE)
#
#     # complement <- complement[order(match(complement |> as.factor() |> as.numeric(),
#     #                                      block_list[[i]]$block |> as.factor() |> as.numeric()))]
#
#
#     # |> # sample correct number of elements
#     #   utils::head(block_list[[i]]$block_size) # get number of elements needed.
#
#     # prioritize elements that were not previously used
#     if(i > 1){
#       complement_stem <- setdiff(complement, unlist(complement_list[i-1]))
#       complement <- unique(c(complement_stem, complement)) |> utils::head(BLOCK_SIZE)
#     }
#     complement_list[[i]]  <- complement
#     # in_complements <- c(in_complements, unlist(complement))
#   }
#
#   return(
#     list(
#       blocks = block_list,
#       complements = complement_list
#       )
#     )
#
#
#   # Combine blocks back into resampled tags
#   final_resampled_tags <- unlist(blocks)
#
#   # Map resampled tags back to data
#   if (tagging_dimension == "row") {
#     resampled_data <- dat[match(final_resampled_tags, dat$tag), ]
#   } else {
#     resampled_data <- dat[, names(final_resampled_tags)]
#   }
#
#   return(resampled_data)
# }
