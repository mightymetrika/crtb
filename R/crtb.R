#' Complementary Resampling of Tags in Blocks
#'
#' Performs complementary resampling of tags in blocks (crtb) on the provided
#' dataset. This function implements crtb allowing for either pooled or non-pooled
#' resampling of data groups. This method is inspired by the concept of
#' complementary pairs subsampling, Shah & Samworth (2013), and attempts to
#' approximate the method in the realm of resampling.
#'
#' @param dat A data frame or vector containing the data to resample.
#'   If a data frame, each column represents a group or variable.
#' @param pooled Logical; if \code{TRUE} (default), data from all groups are
#'   pooled together for resampling. If \code{FALSE}, resampling is performed
#'   separately for each group.
#' @param rowwise Logical; applicable only when \code{pooled = TRUE}.
#'   If \code{TRUE} (default), tagging is done row-wise across groups.
#'   If \code{FALSE}, tagging is done column-wise within each group.
#' @param tie_thresh Numeric; a threshold between 0 and 1 to decide if there are
#'   too many ties in the resampled data. If the proportion of unique resampled
#'   tags is less than \code{tie_thresh}, the function will return \code{NULL}
#'   and issue a warning. Default is \code{0.5}.
#' @param replace Logical; if \code{TRUE} (default), resampling is done with
#'   replacement.
#' @param sample_fun A user-defined function for custom sampling. This function
#'   should accept a vector of tags (the data to be resampled) and return a
#'   resampled vector of the same length. If \code{NULL} (default), the standard
#'   \code{sample} function is used with the \code{replace} argument.
#'
#' @return A list containing two elements:
#'   \item{crdat}{The complementary resampled data.}
#'   \item{ordat}{The original resampled data.}
#'
#' @details
#' The \code{crtb} function implements complementary resampling of tags in blocks
#' based on the concept of complementary pairs subsampling, Shah & Samworth (2013).
#'
#' When \code{pooled = TRUE}, data from all groups are pooled together, and
#' resampling is performed using a combined tagging scheme.
#'
#' When \code{pooled = FALSE}, the function applies the resampling procedure
#' separately to each group.
#'
#' Internally, the function calls either a pooled version or a non-pooled version
#' of the resampling algorithm. This approach provides flexibility depending on
#' whether the groups in the data should be treated together or separately.
#'
#'
#' @references
#' Shah, R. D., & Samworth, R. J. (2013). Variable Selection with Error Control:
#' Another Look at Stability Selection. \emph{Journal of the Royal Statistical
#' Society: Series B (Statistical Methodology)}, 75(1), 55–80.
#' \doi{10.1111/j.1467-9868.2011.01034.x}
#'
#' @examples
#' # Example with a data frame and pooled = TRUE
#' data <- data.frame(group1 = rnorm(100), group2 = rnorm(100))
#' resampled_data <- crtb(data)
#'
#' # Example with a data frame and pooled = FALSE
#' resampled_data_np <- crtb(data, pooled = FALSE)
#'
#' # Example with a vector
#' vector_data <- rnorm(100)
#' resampled_vector <- crtb(vector_data)
#'
#' # Example with rowwise = FALSE (only when pooled = TRUE)
#' resampled_data_colwise <- crtb(data, rowwise = FALSE)
#'
#' # Custom sampling function
#' sample_fun <- function(x) {
#'   return(sample(x, replace = TRUE))
#' }
#'
#' out_custom <- crtb(data, rowwise = TRUE, sample_fun = sample_fun)
#'
#' @export
crtb <- function(dat, pooled = TRUE, rowwise = TRUE, tie_thresh = 0.5,
                 replace = TRUE, sample_fun = NULL){
  if(!pooled){
    res <- crtb_np(dat = dat, tie_thresh = tie_thresh, replace = replace,
                   sample_fun = sample_fun)
  } else {
    res <- crtb_p(dat = dat, rowwise = rowwise, tie_thresh = tie_thresh,
                  replace = replace, sample_fun = sample_fun)
  }

  return(res)
}

#' Complementary Resampling of Tags in Blocks (Pooled Version)
#'
#' Performs complementary resampling of tags in blocks (crtb) on the provided dataset,
#' pooling all groups together.
#'
#' @param dat A dataframe or vector containing the data to resample. If a
#'   dataframe, each column represents a group or variable.
#' @param rowwise Logical; if \code{TRUE} (default), tagging is done row-wise
#'   across groups. If \code{FALSE}, tagging is done column-wise within each group.
#' @param tie_thresh Numeric; a threshold between 0 and 1 to decide if there are
#'   too many ties in the resampled data. If the proportion of unique resampled tags
#'   is less than \code{tie_thresh}, the function will return \code{NULL} and issue
#'   a warning. Default is \code{0.5}.
#' @param replace Logical; if \code{TRUE} (default), resampling is done with
#'   replacement.
#' @param sample_fun A user-defined function for custom sampling. This function
#'   should accept a vector of tags (the data to be resampled) and return a
#'   resampled vector of the same length. If \code{NULL} (default), the standard
#'   \code{sample} function is used with the \code{replace} argument.
#'
#' @return A list containing two elements:
#'   \item{crdat}{The complementary resampled data.}
#'   \item{ordat}{The original resampled data.}
#'
#' @details
#' The function operates by assigning unique tags to each observation in the data.
#' It then resamples these tags (with or without replacement) and rearranges the
#' data into blocks of unique tags and their complements.
#'
#' @keywords internal
crtb_p <- function(dat, rowwise = TRUE, tie_thresh = 0.5,
                   replace = TRUE, sample_fun = NULL){

  # Step 0: Get information

  # get input dat type, count observations, coerce to data.frame
  if(is.data.frame(dat)){
    datdf <- TRUE
    numobs <- ncol(dat)*nrow(dat)
  } else if (is.vector(dat) & !is.list(dat)){
    datdf <- FALSE
    numobs <- length(dat)
    dat <- data.frame("obs" = dat)
  } else {
    stop("dat must be a data.frame or a vector")
  }

  grps <- names(dat) # get group names

  # Step 1: Tag values
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
  if(is.null(sample_fun)){
    resampled_tags <- sample(all_tags, replace = replace)
  } else {
    if(!is.function(sample_fun)){
      stop("sample_fun must be a function")
    }
    resampled_tags <- sample_fun(all_tags)
    if(length(resampled_tags) != length(all_tags)){
      stop("sample_fun must return a vector of the same length as its input")
    }
  }

  # Step 3: Decide if too many ties to use method
  if (length(unique(resampled_tags)) < length(resampled_tags) * tie_thresh) {
    warning("Too many ties to use the method")
    return(NULL)
  }

  # Step 4: Rearrange into blocks with complements
  block_list <- list() # initialize list
  BLOCK_SIZE <- block_size <- floor(length(all_tags) / 2) # get block size (CONSTANT and variable)
  remaining_rtags <- resampled_tags # get resampled tags
  i <- 1 # initialize iterator (for naming blocks)

  # build blocks
  # while(length(remaining_rtags) > 1){
  while(length(remaining_rtags) > 0){
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
      if (length(remaining_rtags) > BLOCK_SIZE){
        # needed to handle replace = FALSE and numobs is odd
        block_stem <- unique(remaining_rtags) |> utils::head(BLOCK_SIZE)
        block_size <- length(block_stem)
      } else {
        block_stem <- unique(remaining_rtags)
        block_size <- length(block_stem)
      }


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

  # Steps 5: Get complement of each block
  complement_list <- vector(mode = "list", length = length(block_list)) # initialize list
  for (i in seq_along(block_list)) {
    complement_list[[i]] <- setdiff(all_tags, block_list[[i]]$block) |> # complement
      sample(BLOCK_SIZE, replace = FALSE) |> # sample correct number of elements
      utils::head(block_list[[i]]$block_size) # get number of elements needed.
  }

  # Combine complementary blocks and the original resampled tags back into the
  # data structure in preparation for mapping of tags back onto data values
  final_resampled_tags <- unlist(complement_list)
  odat <- dat # need original data for original resamples
  resampled_tags <- as.vector(resampled_tags)
  if (ncol(dat) > 1){

    # Tag values
    if (rowwise) {
      # Row-wise tagging (same as before)
      for(i in grps){
        # complements
        dat[[paste0(i, "_rtag")]] <- final_resampled_tags |> utils::head(nrow(dat))
        final_resampled_tags <- final_resampled_tags |> utils::tail(-nrow(dat))

        # orignal resample
        odat[[paste0(i, "_rtag")]] <- resampled_tags |> utils::head(nrow(odat))
        resampled_tags <- resampled_tags |> utils::tail(-nrow(odat))
      }
    } else {
      # Column-wise tagging
      for(i in 1:nrow(dat)){
        for(j in grps){
          # Assign unique tags to each value across columns for each row

          # complement
          dat[[paste0(j, "_rtag")]][i] <- final_resampled_tags |> utils::head(1)
          final_resampled_tags <- final_resampled_tags |> utils::tail(-1)

          # original resample
          odat[[paste0(j, "_rtag")]][i] <- resampled_tags |> utils::head(1)
          resampled_tags <- resampled_tags |> utils::tail(-1)
        }
      }
    }

  } else {
    # Handle single column case
    dat[[paste0(grps, "_rtag")]] <- final_resampled_tags # complement
    odat[[paste0(grps, "_rtag")]] <- resampled_tags # original resample
  }

  # Step 6: Map resampled tags back to original data

  # create lookup
  lookup <- vector(mode = "list", length = numobs)
  olookup <- vector(mode = "list", length = numobs)
  for (col in grps) {
    # Create a lookup table for each column

    # complement
    lookup_grp <- stats::setNames(dat[[col]], dat[[paste0(col, "_tag")]])
    lookup <- c(lookup, lookup_grp)

    # orignal resample
    olookup_grp <- stats::setNames(odat[[col]], odat[[paste0(col, "_tag")]])
    olookup <- c(olookup, olookup_grp)
  }

  # use the lookup table to map rtags back to original values
  for (col in grps) {
    # complement
    dat[[paste0(col, "_resampled")]] <- lookup[as.character(dat[[paste0(col, "_rtag")]])]
    # orignal resample
    odat[[paste0(col, "_resampled")]] <- olookup[as.character(odat[[paste0(col, "_rtag")]])]
  }

  # Step 7: Build return object
  if (length(grps) > 1){

    # complement
    rdat <- dat[,grep("_resampled", names(dat), value = TRUE)] |>
      stats::setNames(grps)
    # orignal resample
    ordat <- odat[,grep("_resampled", names(odat), value = TRUE)] |>
      stats::setNames(grps)
  } else {
    #complement
    rdat <- dat[,ncol(dat)] |> unlist() |> as.vector()
    if(datdf) rdat <- as.data.frame(rdat) |> stats::setNames(grps) # ensure input/output same type

    # orignal resample
    ordat <- odat[,ncol(odat)] |> unlist() |> as.vector()
    if(datdf) ordat <- as.data.frame(ordat) |> stats::setNames(grps) # ensure input/output same type
  }

  #Use following output for research purposes
  return(
    list(
      crdat = rdat, # complementary
      ordat = ordat # orignal resample
    )
    )


}

#' Complementary Resampling of Tags in Blocks (Non-Pooled Version)
#'
#' Performs complementary resampling of tags in blocks (crtb) on the provided data
#' without pooling the groups. This function applies the resampling procedure
#' separately to each group (column) in the data, which can be useful when groups
#' are independent or should not be combined.
#'
#' @param dat A data frame containing the data to resample. Each column represents
#'   a group or variable.
#' @param tie_thresh Numeric; a threshold between 0 and 1 to decide if there are
#'   too many ties in the resampled data. If the proportion of unique resampled
#'   tags is less than \code{tie_thresh}, the function will return \code{NULL}
#'   for that group and issue a warning. Default is \code{0.5}.
#' @param replace Logical; if \code{TRUE} (default), resampling is done with
#'   replacement.
#' @param sample_fun A user-defined function for custom sampling. This function
#'   should accept a vector of tags (the data to be resampled) and return a
#'   resampled vector of the same length. If \code{NULL} (default), the standard
#'   \code{sample} function is used with the \code{replace} argument.
#'
#' @return A list containing two elements:
#'   \item{crdat}{The complementary resampled data.}
#'   \item{ordat}{The original resampled data.}
#'
#' @details
#' The function applies the complementary resampling method implemented in
#' \code{\link{crtb_p}} to each group separately, without pooling the data. This
#' is useful in contexts where the groups are independent or when pooling the data
#' may not be appropriate.
#'
#' @keywords internal
crtb_np <- function(dat, tie_thresh = 0.5, replace = TRUE, sample_fun = NULL) {

  # Check that dat is a data.frame
  if (!is.data.frame(dat)) {
    stop("dat must be a data.frame")
  }

  # Apply crtb to each column (group) separately
  results <- lapply(dat, function(col) {
    crtb(col, rowwise = TRUE, tie_thresh = tie_thresh, replace = replace,
         sample_fun = sample_fun)
  })

  # Check if any results are NULL
  if (any(sapply(results, is.null))) {
    return(NULL)
  }

  # Extract the resampled data from each result
  rdat_list <- lapply(results, function(res) {
    # 'res' could be a vector or data.frame with one column
    if (is.data.frame(res$crdat)) {
      res$crdat[[1]]  # Extract the column as a vector
    } else {
      res$crdat
    }
  })

  odat_list <- lapply(results, function(res) {
    # 'res' could be a vector or data.frame with one column
    if (is.data.frame(res$ordat)) {
      res$ordat[[1]]  # Extract the column as a vector
    } else {
      res$ordat
    }
  })

  # Combine the resampled data into a data frame
  rdat_df <- data.frame(rdat_list)
  names(rdat_df) <- names(dat)

  # Combine the resampled data into a data frame
  odat_df <- data.frame(odat_list)
  names(odat_df) <- names(dat)

  return(list(crdat = rdat_df,
              ordat = odat_df))
}
