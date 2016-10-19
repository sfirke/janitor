

#' @title Find clusters of records that almost match
#'
#' @description
#' Match near duplicates, based on text distance.  Quickly ID records that should possibly be identical, and then use the output to quickly clean up if needed.
#' 
#' @param dat the data.frame to check for duplicates.  Right now takes only a one-vector data.frame.
#' @param max_dist maximum distance to count as a duplicate.  Increase for looser definition of duplicates (and potentially more false positives). 
#'
#' @return a data.frame of duplicates, along with their cluster key (for easier cleaning up) and distance between the two instances.
#' @export
#'
#' @examples
#' library(dplyr)
#' dat <- mtcars %>%
#'   transmute(cars = row.names(.))
#'get_fuzzy_dupes(dat, 1)
#'get_fuzzy_dupes(dat, 3)

get_fuzzy_dupes <- function(dat, max_dist = 2){
  result <- fuzzyjoin::stringdist_inner_join(dat, dat, max_dist = max_dist, distance_col = "distance")
  result <- result[result[[1]] != result[[2]], ] # remove actual 100% accurate duplicates.  Though this makes it harder to return the full original records, if we want to go there later. 
  result <- t(apply(result, 1, sort)) # this line and the next treat A, B as a duplicate of B, A and remove it.  From http://stackoverflow.com/a/9028416
  result <- result[!duplicated(result), ]
  dplyr::as_data_frame(result) %>%
    dplyr::select(instance1 = 2, instance2 = 3, distance = 1) %>%
    dplyr::arrange(instance1, instance2) %>%
    assign_clusters
}


# Assigns near-match duplicates into clusters, for easier cleaning
# Helper function called by get_fuzzy_dupes
assign_clusters <- function(dat){
  # go down rowwise - if either instance in a row matches a previous cluster, assign to that cluster, otherwise new cluster
  dat$cluster <- numeric(length(nrow(dat))) # might be better to do this as a list?
  dat$cluster[1] <- dat$instance1[1] # first row gets unique cluster ID
  for(i in 2:nrow(dat)){
    if(dat[i, "instance1"] %in% c(dat[["instance1"]][1:(i-1)], dat[["instance2"]][1:(i-1)]) | # if there's a match to a previous instance
       dat[i, "instance2"] %in% c(dat[["instance1"]][1:(i-1)], dat[["instance2"]][1:(i-1)])){
      dat$cluster[i] <- dat$cluster[min(which(dat[["instance1"]][i] == dat[["instance1"]][1:(i-1)] | # use the cluster ID from that match 
                                            dat[["instance1"]][i] == dat[["instance2"]][1:(i-1)] |
                                            dat[["instance2"]][i] == dat[["instance1"]][1:(i-1)] |
                                            dat[["instance2"]][i] == dat[["instance2"]][1:(i-1)]
      ))
      ]
    } else{
      dat$cluster[i] <- dat$instance1[i] # assign enw cluster from instance1
    }
  }
  dat
}



