#' @title Find clusters of records that almost match
#'
#' @description
#' Match near duplicates, based on text distance.  Quickly ID records that should possibly be identical, and then use the output to quickly clean up if needed.
#' 
#' @param dat the vector to check for duplicates.
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
  shorter <- tabyl(dat) %>%
    select(-percent)
  
  result <- fuzzyjoin::stringdist_inner_join(shorter, shorter, by = "dat", max_dist = max_dist, distance_col = "distance")
  result <- result[result[[1]] != result[[3]], ] # remove actual 100% accurate duplicates.  Though this makes it harder to return the full original records, if we want to go there later. 
  pair_independent_dupes <- t(apply(result[, c(1, 3)], 1, sort)) # this line and the next treat A, B as a duplicate of B, A and remove it.  From http://stackoverflow.com/a/9028416
  result <- result[!duplicated(pair_independent_dupes), ]
  if(nrow(result) == 0){stop("no fuzzy duplicate found within specified distance")}
  result <- result %>%
    stats::setNames(c("value1", "value1_count", "value2", "value2_count", "distance")) %>%
    dplyr::arrange(value1, value2) %>%
    assign_clusters %>%
    add_cluster_counts

  left_join(result, get_cluster_dominant_values(result)) %>%
    mutate(cluster = dominant_value) %>%
    select(-dominant_value, -value1_count, -value2_count) %>%
    arrange(-cluster_count, cluster) %>% 
    select(cluster, cluster_count, -distance, everything()) # move distance to final column
}


# Assigns near-match duplicates into clusters, for easier cleaning
# Helper function called by get_fuzzy_dupes
assign_clusters <- function(dat){
  # go down rowwise - if either instance in a row matches a previous cluster, assign to that cluster, otherwise new cluster
  dat$cluster <- numeric(length(nrow(dat))) # might be better to do this as a list?
  dat$cluster[1] <- dat$value1[1] # first row gets unique cluster ID
  for(i in 2:nrow(dat)){
    if(dat[i, "value1"] %in% c(dat[["value1"]][1:(i-1)], dat[["value2"]][1:(i-1)]) | # if there's a match to a previous value
       dat[i, "value2"] %in% c(dat[["value1"]][1:(i-1)], dat[["value2"]][1:(i-1)])){
      dat$cluster[i] <- dat$cluster[min(which(dat[["value1"]][i] == dat[["value1"]][1:(i-1)] | # use the cluster ID from that match 
                                            dat[["value1"]][i] == dat[["value2"]][1:(i-1)] |
                                            dat[["value2"]][i] == dat[["value1"]][1:(i-1)] |
                                            dat[["value2"]][i] == dat[["value2"]][1:(i-1)]
      ))
      ]
    } else{
      dat$cluster[i] <- dat$value1[i] # assign new cluster from value1
    }
  }
  dat
}

# returns most central value in the cluster network. Should it instead use the most commonly-occuring value in the raw vector?
get_cluster_dominant_values <- function(cluster_df){
 long <- bind_rows(cluster_df[, c(1:2, 6)] %>% stats::setNames(c("value", "count", "cluster")),
                   cluster_df[, c(3:4, 6)] %>% stats::setNames(c("value", "count", "cluster")))
 
 long %>%
   group_by(cluster, value) %>%
   summarise(count = sum(count)) %>%
   arrange(-count) %>%
   slice(1) %>%
   select(cluster, dominant_value = value)
}

add_cluster_counts <- function(cluster_df){
  cluster_counts <- cluster_df %>%
    count(cluster) %>%
    rename(cluster_count = n)
  inner_join(cluster_df, cluster_counts)
}


# Examples

set.seed(2)
dirty <- with(misspellings,
  c(misspelling, correct)) %>%
  sample(600, replace = TRUE)

get_fuzzy_dupes(dirty, 1)

p_load(qdapDictionaries)
words <- tbl_df(DICTIONARY) %>%
  filter(nchar(word) > 6)
set.seed(4)
vec <- sample(words$word, 5000)

get_fuzzy_dupes(vec, 1)

p_load(wakefield)
set.seed(12)
get_fuzzy_dupes(wakefield::name(100), 1)
