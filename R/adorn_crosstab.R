# take a crosstab() call and print a nice result

adorn_crosstab <- function(crosstab, denom = "row", show_n = TRUE, digits = 1, rounding = "half to even"){
  # some input checks go here
  if(! denom %in% c("row", "col", "all")){stop("'denom' must be one of 'row', 'col', or 'all'")}
  if(! rounding %in% c("half to even", "half up")){stop("'rounding' must be one of 'half to even' or 'half up'")}
  
  n_col <- ncol(crosstab)
  
  # calculate %s
  percs <- crosstab
  if(denom == "row"){
    row_sum <- rowSums(crosstab[, 2:n_col], na.rm = TRUE)
    percs <- crosstab %>%
      mutate_at(vars(2:n_col), funs(. / row_sum))
  } else if(denom == "col"){
    col_sum <- colSums(crosstab[, 2:n_col], na.rm = TRUE)
    percs <- crosstab
    percs[, 2:n_col] <- sweep(percs[, 2:4], 2, col_sum,`/`) # from http://stackoverflow.com/questions/9447801/dividing-columns-by-colsums-in-r
  } else if(denom == "all"){
    all_sum <- sum(crosstab[, 2:n_col], na.rm = TRUE)
    percs[, 2:n_col] <- percs[, 2:n_col] / all_sum 
  }

    # round %s, with specified method, add % sign
  percs <- mutate_at(percs, vars(2:n_col), funs(. * 100)) # since we'll be adding % sign - do this before rounding
  
  if(rounding == "half to even"){ percs <- mutate_at(percs, vars(2:n_col), funs(round(., digits))) }
  else if(rounding == "half up"){ percs <- mutate_at(percs, vars(2:n_col), funs(round_half_up(., digits)))}
  percs <- mutate_at(percs, vars(2:n_col), funs(format(., digits)))

  percs <- mutate_at(percs, vars(2:n_col), funs(paste0(., "%")))

    # paste Ns if needed
  if(show_n){
    result <- paste_ns(percs, crosstab)
  } else{ result <- percs}
  
  result

  
}

# Example
mtcars %>%
  crosstab(gear, cyl) %>%
  adorn_crosstab(denom = "all")

adorn_crosstab(., denom = "col", rounding = "half up", show_n = FALSE, digits = 2)


# From http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
round_half_up <- function(x, n){
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# takes data.frames of Ns and %s, pastes them together
paste_ns <- function(perc_df, n_df){
  n_matrix <- as.matrix(n_df)
  perc_matrix <- as.matrix(perc_df)
  
  pasted <- matrix(paste(perc_matrix, " (", n_matrix, ")", sep = ""), 
                   nrow = nrow(n_matrix),
                   dimnames = dimnames(perc_matrix))
  
  pasted <- as_data_frame(pasted)
  pasted[[1]] <- n_df[[1]] # undo the pasting in this 1st column
  pasted
}