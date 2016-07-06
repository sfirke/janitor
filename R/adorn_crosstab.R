


# take a crosstab() call and print a nice result

adorn_crosstab <- function(crosstab, denom = "row", show_n = "true", digits = 1, rounding = "half to even"){
  # some checks go here
    
  
}

q <- crosstab(mtcars$cyl, mtcars$gear)
n_col <- ncol(q)

row_sum <- rowSums(q[, 2:ncol(q)], na.rm = TRUE)
q %>%
  mutate_at(vars(2:n_col), funs(. / row_sum))

