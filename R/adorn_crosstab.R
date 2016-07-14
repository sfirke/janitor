


# take a crosstab() call and print a nice result

adorn_crosstab <- function(crosstab, denom = "row", show_n = "true", digits = 1, rounding = "half to even"){
  # some input checks go here
    
  
}

round_half_up <- function(x, n){
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}



q <- crosstab(mtcars$cyl, mtcars$gear)
n_col <- ncol(q)

row_sum <- rowSums(q[, 2:ncol(q)], na.rm = TRUE)
q %>%
  mutate_at(vars(2:n_col), funs(. / row_sum))


full_join(tabyl(mtcars$cyl), tabyl(mtcars$gear), by = c("mtcars_cyl" = "mtcars_gear")) %>%
  setNames(c("value", "cyl_n", "cyl_percent", "gear_n", "gear_percent"))