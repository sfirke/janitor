coalesce_dfs <- function(x, y, vars_to_update = "all", by = NULL, ...){
  
  if(vars_to_update[[1]] == "all"){
    vars_to_update <- intersect(names(x), names(y))[!intersect(names(x), names(y)) %in% by] # need better way to avoid manipulating the by variable(s), right now okay if joining on names that match across the data.frames
  }
  result <- left_join(x,
                      y[, c(by, vars_to_update)],
                      by = by,
                      ...)
  for(i in seq_along(vars_to_update)){
  result <- result %>%
    coalesce_joined_var(., vars_to_update[i])
  }
  message(paste0("Coalesced: ", paste(vars_to_update, collapse = ", ")))
result
}

# coalesce two variables .x and .y given the original name, and drop the .x and .y
# takes a data.frame and name of a column, returns a data.frame
coalesce_joined_var <- function(dat, var_name){
  # var_name <- deparse(substitute(var))
  var_name_x <- paste0(var_name, ".x")
  var_name_y <- paste0(var_name, ".y")
  dat <- dat %>%
    mutate(temp___var = coalesce(dat[[var_name_x]],
                               dat[[var_name_y]]))
  names(dat)[names(dat) == "temp___var"] <- var_name
  
  dat[, !names(dat) %in% c(var_name_x, var_name_y)]

}

## Examples 
t1 <- data_frame(id = 1, lo = as.numeric(NA), med = 3, hi = as.numeric(NA), other = "still here")
t2 <- data_frame(id = 1, med = 4, hi = 10, lo = 1)

coalesce_joined_var(test, "lo")
coalesce_dfs(t1, t2, by = "id")
coalesce_dfs(t1, t2, vars_to_update = c("lo"), by = "id") # update just the specified col
