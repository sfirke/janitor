check_bindable <- function(df1, df2){ # should eventually take a list of dfs
  vars_in_df1_only <- names(df1)[!names(df1) %in% names(df2)]
  vars_in_df2_only <- names(df2)[!names(df2) %in% names(df1)]
  
  # subset DFs to only the same for class investigation
  common_vars <- Reduce(intersect, list(names(df1), names(df2)))
  df1_common <- df1[, common_vars]
  df2_common <- df2[, common_vars]
  
  if(sum(names(df1_common) == names(df2_common)) != ncol(df1_common)){stop("something is wrong, names in df1_common don't match names in df2_common")}
  
  df1_col_types <- lapply(df1_common, class) %>%
    lapply(`[[`, 1) %>% # get first class, in case of POSIX there are multiple
    unlist
  
  df2_col_types <- lapply(df2_common, class) %>%
    lapply(`[[`, 1) %>% # get first class, in case of POSIX there are multiple
    unlist
  
  col_mismatch_index <- df1_col_types != df2_col_types
  col_mismatches <- names(df1_common)[col_mismatch_index]
  col_mismatch_class1 <- df1_col_types[col_mismatch_index]
  col_mismatch_class2 <- df2_col_types[col_mismatch_index]
  
  # result is ragged, so return as a list
  list(
    vars_in_df1_only = vars_in_df1_only,
    vars_in_df2_only = vars_in_df2_only,
    column_class_mismatches = data_frame(variable = col_mismatches,
                                         class_in_df1 = col_mismatch_class1,
                                         class_in_df2 = col_mismatch_class2
    )
  )
}

check_bindable(mtcars %>%
                 mutate(cyl = as.factor(cyl),
                        new_var = "hi"),
               mtcars %>%
                 select(-mpg, -wt) %>%
                 rename(CARB = carb)
)
#> $vars_in_df1_only
#> [1] "mpg"     "wt"      "carb"    "new_var"
#> 
#> $vars_in_df2_only
#> [1] "CARB"
#> 
#> $column_class_mismatches
#> # A tibble: 1 × 3
#>   variable class_in_df1 class_in_df2
#>      <chr>        <chr>        <chr>
#> 1      cyl       factor      numeric