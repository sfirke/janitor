get_first_col_classes <- function(dat) {
  all_classes <- lapply(dat, class)
  vapply(all_classes, `[[`, character(1), 1)
}

compare_two_dfs <- function(df1, df2) {
  
  common_vars <- intersect(names(df1), names(df2))
  vars_in_df1_only <- setdiff(names(df1), names(df2))
  vars_in_df2_only <- setdiff(names(df2), names(df1))
  
  df1_common <- df1[common_vars]
  df2_common <- df2[common_vars]
  
  df1_col_types <- get_first_col_classes(df1_common)
  df2_col_types <- get_first_col_classes(df2_common)
  
  col_mismatch_index <- df1_col_types != df2_col_types
  col_mismatches <- names(df1_common)[col_mismatch_index]
  col_mismatch_class1 <- df1_col_types[col_mismatch_index]
  col_mismatch_class2 <- df2_col_types[col_mismatch_index]
  
  dfs_agree <- ! (length(vars_in_df1_only) ||
                  length(vars_in_df2_only) ||
                  any(col_mismatch_index))
  
  if (dfs_agree) {
    NULL
  } else {
    list(
      vars_in_df1_only = vars_in_df1_only,
      vars_in_df2_only = vars_in_df2_only,
      column_class_mismatches = data_frame(variable = col_mismatches,
                                           class_in_df1 = col_mismatch_class1,
                                           class_in_df2 = col_mismatch_class2
      )
    )
  }

}



compare_dfs <- function(...) {
  
  df_list <- list(...)
  
  dataframes <- vapply(df_list, is.data.frame, logical(1))
  if (! all(dataframes)) {
    stop("All arguments to `compare_dfs` must be data frames.", call. = FALSE)
  }
  
  df_names <- names(df_list)
  if (is.null(df_names)) {
    names(df_list) <- paste0("df", seq_along(df_list))
  } else if (any(missing_names <- df_names == "")) {
    missing_name_ind <- which(missing_names)
    names(df_list)[missing_names] <- paste0("df", missing_name_ind)
  }
  
  df_pairs <- utils::combn(names(df_list), 2)
  df_pair_names <- apply(df_pairs, 2, paste0, collapse = "")
  
  comp <- lapply(seq_len(ncol(df_pairs)),
                 function(i) compare_two_dfs(df_list[[df_pairs[1, i]]],
                                             df_list[[df_pairs[2, i]]]))
  
  purrr::set_names(comp, df_pair_names)
  
}