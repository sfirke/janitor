# Return groupings for a factor variable in the top_levels() function

get_level_groups <- function(vec, n, num_levels_in_var) {
  top_n_lvls <- paste(levels(vec)[1:n], collapse = ", ")
  bot_n_lvls <- paste(levels(vec)[(num_levels_in_var - n + 1):num_levels_in_var], collapse = ", ")

  # Identify middle combinations, if needed
  if (num_levels_in_var > 2 * n) {
    mid_lvls <- paste(levels(vec)[(n + 1):(num_levels_in_var - n)], collapse = ", ")
  } else {
    mid_lvls <- NA
  }

  # Truncate strings if needed
  ## Middle groups are variable size, so displaying the N there is useful;
  ## Top/Bottom are user-specified size, so just truncate the labels
  if (!is.na(mid_lvls) & nchar(mid_lvls) > 30) {
    mid_lvls <- paste0("<<< Middle Group (", num_levels_in_var - 2 * n, " categories) >>>")
  }
  if (nchar(top_n_lvls) > 30) {
    top_n_lvls <- paste0(substr(top_n_lvls, 1, 27), "...")
  }
  if (nchar(bot_n_lvls) > 30) {
    bot_n_lvls <- paste0(substr(bot_n_lvls, 1, 27), "...")
  }

  list(top = top_n_lvls, mid = mid_lvls, bot = bot_n_lvls)
}
