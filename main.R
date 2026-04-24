# Load data ----------------------------------------------------------------------------------------
ratings <- nanoparquet::read_parquet("ratings.parquet")
episodes <- nanoparquet::read_parquet("episodes.parquet")

# Wrangle ------------------------------------------------------------------------------------------
combined <- ratings |>
  dplyr::rename(Season = season, Episode = episode) |>
  dplyr::full_join(episodes, by = c("Season", "Episode")) |>
  dplyr::mutate(
    episode_id = sprintf("S%02dE%02d", Season, Episode),
    total_votes = rowSums(dplyr::across(x1:x10)),
    avg_rating = as.vector(as.matrix(dplyr::across(x1:x10)) %*% 1:10) /
      total_votes
  )

threshold <- quantile(combined$total_votes, 0.25) |> unname()
mean_global <- mean(combined$avg_rating, na.rm = TRUE)

xfiles_data <- combined |>
  dplyr::mutate(
    bayes_rating = (total_votes * avg_rating + threshold * mean_global) /
      (total_votes + threshold)
  )

# Plot ---------------------------------------------------------------------------------------------
xfiles_data |>
  dplyr::select(episode_id, Title, bayes_rating, dplyr::starts_with("x")) |>
  gt::gt() |>
  gt::tab_header(gt::md("*The X-Files* Episode Rating")) |>
  gt::fmt_number(columns = bayes_rating, decimals = 1) |>
  gt::cols_label(
    bayes_rating = gt::md("**Rating**"),
    episode_id = "Episode"
  ) |>
  gt::cols_nanoplot(
    columns = dplyr::starts_with("x"),
    autoscale = FALSE,
    autohide = TRUE,
    new_col_name = "distribution",
    new_col_label = gt::md("**Distribution**")
  ) |>
  gt::cols_align(align = "center", columns = gt::everything()) |>
  gt::cols_align(align = "left", columns = "Title")
