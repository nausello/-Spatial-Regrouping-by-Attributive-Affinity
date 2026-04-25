#' Rattacher chaque commune candidate au groupe voisin le plus proche
#' @param data Un objet sf avec colonne "candidate" et "id_row"
#' @param group_var Variable de groupe
#' @param vars_attr Variables attributaires
#' @param profiles Profils moyens par groupe (depuis compute_profiles)
#' @return data.frame avec id_row et best_group
#' @export
assign_best_group <- function(data, group_var, vars_attr, profiles) {
  candidates     <- data |> dplyr::filter(candidate)
  non_candidates <- data |> dplyr::filter(!candidate)
  touches        <- sf::st_touches(candidates, non_candidates)

  results <- lapply(seq_len(nrow(candidates)), function(i) {
    xi  <- as.numeric(sf::st_drop_geometry(candidates[i, vars_attr]))
    idx <- touches[[i]]
    if (length(idx) == 0) {
      idx <- sf::st_nearest_feature(candidates[i, ], non_candidates)
    }
    neighbor_groups <- unique(non_candidates[[group_var]][idx])
    best_group <- NA_character_
    min_dist   <- Inf
    for (g in neighbor_groups) {
      profile <- profiles |>
        dplyr::filter(.data[[group_var]] == g) |>
        dplyr::select(dplyr::all_of(vars_attr))
      if (nrow(profile) == 0) next
      d <- sqrt(sum((xi - as.numeric(profile))^2, na.rm = TRUE))
      if (d < min_dist) { min_dist <- d; best_group <- g }
    }
    data.frame(id_row = candidates$id_row[i], best_group = best_group)
  })
  dplyr::bind_rows(results)
}
