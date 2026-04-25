#' Calculer les profils moyens par groupe
#' @param data Un objet sf
#' @param group_var Variable de groupe
#' @param vars_attr Variables attributaires
#' @return data.frame des profils moyens par groupe
#' @export
compute_profiles <- function(data, group_var, vars_attr) {
  data |>
    sf::st_drop_geometry() |>
    dplyr::filter(!candidate) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vars_attr), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    )
}
