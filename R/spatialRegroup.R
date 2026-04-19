#' Regroupement spatial par affinite attributaire
#' @param data sf object
#' @param group_var Variable de groupe initial (ex: "EPCI")
#' @param vars_attr Variables attributaires pour le calcul d'affinite
#' @param remove_isolates Reintegrer les isolats (defaut TRUE)
#' @param nb_type "queen" ou "rook"
#' @param verbose Messages de progression (defaut TRUE)
#' @return An sf object identical to the input with two new columns: 'EPCI_regroup' (new group assignment) and 'candidate' (logical, TRUE if reclassified).
#' @export
spatialRegroup <- function(data, group_var, vars_attr,
                           remove_isolates = TRUE,
                           nb_type         = "queen",
                           verbose         = TRUE) {

  data <- data %>%
    dplyr::mutate(id_row    = dplyr::row_number(),
                  candidate = FALSE)

  if (verbose) message("[1/5] Construction du voisinage...")
  nb <- build_neighbors(data, type = nb_type)

  if (verbose) message("[2/5] Calcul des affinites...")
  aff <- compute_affinity(data, nb, group_var, vars_attr)
  data$candidate[aff$id_row[aff$affinite > 0]] <- TRUE
  if (verbose) message(sum(data$candidate), " candidates identifiees")

  if (remove_isolates) {
    if (verbose) message("[3/5] Suppression des isolats...")
    data <- remove_isolates(data, nb)
  }

  if (verbose) message("[4/5] Calcul des profils de groupe...")
  profiles <- compute_profiles(data, group_var, vars_attr)

  if (verbose) message("[5/5] Rattachement des candidats...")
  assignments <- assign_best_group(data, group_var, vars_attr, profiles)

  new_var <- paste0(group_var, "_regroup")

  data <- data %>%
    dplyr::left_join(assignments, by = "id_row") %>%
    dplyr::mutate(
      !!new_var := dplyr::if_else(
        candidate, best_group, as.character(.data[[group_var]])
      )
    )

  if (verbose) {
    n <- sum(data$candidate, na.rm = TRUE)
    message("Termine : ", n, "/", nrow(data), " unites reclassees")
  }

  return(data)
}
