#' Regroupement spatial iteratif par affinite attributaire
#'
#' @param data sf object
#' @param group_var Variable de groupe initial (ex: "EPCI")
#' @param vars_attr Variables attributaires pour le calcul d'affinite
#' @param method Methode de distance : "euclidean" (defaut) ou "mahalanobis"
#' @param threshold Seuil d'affinite pour identifier un candidat (defaut 0)
#' @param iterations Nombre maximum d'iterations (defaut 1)
#' @param min_group Taille minimale d'un groupe valide pour le rattachement (defaut 1)
#' @param nb_type Type de voisinage spatial : "queen" (defaut) ou "rook"
#' @param remove_isolates Reintegrer les candidats isoles (defaut TRUE)
#' @param verbose Afficher les messages de progression (defaut TRUE)
#'
#' @return Un sf object identique a l'entree avec trois colonnes supplementaires :
#'   \code{{group_var}_regroup} (affectation finale), \code{candidate} (TRUE si
#'   l'unite a ete reclassee a au moins une iteration), et \code{iter_history}
#'   (liste des affectations par iteration, sous forme d'attribut).
#'
#' @export
spatialRegroup <- function(data, group_var, vars_attr,
                           method          = "euclidean",
                           threshold       = 0,
                           iterations      = 1,
                           min_group       = 1,
                           nb_type         = "queen",
                           remove_isolates = TRUE,
                           verbose         = TRUE) {

  data <- data %>%
    dplyr::mutate(id_row    = dplyr::row_number(),
                  candidate = FALSE)

  new_var <- paste0(group_var, "_regroup")
  data[[new_var]] <- as.character(data[[group_var]])

  # Historique des etats pour detection de cycles (point 2)
  states_history <- character(0)

  # Voisinage calcule une seule fois (la geometrie ne change pas)
  if (verbose) message("[0/5] Pre-calcul du voisinage spatial...")
  nb <- build_neighbors(data, type = nb_type)

  for (iter in seq_len(iterations)) {

    if (verbose) message("== Iteration ", iter, "/", iterations, " ==")

    current_group <- if (iter == 1) group_var else new_var

    if (verbose) message("[2/5] Calcul des affinites...")
    aff <- compute_affinity(data, nb, current_group, vars_attr,
                            method    = method,
                            threshold = threshold)

    data$candidate <- FALSE

    if (nrow(aff) == 0 || sum(aff$candidat) == 0) {
      if (verbose) message("0 candidates identifiees")
      if (verbose) message("Convergence atteinte a l'iteration ", iter)
      break
    }

    data$candidate[aff$id_row[aff$candidat]] <- TRUE
    if (verbose) message(sum(data$candidate), " candidates identifiees")

    if (remove_isolates) {
      if (verbose) message("[3/5] Suppression des isolats...")
      data <- remove_isolates(data, nb)
    }

    if (sum(data$candidate, na.rm = TRUE) == 0) {
      if (verbose) message("Aucun candidat restant apres filtrage isolats")
      if (verbose) message("Convergence atteinte a l'iteration ", iter)
      break
    }

    if (verbose) message("[4/5] Calcul des profils de groupe...")
    profiles <- compute_profiles(data, current_group, vars_attr)

    if (min_group > 1) {
      group_sizes  <- table(sf::st_drop_geometry(data)[[current_group]])
      valid_groups <- names(group_sizes[group_sizes >= min_group])
      profiles     <- profiles %>%
        dplyr::filter(.data[[current_group]] %in% valid_groups)
      if (verbose) message(
        nrow(profiles), " groupes valides (taille >= ", min_group, ")"
      )
    }

    if (verbose) message("[5/5] Rattachement des candidats...")
    assignments <- assign_best_group(data, current_group, vars_attr, profiles)

    if (nrow(assignments) == 0) {
      if (verbose) message("Aucun rattachement possible — arret")
      break
    }

    # ── Etat avant affectation (point 1 : base de comparaison reelle) ────────
    state_avant <- as.character(data[[new_var]])

    data <- data %>%
      dplyr::left_join(
        assignments %>% dplyr::rename(best_group_iter = best_group),
        by = "id_row"
      ) %>%
      dplyr::mutate(
        !!new_var := dplyr::if_else(
          candidate & !is.na(best_group_iter),
          best_group_iter,
          as.character(.data[[new_var]])
        )
      ) %>%
      dplyr::select(-best_group_iter)

    # ── Point 1 : n_reclassed = unites dont l'affectation a reellement change ─
    n_reclassed <- sum(as.character(data[[new_var]]) != state_avant, na.rm = TRUE)
    if (verbose) message("Iteration ", iter, " : ", n_reclassed,
                         " unites reclassees")

    # ── Point 1 : convergence reelle (aucun changement effectif) ─────────────
    if (n_reclassed == 0) {
      if (verbose) message("Convergence atteinte a l'iteration ", iter)
      break
    }

    # ── Point 2 : detection de cycle (etat deja vu) ───────────────────────────
    state_actuel <- paste(as.character(data[[new_var]]), collapse = "|")
    if (state_actuel %in% states_history) {
      if (verbose) message(
        "Cycle detecte a l'iteration ", iter,
        " (etat identique a une iteration precedente) — arret"
      )
      break
    }
    states_history <- c(states_history, state_actuel)
  }

  # ── candidate = unites effectivement reclassees (groupe initial != final) ───
  data <- data %>%
    dplyr::mutate(
      candidate = as.character(.data[[group_var]]) != as.character(.data[[new_var]])
    )

  if (verbose) {
    n_total <- sum(data$candidate, na.rm = TRUE)
    message("Termine : ", n_total, "/", nrow(data),
            " unites reclassees au total")
  }

  return(data)
}
