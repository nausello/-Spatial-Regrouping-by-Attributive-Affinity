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
#' @param weights Vecteur de poids pour les variables attributaires (meme longueur
#'   que vars_attr, defaut NULL = poids egaux). Applique apres standardisation.
#'   Exemple : c(0.5, 0.3, 0.2) pour donner plus de poids au prix.
#' @param standardize Standardiser les variables avant calcul de distance (defaut TRUE).
#'   Recommande pour que les variables soient comparables entre elles.
#' @param local_profile Si TRUE (defaut), compare chaque candidat au profil des membres
#'   adjacents du groupe cible plutot qu'au centroide global du groupe. Plus
#'   representatif pour les grands groupes heterogenes.
#' @param cohesion_tol Seuil minimal de progression de la coherence (eta²) entre deux
#'   iterations. L'algorithme s'arrete si la progression est inferieure a ce seuil
#'   (defaut 0.001). Mettre a NULL pour desactiver ce critere d'arret.
#' @param verbose Afficher les messages de progression (defaut TRUE)
#'
#' @return Un sf object identique a l'entree avec les colonnes supplementaires :
#'   \code{{group_var}_regroup} (affectation finale) et \code{candidate} (TRUE si
#'   l'unite a ete reclassee).
#'
#' @export
spatialRegroup <- function(data, group_var, vars_attr,
                           method        = "euclidean",
                           threshold     = 0,
                           iterations    = 1,
                           min_group     = 1,
                           nb_type       = "queen",
                           remove_isolates = TRUE,
                           weights       = NULL,
                           standardize   = TRUE,
                           local_profile = TRUE,
                           cohesion_tol  = 0.001,
                           verbose       = TRUE) {

  # ── Validation des arguments ─────────────────────────────────────────────────
  if (!is.null(weights)) {
    if (length(weights) != length(vars_attr))
      stop("'weights' doit avoir la meme longueur que 'vars_attr' (",
           length(vars_attr), " variables)")
    if (any(weights < 0))
      stop("'weights' doit contenir des valeurs positives uniquement")
  }

  data <- data %>%
    dplyr::mutate(id_row    = dplyr::row_number(),
                  candidate = FALSE)

  new_var <- paste0(group_var, "_regroup")
  data <- data |>
    dplyr::mutate(!!new_var := as.character(.data[[group_var]]))

  states_history <- character(0)

  # ── Voisinage pre-calcule (la geometrie ne change pas) ───────────────────────
  if (verbose) message("[0/5] Pre-calcul du voisinage spatial...")
  nb <- build_neighbors(data, type = nb_type)

  # ── Coherence initiale (point 4) ─────────────────────────────────────────────
  cohesion_prev <- if (!is.null(cohesion_tol)) {
    compute_cohesion(data, group_var, vars_attr)
  } else NULL

  if (verbose && !is.null(cohesion_tol))
    message("Coherence initiale (eta²) : ", round(cohesion_prev, 4))

  for (iter in seq_len(iterations)) {

    if (verbose) message("== Iteration ", iter, "/", iterations, " ==")

    current_group <- if (iter == 1) group_var else new_var

    if (verbose) message("[2/5] Calcul des affinites...")
    aff <- compute_affinity(data, nb, current_group, vars_attr,
                            method      = method,
                            threshold   = threshold,
                            weights     = weights,
                            standardize = standardize)

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
    assignments <- assign_best_group(data, current_group, vars_attr, profiles,
                                     nb            = nb,
                                     local_profile = local_profile,
                                     weights       = weights,
                                     standardize   = standardize)

    if (nrow(assignments) == 0) {
      if (verbose) message("Aucun rattachement possible — arret")
      break
    }

    # ── Etat avant affectation ───────────────────────────────────────────────
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

    # ── Controle post-rattachement : reversion des isolats ───────────────────
    df_tmp         <- sf::st_drop_geometry(data)
    reclassees_idx <- which(as.character(df_tmp[[new_var]]) != state_avant)

    if (length(reclassees_idx) > 0) {
      isolats_post <- sapply(reclassees_idx, function(i) {
        nouveau_groupe <- df_tmp[[new_var]][i]
        voisins <- unlist(nb[[i]])
        voisins <- voisins[!is.na(voisins) & voisins > 0]
        length(voisins) == 0 ||
          !any(df_tmp[[new_var]][voisins] == nouveau_groupe, na.rm = TRUE)
      })
      idx_reverter <- reclassees_idx[isolats_post]
      if (length(idx_reverter) > 0) {
        if (verbose) message(length(idx_reverter),
                             " isolat(s) post-rattachement reverte(s)")
        data[[new_var]][idx_reverter] <- state_avant[idx_reverter]
      }
    }

    # ── Comptage reclassements reels ─────────────────────────────────────────
    n_reclassed <- sum(as.character(data[[new_var]]) != state_avant, na.rm = TRUE)
    if (verbose) message("Iteration ", iter, " : ", n_reclassed, " unites reclassees")

    if (n_reclassed == 0) {
      if (verbose) message("Convergence atteinte a l'iteration ", iter)
      break
    }

    # ── Point 4 : critere d'arret sur la coherence (eta²) ───────────────────
    if (!is.null(cohesion_tol)) {
      cohesion_cur  <- compute_cohesion(data, new_var, vars_attr)
      delta_cohesion <- cohesion_cur - cohesion_prev
      if (verbose)
        message("  Coherence (eta²) : ", round(cohesion_cur, 4),
                " (delta = ", round(delta_cohesion, 4), ")")
      if (delta_cohesion < cohesion_tol && iter > 1) {
        if (verbose) message("  Progression de coherence < ", cohesion_tol,
                             " — arret (iteration ", iter, ")")
        break
      }
      cohesion_prev <- cohesion_cur
    }

    # ── Detection de cycle ────────────────────────────────────────────────────
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

  # ════════════════════════════════════════════════════════════════════════════
  # POST-TRAITEMENT : isolats (reclasses ET non-reclasses) + groupes vides
  #                 + connectivite des groupes resultants (point 5)
  # ════════════════════════════════════════════════════════════════════════════
  if (verbose) message("── Post-traitement : isolats, groupes vides, connectivite...")

  groupes_originaux <- unique(as.character(sf::st_drop_geometry(data)[[group_var]]))

  for (pass in seq_len(50)) {

    df_post <- sf::st_drop_geometry(data)
    modifie <- FALSE

    # ── Etape 1 : isolats (toutes les unites, pas seulement les reclassees) ───
    for (i in seq_len(nrow(data))) {
      groupe_i <- df_post[[new_var]][i]
      voisins  <- unlist(nb[[i]])
      voisins  <- voisins[!is.na(voisins) & voisins > 0]
      est_isole <- length(voisins) == 0 ||
        !any(df_post[[new_var]][voisins] == groupe_i, na.rm = TRUE)

      if (est_isole) {
        if (df_post[[new_var]][i] != df_post[[group_var]][i]) {
          # Unite reclassee isolee → revert vers groupe d'origine
          data[[new_var]][i]    <- as.character(df_post[[group_var]][i])
          df_post[[new_var]][i] <- as.character(df_post[[group_var]][i])
        } else {
          # Unite non-reclassee mais isolee (tous ses voisins ont change) →
          # reassigner au groupe le plus representé parmi ses voisins
          voisins_groupes <- df_post[[new_var]][voisins]
          voisins_groupes <- voisins_groupes[!is.na(voisins_groupes)]
          if (length(voisins_groupes) > 0) {
            nouveau_groupe <- names(sort(table(voisins_groupes),
                                         decreasing = TRUE))[1]
            data[[new_var]][i]    <- nouveau_groupe
            df_post[[new_var]][i] <- nouveau_groupe
          }
        }
        modifie <- TRUE
      }
    }

    # ── Etape 2 : groupes originaux vides → restituer une commune ─────────────
    groupes_finaux <- unique(as.character(df_post[[new_var]]))
    groupes_vides  <- setdiff(groupes_originaux, groupes_finaux)
    for (g in groupes_vides) {
      idx_g <- which(as.character(df_post[[group_var]]) == g &
                       as.character(df_post[[new_var]]) != g)
      if (length(idx_g) > 0) {
        data[[new_var]][idx_g[1]]    <- g
        df_post[[new_var]][idx_g[1]] <- g
        modifie <- TRUE
      }
    }

    # ── Etape 3 : connectivite des groupes (point 5) ──────────────────────────
    # Pour chaque groupe, detecter les composantes connexes via le voisinage.
    # Les fragments non-principaux (< composante la plus grande) sont reassignes
    # au groupe voisin le plus representé parmi leurs voisins externes.
    df_post <- sf::st_drop_geometry(data)
    groupes_actifs <- unique(as.character(df_post[[new_var]]))

    for (g in groupes_actifs) {
      idx_g <- which(df_post[[new_var]] == g)
      if (length(idx_g) <= 1) next

      # Construire le graphe de voisinage interne au groupe
      edges <- do.call(rbind, lapply(idx_g, function(i) {
        vois <- unlist(nb[[i]])
        vois <- vois[!is.na(vois) & vois > 0 & vois %in% idx_g]
        if (length(vois) == 0) return(NULL)
        cbind(match(i, idx_g), match(vois, idx_g))
      }))

      if (is.null(edges) || nrow(edges) == 0) next

      gr     <- igraph::graph_from_edgelist(edges, directed = FALSE)
      comps  <- igraph::components(gr)
      if (comps$no <= 1) next  # groupe connexe, rien a faire

      # Identifier les fragments (toutes les composantes sauf la plus grande)
      comp_sizes  <- comps$csize
      largest_id  <- which.max(comp_sizes)
      fragment_ids <- setdiff(seq_len(comps$no), largest_id)

      for (frag_id in fragment_ids) {
        frag_vertices <- which(comps$membership == frag_id)
        frag_idx      <- idx_g[frag_vertices]  # indices globaux

        # Pour chaque commune du fragment, trouver ses voisins hors du groupe
        voisins_ext <- unique(unlist(lapply(frag_idx, function(i) {
          vois <- unlist(nb[[i]])
          vois <- vois[!is.na(vois) & vois > 0 & !(vois %in% idx_g)]
          df_post[[new_var]][vois]
        })))
        voisins_ext <- voisins_ext[!is.na(voisins_ext)]

        if (length(voisins_ext) > 0) {
          # Reassigner le fragment au groupe externe le plus representé
          nouveau_groupe <- names(sort(table(voisins_ext), decreasing = TRUE))[1]
          data[[new_var]][frag_idx]    <- nouveau_groupe
          df_post[[new_var]][frag_idx] <- nouveau_groupe
          modifie <- TRUE
        }
      }
    }

    if (!modifie) {
      if (verbose) message("  Post-traitement stable apres ", pass, " passe(s)")
      break
    }
  }

  # ── Coherence finale ─────────────────────────────────────────────────────────
  cohesion_finale <- if (!is.null(cohesion_tol)) {
    compute_cohesion(data, new_var, vars_attr)
  } else compute_cohesion(data, new_var, vars_attr)

  cohesion_init <- compute_cohesion(data, group_var, vars_attr)

  # ── candidate = unites effectivement reclassees ───────────────────────────
  data <- data %>%
    dplyr::mutate(
      candidate = as.character(.data[[group_var]]) != as.character(.data[[new_var]])
    )

  if (verbose) {
    n_total         <- sum(data$candidate, na.rm = TRUE)
    groupes_finaux_ok <- unique(as.character(sf::st_drop_geometry(data)[[new_var]]))
    message("Termine : ", n_total, "/", nrow(data), " unites reclassees")
    message("Groupes : ", length(groupes_originaux), " (initial) -> ",
            length(groupes_finaux_ok), " (final)")
    message("Coherence (eta²) : ", round(cohesion_init, 4),
            " (initial) -> ", round(cohesion_finale, 4),
            " (final, gain = ", round(cohesion_finale - cohesion_init, 4), ")")
  }

  return(data)
}
