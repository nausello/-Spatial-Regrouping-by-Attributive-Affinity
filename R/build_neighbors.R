#' Construire la matrice de voisinage spatial
#' @param data Un objet sf
#' @param type "queen" ou "rook"
#' @return Un objet nb (spdep)
#' @export
build_neighbors <- function(data, type = "queen") {
  if (!inherits(data, "sf")) stop("'data' must be an sf object")
  nb <- if (type == "queen") {
    spdep::poly2nb(data, queen = TRUE)
  } else {
    spdep::poly2nb(data, queen = FALSE)
  }
  stopifnot(length(nb) == nrow(data))
  return(nb)
}
