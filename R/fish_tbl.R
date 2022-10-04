#' Create fish encounter tables
#'
#' @param .data A dataframe containing fish encounter data
#' @param .reach_code A field containg the reach code of the encounter record
#' @param .nativity Visualize "native" or "nonnative" records
#'
#' @return A kable data visualization
#' @export
#'
#' @examples
fish_tbl <- function(.data, .reach_code, .nativity) {
    `%!in%` <- Negate(`%in%`)
    if (.nativity == "native") {
        .data |>
            dplyr::filter(cd_spp %in% vec_spp$nat &
                          cd_rch == .reach_code) |>
            dplyr::arrange(desc(n_fish)) |>
            dplyr::select(-dplyr::matches('^cd_'), -c(nm_rvr, nm_rch)) |>
            knitr::kable(align = "lcc",
                         col.names = c("Species", "Total Captured", "Length Range (mm)"))
    } else if (.nativity == "nonnative") {
        .data |>
            dplyr::filter(cd_spp %!in% vec_spp$nat &
                              cd_rch == .reach_code) |>
            dplyr::arrange(desc(n_fish)) |>
            dplyr::select(-dplyr::matches('^cd_'), -c(nm_rvr, nm_rch)) |>
            knitr::kable(align = "lcc",
                         col.names = c("Species", "Total Captured", "Length Range (mm)"))
    }
}
