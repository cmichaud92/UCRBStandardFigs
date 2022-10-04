#' Get associated USGS gage codes
#'
#' @param .data A dataframe containg siet information including reach codes and river miles
#' @param .reach_code Field containing reach codes
#' @param .river_mile Field containing river miles
#'
#' @return A dataframe
#' @export
#'
#' @examples
set_rch_gage <- function(.data, .reach_code, .river_mile) {

    .data |>
        dplyr::mutate(gage = dplyr::case_when(cd_rch %in% c("DESO", "LGR") ~  "09315000",
                                              cd_rch %in% c("MGR", "DNM") & rmi_mid <= 344.5 ~ "09261000",
                                              cd_rch %in% c("DNM", "UGR") & rmi_mid > 344.5 ~ "09234500",
                                              cd_rch %in% c("LYA", "MYA") ~ "09260050"))

}

