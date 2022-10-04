#' Create hydrograph with sampling days
#'
#' @param .data A dataframe structured to spec
#' @param .gage_code The USGS gage code for the desired station
#' @param .param The data set of interest (either mean_discharge or mean_temperature)
#' @param .pts Visualize sampling events as points or lines
#'
#' @return A dataframe
#' @export
#'
#' @examples
hydro <- function(.data, .gage_code, .param, .pts = TRUE) {
    if (.pts == TRUE) {

        .data2 <- .data |>
            dplyr::filter(cd_station_usgs == .gage_code &
                              !is.na({{.param}}))

        ggplot2::ggplot(.data2) +
            ggplot2::geom_line(aes(x = dt_read, y = {{.param}}), lwd = 1) +
            ggplot2::geom_point(data = filter(.data2, !is.na(cd_study)),
                                aes(x = dt_read, y = {{.param}}),
                                pch = 21,
                                cex = 4,
                                fill = "dodgerblue",
                                alpha = .8) +
            ggplot2::labs(subtitle = paste("Sampling events noted with blue points"),
                          x = "",
                          y = "Discharge (cfs)",
                          caption = paste("Measured at USGS gage:",
                                          {{.gage_code}},
                                          "-",
                                          unique(.data2$nm_station)))
    } else {
        .data2 <- .data |>
            dplyr::filter(cd_station_usgs == .gage_code &
                              !is.na({{.param}}))

        ggplot2::ggplot(.data2) +
            ggplot2::geom_line(aes(x = dt_read, y = {{.param}}), lwd = 1) +
            ggplot2::geom_line(data = filter(.data2, !is.na(cd_study)),
                               aes(x = dt_read, y = {{.param}}, color = cat_pass),
                               pch = 21,
                               cex = 4,
                               fill = "dodgerblue",
                               alpha = .8) +
            ggplot2::labs(subtitle = paste("Sampling passes noted with multicolored line segments"),
                          x = "",
                          y = "Discharge (cfs)",
                          caption = paste("Measured at USGS gage:",
                                          {{.gage_code}},
                                          "-",
                                          unique(.data2$nm_station)))


    }
}
