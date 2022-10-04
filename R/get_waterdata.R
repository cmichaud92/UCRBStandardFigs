#' Fetch and process waterdata for nnf standard visualizations
#'
#' @param .gage Field containing the usgs station codes
#' @param .year The year of interest
#'
#' @return A dataframe
#' @export
#'
#' @examples
get_water_yr <- function(.gage, .year) {

    gage <- UCRBDataTools::tbl_NWIS_gage |>
        dplyr::filter(cd_station_usgs %in% .gage) |>
        dplyr::mutate(across(nm_station, str_to_title))


    tmp_discharge <- dataRetrieval::readNWISuv(siteNumbers = gage$cd_station_usgs,
                                parameterCd = "00060",
                                startDate = paste0(.year, "-01-01"),
                                endDate = paste0(.year, "-12-31")) |>
        dplyr::select(2:4)
    names(tmp_discharge) <-  c("cd_station_usgs", "ts_read", "discharge")

    tmp_temperature <- dataRetrieval::readNWISuv(siteNumbers = gage$cd_station_usgs,
                                  parameterCd = "00010",
                                  startDate = paste0(.year, "-01-01"),
                                  endDate = paste0(.year, "-12-31")) |>
        dplyr::select(2:4)
    names(tmp_temperature) <-  c("cd_station_usgs", "ts_read", "temperature")

    water_data <- list()

    water_data$water <- dplyr::full_join(tmp_discharge, tmp_temperature, by = c("cd_station_usgs", "ts_read")) |>
        dplyr::left_join(select(gage, cd_rvr, nm_rvr, cd_station_usgs, nm_station,), by = "cd_station_usgs")

    water_data$sum_water_yr <- water_data$water |>
        dplyr::group_by(nm_rvr, nm_station) |>
        dplyr::summarise(across(c(discharge, temperature),
                         list(max = ~max(., na.rm = TRUE),
                              min = ~min(., na.rm = TRUE),
                              mean = ~round(mean(., na.rm = TRUE)))),
                         dplyr::across(c(discharge, temperature),
                         list(dt_max = ~format(ts_read[which.max(.)], '%B %d, %Y'),
                              dt_min = ~format(ts_read[which.min(.)], '%B %d, %Y'))),
                  .groups = "drop") %>%
        dplyr::select(nm_station, matches("^discharge"), matches("^temperature")) |>
        dplyr::select(-c(temperature_min, temperature_dt_min))

    water_data$sum_water_samp <- water_data$water |>
        dplyr::mutate(dt_read = lubridate::date(ts_read)) |>
        dplyr::group_by(nm_rvr, cd_rvr, cd_station_usgs, nm_station, dt_read) |>
        dplyr::summarise(mean_discharge = round(mean(discharge, na.rm = TRUE)),
                  mean_temperature = round(mean(temperature, na.rm = TRUE), 1),
                  .groups = "drop") |>
        dplyr::left_join(select(site, cd_study, cat_pass, date), by = c("dt_read" = "date"))

    return(water_data)
}
