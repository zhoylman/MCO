library(magrittr)
library(purrr)
c("httr",
  "readr",
  "units",
  "dplyr",
  "lubridate",
  "writexl",
  "readxl") %>%
  purrr::walk(require,
              character.only = TRUE)

stations <- 
  c("blm1arge",
    "blm3mcca",
    "blm2virg",
    "blm5kidd",
    "blm6glen",
    "blm7terr",
    "blmpumpk",
    "blmbroadu",
    "blmcapit",
    "blmplevn",
    "blmbattl",
    "blmhadri",
    "blmround",
    "blmwarre",
    "blmhavre",
    "lubrecht",
    "namupper") %>%
  magrittr::set_names(.,.)

raw_data <-
  stations %>%
  purrr::map_dfr(function(station){
    httr::GET("https://cfcmesonet-staging.cfc.umt.edu/api/observations",
              query = list(stations = 
                             station,
                           start_time = "2018-01-01",
                           public = FALSE,
                           elements =
                             c("radi_650",
                               "radi_810",
                               "irra_650",
                               "irra_810") %>%
                             stringr::str_c(collapse = ","),
                           simple_datetime = TRUE,
                           type = "csv")) %>%
      httr::content()
  })
  

ndvi <-
  raw_data %>%
  dplyr::mutate(`650 nm Irradiance (W·m⁻²·nm⁻¹)` = 
                  units::as_units(`650 nm Irradiance (W·m⁻²·nm⁻¹)`,"W*(m^-2)*(nm^-1)"),
                `810 nm Irradiance (W·m⁻²·nm⁻¹)` = 
                  units::as_units(`810 nm Irradiance (W·m⁻²·nm⁻¹)`,"W*(m^-2)*(nm^-1)"),
                `650 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)` = 
                  units::as_units(`650 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`,"W*(m^-2)*(nm^-1)*(sr^-1)"),
                `810 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)` = 
                  units::as_units(`810 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`,"W*(m^-2)*(nm^-1)*(sr^-1)")
  ) %>%
  dplyr::mutate(`α` = `650 nm Irradiance (W·m⁻²·nm⁻¹)` / `810 nm Irradiance (W·m⁻²·nm⁻¹)`,
                NDVI = 
                  ((`α` * `810 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`) - `650 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`) / 
                  ((`α` * `810 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`) + `650 nm Radiance (W·m⁻²·nm⁻¹·sr⁻¹)`)
  ) %>%
  dplyr::group_by(station_key) %>%
  dplyr::arrange(station_key, datetime) %>%
  dplyr::mutate(
    `NDVI` = units::drop_units(NDVI),
    `NDVI Change` = (`NDVI` - c(NA,`NDVI`[-length(`NDVI`)])) / c(NA,`NDVI`[-length(`NDVI`)]),
    `NDVI — Filtered` = ifelse(`NDVI Change` > 0.15 | `NDVI Change` < -0.15, NA, `NDVI`),
    `NDVI — Filtered` = ifelse(`NDVI — Filtered` > 1 | `NDVI — Filtered` <= 0.1, NA, `NDVI — Filtered`)
  )

ndvi_aggregated <-
  ndvi %>%
  dplyr::filter(hms::as_hms(datetime) >= lubridate::hm("09:30"),
                hms::as_hms(datetime) <= lubridate::hm("14:30")) %>%
  dplyr::group_by(Station = station_key,
                  Date = lubridate::as_date(datetime)) %>%
  dplyr::summarise(`NDVI — Aggregated` = mean(`NDVI — Filtered`, na.rm = TRUE)) %>%
  dplyr::ungroup()

list(`Daily NDVI` = ndvi_aggregated,
     `Raw Data` = ndvi) %>%
  writexl::write_xlsx("mesonet_ndvi.xlsx")

meter_data <- 
  readxl::read_excel("./ndvi.xlsx") %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::rename(`Excel NDVI` = NDVI,
                `Meter NDVI` = Meter)

cairo_pdf(filename = "blm3mcca_ndvi_compare.pdf")
ndvi_aggregated %>%
  dplyr::filter(Station == "blm3mcca") %>%
  dplyr::right_join(meter_data) %>%
  na.omit() %>%
  dplyr::rename(`Mesonet NDVI (R)` = `NDVI — Aggregated`) %>%
  dplyr::select(`Mesonet NDVI (R)`:`Meter NDVI`) %>%
  pairs()
dev.off()
