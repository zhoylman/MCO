library(magrittr)

blm3mcca_data <-
  httr::GET("https://cfcmesonet-staging.cfc.umt.edu/api/observations",
            query = list(stations="blm3mcca",
                         start_time="2020-01-01",
                         public=FALSE,
                         elements =
                           c("radi_650",
                             "radi_810",
                             "irra_650",
                             "irra_810",
                             "sol_radi") %>%
                           stringr::str_c(collapse = ","),
                         simple_datetime=TRUE,
                         type="csv")) %>%
  httr::content() 

blm3mcca_ndvi <-
  blm3mcca_data %>%
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
                ) %T>%
  writexl::write_xlsx("blm3mcca_ndvi.xlsx")


