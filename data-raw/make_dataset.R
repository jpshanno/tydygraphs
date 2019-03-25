# All values except gauge water level is mm over the lake surface
library(dplyr)

superior_basin_km2 <-
  127700

superior_lake_m2 <-
  82100 * 10000

file_wl <-
  tempfile()

file_p <-
  tempfile()

file_e <-
  tempfile()

file_flow_in <-
  tempfile()

file_out <-
  tempfile()

water_level <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/levels/mGauge/superiorMog.csv",
                destfile = file_wl)

precip <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/precip/superiorPrecipLakemmMo.csv",
                destfile = file_p)

evap <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/evap/superiorEvapmmMo.csv",
                destfile = file_e)

outflow <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/flows/maryFromSuperiorIntoMiHuron.csv",
                destfile = file_out)

inflow <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/runoff/superiorRunoffmmMo.csv",
                destfile = file_flow_in)

water_levels <-
  readr::read_csv(file_wl,
                  skip = 2) %>%
  rename(measurement_date = Date,
         water_level_m = Average) %>%
  mutate(measurement_date = lubridate::mdy(measurement_date))


precip <-
  readr::read_csv(file_p,
                  skip = 3) %>%
  select(-Total) %>%
  tidyr::gather("month",
                "precip_mm",
                -Year) %>%
  arrange(Year) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", Year))) %>%
  select(measurement_date,
         precip_mm)

evap <-
  readr::read_csv(file_e,
                  skip = 3) %>%
  select(-Ann) %>%
  tidyr::gather("month",
                "evaporation_mm",
                -Year) %>%
  arrange(Year) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", Year))) %>%
  select(measurement_date,
         evaporation_mm)

outflow <-
  readr::read_csv(file_out,
                  skip = 4) %>%
    tidyr::gather("month",
                  "discharge_m3_sec",
                  -Year) %>%
    mutate(measurement_date = lubridate::mdy(paste(month, "01", Year)),
           discharge_m3_month = discharge_m3_sec * lubridate::days_in_month(measurement_date) * 86400,
           discharge_mm = discharge_m3_month / superior_lake_m2) %>%
    select(measurement_date,
           discharge_mm)

inflow <-
  readr::read_csv(file_flow_in,
                  skip = 4) %>%
    select(-Ann) %>%
    tidyr::gather("month",
                   "runoff_mm",
                   -YEAR) %>%
    mutate(measurement_date = lubridate::mdy(paste(month, "01", YEAR))) %>%
    select(measurement_date,
           runoff_mm) %>%
    filter(!is.na(runoff_mm))

superior_data <-
  purrr::reduce(list(water_levels, precip, evap, outflow, inflow),
                left_join,
                by = "measurement_date") %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(lake = "Superior") %>%
  select(lake,
         everything())

month_counts <-
  superior_data %>%
  group_by(lubridate::year(measurement_date)) %>%
  summarize(n = n()) %>%
  distinct(n)

if(nrow(month_counts) != 1){
    stop("Some years are missing months")
  }


# mi_huron ----------------------------------------------------------------

# All values except gauge water level is mm over the lake surface

mi_huron_lake_m2 <-
  134100 * 10000 + 118000 * 10000

mih_file_wl <-
  tempfile()

mih_file_p <-
  tempfile()

mih_file_e <-
  tempfile()

mih_file_flow_in <-
  tempfile()

mih_file_out <-
  tempfile()

mih_water_level <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/levels/mGauge/miHuronMog.csv",
                destfile = mih_file_wl)

mih_precip <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/precip/miHuronPrecipLakemmMo.csv",
                destfile = mih_file_p)

mih_evap <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/evap/miHuronEvapmmMo.csv",
                destfile = mih_file_e)

mih_outflow <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/flows/clairFromMiHuronIntoClair.csv",
                destfile = mih_file_out)

mih_inflow <-
  download.file(url = "https://www.glerl.noaa.gov/data/dashboard/data/hydroIO/runoff/miHuronRunoffmmMo.csv",
                destfile = mih_file_flow_in)

mih_water_levels <-
  readr::read_csv(mih_file_wl,
                  skip = 2) %>%
  rename(measurement_date = Date,
         water_level_m = Average) %>%
  mutate(measurement_date = lubridate::mdy(measurement_date))


mih_precip <-
  readr::read_csv(mih_file_p,
                  skip = 3) %>%
  select(-Total) %>%
  tidyr::gather("month",
                "precip_mm",
                -Year) %>%
  arrange(Year) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", Year))) %>%
  select(measurement_date,
         precip_mm)

mih_evap <-
  readr::read_csv(mih_file_e,
                  skip = 3) %>%
  select(-Ann) %>%
  tidyr::gather("month",
                "evaporation_mm",
                -Year) %>%
  arrange(Year) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", Year))) %>%
  select(measurement_date,
         evaporation_mm)

mih_outflow <-
  readr::read_csv(mih_file_out,
                  skip = 4) %>%
  tidyr::gather("month",
                "discharge_m3_sec",
                -Year) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", Year)),
         discharge_m3_month = discharge_m3_sec * lubridate::days_in_month(measurement_date) * 86400,
         discharge_mm = discharge_m3_month / superior_lake_m2) %>%
  select(measurement_date,
         discharge_mm)

mih_inflow <-
  readr::read_csv(mih_file_flow_in,
                  skip = 4) %>%
  select(-Ann) %>%
  tidyr::gather("month",
                "runoff_mm",
                -YEAR) %>%
  mutate(measurement_date = lubridate::mdy(paste(month, "01", YEAR))) %>%
  select(measurement_date,
         runoff_mm) %>%
  filter(!is.na(runoff_mm))

mi_huron_data <-
  purrr::reduce(list(mih_water_levels, mih_precip, mih_evap, mih_outflow, mih_inflow),
                left_join,
                by = "measurement_date") %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(lake = "Michigan-Huron") %>%
  select(lake,
         everything())

month_counts <-
  mi_huron_data %>%
  group_by(lubridate::year(measurement_date)) %>%
  summarize(n = n()) %>%
  distinct(n)

if(nrow(month_counts) != 1){
  stop("Some years are missing months")
}

great_lakes_hydro <-
  bind_rows(superior_data,
            mi_huron_data)

usethis::use_data(great_lakes_hydro)
