## extract data from BIG QUERY
require(bigrquery)
require(DBI)
require(dplyr)
require(lubridate)


# connection
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "new_york_citibike",
  billing = "datasample-296913"
)

# list tables
dbListTables(con)

# general metrics
db_metrics <- tbl(con, 'citibike_trips') %>%
  summarise(
    n_trips = n(),
    min_date = min(starttime, na.rm = TRUE),
    max_date = max(starttime, na.rm = TRUE),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_metrics, 'data/db_metrics.rds')


# trips over time
db_over_time <- tbl(con, 'citibike_trips') %>%
  mutate(
    trip_date = as.Date(starttime)
  ) %>%
  group_by(trip_date) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_over_time, 'data/db_over_time.rds')


# data last year
db_last_year <- tbl(con, 'citibike_trips') %>%
  filter(starttime >= '2018-01-01') %>%
  mutate(
    starttime_year = year(starttime),
    age = starttime_year - birth_year 
  ) %>%
  select(starttime, birth_year, usertype, age, gender, tripduration) %>%
  collect() %>%
  mutate(
    week_day = weekdays(starttime)
  )

saveRDS(db_last_year, 'data/db_last_year.rds')


# trips by hour
db_hour <- tbl(con, 'citibike_trips') %>%
  mutate(
    hour = hour(starttime)
  ) %>%
  group_by(hour) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_hour, 'data/db_hour.rds')

lubridate::week()


# trips by week
db_hour <- tbl(con, 'citibike_trips') %>%
  mutate(
    week = week(starttime)
  ) %>%
  group_by(week) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_hour, 'data/db_hour.rds')


# trips by gender
db_gender <- tbl(con, 'citibike_trips') %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_gender, 'data/db_gender.rds')


# trips by gender
db_usertype <- tbl(con, 'citibike_trips') %>%
  group_by(usertype) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()

saveRDS(db_usertype, 'data/db_usertype.rds')


# trips popular routes
db_routes <- tbl(con, 'citibike_trips') %>%
  group_by(
    start_station_id, 
    start_station_name, 
    start_station_latitude, 
    start_station_longitude,
    end_station_id,
    end_station_name,
    end_station_latitude,
    end_station_longitude) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()


db_routes <- db_routes %>%
  filter(!is.na(start_station_id)) %>%
  arrange(desc(n)) 

db_routes_top10 <- db_routes[1:10,]

saveRDS(db_routes_top10, 'data/db_routes.rds')


# trips by age
db_age <- tbl(con, 'citibike_trips') %>%
  mutate(
    starttime_year = year(starttime),
    age = starttime_year - birth_year 
  ) %>%
  group_by(age) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE),
    tripduration_sd = sd(tripduration),
    tripduration_max = max(tripduration, na.rm = TRUE),
    tripduration_min = min(tripduration, na.rm = TRUE)
  ) %>%
  collect()
  
saveRDS(db_age, 'data/db_age.rds')



test <- tbl(con, 'citibike_trips') %>%
  mutate(
    starttime_year = year(starttime),
    age = starttime_year - birth_year 
  ) %>%
  filter(age > 100) %>%
  collect()






# disconnect
dbDisconnect(con)

