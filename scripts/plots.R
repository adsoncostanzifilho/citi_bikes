# PLOTS
require(ggplot2)
require(highcharter)
require(echarts4r)
require(leaflet)

# last year (only 2018)
db_last_year <- readRDS('data/db_last_year.rds')

db_last_year %>%
  ggplot(aes(y = log(tripduration))) +
  geom_boxplot()



# over time

db_over_time <- readRDS('data/db_over_time.rds')

# number of trips
db_over_time %>%
  filter(trip_date >= '2011-01-01') %>%
  e_charts(trip_date) %>% 
  e_line(n, legend = FALSE, name = "Number of Trips", color = "#509997") %>%
  e_lm(n ~ trip_date, name = "Linear Progression", color = "#f4d19e", legend = FALSE) %>%
  e_datazoom(type = "slider") %>%
  e_title("Number of Trips Over Time") %>%
  e_tooltip(trigger = "axis") 

# avg trip duration
db_over_time %>%
  filter(trip_date >= '2011-01-01') %>%
  e_charts(trip_date) %>% 
  e_line(tripduration_avg, legend = FALSE, name = "Average Trip Duration", color = "#761954") %>%
  e_lm(tripduration_avg ~ trip_date, name = "Linear Progression", color = "#f4d19e", legend = FALSE) %>%
  e_datazoom(type = "slider") %>%
  e_title("Average Trip Duration Over Time") %>%
  e_axis_labels(y = "Trip Duration (seconds)") %>%
  e_tooltip(trigger = "axis") 


# number of trips by hour
db_hour <- readRDS('data/db_hour.rds')

db_hour %>% 
  filter(!is.na(hour)) %>%
  e_charts(hour) %>%
  e_polar() %>%
  e_angle_axis(hour) %>% # angle = x
  e_radius_axis() %>% 
  e_bar(n, coord_system = "polar", legend = FALSE, name = "Number of Trips", color = "#509997") %>%
  e_title("Number of Trips by Hour") %>%
  e_tooltip(trigger = "axis") 


# number of trips/average trip duration by weekday 
db_last_year <- readRDS('data/db_last_year.rds')

db_week <- db_last_year %>% 
  group_by(week_day) %>%
  summarise(
    n = n(),
    tripduration_avg = mean(tripduration, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    week_day = factor(week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  ) %>%
  arrange(week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

  
saveRDS(db_week, 'data/db_week.rds')

db_week %>%
  e_charts(week_day) %>%
  e_bar(n, name = "Number of Trips", color = "#509997") %>%
  e_bar(tripduration_avg, name = "Average Trip Duration", color = "#761954", x_index = 1, y_index = 1) %>%
  e_grid(height = "35%") %>% 
  e_grid(height = "35%", top = "50%") %>% 
  e_y_axis(gridIndex = 1) %>% 
  e_x_axis(gridIndex = 1) %>%
  e_title("Trips by Weekday") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = 0)


# trips by gender
db_gender <- readRDS('data/db_gender.rds')

db_gender <- db_gender %>%
  filter(gender != "")

db_gender %>%
  e_charts(gender) %>%
  e_bar(n, name = "Number of Trips", color = "#509997") %>%
  e_bar(tripduration_avg, name = "Average Trip Duration", color = "#761954", x_index = 1, y_index = 1) %>%
  e_grid(height = "35%") %>% 
  e_grid(height = "35%", top = "50%") %>% 
  e_y_axis(gridIndex = 1) %>% 
  e_x_axis(gridIndex = 1) %>%
  e_title("Trips by Gender") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = 0)


# trips by user type
db_usertype <- readRDS('data/db_usertype.rds')

db_usertype <- db_usertype %>%
  filter(usertype != "")

db_usertype %>%
  e_charts(usertype) %>%
  e_bar(n, name = "Number of Trips", color = "#509997") %>%
  e_bar(tripduration_avg, name = "Average Trip Duration", color = "#761954", x_index = 1, y_index = 1) %>%
  e_grid(height = "35%") %>% 
  e_grid(height = "35%", top = "50%") %>% 
  e_y_axis(gridIndex = 1) %>% 
  e_x_axis(gridIndex = 1) %>%
  e_title("Trips by User Type") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = 0)


# trips by user type
db_age <- readRDS('data/db_age.rds')

db_age <- db_age %>%
  filter(!is.na(age))


db_age %>%
  filter(age <= 70) %>%
  mutate(tripduration_avg = round(tripduration_avg, 1)) %>%
  e_charts(age) %>% 
  e_scatter(n, legend = FALSE, color = "#761954", size = tripduration_avg) %>%
  e_title("Number of Trips by Age") %>%
  e_axis_labels(y = "Trip Duration (seconds)", x = "Age (years)") %>%
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        return(
        '<b>Age</b>: ' + params.value[0] + ' years old' +
        '<br /><b>Number of Trips</b>: ' + params.value[1] +
        '<br /><b>Average Trip Duration</b>: ' + params.value[2]
        )
      }
    ")) 

# trips popular routes
db_routes <- readRDS('data/db_routes.rds')

# create leaflet icon
bike_job_start <- makeIcon(
  iconUrl = "img/bike_job_start.png",
  iconWidth = 75, 
  iconHeight = 50
)

bike_job_end <- makeIcon(
  iconUrl = "img/bike_job_end.png",
  iconWidth = 74, 
  iconHeight = 50
)

bike_tour <- makeIcon(
  iconUrl = "img/bike_tour.png",
  iconWidth = 75, 
  iconHeight = 50
)



db_routes_tour <- db_routes[1:4,]

db_routes_urban <- db_routes[5,]


# create map
leaftlet_map <- leaflet(db_routes_tour) %>% 
  addTiles() %>%
  addMarkers(
    lat = ~db_routes_tour$start_station_latitude, 
    lng = ~db_routes_tour$start_station_longitude, 
    popup = 'Circle Touristic Routes',
    icon = bike_tour) %>%
  addMarkers(
    lat = ~db_routes_urban$start_station_latitude, 
    lng = ~db_routes_urban$start_station_longitude, 
    popup = 'Urban Routes Start',
    icon = bike_job_start) %>%
  addMarkers(
    lat = ~db_routes_urban$end_station_latitude, 
    lng = ~db_routes_urban$end_station_longitude, 
    popup = 'Urban Routes End',
    icon = bike_job_end) %>%
  addPolylines(
    lat = ~c(db_routes_urban$start_station_latitude,db_routes_urban$end_station_latitude),
    lng = ~c(db_routes_urban$start_station_longitude, db_routes_urban$end_station_longitude),
    stroke = TRUE,
    opacity = 1,
    weight = 5,
    color = 'black',
    dashArray =  '10, 10',
    popup = ~paste(round(db_routes_urban$tripduration_avg, 2), "seconds")
  ) 
leaftlet_map



