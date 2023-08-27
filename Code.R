#Libraries
library(mapview) 
library(sp)
library(tidyverse)


dataset_accident <- read_csv("C:/Users/tejas/OneDrive/Desktop/project - class 765/dft-road-casualty-statistics-accident-last-5-years.csv", col_types = cols(accident_index = col_character(), accident_year = col_integer(), location_easting_osgr = col_integer(), location_northing_osgr = col_integer(), police_force = col_integer(),accident_severity = col_character(), number_of_vehicles = col_integer(), number_of_casualties = col_integer(), date = col_date(format = "%d/%m/%Y"), day_of_week = col_integer(), time = col_time(format = "%H:%M"), first_road_class = col_integer(), first_road_number = col_character(), road_type = col_integer(), speed_limit = col_integer(), junction_detail = col_integer(), junction_control = col_integer(), second_road_class = col_integer(), second_road_number = col_integer(),pedestrian_crossing_human_control = col_integer(), pedestrian_crossing_physical_facilities = col_integer(), light_conditions = col_integer(), weather_conditions = col_integer(),road_surface_conditions = col_integer(), special_conditions_at_site = col_integer(), carriageway_hazards = col_integer(), urban_or_rural_area = col_integer(), did_police_officer_attend_scene_of_accident = col_integer(), trunk_road_flag = col_integer())) 

#removing irrelevant 
columns dataset_accident <- dataset_accident %>% select(-c(accident_reference, location_easting_osgr, location_northing_osgr, police_force, local_authority_district ,local_authority_ons_district, local_authority_highway, did_police_officer_attend_scene_of_accident, lsoa_of_accident_location)) 

#Replace null values with median for longitude and latitude 
dataset_accident <- dataset_accident %>% mutate(longitude = ifelse(is.na(longitude), median(longitude, na.rm = TRUE), longitude), latitude = ifelse(is.na(latitude), median(latitude, na.rm = TRUE), latitude)) 

#replacing -1 values 
cols_to_replace <- c("road_type","speed_limit","junction_detail", "junction_detail","second_road_number","pedestrian_crossing_human_control", "pedestrian_crossing_physical_facilities","light_conditions", "weather_conditions","road_surface_conditions","special_conditions_at_site", "carriageway_hazards","urban_or_rural_area","trunk_road_flag") 

#replace null values for speed limit 
mean_speed <- mean(dataset_accident$speed_limit, na.rm = TRUE) dataset_accident$speed_limit <- ifelse(is.na(dataset_accident$speed_limit), mean_speed, dataset_accident$speed_limit) 

for (col in cols_to_replace) 
{ 
  na_vals <- dataset_accident[[col]] == -1 
  if (sum(!is.na(na_vals) & na_vals) > 0) 
  { 
    col_mean <- mean(dataset_accident[[col]][!na_vals]) 
    dataset_accident[[col]][na_vals] <- col_mean 
  } 
}


dataset_casualty <- read_csv("C:/Users/tejas/OneDrive/Desktop/project - class 765/dft-road-casualty-statistics-casualty-last-5-years.csv", col_types = cols(accident_index = col_character(), accident_year = col_integer(), accident_reference = col_character(), vehicle_reference = col_integer(), casualty_reference = col_integer(),casualty_class = col_integer(), sex_of_casualty = col_integer(),age_of_casualty = col_integer(), age_band_of_casualty = col_integer(), casualty_severity = col_integer(), pedestrian_location = col_integer(), pedestrian_movement = col_integer(), car_passenger = col_integer(), bus_or_coach_passenger = col_integer(), pedestrian_road_maintenance_worker = col_integer(), casualty_type = col_integer(), casualty_home_area_type = col_integer(), casualty_imd_decile = col_integer())) 

#remove irrelevant columns 

dataset_casualty <- dataset_casualty %>% select(-c(accident_reference,accident_year)) 

#replacing -1 values 
cols_to_replace <- c("sex_of_casualty","age_of_casualty","age_band_of_casualty", "pedestrian_location","pedestrian_movement","car_passenger", "bus_or_coach_passenger","pedestrian_road_maintenance_worker", "casualty_imd_decile","casualty_home_area_type") 

for (col in cols_to_replace) 
{ 
  na_vals <- dataset_casualty[[col]] == -1 
  if (sum(!is.na(na_vals) & na_vals) > 0) 
    { 
       col_mean <- mean(dataset_casualty[[col]][!na_vals]) 
       dataset_casualty[[col]][na_vals] <- col_mean 
    } 
} 

dataset_vehicle <- read_csv("C:/Users/tejas/OneDrive/Desktop/project - class 765/dft-road-casualty-statistics-vehicle-last-5-years.csv", col_types = cols(accident_index = col_character(),accident_year = col_integer(), accident_reference = col_character(),vehicle_reference = col_integer(), vehicle_type = col_integer(), towing_and_articulation = col_integer(), vehicle_manoeuvre = col_integer(),vehicle_direction_from = col_integer(), vehicle_direction_to = col_integer(), vehicle_location_restricted_lane = col_integer(), junction_location = col_integer(), skidding_and_overturning = col_integer(), hit_object_in_carriageway = col_integer(),vehicle_leaving_carriageway = col_integer(), hit_object_off_carriageway = col_integer(), first_point_of_impact = col_integer(), vehicle_left_hand_drive = col_integer(), journey_purpose_of_driver = col_integer(), sex_of_driver = col_integer(), age_of_driver = col_integer(), age_band_of_driver = col_integer(),engine_capacity_cc = col_integer(), propulsion_code = col_integer(), age_of_vehicle = col_integer(), generic_make_model = col_character(),driver_imd_decile = col_integer(), driver_home_area_type = col_integer())) 

#remove irrelevant columns 
dataset_vehicle <- dataset_vehicle %>% select(-c(towing_and_articulation,accident_year,vehicle_reference)) 

#replacing -1 values 
cols_to_replace <- c("vehicle_type","vehicle_manoeuvre","vehicle_location_restricted_lane", "junction_location","skidding_and_overturning","hit_object_in_carriageway", "vehicle_leaving_carriageway","hit_object_off_carriageway", "first_point_of_impact","vehicle_left_hand_drive","journey_purpose_of_driver", "sex_of_driver","age_of_driver","age_band_of_driver","engine_capacity_cc", "propulsion_code","generic_make_model","driver_imd_decile","driver_home_area_type")

for (col in cols_to_replace) 
{ 
  na_vals <- dataset_casualty[[col]] == -1 
  if (sum(!is.na(na_vals) & na_vals) > 0) 
    { 
      col_mean <- median(dataset_casualty[[col]][!na_vals]) 
      dataset_casualty[[col]][na_vals] <- col_mean 
    } 
} 

#merging data 
data <- dataset_accident %>% left_join(dataset_casualty, by = "accident_index") %>% left_join(dataset_vehicle, by = "accident_index")

# Plot code 1 - proportion of -1 values
value_counts <- table(dataset_accident_before$second_road_number)
count_minus_one <- value_counts["-1"]
count_remaining <- sum(value_counts) - count_minus_one
pie(c(count_remaining, count_minus_one), labels = c("Values other than -1", "-1"), col = c("blue", "red"), main = "Proportion of -1 Values in Second Road Number")

#Plot code 2 - Impact of -1
ggplot(dataset_casualty_before, aes(x = casualty_imd_decile)) + geom_histogram(binwidth = 1, fill = "grey", color = "black") + geom_vline(xintercept = -1, linetype = "dashed", color = "red") + labs(x = "(IMD) Index of Multiple Deprivation", y = "Frequency", title = "Impact of -1 Values") + theme_bw()

#Plot 1
data_new <- data 
data_new$road_surface_conditions <- ifelse(data_new$road_surface_conditions == 1, 
                                           "Dry", ifelse(data_new$road_surface_conditions == 2, 
                                           "Wet or damp", ifelse(data_new$road_surface_conditions == 3, 
                                           "Snow", ifelse(data_new$road_surface_conditions == 4, 
                                           "Frost or Ice", ifelse(data_new$road_surface_conditions == 5, 
                                           "Flood over 3cm. deep", ifelse(data_new$road_surface_conditions == 6,
                                           "Oil or diesel", ifelse(data_new$road_surface_conditions == 7, 
                                           "Mud", "Unknown"))))))) 

category_counts <- table(data_new$road_surface_conditions) 

# Define custom colors 
custom_colors <- c("skyblue", "lightgreen", "lightpink", "orange", "yellow", "purple", "brown")

# Plot a pie chart with category labels on the side 
pie(category_counts, labels = "", main = "Road Surface Conditions", col = custom_colors) 
legend("right", legend = names(category_counts), fill = custom_colors, cex = 0.6)

#Plot 2
data_new$weather_conditions <- ifelse(data_new$weather_conditions == 1, 
                                      "Fine no high winds", ifelse(data_new$weather_conditions == 2, 
                                      "Raining no high winds", ifelse(data_new$weather_conditions == 3, 
                                      "Snowing no high winds", ifelse(data_new$weather_conditions == 4, 
                                      "Fine + high winds", ifelse(data_new$weather_conditions == 5, 
                                      "Raining + high winds",ifelse(data_new$weather_conditions == 6, 
                                      "Snowing + high winds", ifelse(data_new$weather_conditions == 7, 
                                      "Fog or mist", ifelse(data_new$weather_conditions == 8, 
                                      "Other","Unknown")))))))) 

category_counts <- table(data_new$weather_conditions) 

# Plot a bar graph 
barplot(category_counts, main = "Weather Conditions", xlab = "Conditions", ylab = "Frequency", col = "skyblue")

selected_cols <- c("accident_year", "road_type" , "speed_limit","second_road_class", 
                   "second_road_number" , "light_conditions","weather_conditions",
                   "road_surface_conditions", "special_conditions_at_site","carriageway_hazards", 
                   "urban_or_rural_area", "trunk_road_flag" , "pedestrian_location", "vehicle_type", 
                   "vehicle_manoeuvre" ,"skidding_and_overturning","age_of_driver", "sex_of_driver", 
                   "age_of_vehicle","driver_imd_decile") 

df_selected <- subset(data, select = selected_cols) 
grouped_data <- df_selected %>% group_by(accident_year, road_type , speed_limit, second_road_class, second_road_number , light_conditions, 
                                         weather_conditions, road_surface_conditions, special_conditions_at_site, carriageway_hazards, 
                                         urban_or_rural_area, trunk_road_flag , pedestrian_location, vehicle_type, vehicle_manoeuvre , 
                                         skidding_and_overturning, age_of_driver, sex_of_driver, age_of_vehicle, 
                                         driver_imd_decile) %>% summarise(number_of_accidents = n())

#sampling 
# Define the sample size
sample_size <- 3500 

# Calculate the sampling interval 
sampling_interval <- ceiling(nrow(grouped_data) / sample_size) 

# Randomly select a starting point
starting_point <- sample(1:sampling_interval, 1) 

# Perform systematic sampling 
sample_indices <- seq(starting_point, nrow(grouped_data), by = sampling_interval) 

# Extract the sample from the dataset 
sample_data <- grouped_data[sample_indices, ]

#Train and Test data
set.seed(123) # Set seed for reproducibility 
indices <- sample(1:nrow(sample_data), nrow(sample_data)*0.7) train_data <- sample_data[indices, ] test_data <- sample_data[-indices, ]

#Possion Reg
model <- glm(number_of_accidents ~ ., data = train_data, family = poisson) 

# Perform stepwise selection using AIC 
step_model <- stepAIC(model, direction = "both") 

# Print the summary of the selected model
summary(step_model) # Predict the values using the test data 

predictions <- predict(step_model, newdata = test_data, type = "response")

#Plot
old.par = par(mfrow = c(2, 2)) plot(model) par(old.par)

#second task plot 
# Calculate the count of accidents for each year 
accident_counts <- data %>% group_by(accident_year) %>% summarise(count = n()) 
# Create the line plot 
ggplot(accident_counts, aes(x = accident_year, y = count, group = 1)) + geom_line() + geom_point() + labs(x = "Accident Year", y = "Count of Accidents", title = "Line Plot of Accident Year vs Count of Accidents")

#task 3
sp_points <- SpatialPointsDataFrame(coords = locations[, c("longitude", "latitude")], data = locations, proj4string = CRS("+proj=longlat +datum=WGS84")) 
# Plot the points using mapview 
mapview(sp_points)