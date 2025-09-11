

#file.choose()
dic_data= read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/R/Input/CO2Calculation/Trollberget DIC.xlsx", sheet=2)
temp_data=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/R/Input/CO2Calculation/Water Temp DC2 and DC3 sensors.xlsx", sheet=1)



library(lubridate)

dic_data$Timestamp=as.POSIXct(dic_data$Timestamp)
dic_data$Date=as.Date(dic_data$FieldDate)
#dic_data$DOY <- doy_with_hours(dic_data$Timestamp)
View(dic_data)


temp_data$Timestamp=as.POSIXct(temp_data$Timestamp)
#temp_data$DOY <- doy_with_hours(temp_data$Timestamp)


# Prepare temperature reference data from 2021
temp_ref = temp_data %>%
  mutate(
    date = as.Date(Timestamp),
    month_day = format(date, "%m-%d"),
    wt = Average
  ) %>%
  select(month_day, wt)

# Create daily averages for temperature (in case multiple readings per day)
temp_ref = temp_ref %>%
  group_by(month_day) %>%
  summarise(wt = mean(wt, na.rm = TRUE), .groups = 'drop')


# Process DIC data
dic_merged = dic_data %>%
  mutate(
    date = as.Date(Date),
    month_day = format(date, "%m-%d")
  ) %>%
  left_join(temp_ref, by = "month_day")


View(dic_merged)


dic_merged$wt_filled=ifelse(is.na(dic_merged$wt.y), 2,dic_merged$wt.y)


write.csv(dic_merged, "trollberget_dic_with_temp.csv", row.names = FALSE)


