

# Here is the information provided by Alberto on April 15th 2024 
  #concerning the dates of clearcut and ditch cleaning for different sites

        #Here are the periods of time when the operations occurred:
        #DC sites – clear cuts – depends on the catchment, 
        # Start 20 July 2020 - End 24 August 2020 (but wood was stacked on the road, so some trucks were visiting the site for at least one more month;
        #(+ Site preparation - July 6, 2021 (maybe one day before/after))
        #DC sites – ditch cleaning - 9:00am on the 20th of September, 2021, typically worked from 8:00-17:00 until the 22nd or 23rd.  
                                                                                                     
        #Given this, we started counting the treatments as when they were completed. Here are the reference dates:
        #DC – clearcut – 25 August 2020
        #DC ditch cleaning – 23 September 2021 (but the “during” effects of this could be big..)
                                                                                             

#Make a treatment categorical variable
library(dplyr)


#predisturbance_start =  as.Date("2020-01-01") 
#predisturbance_end = as.Date("2020-08-24")

#clearcut_start = as.Date("2020-07-01")
#clearcut_end <- as.Date("2020-08-25")

#postharvest_start = as.Date("2020-08-25")
#postharvest_end= as.Date("2021-09-22")

#ditch_cleaning_start <- as.Date("2021-09-01")
#ditch_cleaning_end <- as.Date("2021-09-30")

#postditch_start = as.Date("2021-09-23")
#postditch_end= as.Date("2022-11-01") 



#Create a new variable called Treatment
#range of dates "2018-12-18" "2023-10-25"

DC1_Q_Meteo=DC1_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "PreDisturbance",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "PostHarvest",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "PostDrainage"
  ))

DC3_Q_Meteo=DC3_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "PreDisturbance",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "PostHarvest",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "PostDrainage"
  ))


DC2_Q_Meteo=DC2_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "PreDisturbance",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "PostHarvest",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "2yrPostHarvest"
  ))

DC4_Q_Meteo =DC4_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "PreDisturbance",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "PostHarvest",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "2yrPostHarvest"
  ))

C2_Q_Meteo =C2_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "Reference",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "Reference",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "Reference"
  ))

C1_Q_Meteo =C1_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-07-14")) ~ "Reference",
    between(Date, as.Date("2020-07-15"), as.Date("2021-09-13")) ~ "Reference",
    between(Date, as.Date("2021-09-14"), as.Date("2023-10-25")) ~ "Reference"
  ))


# Combine the four DC_Q_Meteo data frames
DC_Q_Meteo=rbind(DC1_Q_Meteo, 
                 DC2_Q_Meteo, 
                 DC3_Q_Meteo, 
                 DC4_Q_Meteo,
                 C2_Q_Meteo,
                 C1_Q_Meteo)

# Format to factor Site_id and Treatment
DC_Q_Meteo$Site_id=as.factor(DC_Q_Meteo$Site_id)
DC_Q_Meteo$Treatment=as.factor(DC_Q_Meteo$Treatment)


#Order factors
DC_Q_Meteo$Treatment <- factor(DC_Q_Meteo$Treatment, #reorder the treatment types so they are in the right sequence
                         levels = c("Reference", "PreDisturbance", "PostHarvest", "PostDrainage", "2yrPostHarvest"))



#Save DC_Q_Meteo dataset to output folder
saveRDS(DC_Q_Meteo, "Output/Data/DC_Q_Meteo.rds")

ggplot(DC_Q_Meteo, aes(x=Date, y=q_int_mmd, color=Treatment))+
  geom_point()+
  facet_wrap(~Site_id)

