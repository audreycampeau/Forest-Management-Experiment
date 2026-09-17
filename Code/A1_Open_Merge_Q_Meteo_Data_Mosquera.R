library(readxl)
library(tidyverse)

# Site naming system:
# DC1 = C58 ---> FC_DC1
# DC2 = C59 ---> FC1
# DC3 = C60 ---> FC_DC2
# DC4 -C57 ---> FC2

# Catchment areas 
#DC2_Area_m2=4.4*10000
#DC1_Area_m2=8.4*10000 
#DC3_Area_m2=8.4*10000
#DC4_Area_m2=10.7*10000


# Open Discharge data _____________________________________________________________________________
Q_Mosquera=read_xlsx("Input/Virginia Q data/DB.Q.Interpolated.Audrey.xlsx")
colnames(Q_Mosquera)[3]="q_int_mmd"


ggplot(data=Q_Mosquera, 
       aes (x=Date, y=q_int_mmd, colour = as.factor(Site)))+
  geom_point()


Q_DC2= Q_Mosquera %>% filter (Site == "59")
Q_DC2= Q_DC2 %>% select (c("Date","Q","q_int_mmd"))

Q_DC3= Q_Mosquera %>% filter (Site == "60")
Q_DC3= Q_DC3 %>% select (c("Date","Q","q_int_mmd"))

Q_C2= Q_Mosquera %>% filter (Site == "2")
Q_C2= Q_C2 %>% select (c("Date","Q","q_int_mmd"))



#Open Meteo Daily _________________________________________________________________________________
Meteo=read_xlsx("Input/Q and Meteo/Meteo_Daily.xlsx")
Meteo$Date=as.Date(Meteo$TimeStamp)

Meteo= Meteo %>% # Remove dates in 2019, and 2023
        filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2022-12-31"))
        


# Combine meteo data to Q_DC
DC2_Q_Meteo=left_join(Meteo, Q_DC2, by = 'Date', suffix = c( "_Meteo", ""))
DC4_Q_Meteo=DC2_Q_Meteo 

DC3_Q_Meteo=left_join(Meteo, Q_DC3, by = 'Date', suffix = c( "_Meteo", ""))
DC1_Q_Meteo=DC3_Q_Meteo 

C2_Q_Meteo=left_join(Meteo, Q_C2, by = 'Date', suffix = c( "_Meteo", ""))
C1_Q_Meteo=C2_Q_Meteo 


# Add a column to identify the site
DC2_Q_Meteo$Site_id=rep("DC2", nrow(DC4_Q_Meteo))
DC4_Q_Meteo$Site_id=rep("DC4", nrow(DC4_Q_Meteo))
DC3_Q_Meteo$Site_id=rep("DC3", nrow(DC3_Q_Meteo))
DC1_Q_Meteo$Site_id=rep("DC1", nrow(DC1_Q_Meteo))

C2_Q_Meteo$Site_id=rep("C2", nrow(C2_Q_Meteo))
C1_Q_Meteo$Site_id=rep("C1", nrow(C2_Q_Meteo))

#samplingdates= # 1. Define your target list of dates
samplingdates_C2C1 <- c(
  "2020-03-11",
  "2020-04-27",
  "2020-08-28",
  "2020-10-21",
  
  "2021-04-29",
  "2021-08-19",
  "2021-09-25",
  "2021-10-24",
  
  "2022-05-03",
  "2022-08-23",
  "2022-10-24"
)

# 2. Filter the dataframe
filtered_data <- Q_C2 %>% 
  filter(as.character(as.Date(Date)) %in% samplingdates_C2C1)


write.csv(filtered_data, "Output/Data/C2C1_meteoQ_data.csv")


# Timeseries of Q in all sites _______________________________________________________________________
ggplot(data=rbind(DC2_Q_Meteo, DC3_Q_Meteo, C2_Q_Meteo), aes(x=as.Date(Date), y=q_int_mmd, color=Site_id))+
  geom_point()+
  scale_x_date(limits= c(as.Date("2020-01-01"), as.Date("2022-12-31")))+
  labs(x="Date", y="q (mm/d)")

#View(rbind(DC2_Q_Meteo, DC3_Q_Meteo, C2_Q_Meteo))

library(ggcorrplot)

DC_q <- Q_Mosquera %>%
  select(Date, Site, q_int_mmd) %>%
  pivot_wider(names_from = Site, 
              values_from = q_int_mmd,
              names_prefix = "")

cor_matrix_q <- cor(DC_q[1:nrow(DC_q),c(2:4)], 
                    method = "pearson",use = "pairwise.complete.obs")


cor_matrix_q
ggcorrplot(cor_matrix_q, method="circle", type="upper", lab=T,insig = "blank",outline.col = "white")


