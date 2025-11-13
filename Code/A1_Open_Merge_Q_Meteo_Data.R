library(readxl)
library(tidyverse)

# Open Water balance, Discharge data


# Site naming system:
# DC3 = C60
# DC1 = C58
# DC2 = C59
# DC4 -C57


#Data Source: Johannes Tiwari (Sent by Alberto in April 2024)
#Q units are most likely L s-1
# Q units can't be L/s, they are too high!! 
Q_DC2=read.csv("Input/Q and Meteo/Q_C57_Daily.Time_Series_Data.2024031816461304.csv", skip=11)
#Q_DC4=read.csv("Input/Q and Meteo/Q_C57_Daily.Time_Series_Data.2024031816461304.csv", skip=11) # !!! Replace with DC2 data (analogue treatment types)

Q_DC3=read.csv("Input/Q and Meteo/Q_60_daily.Time_Series_Data.2024031816502188.csv", skip=11)
#Q_DC1=read.csv("Input/Q and Meteo/Q_60_daily.Time_Series_Data.2024031816502188.csv", skip=11) # !!! Replace with DC3 (analogue treatment types)

range (Q_DC2$Q, na.rm = T)

#Data Source: SITES data Portal (retreived in April 2024)
#Q units are L s1 (from Sites Data Portal)
        #Q_C2=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB-VAB-C2_20050406-20231116_daily.csv", skip=24)
        #Q_C4=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB_KKM-C4_20070529-20231113_L2_daily.csv", skip=23)
        #Q_C18=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB_DEG-C18_20120425-20231123_L2_daily.csv", skip=22)
        #Q_C1=read.csv("Input/Q and Meteo/Q_C01_Daily_PRELIMINARY.Time_Series_Data.2024031816562806.csv", skip=11)



#Format Dates
#Q_DC4$Date=as.Date(Q_DC4$TimeStamp)
Q_DC2$Date=as.Date(Q_DC2$TimeStamp)
Q_DC3$Date=as.Date(Q_DC3$TimeStamp)
#Q_DC1$Date=as.Date(Q_DC1$TimeStamp)

        #Q_C1$Date=as.Date(Q_C1$TimeStamp)
        #Q_C2$Date=as.Date(Q_C2$TIMESTAMP)
        #Q_C4$Date=as.Date(Q_C4$TIMESTAMP)
        #Q_C18$Date=as.Date(Q_C18$TIMESTAMP)


#Convert all Q measurements from L/s to mm3/d
## Remove all conversion and assume that Q data is already in m3/d (DC3 range from 0.0001 to 47, which makes sense)
#Q_DC4$Q_m3d=Q_DC4$Q #/1000*60*60*24
Q_DC2$Q_m3d=Q_DC2$Q /1000*60*60*24
Q_DC3$Q_m3d=Q_DC3$Q /1000*60*60*24
#Q_DC1$Q_m3d=Q_DC1$Q #/1000*60*60*24

        #Q_C2$Q_m3d=Q_C2$Q*60*60*24
        #Q_C4$Q_m3d=Q_C4$Q*60*60*24
        #Q_C18$Q_m3d=Q_C18$Q*60*60*24
        #Q_C1$Q_m3d=Q_C1$Q# /1000*60*60*24


#Catchment Area in m2 (originally in ha)
    #DC1_Area_m2=6.7*10000
DC2_Area_m2=4.4*10000
DC3_Area_m2=8.4*10000
#DC4_Area_m2=10.7*10000
#DC1_Area_m2=8.4*10000 # !!!!! Same at DC3

        #C2_Area_m2=12*10000
        #C1_Area_m2=48*10000
        #C4_Area_m2=18*10000
        #C18_Area_m2=650*10000


#Convert all Q measurements to Specific Discharge m/d
#Q_DC4$q_md=Q_DC4$Q_m3d/DC4_Area_m2
Q_DC2$q_md=Q_DC2$Q_m3d/DC2_Area_m2
Q_DC3$q_md=Q_DC3$Q_m3d/DC3_Area_m2
#Q_DC1$q_md=Q_DC1$Q_m3d/DC1_Area_m2

        #Q_C1$q_md=Q_C1$Q_m3d/C1_Area_m2
        #Q_C2$q_md=Q_C2$Q_m3d/C2_Area_m2
        #Q_C4$q_md=Q_C4$Q_m3d/C4_Area_m2
        #Q_C18$q_md=Q_C18$Q_m3d/C18_Area_m2




#Open Meteo Daily _________________________________________________________________________________
Meteo=read_xlsx("Input/Q and Meteo/Meteo_Daily.xlsx")
Meteo$Date=as.Date(Meteo$TimeStamp)

Meteo= Meteo %>% # Remove dates in 2019, and 2023
        filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2022-12-31"))
        



# Combine meteo data to Q_DC
DC2_Q_Meteo=left_join(Meteo, Q_DC2, by = 'Date', suffix = c( "_Meteo", "_Q"))
DC4_Q_Meteo=DC2_Q_Meteo  # Assume DC4 site Q is like DC2
DC3_Q_Meteo=left_join(Meteo, Q_DC3, by = 'Date', suffix = c( "_Meteo", "_Q"))
DC1_Q_Meteo=DC3_Q_Meteo # Assume DC1 site Q is like DC3


# Add a column to identify the site
DC4_Q_Meteo$Site_id=rep("DC4", nrow(DC4_Q_Meteo))
DC2_Q_Meteo$Site_id=rep("DC2", nrow(DC2_Q_Meteo))
DC3_Q_Meteo$Site_id=rep("DC3", nrow(DC3_Q_Meteo))
DC1_Q_Meteo$Site_id=rep("DC1", nrow(DC1_Q_Meteo))


          #DC_all_Q_Meteo=rbind(DC4_Q_Meteo, 
          #           DC2_Q_Meteo, 
          #          DC3_Q_Meteo,
          #           DC1_Q_Meteo)


# Timeseries of Q in all sites _______________________________________________________________________
ggplot(data=rbind(DC2_Q_Meteo, DC3_Q_Meteo), aes(x=Date, y=Q_m3d, color=Site_id))+
  geom_point()+
  #scale_color_manual(values=c(site_colors))+
  scale_x_date(limits= c(as.Date("2020-01-01"), as.Date("2022-12-31")))+
  labs(x="Date", y="q (m/d)")





# Fill NA's in Q data_________________________________________________________________________________
# Missing Q data in DC2 and DC4, early 2020 and late 2022
# DC3 Q measurements cover all 14C measurements 
# Since there is an acceptable correlation between DC2 and DC3 Q data, fill Na's in DC2 with DC3
        # plot(log(DC3_Q_Meteo$q_md), log(DC2_Q_Meteo$q_md))
        # abline(0,1)
        # cor.test(x=log(DC2_Q_Meteo$q_md), y=log(DC3_Q_Meteo$q_md), na.rm=T )

library(zoo)

DC4_Q_Meteo$q_md_filled= ifelse(is.na(DC2_Q_Meteo$q_md),DC3_Q_Meteo$q_md, DC2_Q_Meteo$q_md ) # When DC2_qmd is missing, use DC3_qmd
DC2_Q_Meteo$q_md_filled= ifelse(is.na(DC2_Q_Meteo$q_md),DC3_Q_Meteo$q_md, DC2_Q_Meteo$q_md ) # When DC2_qmd is missing, use DC3_qmd
DC3_Q_Meteo$q_md_filled= DC3_Q_Meteo$q_md
DC1_Q_Meteo$q_md_filled= DC1_Q_Meteo$q_md

