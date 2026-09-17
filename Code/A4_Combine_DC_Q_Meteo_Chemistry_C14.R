



#________________________________________________________________________________________
#________________________________________________________________________________________
#________________________________________________________________________________________


#Open dataset
C14_wide <- read_xlsx("Input/C14_wide_data.xlsx")
C14_wide=C14_wide[,1:27] # Remove the chemistry data from this data base. They are already intergrated in the DC_Q_Meteo_chem dataframe


#________________________________________________________________________________________
# Join Q_Meteo and Chemistry Wide databases
#________________________________________________________________________________________

C2= full_join(C2_Q_Meteo_chem,
              filter(C14_wide, Site_id== "C2"), 
              by=join_by("Date"),
              suffix = c("", "_C14"))

C1= full_join(C1_Q_Meteo_chem,
              filter(C14_wide, Site_id == "C1"), 
              by=join_by("Date"),
              suffix = c("", "_C14"))


DC2= left_join(DC2_Q_Meteo_chem,
               filter(C14_wide, Site_id == "DC2"), # Only merge with the right DC site data
               by=join_by("Date"),
               suffix = c("", "_C14"))
 

DC3= left_join(DC3_Q_Meteo_chem,
               filter(C14_wide, Site_id == "DC3"), # Only merge with the right DC site data
               by=join_by("Date"),
               suffix = c("", "_C14"))


DC4= full_join(DC4_Q_Meteo_chem,
               filter(C14_wide, Site_id == "DC4"), # Only merge with the right DC site data
               by=join_by("Date"),
               suffix = c("", "_C14"))


DC1= full_join(DC1_Q_Meteo_chem,
               filter(C14_wide, Site_id == "DC1"), # Only merge with the right DC site data
               by=join_by("Date"),
               suffix = c("", "_C14"))

#C4= full_join(Q_C4[,3:5],
#              filter(C14_wide_chemistry_clean, Site_id == "C4"), 
#              by=join_by("Date"),
#              suffix = c(" ", " "))

#C18= full_join(Q_C18[,3:5],
#               filter(C14_wide_chemistry_clean, Site_id == "C18"), 
#               by=join_by("Date"),
#               suffix = c(" ", " "))


#Combine all DC data sets
DC_all=rbind(DC2,DC3,DC4,DC1,C1, C2)

         
         
         saveRDS(DC_all, "Output/Data/DC_Q_Meteo_chem_14C.rds")
write.csv(DC_all, "Output/Data/DC_Q_Meteo_chem_14C.csv")



#Export DC_Q_14C data in short format 
DC_Q_14C_short= DC_all %>% 
  filter(!is.na(DOC_14C_Modern))

write.csv(DC_Q_14C_short, "Output/Data/DC_Q_14C.csv")



# Add C1 and C2 data
#file.choose()
#library(readxl)
#C1_C2= read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/R/Input/C2_C1_Database.xlsx", sheet=1)
#C1_C2 = C1_C2[, -1]    

#colnames(C1_C2) =colnames(DC_all)

#DC_Cs_all= rbind(DC_all, C1_C2)
saveRDS(DC_all, "Output/Data/DC_Cs_Q_Meteo_chem_14C.rds")

