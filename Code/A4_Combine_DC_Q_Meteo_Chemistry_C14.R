



#________________________________________________________________________________________
#________________________________________________________________________________________
#________________________________________________________________________________________


#Open dataset
C14_wide <- read_xlsx("Input/C14_wide_data.xlsx")
C14_wide=C14_wide[,1:27] # Remove the chemistry data from this data base. They are already intergrated in the DC_Q_Meteo_chem dataframe


#________________________________________________________________________________________
# Join Q_Meteo and Chemistry Wide databases
#________________________________________________________________________________________


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




#Combine all DC data sets
DC_all=rbind(DC2,DC3,DC4,DC1)

         
         
saveRDS(DC_all, "Output/Data/DC_Q_Meteo_chem_14C.rds")
write.csv(DC_all, "Output/Data/DC_Q_Meteo_chem_14C.csv")



#Export DC_Q_14C data in short format 
#DC_Q_14C_short= DC_all %>% 
#  filter(!is.na(DOC_14C_Modern))

#write.csv(DC_Q_14C_short, "Output/Data/DC_Q_14C.csv")





