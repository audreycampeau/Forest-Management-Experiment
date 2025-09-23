
#Open and merge datasets
library(readxl)
library(lubridate)
library(tidyverse)



# Reopen database
#C14_wide_chemistry=read.csv("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.csv")
#read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.xlsx", sheet=2)
#colnames(C14_wide_chemistry)
#C14_wide_chemistry[,c(28:40)]=sapply(C14_wide_chemistry[,c(28:40)], FUN=as.numeric)

#C14_wide_chemistry$Date=as.Date(C14_wide_chemistry$Date)


#DC_database=filter(C14_wide_chemistry, Site_id %in% c("DC1","DC2","DC3","DC4"))




#_________________________________________________________________________________________________
# Open general water chemistry database (from second batch of data)

#Open General Chemistry database
#chemistry=read_xlsx("Input/Trollberget data - 2024-04-12.xlsx", sheet=1)
chemistry=read_xlsx("Input/Trollberget data - 2025-08-11.xlsx", sheet=2)
chemistry$Date=as.Date(chemistry$Date)
chemistry[,3:26]=sapply(chemistry[,3:26], as.numeric)


# Calculate excess deuterieum (Index of evaporation)
chemistry$dexcess= chemistry$d2H - (8*chemistry$d18O)




#_________________________________________________________________________________________________
# Open results from Marcus' DIC calculations (from Third batch of data)

DIC_MW=read_xlsx("Input/DC&C_Trollberget_GHG_data_AZ.xlsx", sheet=1)
DIC_MW$Date=as.Date(DIC_MW$Date)
DIC_MW=DIC_MW[,c(1:2, 4:8)] # Erase the last two columns that contained Marcus comment
DIC_MW$Date_MW=DIC_MW$Date #Copy Marcus Date
colnames(DIC_MW)=c("Site_id", "Date","pH_MW","WT_MW","DIC_mgL_MW","CO2_mgL_MW", "CH4_ugL_MW","Date_MW")




#_________________________________________________________________________________________________
# Join General chemistry with DIC_MW data
chemistry_DIC_MW= full_join(chemistry, 
                            DIC_MW, 
                              by=join_by("Site_id", 
                                         "Date"),
                                         #closest(Date>=Date)),
                              suffix = c(" ", " "))


chemistry_DIC_MW_cleaned=chemistry_DIC_MW[ -which(is.na(chemistry_DIC_MW$DOC_mgL)), ]
#Make a row ID column
chemistry_DIC_MW_cleaned$row_id=seq(1,nrow(chemistry_DIC_MW_cleaned),1)
#Use Marcus' data (_MW) for CO2 and DIC, they are more complete and accurate



# Check for complete duplicates across all columns
complete_duplicates = chemistry_DIC_MW_cleaned[duplicated(chemistry_DIC_MW_cleaned), ]

# Check for duplicates based on Site_id and Date only
key_duplicates = chemistry_DIC_MW_cleaned[duplicated(chemistry_DIC_MW_cleaned[, c("Site_id", "Date")]) | 
                                       duplicated(chemistry_DIC_MW_cleaned[, c("Site_id", "Date")], fromLast = TRUE), ]


# Inspect the duplicate rows
key_duplicates[, c("row_id", "Site_id", "Date", "DOC_mgL", "DIC_mgL", "CO2_mgL")]

row_to_delete = key_duplicates$row_id #get row id for all duplicated rows

row_to_delete=row_to_delete[seq(2, length(row_to_delete), 2)] #get row id for every second row



# Remove duplicates keeping first occurrence
chemistry_DIC_MW_cleaned <- chemistry_DIC_MW_cleaned[-row_to_delete, ]



#Export the chemistry data
write.csv(chemistry_DIC_MW_cleaned, "Output/Data/chemistry_DIC_MW_2024.06.03.csv")



# Combine Q_Meteo data for each DC site, with chemistry data ___________________________________________________________________________
DC_Q_Meteo=readRDS(file="Output/Data/DC_Q_Meteo.rds") # Open dataset saved in A2_CreateTreatmentVariable


DC4_Q_Meteo_chem=left_join(filter(DC_Q_Meteo, Site_id=="DC4"), 
                      filter(chemistry_DIC_MW_cleaned, Site_id == "DC4"),
                      by = 'Date', suffix = c( "", "_chem"))

DC2_Q_Meteo_chem=left_join(filter(DC_Q_Meteo, Site_id=="DC2"), 
                           filter(chemistry_DIC_MW_cleaned, Site_id == "DC2"),
                           by = 'Date', suffix = c( "", "_chem"))

DC3_Q_Meteo_chem=left_join(filter(DC_Q_Meteo, Site_id=="DC3"), 
                           filter(chemistry_DIC_MW_cleaned, Site_id == "DC3"),
                           by = 'Date', suffix = c( "", "_chem"))

DC1_Q_Meteo_chem=left_join(filter(DC_Q_Meteo, Site_id=="DC1"), 
                           filter(chemistry_DIC_MW_cleaned, Site_id == "DC1"),
                           by = 'Date', suffix = c( "", "_chem"))




# interpolate between water chemistry measurements to match 14C measurements 
library(zoo)

#Create a function to fill the NA's in CO2
fill_co2_data <- function(data) {
  data$CO2_mgL_filled <- na.fill( # Na interpolate between measurements 
                                        ifelse(is.na(data$CO2_mgL_MW), # use Alberto's data if Marcus's data is NA
                                        data$CO2_mgL, 
                                        data$CO2_mgL_MW),
                                 "extend")
  return(data)
}

DC1_Q_Meteo_chem <- fill_co2_data(DC1_Q_Meteo_chem)
DC2_Q_Meteo_chem <- fill_co2_data(DC2_Q_Meteo_chem)
DC3_Q_Meteo_chem <- fill_co2_data(DC3_Q_Meteo_chem)
DC4_Q_Meteo_chem <- fill_co2_data(DC4_Q_Meteo_chem)




fill_DIC_data <- function(data) {
  data$DIC_mgL_filled <- na.fill( # Na interpolate between measurements 
    ifelse(is.na(data$DIC_mgL_MW), # use Alberto's data if Marcus's data is NA
           data$DIC_mgL, 
           data$DIC_mgL_MW),
    "extend")
  return(data)
}

DC1_Q_Meteo_chem <- fill_DIC_data(DC1_Q_Meteo_chem)
DC2_Q_Meteo_chem <- fill_DIC_data(DC2_Q_Meteo_chem)
DC3_Q_Meteo_chem <- fill_DIC_data(DC3_Q_Meteo_chem)
DC4_Q_Meteo_chem <- fill_DIC_data(DC4_Q_Meteo_chem)


#Create a function to fill the NA's in Krycklan basic chemistry  monitoring data 
fill_multiple_columns <- function(data, columns) {
  for (col in columns) {
    # Create the new column name with "_filled" suffix
    new_col_name <- paste0(col, "_filled")
    # Apply na.fill to the column
    data[[new_col_name]] <- na.fill(data[[col]], "extend")
  }
  return(data)
}

columns_to_fill <- c("pH", "EC_uScm", "DOC_mgL", "NO2_NO3_ugL", "DN_mgL", "NH4_ugL",
                     "PO4_ugL", "SO4_ugL", "Mn_ugL", "Na_ugL", "Cl_ugL", "Ca_ugL",
                     "P_ugL", "Si_ugL", "Mg_ugL", "K_ugL", "Fe_ugL", "Hg_ugL",
                     "d18O", "d2H", "dexcess")

# Apply to each dataset
DC1_Q_Meteo_chem <- fill_multiple_columns(DC1_Q_Meteo_chem, columns_to_fill)
DC2_Q_Meteo_chem <- fill_multiple_columns(DC2_Q_Meteo_chem, columns_to_fill)
DC3_Q_Meteo_chem <- fill_multiple_columns(DC3_Q_Meteo_chem, columns_to_fill)
DC4_Q_Meteo_chem <- fill_multiple_columns(DC4_Q_Meteo_chem, columns_to_fill)




