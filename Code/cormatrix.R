


library(ggcorrplot)
library(ggpubr)

# Correlation matrix for DOC concentration across sites over time
DC_wide_DOC <- DC_Q %>%
  select(Date, Site_id, DOC_mgL) %>%
  filter(!is.na(DOC_mgL)) %>%  # Remove NA values first
  group_by(Date, Site_id) %>%
  summarise(DOC_mgL = mean(DOC_mgL, na.rm = TRUE), .groups = 'drop') %>%  # Take mean if duplicates
  pivot_wider(names_from = Site_id, 
              values_from = DOC_mgL,
              names_prefix = "")

cor_matrix_DOC <- cor(DC_wide_DOC[, -1], 
                      method = "spearman", 
                      use = "pairwise.complete.obs")
print(round(cor_matrix_DOC, 1))



# Correlation matrix for CO2 concentration across sites over time
DC_wide_CO2 <- DC_Q %>%
  select(Date, Site_id, CO2_mgL) %>%
  filter(!is.na(CO2_mgL)) %>%  # Remove NA values first
  group_by(Date, Site_id) %>%
  summarise(CO2_mgL = mean(CO2_mgL, na.rm = TRUE), .groups = 'drop') %>%  # Take mean if duplicates
  pivot_wider(names_from = Site_id, 
              values_from = CO2_mgL,
              names_prefix = "")

cor_matrix_CO2 <- cor(DC_wide_CO2[, -1], 
                      method = "spearman", 
                      use = "pairwise.complete.obs")
print(round(cor_matrix_CO2, 1))




# Correlation matrix for 14CCO2 concentration across sites over time
DC_wide_CO2_14C <- DC_Q %>%
  select(Date, Site_id, CO2_14C_Modern) %>%
  filter(!is.na(CO2_14C_Modern)) %>%  # Remove NA values first
  group_by(Date, Site_id) %>%
  summarise(CO2_14C = mean(CO2_14C_Modern, na.rm = TRUE), .groups = 'drop') %>%  # Take mean if duplicates
  pivot_wider(names_from = Site_id, 
              values_from = CO2_14C,
              names_prefix = "")


cor_matrix_CO2_14C <- cor(DC_wide_CO2_14C[, -1], 
                      method = "spearman", 
                      use = "pairwise.complete.obs")
print(round(cor_matrix_CO2_14C, 1))





# Correlation matrix for 14C-DOC concentration across sites over time
DC_wide_DOC_14C <- DC_Q %>%
  select(Date, Site_id, DOC_14C_Modern) %>%
  filter(!is.na(DOC_14C_Modern)) %>%  # Remove NA values first
  group_by(Date, Site_id) %>%
  summarise(DOC_14C = mean(DOC_14C_Modern, na.rm = TRUE), .groups = 'drop') %>%  # Take mean if duplicates
  pivot_wider(names_from = Site_id, 
              values_from = DOC_14C,
              names_prefix = "")


cor_matrix_DOC_14C <- cor(DC_wide_DOC_14C[, -1], 
                      method = "spearman", 
                      use = "pairwise.complete.obs")

print(round(DC_wide_DOC_14C, 1))



#___________________________________________________________________________________

corplot_DOC=ggcorrplot(cor_matrix_DOC, method="square", type="upper", lab=T,insig = "blank",outline.col = "white", title = "[DOC]")
corplot_14DOC=ggcorrplot(cor_matrix_DOC_14C, method="square", type="upper", lab=T,insig = "blank",outline.col = "white", title = "14C-DOC")
corplot_CO2=ggcorrplot(cor_matrix_CO2, method="square", type="upper", lab=T,insig = "blank",outline.col = "white", title = "[CO2]")
corplot_14CO2=ggcorrplot(cor_matrix_CO2_14C, method="square", type="upper", lab=T,insig = "blank",outline.col = "white",title = "14C-CO2")


ggarrange(corplot_DOC,corplot_14DOC,
          corplot_CO2, corplot_14CO2,
          labels = "AUTO", nrow=2, ncol=2)

round(range(DC_Q$q_md*1000, na.rm=T), digits = 5)
round(range(DC_Q$DOC_14C_Modern, na.rm=T), digits =0)

