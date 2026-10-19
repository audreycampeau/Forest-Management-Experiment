


#______________________________________________________________________________
#Biological control over 14C-CO2 

ggplot( DC_Q,
        aes(y=CO2_14C_Modern, x=CO2_mgL_filled, 
            color=Treatment))+
  geom_point( aes(shape=Treatment))+
  geom_smooth(method="lm", se=F,show.legend = F)+
  
  scale_color_manual(values=site_colors_6)+ 
  scale_fill_manual(values=site_colors_6)+
  scale_x_log10()+
  
  #stat_regline_equation(
  #  label.y.npc = "top", label.x.npc = "left", #0.5
  #  aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), #, color = Site_id
  #  show.legend = F, size=4)+
  #stat_chull()+
  #stat_ellipse(aes(color=Treatment), size = 1.5, show.legend = F) +
  
  stat_cor(
    method = "spearman",
    label.y.npc = "top", label.x.npc = "left", cor.coef.name = "rho",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    r.accuracy = 0.01, p.accuracy = 0.01, show.legend = F
  ) +
  #labs(x="specific discharge (m/d)", y=bquote("DOC (mg C L"^-1*")"), shape="Watershed ID")+
  theme(legend.position = "right")




# Generalized Linear Mixed Models (GLMM) 
#library(lme4)
#glmer_14CCO2 <- glmer(CO2_14C_Modern ~ CO2_mgL_filled_keeling * Treatment + (1|Site_id), 
#                   , data = DC_Q)
#summary(glmer_14CCO2)
#Site random effect nearly zero (0.0057 vs 0.029 residual) - we can use regular GLM:
#glm_14CCO2 <- glm(CO2_14C_Modern ~ CO2_mgL_filled_keeling * Treatment, 
#                     , data = DC_Q)

#summary(glm_14CCO2)

#Anova(glm_14CCO2, type = "III")

# Get treatment differences
#library(emmeans)
#emmeans(glm_14CCO2, pairwise ~ Treatment)


# Results show that the age of CO2 is affected by treatment, while controling for Site_id, but the age of CO2 is still independant from the CO2 concentration.





#Run ANCOVA test
library(rstatix)
library(coin)
anova_test(data=DC_Q, CO2_14C_Modern ~ log(CO2_mgL_filled)+Treatment, effect.size = "ges")

anova_test(data=DC_Q, CO2_14C_Modern ~ log(CO2_mgL_filled)*Site_id)


library(lmPerm)

# Permutation-based ANCOVA
perm_result <- aovp(CO2_14C_Modern ~ log(CO2_mgL_filled) * Treatment, data = DC_Q)
summary(perm_result)



library(vegan)
permanova_result <- (CO2_14C_Modern ~ log(CO2_mgL_filled)*Site_id, data=DC_Q, permutations = 10)
#adonis2(dist_matrix ~ grouping_variable, 
#                          data = your_data_frame, permutations = 999)
print(permanova_result)



# Keeling plot :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
keeling= ggplot(data = DC_Q,
       aes(y = d13C_CO2, x =CO2_mgL_filled_keeling, shape=Treatment,
           color = Site_id, size=CO2_14C_Modern)) +
  
    geom_smooth(method="lm", se=F, show.legend = F)+ #aes(color=Site_id),
    stat_regline_equation(
    label.y.npc = "top", label.x.npc = "left", #0.5
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), #, color = Site_id
    show.legend = F, size=4)+#stat_chull()+
    geom_point() +
  scale_color_manual(values = site_colors_6) +
  scale_fill_manual(values = site_colors_6) +
  
  #facet_wrap(~Site_id) +
  theme_minimal() +
  theme(legend.position = "top") +
  
  
  labs(title = "Keeling plot of d13C-CO2 values") 



ggExtra::ggMarginal(keeling, 
                    type = "density", 
                    groupColour = TRUE, 
                    groupFill = TRUE,
                    alpha = 0.5)







# Biological effect on CO2 age ::::::::::::::::::::::::::::::::::::::::::::::::::::::

