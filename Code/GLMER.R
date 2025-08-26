

# Generalized linear mixed effect models and linear models 

# Generalized Linear Mixed Models (GLMM) 
library(lme4)
#Treatment is the random effet, while Site_id is the fixed effect

DC_Q$q_md_scaled = scale(DC_Q$q_md_filled)#Q is scaled following model recommendatino
lmer_14CDOC_q = glmer(DOC_14C_Modern ~ q_md_scaled * Treatment + (1|Site_id), 
                      family = Gamma(link = "log"), data = DC_Q)
summary(lmer_14CDOC_q)

#Site random effect nearly zero (0.0057 vs 0.029 residual) - we can use regular GLM:

#Generalized Linear model
glm_14CDOC_q <- glm(DOC_14C_Modern ~ q_md_filled * Treatment, 
                    data = DC_Q)

# Create a tidy summary table
glm_summary <- broom::tidy(lmer_14CDOC_q) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      p.value < 0.01 ~ "< 0.01", 
      p.value < 0.05 ~ "< 0.05",
      TRUE ~ as.character(round(p.value, 3))
    )
  )

knitr::kable(glm_summary,
             caption = "Summary table of GLM for 14C-DOC ~ q × treatment",
             col.names = c("Term", "Estimate", "Std. Error", "t-value", "p-value"))

#Anova(glm_14CCO2, type = "III")

# Get treatment differences
#library(emmeans)
#emmeans(glm_14CCO2, pairwise ~ Treatment)


# Results show that the age of CO2 is affected by treatment, while controling for Site_id, but the age of CO2 is still independant from the CO2 concentration.
