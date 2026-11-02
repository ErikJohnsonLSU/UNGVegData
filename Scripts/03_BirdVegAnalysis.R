# Use this script to analyze bird-veg relationships

library(lme4)
library(lmerTest)
library(bild)
library(broom.mixed)
library(ggplot2)
library(dplyr)
library(lme4)
shaffer_data_filtered$Used <- as.factor(shaffer_data_filtered$Used)


#####What Alex did---------------------------------------------------------------
####all the models run---------------------------------------------
library(lme4)
#Percent fledge binomial 
#dont want to divide by zero by accident
shaffer_data_PROW <- shaffer_data_PROW %>%
  mutate(
    shrub_inv_native_ratio =
      shrub_invasive_cover / (shrub_cover - shrub_invasive_cover + 0.01),
    
    herb_inv_native_ratio =
      herb_invasive_cover / (herb_cover - herb_invasive_cover + 0.01)
  )


#binomial chosen because it is not just sheer percent fledged
m_canopy <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    avg_canopy_cover +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

m_ba <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    avg_BA_m2.x +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

m_treeba <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    avg_BA_m2_pertree +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

m_shrub <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    shrub_cover +
    shrub_inv_native_ratio +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

m_herb <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    herb_cover +
    herb_inv_native_ratio +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

m_forest <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    avg_canopy_cover +
    avg_BA_m2.x +
    avg_BA_m2_pertree +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)


m_understory <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    shrub_cover +
    herb_cover +
    shrub_inv_native_ratio +
    herb_inv_native_ratio +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

AIC(
  m_canopy,
  m_ba,
  m_treeba,
  m_shrub,
  m_herb,
  m_forest,
  m_understory
)


#sample sizes are differing per model as (NAs) may be affecting this
#NOBS = number of observed observations
#How to fix?
#AIC cannot be compared as they are different data sets
nobs(m_canopy)
nobs(m_ba)
nobs(m_treeba)
nobs(m_shrub)
nobs(m_herb)
nobs(m_forest)
nobs(m_understory)

#Model selection things
library(MuMIn)

model.sel(
  m_canopy,
  m_ba,
  m_treeba,
  m_shrub,
  m_herb,
  m_forest,
  m_understory
)

#model coeffiecents put together
library(sjPlot)

plot_models(
  m_canopy,
  m_ba,
  m_treeba,
  m_shrub,
  m_herb,
  m_forest,
  m_understory,
  std.est = "std2"
)
#error bars cross 1 for all. no correlation between nest success across plots?
#there maybe severe coelinearity between these values
#####Plotting each model individually-------------------------

library(ggeffects)
library(ggplot2)

#plot_glmm_effect defined
plot_glmm_effect <- function(model, term, xlab){
  
  pred <- ggpredict(model, terms = term)
  
  ggplot(pred, aes(x = x, y = predicted * 100)) +
    
    geom_ribbon(
      aes(
        ymin = conf.low * 100,
        ymax = conf.high * 100
      ),
      alpha = 0.25,
      fill = "#FDB515"
    ) +
    
    geom_line(
      linewidth = 1.3,
      color = "#a41de6"
    ) +
    
    labs(
      x = xlab,
      y = "Predicted Percent Fledged"
    ) +
    
    theme_classic(base_size = 14) +
    
    theme(
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(
        fill = NA,
        color = "black"
      )
    )
}
#actual plot codes for each value
#plot_glmm_effect called when "m_canopy" is run
#ribbon plots. 95% confidence interval shown in the shade
plot_glmm_effect(
  m_canopy,
  "avg_canopy_cover",
  "Average Canopy Cover (%)"
)

plot_glmm_effect(
  m_ba,
  "avg_BA_m2.x",
  "Average Basal Area (m²)"
)

plot_glmm_effect(
  m_treeba,
  "avg_BA_m2_pertree",
  "Average Basal Area per Tree (m²)"
)

plot_glmm_effect(
  m_shrub,
  "shrub_cover",
  "Total Shrub Cover (%)"
)

plot_glmm_effect(
  m_shrub,
  "shrub_inv_native_ratio",
  "Invasive : Native Shrub Ratio"
)

plot_glmm_effect(
  m_herb,
  "herb_cover",
  "Total Herb Cover (%)"
)

plot_glmm_effect(
  m_herb,
  "herb_inv_native_ratio",
  "Invasive : Native Herb Ratio"
)


#actual nest data compared to model
#WHAT DID I DO?
prediction <- ggpredict(
  m_treeba,
  terms = "avg_BA_m2_pertree"
)

ggplot() +
  geom_jitter(
    data = shaffer_data_PROW,
    aes(
      x = "avg_BA_m2_pertree",
      y = Pct_Fledged
    ),
    alpha = 0.5,
    width = 0.5,
    height = 0.02
  ) +
  
  geom_ribbon(
    data = prediction,
    aes(
      x = x,
      ymin = conf.low * 100,
      ymax = conf.high * 100
    ),
    fill = "#FFECB3",
    alpha = 0.25
  ) +
  
  geom_line(
    data = prediction,
    aes(
      x = x,
      y = predicted * 100
    ),
    color = "#a41De6",
    linewidth = 1.5
  ) +
  
  labs(
    x = "Avg BA per Tree / hectare",
    y = "Percent Fledged"
  ) +
  
  theme_classic(base_size = 14)


#best-supported predictors and final model----------------------------------















# Linear mixed model using % fledged as the response variable.
# Multiple hypotheses for what drives variation in % fledging can be tested.
library(lme4)
#PCA values code
#family = binomial : response is a proportion based on successes and failures
#right may to do it?
m_pca <- glmer(
  cbind(N_fledged, N_eggs - N_fledged) ~
    PC1 +
    PC2 +
    (1 | BoxID),
  family = binomial,
  data = shaffer_data_PROW
)

#####Pct_fledged Models we did ----------------------------------------------------------
m1PFABAPT <- lm(Pct_Fledged ~ avg_BA_m2_pertree, data = shaffer_data_PROW)
m1PFABAPT
summary(m1PFABAPT)


m2PFBA <- lmer(Pct_Fledged ~ avg_BA_m2.x + (1|BoxID) , data = shaffer_data_PROW)
m2
summary(m2PFBA)
a <- allFit(m2PFBA)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m3PFBA <- lmer(Pct_Fledged ~ avg_BA_m2_pertree * avg_BA_m2.x + (1|BoxID), data = shaffer_data_PROW)
m3PFBA
summary(m3PFBA)
a2 <- allFit(m3PFBA)
glance(a2) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m4PFSCHC <- lmer(Pct_Fledged ~ shrub_cover * herb_cover + (1|BoxID), data = shaffer_data_PROW)
m4PFSCHC
summary(m4PFSCHC)
a3 <- allFit(m4PFSCHC)
glance(a3) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m5PFISHC <- lmer(Pct_Fledged ~ shrub_invasive_cover * herb_invasive_cover, data = shaffer_data_PROW)
m5PFISC
summary(m5PFISC)
a <- allFit(m5PFISC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)

m6PFISC <- lmer(Pct_Fledged ~ shrub_invasive_cover, data = shaffer_data_PROW)
m6PFISC
summary(m6PFISC)
a <- allFit(m6PFISC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)

m7PFIHC <- lmer(Pct_Fledged ~ herb_invasive_cover, data = shaffer_data_PROW)
m7PFISC
summary(m7PFIHC)
a <- allFit(m7PFIHC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)



mPFnull <- lmer(Pct_Fledged ~ #......, data = shaffer_data_filtered)
mPFnull
summary(mPFnull)
a <- allFit(mPFnull)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


#####Number Fledged Models ---------------------------------------------------

m1NFABAPT <- lm(N_fledged ~ avg_BA_m2_pertree, data = shaffer_data_PROW)
m1NFABAPT
summary(m1NFABAPT)


m2NFBA <- lmer(N_fledged ~ avg_BA_m2, data = shaffer_data_PROW)
m2NFBA
summary(m2NFBA)
a <- allFit(m2NFBA)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m3NFBA <- lmer(N_fledged ~ avg_BA_m2_pertree * avg_BA_m2, data = shaffer_data_PROW)
m3NFBA
summary(m3PFMA)
a <- allFit(m3PFMA)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m4NFSCHC <- lmer(N_fledged ~ shrub_cover * herb_cover, data = shaffer_data_PROW)
m4SCHC
summary(m4NFSCHC)
a <- allFit(m4NFSCHC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m5NFISHC <- lmer(N_fledged ~ shrub_invasive_cover * herb_invasive_cover, data = shaffer_data_PROW)
m5NFISC
summary(m5NFISC)
a <- allFit(m5NFISC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m6NFISC <- lmer(N_fledged ~ shrub_invasive_cover, data = shaffer_data_PROW)
m6NFISC
summary(m6NFISC)
a <- allFit(m6NFISC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)


m7NFIHC <- lmer(N_fledged ~ herb_invasive_cover, data = shaffer_data_PROW)
m7NFISC
summary(m7NFIHC)
a <- allFit(m7NFIHC)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)



mNFnull <- lmer(N_fledged ~ #......, data = shaffer_data_filtered)
mPFnull
summary(mPFnull)
a <- allFit(mPFnull)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)



# This isn't working
p <- ggplot(shaffer_data_PROW, aes(x = avg_BA_m2_pertree, y = Pct_Fledged, group = Species)) + 
  labs(x = "BA", y = "% Fledged")
p


# Logistic regression mixed model looking at the effects of habitat on box occupancy

m1 <- glmer(Used ~ avg_BA_m2_pertree + (1|Species), family = binomial(link='logit'), data = shaffer_data_filtered)
m1
summary(m1)


# Logistic regression fixed model looking at the effects of habitat on box occupancy

m1 <- glm(Used ~ avg_BA_m2_pertree, family = binomial(link='logit'), data = shaffer_data_filtered)
m1
summary(m1)
AIC(m1, m2, m3)


