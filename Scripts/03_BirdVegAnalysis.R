# Use this script to analyze bird-veg relationships

library(lme4)
library(lmerTest)
library(bild)
library(broom.mixed)
library(ggplot2)

shaffer_data_filtered$Used <- as.factor(shaffer_data_filtered$Used)


# Need to create a dataframe that is specific to PROW (and CARW)
# For example, Aquaculture - 6 is N for PROW, but Y for CARW


# Linear mixed model using % fledged as the response variable.
# Multiple hypotheses for what drives variation in % fledging can be tested.

m1 <- lm(Pct_Fledged ~ avg_BA_m2_pertree, data = PROW)
m1
summary(m1)


m2 <- lmer(Pct_Fledged ~ shrub_cover + (1|Species), data = shaffer_data_filtered)
m2
summary(m2)
a <- allFit(m2)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)

m3 <- lmer(Pct_Fledged ~ avg_BA_m2_pertree * shrub_cover + (1|Species), data = shaffer_data_filtered)
m3
summary(m3)
a <- allFit(m3)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)

m4 <- lmer(Pct_Fledged ~ (1|Species), data = shaffer_data_filtered)
m4
summary(m4)
a <- allFit(m4)
glance(a) |> select(optimizer, AIC, NLL_rel) |> arrange(NLL_rel)

# This isn't working
p <- ggplot(shaffer_data_filtered, aes(x = avg_BA_m2_pertree, y = Pct_Fledged, group = Species)) + 
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
