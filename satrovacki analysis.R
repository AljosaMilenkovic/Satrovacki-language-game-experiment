library(ordinal)
library(emmeans)

satro_data <- read.csv("nouns_experiment.csv", header = TRUE) #imports the clean data

#adds the Remorphologization column ("1" when remorphologization is possible, "0" when it isn't) 
#adds the Condotion column (like Strategy, but groups double marking and cropping under "other')
satro_data <- satro_data %>%
  mutate(Remorph = if_else((Noun == "lopta" | Noun == "noz"), "impossible", "possible")) %>%
  mutate(Condition = if_else((Strategy == "late inversion"), "late inversion",
                             if_else((Strategy == "early inversion"), "early inversion", "other")))


#mean scores by the availability of Remorphologization ("0" ns "1") and Ending shape (-V vs -VC)
satro_data %>%
  mutate(ending = if_else((Form == "ins.sg"), "-VC", "-V")) %>%
  group_by(Condition,Remorph,ending) %>%
  summarise(mean_rating = mean(as.numeric(rating)),
            stdev_rating = sd(as.numeric(rating)),
            count = n(),
            st_error_rating = stdev_rating/sqrt(count)) -> mean_scores_by_remorph_ending

#ordinal regression
#three levels within Condition (early inversion Baseline)
model_all_conditions <- clmm(as.factor(rating) ~ Condition*Remorph  + (1|participant), data = satro_data) #fits the model
summary(model_all_conditions) #model results
contrast(emmeans(model_all_conditions, ~ Condition*Remorph), "tukey") #pairwise comparisons

#filtering out cropping and double marking
satro_data_only_inversion <- satro_data %>%
  filter(!Condition == "other")
#inversion-only model: early inversion versus late inversion (early inversion Baseline)
model_inversion <- clmm(as.factor(rating) ~ Condition*Remorphologization  + (1|participant), data = satro_data_only_inversion) #fits the model
summary(model_inversion) #model results
contrast(emmeans(model_inversion, ~ Condition*Remorphologization), "tukey") #pairwise comparisons

library(ggplot2)
satro_data_only_inversion %>%
  ggplot(aes(y=age)) +
  geom_boxplot() -> plot_age

satro_data_only_inversion %>%
  mutate(younger = if_else((age <= 27), "younger", if_else((age <= 33), "middle", "older"))) -> satro_data_only_inversion

model_inversion_age1 <- clmm(as.factor(rating) ~ Condition*Remorphologization  + (1|participant) + (1|younger), data = satro_data_only_inversion) #fits the model
summary(model_inversion_age1)
anova(model_inversion,model_inversion_age1, "ChiSq")
AIC(model_inversion)
AIC(model_inversion_age1)

model_inversion_case <- clmm(as.factor(rating) ~ Condition*Form  + (1|participant), data = satro_data_only_inversion) #fits the model
summary(model_inversion_case)
