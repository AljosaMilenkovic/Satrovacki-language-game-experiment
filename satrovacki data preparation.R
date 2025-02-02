library(dplyr)
library(tidyr)

satro <- read.csv("satro.csv", header = TRUE) #import raw data

#delete unnecessary rows
satro <- satro[-2, ]
satro <- satro[-1, ]

#add participant id
satro$participant <- row_number(satro)

#replace ratings in Serbian with numbers
satro <- satro %>%
  mutate_all(~ ifelse(. == "skoro neprihvatljivo", 2, .))
satro <- satro %>%
  mutate_all(~ ifelse(. == "neprihvatljivo", 1, .))
satro <- satro %>%
  mutate_all(~ ifelse(. == "neutralno", 3, .))
satro <- satro %>%
  mutate_all(~ ifelse(. == "skoro prihvatljivo", 4, .))
satro <- satro %>%
  mutate_all(~ ifelse(. == "prihvatljivo", 5, .))

#reorganize the data such that all questions occur in a single column
satro <- satro %>%
  gather(key = "question", value = "rating", znati_baseline, Q536, Q537, Q608, Q609, Q610, Q578, Q579, Q580, Q611, Q612, Q613, Q581, Q582, Q583, Q590, Q591, Q592, Q593, Q594, Q595, Q599, Q600, Q601, Q596, Q597, Q598, Q602, Q603, Q604, Q605, Q606, Q607, Q614, Q615, Q616)
satro <- satro[, c(12,13,14,22,23,24)]

#filtering out questions that were not displayed
satro <- satro %>%
  filter(!rating == "")

#adding the Noun variable
satro <- satro %>%
  mutate(Noun = if_else((question == "znati_baseline" | question == "Q536" | question == "Q537" | question == "Q578" | question == "Q579" | question == "Q580"), "kafa", 
                        if_else((question == "Q608" | question == "Q609" | question == "Q610" | question == "Q611" | question == "Q612" | question == "Q613"), "glava",
                                if_else((question == "Q581" | question == "Q582" | question == "Q583" | question == "Q590" | question == "Q591" | question == "Q592"), "lopta",
                                        if_else((question == "Q593" | question == "Q594" | question == "Q595" | question == "Q596" | question == "Q597" | question == "Q598"), "debil",
                                                if_else((question == "Q599" | question == "Q600" | question == "Q601" | question == "Q602" | question == "Q603" | question == "Q604"), "seljak", "noz"))))))

#adding the Form variable
satro <- satro %>%
  mutate(Form = if_else((question == "znati_baseline" | question == "Q536" | question == "Q537" | question == "Q608" | question == "Q609" | question == "Q610"), "gen.sg", 
                        if_else((question == "Q578" | question == "Q579" | question == "Q580" | question == "Q611" | question == "Q612" | question == "Q613" | question == "Q590" | question == "Q591" | question == "Q592" | question == "Q593" | question == "Q594" | question == "Q595" | question == "Q599" | question == "Q600" | question == "Q601" | question == "Q605" | question == "Q606" | question == "Q607"), "ins.sg", "nom.pl")))



#adding the Strategy variable
satro <- satro %>%
  mutate(Strategy = if_else((question == "Q601" | question == "Q604"), "cropping", 
                        if_else((question == "Q537" | question == "Q610" | question == "Q580" | question == "Q613" | question == "Q583" | question == "Q592" | question == "Q595" | question == "Q598"), "double marking",
                                if_else((question == "znati_baseline" | question == "Q608" | question == "Q578" | question == "Q611" | question == "Q581" | question == "Q590" | question == "Q593" | question == "Q599" | question == "Q596" | question == "Q602" | question == "Q605" | question == "Q614"), "late inversion", "early inversion"))))


#remove the "question" variable (unnecessary)
satro <- satro[ ,-5]

#export the new csv file
write.csv(satro, "nouns_experiment.csv")