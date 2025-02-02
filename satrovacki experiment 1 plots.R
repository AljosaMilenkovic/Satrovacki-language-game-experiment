library(ggplot2)
library(dplyr)

#mean scores by Noun, Form and Timing
satro_data %>%
  group_by(Noun, Form, Timing, Remorph) %>%
  summarise(mean_rating = mean(as.numeric(rating)),
            stdev_rating = sd(as.numeric(rating)),
            count = n(),
            st_error_rating = stdev_rating/sqrt(count)) -> mean_scores_by_noun_form_Timing

#barplot with mean scores for feminine nouns by Noun and Ending
mean_scores_by_noun_form_Timing %>%
  filter(!Noun == "debil") %>%
  filter(!Noun == "seljak") %>%
  filter(!Noun == "noz") %>%
  mutate(ending_shape = if_else((Form == "ins.sg"), "-VC", "-V")) %>%
  filter(!Timing == "other") %>%
  ggplot(aes(x = Timing, y = mean_rating, fill = Timing, , color = Remorph)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  geom_errorbar(aes(ymin = mean_rating - st_error_rating,
                    ymax = mean_rating + st_error_rating,
                    width = .2),
                position = position_dodge(width=.5)) +
  facet_grid(ending_shape~Noun, labeller = label_both, scales = "free") +
  scale_fill_grey(start = .4, end = .8) +
  coord_cartesian(ylim=c(1,5)) +
  labs(y = "mean acceptability rating") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  guides(fill = guide_legend(title = "Timing"),
         color = guide_legend(title = "Remorphologization", order = 1)) -> plot_means_feminine

ggsave("mean scores fem.png", plot_means_feminine, dpi=1200) #saves the plot

#mean ratings for masculine nouns
mean_scores_by_noun_form_Timing %>%
  filter(!Noun == "kafa") %>%
  filter(!Noun == "glava") %>%
  filter(!Noun == "lopta") -> mean_scores_by_noun_form_Timing_masculine

mean_scores_by_noun_form_Timing_masculine$Noun <- factor(mean_scores_by_noun_form_Timing_masculine$Noun,
                                                            levels = c("debil", "seljak", "noz"))

mean_scores_by_noun_form_Timing_masculine %>%
  mutate(ending_shape = if_else((Form == "ins.sg"), "-VC", "-V")) %>%
  filter(!Timing == "other") %>%
  ggplot(aes(x = Timing, y = mean_rating, fill = Timing, , color = Remorph)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  geom_errorbar(aes(ymin = mean_rating - st_error_rating,
                    ymax = mean_rating + st_error_rating,
                    width = .2),
                position = position_dodge(width=.5)) +
  facet_grid(ending_shape~Noun, labeller = label_both, scales = "free") +
  scale_fill_grey(start = .4, end = .8) +
  coord_cartesian(ylim=c(1,5)) +
  labs(y = "mean acceptability rating") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  guides(fill = guide_legend(title = "Timing"),
         color = guide_legend(title = "Remorphologization", order = 1)) -> plot_means_masculine

ggsave("mean scores masc.png", plot_means_masculine, dpi=1200) #saves the plot


###########stacked barplots that indicate percentage of participants per response
satro_data <- satro_data %>%
  mutate(Gender = if_else((Noun == "lopta" | Noun == "glava" | Noun == "kafa"), "feminine", "masculine"))

satro_data_feminine <- satro_data %>%
  filter(Gender == "feminine") %>%
  mutate(final_v = if_else((Noun == "kafa" | Noun == "glava"), "a", "o"))

#summary stats for feminine nouns
satro_data_feminine_stats <- satro_data_feminine %>%
  group_by(final_v,Strategy,rating) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

#score labels (for the plot)
score_labels <- c("5" = "Acceptable", "4" = "Almost Acceptable", "3" = "Neutral", "2" = "Almost Unacceptable", "1" = "Unacceptable")

#creates the bar plot
satro_data_feminine_stats %>%
  filter(!Strategy == "double marking") %>%
  ggplot(aes(x = factor(Strategy, levels = c("early inversion", "late inversion")), y = percentage, fill = factor(rating, levels = 5:1))) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~final_v, labeller = label_both) +
  labs(x = "Timing", y = "%Participants", fill = "Acceptability rating") +
  scale_fill_manual(values = c("darkgreen", "green", "yellow", "orange", "red"), labels = score_labels) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1),
        axis.title.x = element_text(size=12),
        strip.text = element_text(size = 14)) -> feminine_plot

ggsave("feminine.png", feminine_plot, dpi=1200)

#masculine nouns
satro_data_masculine <- satro_data %>%
  filter(!Gender == "feminine") %>%
  mutate(final_v = if_else((Noun == "seljak" | Noun == "debil"), "e", "o"))

satro_data_masculine_stats <- satro_data_masculine %>%
  filter(!Strategy == "double marking") %>%
  filter(!Strategy == "cropping") %>%
  group_by(final_v,Strategy,rating) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

#score labels (for the plot)
score_labels <- c("5" = "Acceptable", "4" = "Almost Acceptable", "3" = "Neutral", "2" = "Almost Unacceptable", "1" = "Unacceptable")

#creates the bar plot (no double marking/cropping)
satro_data_masculine_stats %>%
  ggplot(aes(x = factor(Strategy, levels = c("early inversion", "late inversion")), y = percentage, fill = factor(rating, levels = 5:1))) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~final_v, labeller = label_both) +
  labs(x = "Timing", y = "%Participants", fill = "Acceptability rating") +
  scale_fill_manual(values = c("darkgreen", "green", "yellow", "orange", "red"), labels = score_labels) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1),
        axis.title.x = element_text(size=12),
        strip.text = element_text(size = 14)) -> masculine_plot

ggsave("masculine plot.png", masculine_plot, dpi=1200)
