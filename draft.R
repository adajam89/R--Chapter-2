exp3_grey__anova__PROP.REC.FA_data <- exp3_grey_data %>%
  select(id, resp_option, prop_word_FA_rec_total, prop_drawing_FA_rec_total, prop_photo_FA_rec_total) %>%
  rename(word = "prop_word_FA_rec_total", 
         drawing = "prop_drawing_FA_rec_total", 
         photo = "prop_photo_FA_rec_total") %>%
  gather(key = "stim_format", value = "PROP.REC.FA", word, drawing, photo) %>%
  mutate_at(vars("id", "resp_option", "stim_format"), factor) %>%
  mutate(stim_format = str_replace(stim_format, "word", "Words")) %>%
  mutate(stim_format = str_replace(stim_format, "drawing", "Drawings")) %>%
  mutate(stim_format = str_replace(stim_format, "photo", "Photographs")) %>%
  transform(stim_format = factor(stim_format, levels=c("Words","Drawings","Photographs"))) %>%
  transform(resp_option = factor(resp_option, levels=c("RFG","RFBG")))
#---------------------------------------------------------------------------------------------------------------#
##  Run 2-way mixed ANOVA:
exp3_grey__anova__PROP.REC.FA <- aov_ez("id", "PROP.REC.FA", exp3_grey__anova__PROP.REC.FA_data,
                                   between = "resp_option", within = "stim_format")

##  Format ANOVA main effects for use in report:  
exp3_apa__grey__anova__PROP.REC.FA <- papaja:::apa_print.afex_aov(exp3_grey__anova__PROP.REC.FA) 
exp3_apa__grey__anova__PROP.REC.FA__stim.format.main.effect <-
  exp3_apa__grey__anova__PROP.REC.FA$full$stim_format %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.REC.FA__resp.option.main.effect <-
  exp3_apa__grey__anova__PROP.REC.FA$full$resp_option %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.REC.FA__interaction.main.effect <-
  exp3_apa__grey__anova__PROP.REC.FA$full$resp_option_stim_format %>% 
  str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
#---------------------------------------------------------------------------------------------------------------#
##  Run post-hoc contrasts:
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Response-option:  Not significant.
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Stimuli format:  Not significant.
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Interaction:  Not significant.

#################################################################################################################
##    PROP FAM FA:                                                                                             ##
#################################################################################################################
## Prep data for analysis:                                               
exp3_grey__anova__PROP.FAM.FA_data <- exp3_grey_data %>%
  select(id, resp_option, prop_word_FA_fam_total, prop_drawing_FA_fam_total, prop_photo_FA_fam_total) %>%
  rename(word = "prop_word_FA_fam_total",
         drawing = "prop_drawing_FA_fam_total",
         photo = "prop_photo_FA_fam_total") %>%
  gather(key = "stim_format", value = "PROP.FAM.FA", word, drawing, photo) %>%
  mutate_at(vars("id", "resp_option", "stim_format"), factor) %>%
  mutate(stim_format = str_replace(stim_format, "word", "Words")) %>%
  mutate(stim_format = str_replace(stim_format, "drawing", "Drawings")) %>%
  mutate(stim_format = str_replace(stim_format, "photo", "Photographs")) %>%
  transform(stim_format = factor(stim_format, levels=c("Words","Drawings","Photographs"))) %>%
  transform(resp_option = factor(resp_option, levels=c("RFG","RFBG")))
#---------------------------------------------------------------------------------------------------------------#
##  Run 2-way mixed ANOVA:
exp3_grey__anova__PROP.FAM.FA <- aov_ez("id", "PROP.FAM.FA", exp3_grey__anova__PROP.FAM.FA_data,
                                   between = "resp_option", within = "stim_format")

##  Format ANOVA main effects for use in report:  
exp3_apa__grey__anova__PROP.FAM.FA <- papaja:::apa_print.afex_aov(exp3_grey__anova__PROP.FAM.FA) 
exp3_apa__grey__anova__PROP.FAM.FA__stim.format.main.effect <-
  exp3_apa__grey__anova__PROP.FAM.FA$full$stim_format %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.FAM.FA__resp.option.main.effect <-
  exp3_apa__grey__anova__PROP.FAM.FA$full$resp_option %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.FAM.FA__interaction.main.effect <-
  exp3_apa__grey__anova__PROP.FAM.FA$full$resp_option_stim_format %>%   
  str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
#---------------------------------------------------------------------------------------------------------------#
##  Run post-hoc contrasts:
##  Interaction:
exp3_grey__anova__PROP.FAM.FA__interaction.means <- 
  emmeans(exp3_grey__anova__PROP.FAM.FA, ~ resp_option:stim_format)
exp3_grey__anova__PROP.FAM.FA__interaction.pairwise <- pairs(exp3_grey__anova__PROP.FAM.FA__interaction.means,
                                                        adjust = "bonferroni")

##  Format pairwise results for use in report:  
exp3_apa__grey__anova__PROP.FAM.FA__interaction.means <- 
  papaja:::apa_print.emmGrid(exp3_grey__anova__PROP.FAM.FA__interaction.means)
exp3_apa__grey__anova__PROP.FAM.FA__interaction.full.pairwise <- 
  papaja:::apa_print.emmGrid(exp3_grey__anova__PROP.FAM.FA__interaction.pairwise)

##  Prep data for figure:  
exp3_grey__PROP.FAM.FA__interaction.figure.data <- as.tibble(exp3_grey__anova__PROP.FAM.FA__interaction.means) %>%
  select(-df, -lower.CL, -upper.CL)
##  Create labels for figure (replaces plot legend):
exp3_grey__PROP.FAM.FA__interaction.figure.labels <- exp3_grey__PROP.FAM.FA__interaction.figure.data %>%
  select(stim_format, resp_option, emmean) %>%
  filter(stim_format == "Photographs") %>%
  mutate(label = case_when(
    resp_option == "RFG" ~ "RFG",
    resp_option == "RFBG" ~ "RFBG")) %>%
  group_by(stim_format, resp_option, label)
##  Interaction figure:
exp3_grey__PROP.FAM.FA__interaction.figure <- exp3_grey__PROP.FAM.FA__interaction.figure.data %>%
  ggplot() +
  aes(x = stim_format, colour = resp_option, group = resp_option, y = emmean) +
  stat_summary(fun.y = mean, geom = "point", size = 2.4, shape = 15) +
  stat_summary(fun.y = mean, geom = "line", size = 1.2) +
  geom_text(size = 4, aes(label = label, colour = resp_option), nudge_x = 0.08, hjust = 0,
            data = grey__PROP.FAM.FA__interaction.figure.labels) +
  scale_colour_manual(values = c('#8ec6c5', '#7d9ab5', '#8566aa')) +
  labs(x = "Stimuli-format", y = "Mean proportion of FA assigned:  'Familiarity'") +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10, vjust = -4),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 6),
        axis.text.y = element_text(size = 10),
        plot.background = element_rect(size = 0.5, linetype = 'solid', colour = "black"),
        plot.margin = margin(20, 20, 24, 30))
#################################################################################################################
##    PROP GUESS FA:                                                                                           ##
#################################################################################################################
## Prep data for analysis:                                               
exp3_grey__anova__PROP.GUESS.FA_data <- exp3_grey_data %>%
  select(id, resp_option, prop_word_FA_guess, prop_drawing_FA_guess, prop_photo_FA_guess) %>%
  rename(word = "prop_word_FA_guess", drawing = "prop_drawing_FA_guess", photo = "prop_photo_FA_guess") %>%
  gather(key = "stim_format", value = "PROP.GUESS.FA", word, drawing, photo) %>%
  mutate_at(vars("id", "resp_option", "stim_format"), factor) %>%
  mutate(stim_format = str_replace(stim_format, "word", "Words")) %>%
  mutate(stim_format = str_replace(stim_format, "drawing", "Drawings")) %>%
  mutate(stim_format = str_replace(stim_format, "photo", "Photographs")) %>%
  transform(stim_format = factor(stim_format, levels=c("Words","Drawings","Photographs"))) %>%
  transform(resp_option = factor(resp_option, levels=c("RFG","RFBG")))
#---------------------------------------------------------------------------------------------------------------#
##  Run 2-way mixed ANOVA:
exp3_grey__anova__PROP.GUESS.FA <- aov_ez("id", "PROP.GUESS.FA", exp3_grey__anova__PROP.GUESS.FA_data,
                                     between = "resp_option", within = "stim_format")

##  Format ANOVA main effects for use in report:  
exp3_apa__grey__anova__PROP.GUESS.FA <- papaja:::apa_print.afex_aov(exp3_grey__anova__PROP.GUESS.FA) 
exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.main.effect <-
  exp3_apa__grey__anova__PROP.GUESS.FA$full$stim_format %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.GUESS.FA__resp.option.main.effect <-
  exp3_apa__grey__anova__PROP.GUESS.FA$full$resp_option %>% str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
exp3_apa__grey__anova__PROP.GUESS.FA__interaction.main.effect <-
  exp3_apa__grey__anova__PROP.GUESS.FA$full$resp_option_stim_format %>%
  str_extract("^(?:[^\\,]*\\,){3}([^\\,]*)")
#---------------------------------------------------------------------------------------------------------------#
##  Run post-hoc contrasts:
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Response-option:  Not significant.
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Stimuli format:
exp3_grey__anova__PROP.GUESS.FA__stim.format.means <- emmeans(exp3_grey__anova__PROP.GUESS.FA, ~ stim_format)
exp3_grey__anova__PROP.GUESS.FA__stim.format.pairwise <- pairs(exp3_grey__anova__PROP.GUESS.FA__stim.format.means,
                                                          adjust = "bonferroni")
##  Format pairwise results for use in report:  
exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.means <- 
  papaja:::apa_print.emmGrid(exp3_grey__anova__PROP.GUESS.FA__stim.format.means)
exp3_apa__grey__anova__PROP.GUESS.FA__words.mean <- 
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.means$estimate$Words %>% str_remove("\\,.*")
exp3_apa__grey__anova__PROP.GUESS.FA__drawings.mean <- 
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.means$estimate$Drawings %>% str_remove("\\,.*")
exp3_apa__grey__anova__PROP.GUESS.FA__photographs.mean <-
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.means$estimate$Photographs %>% str_remove("\\,.*")
exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.pairwise <- 
  papaja:::apa_print.emmGrid(exp3_grey__anova__PROP.GUESS.FA__stim.format.pairwise)

exp3_apa__grey__anova__PROP.GUESS.FA__words.vs.drawings.pairwise <-
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.pairwise$full$Words_Drawings %>% 
  str_remove("^(?:[^\\$]*\\$){6}([^\\$]*)")
exp3_apa__grey__anova__PROP.GUESS.FA__words.vs.photographs.pairwise <-
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.pairwise$full$Words_Photographs %>% 
  str_remove("^(?:[^\\$]*\\$){6}([^\\$]*)")
exp3_apa__grey__anova__PROP.GUESS.FA__drawings.vs.photographs.pairwise <-
  exp3_apa__grey__anova__PROP.GUESS.FA__stim.format.pairwise$full$Drawings_Photographs %>%
  str_remove("^(?:[^\\$]*\\$){6}([^\\$]*)")
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
##  Interaction:  Not significant.
#################################################################################################################
##    FA FIGURE:                                                                                               ##
#################################################################################################################
##  Prep data for figures:  
exp3_grey__PROP.REC.FA__figure.data <- 
  as.tibble(emmeans(exp3_grey__anova__PROP.REC.FA, ~ resp_option:stim_format)) %>%
  select(-df, -lower.CL, -upper.CL)
exp3_grey__PROP.FAM.FA__figure.data <- 
  as.tibble(emmeans(exp3_grey__anova__PROP.FAM.FA, ~ resp_option:stim_format)) %>%
  select(-df, -lower.CL, -upper.CL)
exp3_grey__PROP.GUESS.FA__figure.data <- 
  as.tibble(emmeans(exp3_grey__anova__PROP.GUESS.FA, ~ resp_option:stim_format)) %>%
  select(-df, -lower.CL, -upper.CL)

exp3_grey__PROP.REC.FAM.GUESS.FA__figure.data <- 
  tribble(~"resp_option", ~"stim_format", ~"rfg", ~"emmean", ~"SE",
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[1,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[1,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[1,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[1,4])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[2,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[2,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[2,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[2,4])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[3,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[3,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[3,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[3,4])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[4,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[4,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[4,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[4,4])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[5,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[5,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[5,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[5,4])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[6,1])),
          as.character(pull(exp3_grey__PROP.REC.FA__figure.data[6,2])), "Recollection",
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[6,3])),
          as.double(pull(exp3_grey__PROP.REC.FA__figure.data[6,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[1,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[1,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[1,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[1,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[2,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[2,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[2,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[2,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[3,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[3,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[3,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[3,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[4,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[4,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[4,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[4,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[5,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[5,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[5,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[5,4])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[6,1])),
          as.character(pull(exp3_grey__PROP.FAM.FA__figure.data[6,2])), "Familiarity",
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[6,3])),
          as.double(pull(exp3_grey__PROP.FAM.FA__figure.data[6,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[1,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[1,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[1,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[1,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[2,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[2,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[2,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[2,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[3,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[3,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[3,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[3,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[4,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[4,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[4,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[4,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[5,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[5,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[5,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[5,4])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[6,1])),
          as.character(pull(exp3_grey__PROP.GUESS.FA__figure.data[6,2])), "Guessing",
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[6,3])),
          as.double(pull(exp3_grey__PROP.GUESS.FA__figure.data[6,4]))) %>%
  transform(resp_option = factor(resp_option, levels=c("RFG","RFBG"))) %>%
  transform(rfg = factor(rfg, levels=c("Recollection","Familiarity","Guessing"))) %>%
  transform(stim_format = factor(stim_format, levels=c("Words","Drawings", "Photographs")))

##  Prep data for figures:  
exp3_grey__PROP.REC.FAM.GUESS.FA__figure <-
  ggplot(exp3_grey__PROP.REC.FAM.GUESS.FA__figure.data, aes(x = stim_format, y = emmean, fill = rfg)) +
  scale_fill_manual("legend",values = c("Recollection" = "#1B4251", 
                                        "Familiarity" = "#6A6C93", 
                                        "Guessing" = "#D491CA")) + 
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = pmax(emmean-SE, 0), ymax = emmean+SE), 
                width = 0.2, size = 0.3, position = position_dodge(0.9)) +
  facet_wrap(~resp_option, strip.position = "bottom") +
  labs(x = "Response-option condition", y = "Mean proportion of FAs") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_classic() +    # Removes grey background and grid lines. 
  theme(
    strip.placement = "outside",
    strip.background = element_rect(fill = "white", size = 0.5, linetype = "solid", colour = "black"),
    axis.title.x = element_text(vjust=-4),    # Move x-axis label further away from the figure.
    axis.title.y = element_text(vjust=6),     # Move y-axis label further away from the figure.
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.background = element_rect(fill = "white", size = 0.2, linetype = "solid", colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.5, 0.896),
    legend.margin = margin(6, 6, 6, 6),
    legend.spacing.y = unit(-1, "pt"),
    plot.background = element_rect(size = 0.5, linetype = 'solid', colour = "black"),
    plot.margin = margin(20, 20, 24, 30))