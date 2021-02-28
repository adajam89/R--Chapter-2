```{r, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.show='hide'}
#################################################################################################################
##    Ensor d-prime (exp1):                                                                                    ##
#################################################################################################################
new.d.prime.test <- ensor.exp1.data %>%
  select(prop.hit__grey.word, prop.miss__grey.word, prop.FA__grey.word, prop.CR__grey.word) %>%
  mutate(prop.hit__grey.word = dplyr::recode(prop.hit__grey.word, `1` = 0.99)) %>%
  mutate(prop.hit__grey.word = dplyr::recode(prop.hit__grey.word, `0` = 0.01)) %>%
  mutate(prop.FA__grey.word = dplyr::recode(prop.FA__grey.word, `1` = 0.99)) %>%
  mutate(prop.FA__grey.word = dplyr::recode(prop.FA__grey.word, `0` = 0.01)) %>%
  
  #mutate(prop.miss__grey.word = dplyr::recode(prop.miss__grey.word, `1` = 0.99)) %>%
  #mutate(prop.miss__grey.word = dplyr::recode(prop.miss__grey.word, `0` = 0.01)) %>%
  #mutate(prop.CR__grey.word = dplyr::recode(prop.CR__grey.word, `1` = 0.99)) %>%
  #mutate(prop.CR__grey.word = dplyr::recode(prop.CR__grey.word, `0` = 0.01)) %>%
  
  mutate("z.prop.hit__grey.word" = qnorm((prop.hit__grey.word) / 
                                           (prop.hit__grey.word + prop.miss__grey.word))) %>%
  mutate("z.prop.FA__grey.word" = qnorm((prop.FA__grey.word) / 
                                          (prop.FA__grey.word + prop.CR__grey.word + 1))) %>%
  
  mutate("z.prop.hit__grey.word" = dplyr::recode(z.prop.hit__grey.word, `Inf` = 0)) %>%
  mutate("z.prop.FA__grey.word" = dplyr::recode(z.prop.FA__grey.word, `Inf` = 0)) %>%
  
  
  
  mutate("d.prime" = z.prop.hit__grey.word - z.prop.FA__grey.word)

mean(new.d.prime.test$d.prime)


##  d'prime (calculated in the same way as we did previously in Excel):
mutate("old_drawing_d_prime" = z_total_drawing_hit - z_total_drawing_FA) %>%
  mutate("old_photo_d_prime" = z_total_photo_hit - z_total_photo_FA)

##    One-way repeated measures ANOVA:
ensor.exp1__d.prime.aov <- afex::aov_ez('id', 'd.prime', fun_aggregate = mean, ensor.exp1__d.prime,
                                        within = 'stim', anova_table=list(correction = "none", es = "none"))

##    Mauchlys test of sphericity:
ensor.exp1__d.prime.aov__sphericity <- test_sphericity(ensor.exp1__d.prime.aov)
ensor.exp1__d.prime.aov__sphericity <- ensor.exp1__d.prime.aov__sphericity[1,2]

##    Assumption of sphericity not violated (p<.05). Greenhouse-Geisser correction not applied:
ensor.exp1__d.prime.aov <- afex::aov_ez('id', 'd.prime', fun_aggregate = mean, ensor.exp1__d.prime, 
                                        within = 'stim', anova_table=list(correction = "none", es = "ges"))
#-----------------------------------------------------------------------------------------------------------------
##    Significant main effect, so perform Paired Samples t-test:
ensor.exp1__d.prime__ttest <- ensor.exp1__d.prime %>% filter(stim == "grey.word" | stim == "col.word") %>%
  t.test(d.prime~stim, paired=TRUE, data = .)
##    Get means:
ensor.exp1__d.prime.means <- emmeans(ensor.exp1__d.prime.aov, ~stim)
ensor.exp1__d.prime.means__grey.word <- summary(ensor.exp1__d.prime.means)$emmean[1] %>% round(3)
ensor.exp1__d.prime.means__col.word <- summary(ensor.exp1__d.prime.means)$emmean[2] %>% round(3)

#################################################################################################################
##    Ensor d-prime (exp2):                                                                                    ##
#################################################################################################################   ##    One-way repeated measures ANOVA:
ensor.exp2__d.prime.aov <- afex::aov_ez('id', 'd.prime', fun_aggregate = mean, ensor.exp2__d.prime,
                                        within = 'stim', anova_table=list(correction = "none", es = "none"))

##    Mauchlys test of sphericity:
ensor.exp2__d.prime.aov__sphericity <- test_sphericity(ensor.exp2__d.prime.aov)
ensor.exp2__d.prime.aov__sphericity <- ensor.exp2__d.prime.aov__sphericity[1,2]

##    Assumption of sphericity not violated (p<.05). Greenhouse-Geisser correction not applied:
ensor.exp2__d.prime.aov <- afex::aov_ez('id', 'd.prime', fun_aggregate = mean, ensor.exp2__d.prime, 
                                        within = 'stim', anova_table=list(correction = "none", es = "ges"))
#-----------------------------------------------------------------------------------------------------------------
##    Significant main effect, so perform Paired Samples t-test:
ensor.exp2__d.prime__ttest <- ensor.exp2__d.prime %>% filter(stim == "grey.word" | stim == "col.word") %>%
  t.test(d.prime~stim, paired=TRUE, data = .)
##    Get means:
ensor.exp2__d.prime.means <- emmeans(ensor.exp2__d.prime.aov, ~stim)
ensor.exp2__d.prime.means__grey.word <- summary(ensor.exp2__d.prime.means)$emmean[1] %>% round(3)
ensor.exp2__d.prime.means__col.word <- summary(ensor.exp2__d.prime.means)$emmean[2] %>% round(3)
```



\newpage
```{r echo=FALSE, results = FALSE, cache=FALSE}
table_numbering(name = "exp5__table__ensor.vs.current__d.prime", caption = "*d'* scores for regular and distinctive word stimuli, across the current experiment and @ensor2019b. Within-subjects contrasts are presented following significant main effects of stimuli format (reported in text).", display = FALSE)
```




In the current study, mean *d'* scores showed a significant main effect of stimuli format [`r exp5__d.prime.aov__main.effect`]. Distinctive words (`r exp5__d.prime.means__col.word`) produced significantly better discrimination between hits and FAs than regular words (`r exp5__d.prime.means__grey.word`), `r exp5__d.prime.contrasts__grey.word.col.word`. The same was evident in both of the @ensor2019b experiments (Experiment 1: `r ensor.exp1__d.prime.aov__main.effect`; Experiment 2: `r ensor.exp2__d.prime.aov__main.effect`), again with distinctive words showing higher *d'* scores than regular words (see `r table_numbering("exp5__table__ensor.vs.current__d.prime", display = "cite")`). Visual inspection of the data shows *d'* scores gradually decrease as the number of trials increases, though for distinctive words, discrimination was the same between the experiment with the highest number of trials (80/160) and the current study (60/120). 


&nbsp;

`r table_numbering("exp5__table__ensor.vs.current__d.prime")`
```{r, echo=FALSE, cache=FALSE}
      exp5__table__ensor.vs.current__d.prime %>%
          kable("latex", booktabs = T, align = "rccc", escape = F) %>%
          kable_styling(font_size = 10, full_width = T, latex_options = c("hold_position", "condensed")) %>%
          column_spec(2:3, width = "1.6cm") %>%
          column_spec(4, width = "5cm") %>%
          add_header_above(c(" " = 1, "d'" = 3), bold = T, italic = T) %>%
          pack_rows("Ensor, Surprenant, & Neath (2019)", 1, 2) %>%
          pack_rows("Current study", 3, 0)
```


#################################################################################################################
##    Ensor Exp1 c:                                                                                           ##
#################################################################################################################
##    One-way repeated measures ANOVA:
      ensor.exp1__c.aov <- ensor.exp1__c %>% afex::aov_4(c ~ 1 + (stim|id), data = .)
##    Get means: 
      ensor.exp1__c.means <- emmeans(ensor.exp1__c.aov, ~stim)
##    Create list of desired contrasts from the above reference grid (1 = row of interest, 0 = ignore): 
      grey.word = c(1, 0, 0, 0)
      col.word = c(0, 1, 0, 0)
##    Run contrasts:                                    
      ensor.exp1__c.contrasts <- contrast(ensor.exp1__c.means, method = 
                                                list("grey.word - col.word" = grey.word - col.word), 
                                                adjust = "bonf")
##    Format results for use in report:
      ensor.exp1__c.aov__apa <- papaja:::apa_print.afex_aov(ensor.exp1__c.aov)
      ensor.exp1__c.aov__main.effect <- ensor.exp1__c.aov__apa$full
      ensor.exp1__c.means__apa <- papaja:::apa_print.emmGrid(ensor.exp1__c.means) 
      ensor.exp1__c.means__grey.word <- ensor.exp1__c.means__apa$estimate$greyword %>%
          str_remove("\\,.*")
      ensor.exp1__c.means__col.word <- ensor.exp1__c.means__apa$estimate$colword %>%
          str_remove("\\,.*")
      ensor.exp1__c.contrasts__apa <- papaja:::apa_print.emmGrid(ensor.exp1__c.contrasts) 
      ensor.exp1__c.contrasts__grey.word.col.word <- ensor.exp1__c.contrasts__apa$statistic
      
#################################################################################################################
##    Ensor Exp2 c:                                                                                           ##
#################################################################################################################
##    One-way repeated measures ANOVA:
      ensor.exp2__c.aov <- ensor.exp2__c %>% afex::aov_4(c ~ 1 + (stim|id), data = .)
##    Get means: 
      ensor.exp2__c.means <- emmeans(ensor.exp2__c.aov, ~stim)
##    Format results for use in report:
      ensor.exp2__c.aov__apa <- papaja:::apa_print.afex_aov(ensor.exp2__c.aov)
      ensor.exp2__c.aov__main.effect <- ensor.exp2__c.aov__apa$full
      ensor.exp2__c.means__apa <- papaja:::apa_print.emmGrid(ensor.exp2__c.means) 
      ensor.exp2__c.means__grey.word <- ensor.exp2__c.means__apa$estimate$greyword %>%
          str_remove("\\,.*")
      ensor.exp2__c.means__col.word <- ensor.exp2__c.means__apa$estimate$colword %>%
          str_remove("\\,.*")
      
##    Ensor vs. current c table: 
      exp5__table__ensor.vs.current__c <- 
            tribble(~" ", ~"Grey words", ~"Distinctive words", ~"Planned comparisons",
                  "Trials: 40 study / 80 test:",
                      round(mean(ensor.exp1.data$c__grey.word),2),
                      round(mean(ensor.exp1.data$c__col.word),2),
                      ensor.exp1__c.contrasts__grey.word.col.word$greyword_colword,
                  "Trials: 80 study / 160 test:",
                      round(mean(ensor.exp2.data$c__grey.word),2),
                      round(mean(ensor.exp2.data$c__col.word),2),
                      "No significant main effect",
                  "Trials: 60 study / 120 test:", 
                      round(exp5__c %>% filter(stim == "grey.word") %>% pull(c) %>% mean(),2),
                      round(exp5__c %>% filter(stim == "col.word") %>% pull(c) %>% mean(),2),
                      exp5__c.contrasts__grey.word.col.word$greyword_colword)
```
\newpage
```{r echo=FALSE, results = FALSE, cache=FALSE}
      table_numbering(name = "exp5__table__ensor.vs.current__c", caption = "*c* scores for regular and distinctive word stimuli, across the current experiment and @ensor2019b. Within-subjects contrasts are presented following significant main effects of stimuli format (reported in text).", display = FALSE)
```
 The same was not apparent in either of the @ensor2019b experiments; while the first experiment did show a significant main effect of stimuli format [`r ensor.exp1__c.aov__main.effect`], there was no significant difference in *c* scores between distinctive and regular words. The second experiment showed no main effect of stimuli format [`r ensor.exp2__c.aov__main.effect`].

Visual inspection of the data shows *d'* scores gradually decrease as the number of trials increases, though for distinctive words, discrimination was the same between the experiment with the highest number of trials (80/160) and the current study (60/120). 

&nbsp;

`r table_numbering("exp5__table__ensor.vs.current__c")`
```{r, echo=FALSE, cache=FALSE}
      exp5__table__ensor.vs.current__c %>%
          kable("latex", booktabs = T, align = "rccc", escape = F) %>%
          kable_styling(font_size = 10, full_width = T, latex_options = c("hold_position", "condensed")) %>%
          column_spec(2:3, width = "1.6cm") %>%
          column_spec(4, width = "5cm") %>%
          add_header_above(c(" " = 1, "c" = 3), bold = T, italic = T) %>%
          pack_rows("Ensor, Surprenant, & Neath (2019)", 1, 2) %>%
          pack_rows("Current study", 3, 0)
```












```{r, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.show='hide'}
#################################################################################################################
##    Exp5 Prop FAs:                                                                                           ##
#################################################################################################################
##    One-way repeated measures ANOVA:
      exp5__prop.FA.aov <- exp5__prop.FA %>% afex::aov_4(prop.FA ~ 1 + (stim|id), data = .)
##    Get means: 
      exp5__prop.FA.means <- emmeans(exp5__prop.FA.aov, ~stim)
##    Create list of desired contrasts from the above reference grid (1 = row of interest, 0 = ignore): 
      grey.word = c(1, 0, 0, 0, 0, 0)
      col.word = c(0, 1, 0, 0, 0, 0)
##    Run contrasts:                                    
      exp5__prop.FA.contrasts <- contrast(exp5__prop.FA.means, method = 
                                            list("grey.word - col.word" = grey.word - col.word), adjust = "bonf")
##    Format results for use in report:
      exp5__prop.FA.aov__apa <- papaja:::apa_print.afex_aov(exp5__prop.FA.aov)
      exp5__prop.FA.aov__main.effect <- exp5__prop.FA.aov__apa$full
      exp5__prop.FA.means__apa <- papaja:::apa_print.emmGrid(exp5__prop.FA.means) 
      exp5__prop.FA.means__grey.word <- exp5__prop.FA.means__apa$estimate$greyword %>% str_remove("\\,.*")
      exp5__prop.FA.means__col.word <- exp5__prop.FA.means__apa$estimate$colword %>% str_remove("\\,.*")
      exp5__prop.FA.contrasts__apa <- papaja:::apa_print.emmGrid(exp5__prop.FA.contrasts) 
      exp5__prop.FA.contrasts__grey.word.col.word <- exp5__prop.FA.contrasts__apa$statistic

#################################################################################################################
##    Ensor Exp1 Prop FAs:                                                                                     ##
#################################################################################################################
##    One-way repeated measures ANOVA:
      ensor.exp1__prop.FA.aov <- ensor.exp1__prop.FA %>% afex::aov_4(prop.FA ~ 1 + (stim|id), data = .)
##    Get means: 
      ensor.exp1__prop.FA.means <- emmeans(ensor.exp1__prop.FA.aov, ~stim)

##    Format results for use in report:
      ensor.exp1__prop.FA.aov__apa <- papaja:::apa_print.afex_aov(ensor.exp1__prop.FA.aov)
      ensor.exp1__prop.FA.aov__main.effect <- ensor.exp1__prop.FA.aov__apa$full
      ensor.exp1__prop.FA.means__apa <- papaja:::apa_print.emmGrid(ensor.exp1__prop.FA.means) 
      ensor.exp1__prop.FA.means__grey.word <- ensor.exp1__prop.FA.means__apa$estimate$greyword %>%
          str_remove("\\,.*")
      ensor.exp1__prop.FA.means__col.word <- ensor.exp1__prop.FA.means__apa$estimate$colword %>% 
          str_remove("\\,.*")
#################################################################################################################
##    Ensor Exp2 Prop FAs:                                                                                     ##
#################################################################################################################
##    One-way repeated measures ANOVA:
      ensor.exp2__prop.FA.aov <- ensor.exp2__prop.FA %>% afex::aov_4(prop.FA ~ 1 + (stim|id), data = .)
##    Get means: 
      ensor.exp2__prop.FA.means <- emmeans(ensor.exp2__prop.FA.aov, ~stim)
##    Create list of desired contrasts from the above reference grid (1 = row of interest, 0 = ignore): 
      grey.word = c(1, 0, 0, 0)
      col.word = c(0, 1, 0, 0)
##    Run contrasts:                                    
      ensor.exp2__prop.FA.contrasts <- contrast(ensor.exp2__prop.FA.means, method = 
                                                list("grey.word - col.word" = grey.word - col.word), 
                                                adjust = "bonf")
##    Format results for use in report:
      ensor.exp2__prop.FA.aov__apa <- papaja:::apa_print.afex_aov(ensor.exp2__prop.FA.aov)
      ensor.exp2__prop.FA.aov__main.effect <- ensor.exp2__prop.FA.aov__apa$full
      ensor.exp2__prop.FA.means__apa <- papaja:::apa_print.emmGrid(ensor.exp2__prop.FA.means) 
      ensor.exp2__prop.FA.means__grey.word <- ensor.exp2__prop.FA.means__apa$estimate$greyword %>%
          str_remove("\\,.*")
      ensor.exp2__prop.FA.means__col.word <- ensor.exp2__prop.FA.means__apa$estimate$colword %>% 
          str_remove("\\,.*")
      ensor.exp2__prop.FA.contrasts__apa <- papaja:::apa_print.emmGrid(ensor.exp2__prop.FA.contrasts) 
      ensor.exp2__prop.FA.contrasts__grey.word.col.word <- ensor.exp2__prop.FA.contrasts__apa$statistic 
      

```









#### Elimination of the PSE
```{r, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.show='hide'}
#################################################################################################################
##    Exp5 Prop hits (Col words vs. grey drawings):                                                            ##
#################################################################################################################
      col.word = c(0, 1, 0, 0, 0, 0)
      grey.drawing = c(0, 0, 1, 0, 0, 0)
##    Run contrasts:                                    
      exp5__prop.hit.contrasts2 <- contrast(exp5__prop.hit.means, method = 
                                            list("col.word - grey.drawing" = col.word - grey.drawing), 
                                            adjust = "bonf")
##    Format results for use in report:
      exp5__prop.hit.means__grey.drawing <- exp5__prop.hit.means__apa$estimate$greydrawing %>% str_remove("\\,.*")
      exp5__prop.hit.contrasts2__apa <- papaja:::apa_print.emmGrid(exp5__prop.hit.contrasts2) 
      exp5__prop.hit.contrasts__col.word.grey.drawing <- exp5__prop.hit.contrasts2__apa$statistic

#################################################################################################################
##    Ensor Exp1 Prop hits (Col words vs. grey drawings):                                                      ##
#################################################################################################################
      col.word = c(0, 1, 0, 0)
      grey.drawing = c(0, 0, 1, 0)
##    Run contrasts:                                    
      ensor.exp1__prop.hit.contrasts2 <- contrast(ensor.exp1__prop.hit.means, method = 
                                                list("col.word - grey.drawing" = col.word - grey.drawing), 
                                                adjust = "bonf")
##    Format results for use in report:
      ensor.exp1__prop.hit.means__grey.drawing <- ensor.exp1__prop.hit.means__apa$estimate$greydrawing %>% 
          str_remove("\\,.*")
      ensor.exp1__prop.hit.contrasts2__apa <- papaja:::apa_print.emmGrid(ensor.exp1__prop.hit.contrasts2) 
      ensor.exp1__prop.hit.contrasts__col.word.grey.drawing <- ensor.exp1__prop.hit.contrasts2__apa$statistic
      
#################################################################################################################
##    Ensor Exp2 Prop hits:                                                                                    ##
#################################################################################################################
      col.word = c(0, 1, 0, 0)
      grey.drawing = c(0, 0, 1, 0)
##    Run contrasts:                                    
      ensor.exp2__prop.hit.contrasts2 <- contrast(ensor.exp2__prop.hit.means, method = 
                                                list("col.word - grey.drawing" = col.word - grey.drawing), 
                                                adjust = "bonf")
##    Format results for use in report:
      ensor.exp2__prop.hit.means__grey.drawing <- ensor.exp2__prop.hit.means__apa$estimate$greydrawing %>% 
          str_remove("\\,.*")
      ensor.exp2__prop.hit.contrasts2__apa <- papaja:::apa_print.emmGrid(ensor.exp2__prop.hit.contrasts2) 
      ensor.exp2__prop.hit.contrasts__col.word.grey.drawing <- ensor.exp2__prop.hit.contrasts2__apa$statistic
      
##    Ensor vs. current hits table: 
      exp5__table__ensor.vs.current__hits2 <- 
            tribble(~" ", ~"Distinctive words", ~"Grey drawings", ~"Planned comparisons",
                  "Trials: 40 study / 80 test:",
                      round(mean(ensor.exp1.data$prop.hit__col.word),2),
                      round(mean(ensor.exp1.data$prop.hit__grey.drawing),2),
                      ensor.exp1__prop.hit.contrasts__col.word.grey.drawing$colword_greydrawing,
                  "Trials: 80 study / 160 test:",
                      round(mean(ensor.exp2.data$prop.hit__col.word),2),
                      round(mean(ensor.exp2.data$prop.hit__grey.drawing),2),
                      ensor.exp2__prop.hit.contrasts__col.word.grey.drawing$colword_greydrawing,
                  "Trials: 60 study / 120 test:", 
                      round(exp5__prop.hit %>% filter(stim == "col.word") %>% pull(prop.hit) %>% mean(),2),
                      round(exp5__prop.hit %>% filter(stim == "grey.drawing") %>% pull(prop.hit) %>%
                            mean(),2),
                      exp5__prop.hit.contrasts__col.word.grey.drawing$colword_greydrawing)
```

```{r echo=FALSE, results = FALSE, cache=FALSE}
      table_numbering(name = "exp5__table__ensor.vs.current__hits2", caption = "Mean proportion of hits for distinctive word stimuli and grey drawings, across the current experiment and @ensor2019b. Within-subjects contrasts are presented following significant main effects of stimuli format (reported in text).", display = FALSE)
```

Further planned contrasts showed that, for the mean proportion of hits, the PSE remained intact in the current experiment: grey drawings (`r ensor.exp2__prop.hit.means__grey.drawing`) showed a significantly higher proportion of hits than distinctive words (`r exp5__prop.hit.means__col.word`), `r exp5__prop.hit.contrasts__col.word.grey.drawing`, indicating the distinctiveness manipulation did not increase the distinctiveness of the word stimuli to a level similar to that of the grey drawings. This was in stark contrast to the findings of @ensor2019b whereby the PSE was eliminated in both experiments, with no difference in the mean proportion of hits between distinctive words and grey drawings (see `r table_numbering("exp5__table__ensor.vs.current__hits2", display = "cite")`).

&nbsp;

`r table_numbering("exp5__table__ensor.vs.current__hits2")`
```{r, echo=FALSE, cache=FALSE}
      exp5__table__ensor.vs.current__hits2 %>%
          kable("latex", booktabs = T, align = "rccl", escape = F) %>%
          kable_styling(font_size = 10, full_width = T, latex_options = c("hold_position", "condensed")) %>%
          column_spec(2:3, width = "1.8cm") %>%
          column_spec(4, width = "5cm") %>%
          add_header_above(c(" " = 1, "Hits" = 3), bold = T) %>%
          pack_rows("Ensor, Surprenant, & Neath (2019)", 1, 2) %>%
          pack_rows("Current study", 3, 0)
```

\newpage
#### Reduced?
```{r, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE, cache=TRUE, fig.show='hide'}
#----------------------------------------------------------------------------------------------------------------
#     Tables and figures:
#----------------------------------------------------------------------------------------------------------------
      ##    Prep data for RFG figure:  
      exp5__prop.hit__grey.word <- mean_se(exp5__prop.hit %>% filter(stim == "grey.word") %>% pull(prop.hit))
      exp5__prop.hit__col.word <- mean_se(exp5__prop.hit %>% filter(stim == "col.word") %>% pull(prop.hit))
      exp5__prop.hit__grey.drawing <- mean_se(exp5__prop.hit %>% filter(stim == "grey.drawing") %>% pull(prop.hit))
      exp5__prop.hit__col.drawing <- mean_se(exp5__prop.hit %>% filter(stim == "col.drawing") %>% pull(prop.hit))
      exp5__prop.hit__grey.photo <- mean_se(exp5__prop.hit %>% filter(stim == "grey.photo") %>% pull(prop.hit))
      exp5__prop.hit__col.photo <- mean_se(exp5__prop.hit %>% filter(stim == "col.photo") %>% pull(prop.hit))

      exp5__figure__hits.data <- 
          tribble(~"type", ~stim_format, ~emmean, ~"ymin", ~"ymax",
                  "Hits", "Grey words", 
                              exp5__prop.hit__grey.word$y,
                              exp5__prop.hit__grey.word$ymin, 
                              exp5__prop.hit__grey.word$ymax,
                          "Hits", "Distinctive words", 
                              exp5__prop.hit__col.word$y,
                              exp5__prop.hit__col.word$ymin, 
                              exp5__prop.hit__col.word$ymax,                  
                          "Hits", "Grey drawings", 
                              exp5__prop.hit__grey.drawing$y,
                              exp5__prop.hit__grey.drawing$ymin, 
                              exp5__prop.hit__grey.drawing$ymax,                                          
                          "Hits", "Colour drawings", 
                              exp5__prop.hit__col.drawing$y,
                              exp5__prop.hit__col.drawing$ymin,
                              exp5__prop.hit__col.drawing$ymax,          
                          "Hits", "Grey photos", 
                              exp5__prop.hit__grey.photo$y,
                              exp5__prop.hit__grey.photo$ymin,
                              exp5__prop.hit__grey.photo$ymax, 
                          "Hits", "Colour photos", 
                              exp5__prop.hit__col.photo$y,
                              exp5__prop.hit__col.photo$ymin,
                              exp5__prop.hit__col.photo$ymax) %>%
          transform(stim_format = factor(stim_format, levels=c("Grey words","Distinctive words",
                                                               "Grey drawings","Colour drawings", 
                                                               "Grey photos", "Colour photos"))) %>%
          transform(type = factor(type, levels=c("Hits")))
      
      
      
##    Create figure:  
      exp5__figure__hits <-
          ggplot(exp5__figure__hits.data, aes(x = stim_format, y = emmean, fill = stim_format)) +
          scale_fill_manual("legend",values = c("Grey words" = "#4B2D3B", 
"Distinctive words" = "#504056", 
"Grey drawings" = "#49576E",
"Colour drawings" = "#3A6F7D", 
"Grey photos" = "#32857F",
"Colour photos" = "#499A76")) + 
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = exp5__figure__hits.data$ymin, 
                    ymax = exp5__figure__hits.data$ymax), width = 0.2, size = 0.3, position = position_dodge(0.9)) +
  labs(x = "Response option condition", y = "Mean proportion of hits") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_classic() +    # Removes grey background and grid lines. 
  theme(
    axis.title.x = element_text(vjust=-4),    # Move x-axis label further away from the figure.
    axis.title.y = element_text(vjust=6),     # Move y-axis label further away from the figure.
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "none",
    plot.background = element_rect(size = 0.5, linetype = 'solid', colour = "black"),
    plot.margin = margin(20, 20, 24, 30))





grey.word = c(1, 0, 0, 0, 0, 0)
col.word = c(0, 1, 0, 0, 0, 0)
grey.photo = c(0, 0, 0, 0, 1, 0)
exp5__prop.hit.contrasts3 <- contrast(exp5__prop.hit.means, method = 
                                        list("grey.word - grey.photo" = grey.word - grey.photo,
                                             "col.word - grey.photo" = col.word - grey.photo), adjust = "bonf")
exp5__prop.hit.means__grey.photo <- exp5__prop.hit.means__apa$estimate$greyphoto %>% str_remove("\\,.*")

exp5__prop.hit.contrasts3__apa <- papaja:::apa_print.emmGrid(exp5__prop.hit.contrasts3) 
exp5__prop.hit.contrasts__grey.word.grey.photo <- exp5__prop.hit.contrasts3__apa$statistic$greyword_greyphoto
exp5__prop.hit.contrasts__col.word.grey.photo <- exp5__prop.hit.contrasts3__apa$statistic$colword_greyphoto
```

&nbsp;

```{r fig.width=6, fig.height=5.5, echo=FALSE, cache=TRUE, warning=FALSE}
exp5__figure__hits
```

Grey words (`r exp5__prop.hit.means__grey.word`) vs. grey photos (`r exp5__prop.hit.means__grey.photo`),
`r exp5__prop.hit.contrasts__grey.word.grey.photo`.

Col words (`r exp5__prop.hit.means__col.word`) vs. grey photos (`r exp5__prop.hit.means__grey.photo`),
`r exp5__prop.hit.contrasts__col.word.grey.photo`.

\newpage
#### RFG

see `r figure_numbering("exp5__figure__rfg.hits", display = "cite")`).

&nbsp;

```{r echo=FALSE, results = FALSE, cache=FALSE}
figure_numbering(name = "exp5__figure__rfg.hits",
                 caption = "Mean proportion of hits assigned Recollection, Familiarity, and Guessing, for regular and distinctive word stimuli.")
```

```{r fig.width=7, fig.height=5.5, echo=FALSE, cache=TRUE, warning=FALSE}
exp5__figure__rfg.hits
```
`r figure_numbering("exp5__figure__rfg.hits")`

&nbsp;



