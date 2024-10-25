## -------------------------------------------------------------------------------------
library(easystats)
library(tidyverse)
library(MASS)

## -- load data ----------------------------------------------------------------
load(file = "Scripts/2022-09-01-statistical_modeling_results.RData")
lit_seq = lit_seq %>% mutate(litarea = Literature.Count/area, seqarea = seqs/area)

## ----set the reference groups ---------------------------------------------------------
lit_seq <- within(lit_seq, CRPR.group <-  relevel(factor(CRPR.group), ref = 7)) # FIRST REASSIGN THE REFERENCE CATEGORY TO THE NORMATIVE GROUP (E.G. NATIVES)
lit_seq <- within(lit_seq, CRPR <- relevel(factor(CRPR), ref = 5)) # FIRST REASSIGN THE REFERENCE CATEGORY TO THE NORMATIVE GROUP (E.G. NATIVES)


## ----check colinearity----------------------------------------------------------------
# pdf("Output/tables & figures/pearsons_collinearity.pdf")
psych::pairs.panels(lit_seq[,c("seqs", "Literature.Count", "area", "CRPR")])

#Number of sequences and literature count is colinear (r = 0.77)

## ----summary statistics---------------------------------------------------------------
lit_seq %>%
  group_by(CRPR) %>% 
  summarize(max.lit = max(Literature.Count), 
            max.seq = max(seqs),
            median.lit = median(Literature.Count),
            median.seq = median(seqs),
            min.lit = min(Literature.Count),
            min.seq = min(seqs))



## ----check area / CRPR relationship --------------------------------------------------
summary(MASS::glm.nb(area ~ CRPR, data = lit_seq)) # Shows that there are differences between groups
#Post-hoc tests
area_CRPR_posthoc <- emmeans::emmeans(MASS::glm.nb(area ~ CRPR, data = lit_seq), specs = "CRPR")

area.glm<- glm(formula = area ~ CRPR, data = lit_seq, family = "poisson")
summary(area.glm)
check_overdispersion(area.glm)



## ----NB for literature / CRPR w/threat------------------------------------------------
lit_CRPR_threatlvl_nb <- MASS::glm.nb(Literature.Count ~ CRPR  + area, data = lit_seq)

summary(lit_CRPR_threatlvl_nb)

car::Anova(lit_CRPR_threatlvl_nb)
performance::r2(lit_CRPR_threatlvl_nb)

# Post Hoc Plot #
lit_CRPR_threatlvl_nb_emmeans <- emmeans::emmeans(lit_CRPR_threatlvl_nb, specs = "CRPR")
TukeyHSD(lit_CRPR_threatlvl_nb)

## ----NB for sequences / CRPR w/threat-------------------------------------------------
seq_CRPR_threatlvl_nb <- MASS::glm.nb(seqs ~ CRPR  + area, data = lit_seq)
performance::check_model(seq_CRPR_threatlvl_nb)
summary(seq_CRPR_threatlvl_nb)
seq_CRPR_threatlvl_nb

performance::r2(seq_CRPR_threatlvl_nb)

# ----Post Hoc Plot #
seq_CRPR_threatlvl_nb_emmeans <- emmeans::emmeans(seq_CRPR_threatlvl_nb, specs = "CRPR")


## -----NB for Literature w/o threat rank------------------------------------------------
lit_seq <- within(lit_seq, CRPR.group <-  relevel(factor(CRPR.group), ref = 4))
lit_CRPR_group_nb <- MASS::glm.nb(Literature.Count ~ CRPR.group + area, data = lit_seq)
summary(lit_CRPR_group_nb)
plot(lit_CRPR_group_nb)

# Post Hoc Plot
lit_CRPR_group_nb_emmeans <- emmeans::emmeans(lit_CRPR_group_nb, specs = "CRPR.group")

## -------NB for Sequences w/o threat rank------------------------------------------------
seq_CRPR_group_nb <- MASS::glm.nb(seqs ~ CRPR.group + area, data = lit_seq)
summary(seq_CRPR_group_nb)
emmeans(seq_CRPR_group_nb, specs = "CRPR.group")


# Post Hoc Plot #
seq_CRPR_group_nb_emmeans <- emmeans::emmeans(seq_CRPR_group_nb, specs = "CRPR.group")


## -------Data Proportionality------------------------------------------------
lit_seq = read_csv('Data/20220710-CA-literature-seqs-complete.csv')


plot(lit_seq$area, lit_seq$Literature.Count, xlim = c(0,500000))

lit_seq %>% 
  filter(CRPR == "1B.2") %>% 
  print(n = 100)




lit_seq = lit_seq %>% 
  mutate(lit_area = Literature.Count/area)

## Inset plot
lit_seq[!is.infinite(lit_seq$lit_area),] %>% 
  ggplot()+
  geom_boxplot(mapping = aes(x = CRPR, y = log1p(lit_area)))+
  theme_classic()
  
## Larger plot
lit_seq[!is.infinite(lit_seq$lit_area),] %>% 
  ggplot()+
  geom_boxplot(mapping = aes(x = CRPR, y = log(lit_area)))+
  # ylim(0,1)+
  theme_classic()


summary(lit_seq[!is.infinite(lit_seq$lit_area),])



## --------------------------------------
# Literature/m2 ~ CRPR 
## --------------------------------------
lit_seq_inf = lit_seq[!is.infinite(lit_seq$litarea),]
lit_seq_inf <- within(lit_seq_inf, CRPR.group <-  relevel(factor(CRPR.group), ref = 4))
litarea_CRPR_group_nb <- MASS::glm.nb(litarea ~ CRPR, data = lit_seq_inf)
summary(litarea_CRPR_group_nb)
plot(litarea_CRPR_group_nb)
emmeans::emmeans(litarea_CRPR_group_nb, specs = "CRPR") %>% pairs() %>% plot()

## --------------------------------------
# seq/m2 ~ CRPR 
## --------------------------------------








