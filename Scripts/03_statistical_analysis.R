## -------------------------------------------------------------------------------------
library(easystats)


## ----set the reference groups---------------------------------------------------------
lit_seq <- within(lit_seq, CRPR.group <-  relevel(factor(CRPR.group), ref = 7)) # FIRST REASSIGN THE REFERENCE CATEGORY TO THE NORMATIVE GROUP (E.G. NATIVES)
lit_seq <- within(lit_seq, CRPR <-  relevel(factor(CRPR), ref = 13)) # FIRST REASSIGN THE REFERENCE CATEGORY TO THE NORMATIVE GROUP (E.G. NATIVES)


## ----check colinearity----------------------------------------------------------------
pdf("Output/tables & figures/pearsons_collinearity.pdf")
psych::pairs.panels(lit_seq[,c("seqs", "Literature.Count", "area", "CRPR")])
dev.off()
#Number of sequences and literature count is colinear (r = 0.77)


## ----poisson model--------------------------------------------------------------------
#M1 <- lme4::glmer(Literature.Count ~ CRPR + (1 | area), data = lit_seq, family = poisson)
performance::check_overdispersion(M1) 
performance::check_model(M1)
performance::r2(M1)


## ----NB for literature / CRPR w/threat------------------------------------------------
lit_CRPR_threatlvl_nb <- MASS::glm.nb(Literature.Count ~ CRPR  + area, data = lit_seq)
performance::check_model(lit_CRPR_threatlvl_nb)
easystats::model_dashboard(lit_CRPR_threatlvl_nb, output_dir = 'Output/', output_file = 'lit_CRPR_threatlvl_nb.html')
performance::r2(lit_CRPR_threatlvl_nb)


## ----NB for sequences / CRPR w/threat-------------------------------------------------
seq_CRPR_threatlvl_nb <- MASS::glm.nb(seqs ~ CRPR  + area, data = lit_seq)
performance::check_model(seq_CRPR_threatlvl_nb)
easystats::model_dashboard(seq_CRPR_threatlvl_nb, output_file = 'seq_CRPR_threatlvl_nb.html', output_dir = 'Output/')
performance::r2(seq_CRPR_threatlvl_nb)


## -------------------------------------------------------------------------------------
lit_CRPR_group_nb <- MASS::glm.nb(Literature.Count ~ CRPR.group + area, data = lit_seq)
easystats::model_dashboard(lit_CRPR_group_nb, output_file = 'lit_CRPR_group_nb.html', output_dir = 'Output/')


## -------------------------------------------------------------------------------------
seq_CRPR_group_nb <- MASS::glm.nb(seqs ~ CRPR.group + area, data = lit_seq)
easystats::model_dashboard(seq_CRPR_group_nb, output_file = 'seq_CRPR_group_nb.html', output_dir = 'Output/')

