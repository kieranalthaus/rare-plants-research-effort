library(ggplot2)
library(multcompView)
library(dplyr)
library(readr)
library(magrittr)
library(emmeans)

## Load Data -------------------------------------------------------------------
lit_seq = read_csv(file = "Output/2022-10-07-CA_lit_seq_complete.csv")

## Figure 1: Lit / Seq Histograms ----------------------------------------------
# pdf(file = "Output/tables & figures/20241024_lit-seq_histograms.pdf",
#     height = 6.05,
    # width = 11.84)
par(mar = c(5.1,6,1,1),
    mfrow = c(1,2))
x = hist(lit_seq$Literature.Count, plot = F, breaks = 100000)
plot(x = NA,
     y = NA,
     xlim = c(0,3000),
     ylim = c(0, max(x$counts)),
     bty = "n",
     axes = FALSE,
     xlab = "Number of Papers",
     cex.lab = 2,
     ylab = "")
grid()
hist(lit_seq$Literature.Count, 
     breaks = 100000,
     xlim = c(0,3000),
     xlab = "Number of Papers",
     cex.lab = 2,
     col = "black",
     las = 1, 
     axes = F, 
     add = T)
axis(side = 1,
     at = c(0,1000,2000,3000),
     cex.axis = 2,
     lwd = 2)
axis(side = 2,
     cex.axis = 2,
     lwd = 2,
     las = 1)
segments(x0 = median(lit_seq$Literature.Count),
         x1 = median(lit_seq$Literature.Count),
         y0 = 0,
         y1 = max(x$counts),
         col = 'red',
         lwd = 2)
text(x = 0, 
     y = max(x$counts),
     label = median(lit_seq$Literature.Count),
     pos = 4,
     col = 'red',
     cex = 1.5)
box(lwd = 2)

par(mar = c(5.1,6,1,1))
x = hist(lit_seq$seqs, plot = F, breaks = 1000000)
plot(x = NA,
     y = NA,
     xlim = c(0,500),
     ylim = c(0, max(x$counts)),
     bty = "n",
     axes = FALSE,
     xlab = "Number of Accessions",
     cex.lab = 2,
     ylab = "")
grid()
hist(lit_seq$seqs, 
     breaks = 1000000,
     xlim = c(0,500),
     xlab = "Number of Accessions",
     cex.lab = 2,
     main = "",
     col = "black",
     las = 1, 
     axes = F,
     add = TRUE)
axis(side = 1,
     at = c(0,125,250,375,500),
     cex.axis = 2,
     lwd = 2)
axis(side = 2,
     cex.axis = 2,
     lwd = 2,
     las = 1)
segments(x0 = median(lit_seq$seqs),
         x1 = median(lit_seq$seqs),
         y0 = 0,
         y1 = max(x$counts),
         col = 'red',
         lwd = 2)
text(x = 0, 
     y = max(x$counts),
     label = median(lit_seq$seqs),
     pos = 4,
     col = 'red',
     cex = 1.5)
box(lwd = 2)
dev.off()

### Figure 2: Papers X Literature Dotplot --------------------------------------
# pdf(file = "Output/tables & figures/20241024_dotplot_v2.pdf",
#     height = 6,
#     width = 6)
#     # height = 4.40,
#     # width = 8.12)
lm = summary(lm(formula = log1p(Literature.Count) ~ log1p(seqs), data = lit_seq))
b = lm$coefficients[1,1]
m = lm$coefficients[2,1]
par(mar = c(5.1,5.1,1,1))
plot(x = NA,
     y = NA,
     xlim = c(0,log(signif(max(lit_seq$seqs),1))),
     ylim = c(0,log(max(lit_seq$Literature.Count))),
     bty = "n",
     axes = FALSE,
     xlab = "Number of Accessions",
     ylab = "",
     cex.lab = 2)
segments(x0 = log(c(1,10,100,1000,10000,100000,1000000)),
         x1 = log(c(1,10,100,1000,10000,100000,1000000)),
         y0 = -10,
         y1 = log(10000000),
         lty = 2,
         col = "grey")
segments(x0 = -1,
         x1 = log(10000000),
         y0 = log(c(1,10,100,1000,10000,100000,1000000)),
         y1 = log(c(1,10,100,1000,10000,100000,1000000)),
         lty = 2,
         col = "grey")

title(ylab = "Number of Papers",
      cex.lab = 2,
      line = 3.5)
points(
  x = log(lit_seq$seqs),
  y = log(lit_seq$Literature.Count),
  pch = 20
  )
axis(side = 1,
     at = log(c(1,10,100,1000,10000,100000,1000000)),
     labels = c(0,10,100,1000,10000,100000,1000000),
     lwd = 2)
axis(
  at = log(c(1,10,100,1000,10000, 100000,1000000)),
  labels = c(0,10,100,1000,10000,100000,1000000),
  side = 2,
  las = 1,
  lwd = 2
  )
box(lwd = 2)
abline(a = b,
       b = m,
       col = "dodgerblue",
       lwd = 3)
text(x = 4,
     y = 13,
    " R = 0.74, p-value < 0.05")
dev.off()

## Figure 3: Boxplots w/o threatlevel ------------------------------------------
# Literature Boxplot
lit_CRPR.group_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR.group, y = log10(Literature.Count + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4)+
  xlab(NULL)+
  ggtitle("(a) Literature")+
  scale_y_continuous(breaks = 0:7, labels = expression(0, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7))+
  
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(linewidth = 1)
  ); lit_CRPR.group_boxplot

# Sequence Boxplots
seq_CRPR.group_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR.group, y = log10(seqs + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4)+
  xlab(NULL)+
  ggtitle("(b) Accessions")+
  
  scale_y_continuous(breaks = 0:7, labels = expression(0, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(linewidth = 1)
  ); seq_CRPR.group_boxplot
# Stack
plot = plot_grid(lit_CRPR.group_boxplot, seq_CRPR.group_boxplot)
# Save
ggsave(plot = plot, filename = "Output/tables & figures/20240720_seqs_literature_CRPR.group_boxplots.pdf", 
       width = 6.23,
       height = 4.03)

## Figure 4: Boxplots w threatlevel --------------------------------------------
# Literature Boxplot
lit_CRPR_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR, y = log10(Literature.Count + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("(a) Literature")+
  
  scale_y_continuous(breaks = 0:7, labels = expression(0, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1),
        axis.ticks.x = element_line(linewidth = 1)
  ); lit_CRPR_boxplot

# Sequence Boxplot
seq_CRPR_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR, y = log10(seqs + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("(b) Accessions")+
  
  scale_y_continuous(breaks = 0:7, labels = expression(0, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1),
        axis.ticks.x = element_line(linewidth = 1)
  ); seq_CRPR_boxplot
# Stack
plot = plot_grid(lit_CRPR_boxplot, seq_CRPR_boxplot, ncol = 2)
# Save
ggsave(plot = plot, filename = "Output/tables & figures/20240720_seqs_literature_CRPR.threat_boxplots.pdf", 
       width = 6.23, height = 4.03)


## Helper Figures --------------------------------------------------------------
#' Below are figures that I used that aided in model interpretation, but 
#' that I didn't want to include in the final supplement or paper.
#' There are two sets of figures here, all of which help in the post-hoc 
#' interpretation of of the TukeyHSD tests that show significant and non-
#' significant pairwise p-values.

# P-value literature matrix for threatlevels
## For literature
lit_CRPR_threatlevel_pmatrix = pwpm(lit_CRPR_threatlvl_nb_emmeans, 
                                    means = TRUE, 
                                    diffs = FALSE, 
                                    flip = TRUE, 
                                    digits = 20, 
                                    pvals = TRUE)

lit_CRPR_threatlevel_pmatrix = matrix(data = as.numeric(lit_CRPR_threatlevel_pmatrix),
                                      nrow = length(lit_CRPR_threatlevel_pmatrix[1,]),
                                      ncol = length(lit_CRPR_threatlevel_pmatrix[,1]),
                                      dimnames = list(
                                        c(rownames(lit_CRPR_threatlevel_pmatrix)),
                                        c(colnames(lit_CRPR_threatlevel_pmatrix))
                                      ))

diag(lit_CRPR_threatlevel_pmatrix) = 999
lit_CRPR_threatlevel_pmatrix[is.na(lit_CRPR_threatlevel_pmatrix)] = 999


# Plot Matrix
col = reshape2::melt(lit_CRPR_threatlevel_pmatrix) %>%
  as.data.frame() %>% 
  mutate(color = case_when(
    value == 999 ~ "transparent",
    value < 0.05 ~ "#5584B0",
    between(x = value, left = 0.05, right = 999) ~ "#EB5C53"
  ),
  group = case_when(
    value == 999 ~ "nofill",
    value < 0.05 ~ "bluefill",
    between(x = value, left = 0.05, right = 999) ~ "redfill"
  )
  )

lit_corplot = ggplot(col, aes(reorder(Var1, desc(Var1)), Var2, fill = group))+
  geom_tile()+
  scale_fill_manual(values = c("#5584B0", "transparent", "#EB5C53"),
                    labels = c("p < 0.05", NA, "p > 0.05"),
                    breaks = c("bluefill", "nofill", "redfill"),
                    name = "P-Values")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8, angle = 22.5, hjust = 1),
        axis.title = element_blank(),
        plot.margin = unit(rep(.5, 4), "cm"),
        legend.position = "none")+
  ggtitle("Literature -- Sig. Diff.")

## For sequences
seq_CRPR_threatlvl_pmatrix = pwpm(seq_CRPR_threatlvl_nb_emmeans, 
                                  means = TRUE, 
                                  diffs = FALSE, 
                                  flip = TRUE, 
                                  digits = 20, 
                                  pvals = TRUE)

seq_CRPR_threatlvl_pmatrix = matrix(data = as.numeric(seq_CRPR_threatlvl_pmatrix),
                                    nrow = length(seq_CRPR_threatlvl_pmatrix[1,]),
                                    ncol = length(seq_CRPR_threatlvl_pmatrix[,1]),
                                    dimnames = list(
                                      c(rownames(seq_CRPR_threatlvl_pmatrix)),
                                      c(colnames(seq_CRPR_threatlvl_pmatrix))
                                    ))

diag(seq_CRPR_threatlvl_pmatrix) = 999
seq_CRPR_threatlvl_pmatrix[is.na(seq_CRPR_threatlvl_pmatrix)] = 999


# Plot Matrix
col = reshape2::melt(seq_CRPR_threatlvl_pmatrix) %>%
  as.data.frame() %>% 
  mutate(color = case_when(
    value == 999 ~ "transparent",
    value < 0.05 ~ "#5584B0",
    between(x = value, left = 0.05, right = 999) ~ "#EB5C53"
  ),
  group = case_when(
    value == 999 ~ "nofill",
    value < 0.05 ~ "bluefill",
    between(x = value, left = 0.05, right = 999) ~ "redfill"
  )
  )

seq_corplot = ggplot(col, aes(reorder(Var1, desc(Var1)), Var2, fill = group))+
  geom_tile()+
  scale_fill_manual(values = c("#5584B0", "transparent", "#EB5C53"),
                    labels = c("p < 0.05", NA, "p > 0.05"),
                    breaks = c("bluefill", "nofill", "redfill"),
                    name = "P-Values")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8, angle = 22.5, hjust = 1),
        axis.title = element_blank(),
        plot.margin = unit(rep(.5, 4), "cm"),
        legend.position = "none")+
  ggtitle("Sequence -- Sig. Diff.")

corplot_grid = cowplot::plot_grid(lit_corplot, seq_corplot, legend,  align = "hv", ncol = 2, nrow = 2)
ggsave(filename = "Output/tables & figures/20231105-corplot_threatlvl.pdf", plot = corplot_grid,
       height = 7, width = 10)

# P-value literature matrix without threatlevels
## For literature
lit_CRPR_group_pmatrix = pwpm(lit_CRPR_group_nb_emmeans, 
                              means = TRUE, 
                              diffs = FALSE, 
                              flip = TRUE, 
                              digits = 20, 
                              pvals = TRUE)

lit_CRPR_group_pmatrix = matrix(data = as.numeric(lit_CRPR_group_pmatrix),
                                nrow = length(lit_CRPR_group_pmatrix[1,]),
                                ncol = length(lit_CRPR_group_pmatrix[,1]),
                                dimnames = list(
                                  c(rownames(lit_CRPR_group_pmatrix)),
                                  c(colnames(lit_CRPR_group_pmatrix))
                                ))

diag(lit_CRPR_group_pmatrix) = 999
lit_CRPR_group_pmatrix[is.na(lit_CRPR_group_pmatrix)] = 999


# Plot Matrix
col = reshape2::melt(lit_CRPR_group_pmatrix) %>%
  as.data.frame() %>% 
  mutate(color = case_when(
    value == 999 ~ "transparent",
    value < 0.05 ~ "#5584B0",
    between(x = value, left = 0.05, right = 999) ~ "#EB5C53"
  ),
  group = case_when(
    value == 999 ~ "nofill",
    value < 0.05 ~ "bluefill",
    between(x = value, left = 0.05, right = 999) ~ "redfill"
  )
  )

lit_group_corplot = ggplot(col, aes(reorder(Var1, desc(Var1)), Var2, fill = group))+
  geom_tile()+
  scale_fill_manual(values = c("#5584B0", "transparent", "#EB5C53"),
                    labels = c("p < 0.05", NA, "p > 0.05"),
                    breaks = c("bluefill", "nofill", "redfill"),
                    name = "P-Values")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8, angle = 22.5, hjust = 1),
        axis.title = element_blank(),
        plot.margin = unit(rep(.5, 4), "cm"),
        legend.position = "none")+
  ggtitle("Literature -- Sig. Diff.")

## For sequences
seq_CRPR_group_pmatrix = pwpm(seq_CRPR_group_nb_emmeans, 
                              means = TRUE, 
                              diffs = FALSE, 
                              flip = TRUE, 
                              digits = 20, 
                              pvals = TRUE)

seq_CRPR_group_pmatrix = matrix(data = as.numeric(seq_CRPR_group_pmatrix),
                                nrow = length(seq_CRPR_group_pmatrix[1,]),
                                ncol = length(seq_CRPR_group_pmatrix[,1]),
                                dimnames = list(
                                  c(rownames(seq_CRPR_group_pmatrix)),
                                  c(colnames(seq_CRPR_group_pmatrix))
                                ))

diag(seq_CRPR_group_pmatrix) = 999
seq_CRPR_group_pmatrix[is.na(seq_CRPR_group_pmatrix)] = 999


# Plot Matrix
col = reshape2::melt(seq_CRPR_group_pmatrix) %>%
  as.data.frame() %>% 
  mutate(color = case_when(
    value == 999 ~ "transparent",
    value < 0.05 ~ "#5584B0",
    between(x = value, left = 0.05, right = 999) ~ "#EB5C53"
  ),
  group = case_when(
    value == 999 ~ "nofill",
    value < 0.05 ~ "bluefill",
    between(x = value, left = 0.05, right = 999) ~ "redfill"
  )
  )

seq_group_corplot = ggplot(col, aes(reorder(Var1, desc(Var1)), Var2, fill = group))+
  geom_tile()+
  scale_fill_manual(values = c("#5584B0", "transparent", "#EB5C53"),
                    labels = c("p < 0.05", NA, "p > 0.05"),
                    breaks = c("bluefill", "nofill", "redfill"),
                    name = "P-Values")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8, angle = 22.5, hjust = 1),
        axis.title = element_blank(),
        plot.margin = unit(rep(.5, 4), "cm"),
        legend.position = "none")+
  ggtitle("Sequence -- Sig. Diff.")

corplot_grid = cowplot::plot_grid(lit_group_corplot, seq_group_corplot, legend,  align = "hv", ncol = 2, nrow = 2)
ggsave(filename = "Output/tables & figures/20231105-corplot_group.pdf", plot = corplot_grid,
       height = 7, width = 10)