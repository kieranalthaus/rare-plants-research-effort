## ----Load Packages-------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(cowplot)
library(viridis)
library(gridExtra)
library(ggpubr)


## ----Lit Seq Dotplot-----------------------------------------------------------------
# Plot sequences vs literature and plot the slope 
seq_lit_plot_dotplot <- ggplot(lit_seq, aes(x = log10(seqs+1), y =log10(Literature.Count+1)))+
  xlab("Sequences")+
  ylab("Amount of Literature")+
  geom_smooth(method = "lm", se = TRUE, size = 1, alpha = 0.4)+
  geom_point(fill = "grey20",
    alpha = 0.4, size = 2)+
  
  scale_x_continuous(breaks = 1:7, labels = c("10", "100", "1000", "10000", "100000", "1000000", "10000000"))+
  scale_y_continuous(breaks = 1:7, labels = c("10", "100", "1000", "10000", "100000", "1000000", "10000000"))+
  annotation_logticks(alpha = 0.4, sides = "lb")+
  
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "black",size = 0.2), 
          axis.ticks.y = element_line(color = "black",size = 0.2),
          axis.text.y = element_text(), 
          axis.title.y = element_text(color = "black"),
          
          axis.text.x = element_text(size=8, angle = 22.5, hjust = 1))+
  stat_cor(method = "pearson", label.x = 0, label.y = 6)+
  geom_abline(slope = 1, linetype = "dashed")
seq_lit_plot_dotplot

# Save the figure
png("../Output/tables & figures/seqs-literature-correlation-log.png", width = 800, height = 493)
seq_lit_plot_dotplot
dev.off()


## ----Literature CRPR Boxplot---------------------------------------------------------
lit_CRPR.group_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR.group, y = log10(Literature.Count + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4, fill = viridis::turbo(8))+
  xlab(NULL)+
  ylab("Number of papers")+
  ggtitle("(a)")+
  
  scale_y_continuous(breaks = 1:6, labels = c("10", "100", "1000", "10000", "100000", "1000000"))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1))
lit_CRPR.group_boxplot


## ----Sequqnce CRPR Boxplot-----------------------------------------------------------
seq_CRPR.group_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR.group, y = log10(seqs + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4, fill = viridis::turbo(8))+
  xlab(NULL)+
  ylab("Number of sequences")+
  ggtitle("(b)")+
  
  scale_y_continuous(breaks = 1:6, labels = c(10, 100, 1000, 10000, "100000", "1000000"))+
  annotation_logticks(sides = "l", alpha = 0.4)+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1))
seq_CRPR.group_boxplot


## ----Save the CRPR Seq-Literature Boxplot--------------------------------------------
png("../Output/tables & figures/seqs_literature_CRPR.group_boxplots.png", height = 900, width = 900)
boxplot_relationships <- grid.arrange(lit_CRPR.group_boxplot, seq_CRPR.group_boxplot,
                                      ncol = 2, nrow = 2, widths = c(3.5, 3.5), heights = c(3.5, 3.5))
dev.off()


## ----Literature Boxplots w/ Threat Rank----------------------------------------------
lit_CRPR_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR, y = log10(Literature.Count + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4, fill = viridis::turbo(14))+
  xlab(NULL)+
  ylab("Number of papers")+
  ggtitle("(a)")+
  
  scale_y_continuous(breaks = 1:6, labels = c("10", "100", "1000", "10000", "100000", "1000000"))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1))
lit_CRPR_boxplot


## ----Sequence Boxplot w/ Threat Rank-------------------------------------------------
seq_CRPR_boxplot <- lit_seq %>%
  ggplot(aes(x = CRPR, y = log10(seqs + 1)))+
  geom_jitter(alpha = 0.4, color = "grey30", cex = 0.1)+
  geom_boxplot(alpha = 0.4, outlier.color = "black", outlier.alpha = 0.4, fill = viridis::turbo(14))+
  xlab(NULL)+
  ylab("Number of sequences")+
  ggtitle("(b)")+
  
  scale_y_continuous(breaks = 1:6, labels = c("10", "100", "1000", "10000", "100000", "1000000"))+
  annotation_logticks(alpha = 0.4, sides = "l")+
  
  theme_minimal()+
  theme(axis.line.y = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.line.x = element_line(color = "grey20", size = 0.4, linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.1))
seq_CRPR_boxplot

## ----Save CRPR Lit/Seq boxplots w/ Threat risk---------------------------------------
png("../Output/tables & figures/seqs_literature_CRPR_boxplots.png", height = 900, width = 900)
grid.arrange(lit_CRPR_boxplot, seq_CRPR_boxplot,
                                      ncol = 2, nrow = 2, widths = c(3.5, 3.5), heights = c(3.5, 3.5))

dev.off()


## ------------------------------------------------------------------------------------
knitr::purl("02_rareplant_figures.R")

