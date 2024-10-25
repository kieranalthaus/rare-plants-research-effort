# This handy argument #
`%notin%` <- Negate(`%in%`)

### Overall most sequences and literature
lit_seq %>%
  filter(Literature.Count == max(lit_seq$Literature.Count))
# Arabadpsis thaliana occurs in the most amount of literature

lit_seq %>%
  filter(seqs == max(lit_seq$seqs))
# Hordeum vulgare has the most sequences in GenBank


### Most literature per group
max_lit_per_group <- lit_seq %>%
  group_by(CRPR) %>%
  filter(Literature.Count == max(Literature.Count)) %>%
  as.data.frame() %>%
  select(Scientific.Name, Literature.Count, CRPR)

### Most sequences per group
max_seqs_per_group <- lit_seq %>%
  group_by(CRPR) %>%
  filter(seqs == max(seqs)) %>%
  as.data.frame() %>%
  dplyr::select(Scientific.Name, seqs, CRPR)

### Rare Plant Outliers
lit_outliers_rare <- lit_seq %>%
  filter(CRPR %notin% c("Non-Rare-Native", "weed")) %>%
  arrange(desc(factor(Literature.Count)))

## Non-Rare Native Outliers
lit_outliers_common <- lit_seq %>%
  filter(CRPR == "Non-Rare-Native") %>%
  arrange(desc(factor(Literature.Count)))









