### Load Packages
library(tidyverse);library(data.table)

### Load data
lit_seq <- fread('Data/20220710-CA-literature-seqs-complete.csv') # Here is the MOSTLY complete dataframe

cnps_1a2a <- read_csv('Data/CNPS_Results_2022_07_17_T1354_pst.csv') %>% # Load in CNPS rank 1a data
  rename("Scientific.Name" = "ScientificName") #Rename columsn to better match the lit_seq df``

### Merge these two datasets together
lit_seq <- merge(lit_seq, cnps_1a2a[,c("Scientific.Name", "CRPR")], by = "Scientific.Name", all.x = TRUE) %>%
  as.data.frame()

### Add the values from the CNPS 1A and 2A into the dataset
lit_seq$CRPR.x <- ifelse((is.na(lit_seq$CRPR.y) == FALSE),
         lit_seq$CRPR.y,
         lit_seq$CRPR.x)

### Delete the redundant column and rename CRPR.x to CRPR
lit_seq <- lit_seq %>%
  select("Scientific.Name","seqs","Species.Group", "CRPR.x", "Literature.Count","area") %>%
  rename("CRPR" = "CRPR.x")

### Adding broader categories to dataframe
# First just the rare plant risks
lit_seq <- lit_seq %>%
  mutate(CRPR.group = dplyr::case_when(
    CRPR == "1A" ~ "1A",
    
    CRPR == "1B.1" ~ "1B",
    CRPR == "1B.2" ~ "1B",
    CRPR == "1B.3" ~ "1B",
    
    CRPR == "2A" ~ "2A",
    
    CRPR == "2B.1" ~ "2B",
    CRPR == "2B.2" ~ "2B",
    CRPR == "2B.3" ~ "2B",
    
    CRPR == "3" ~ "3",
    
    CRPR == "4.1" ~ "4",
    CRPR == "4.2" ~ "4",
    CRPR == "4.3" ~ "4",
    
    CRPR == "native" ~ "Non-Rare Native",
    CRPR == "weed" ~ "Weed"
  ))

lit_seq$CRPR <- ifelse(lit_seq$CRPR == "native",
                  "Non-Rare-Native",
                  lit_seq$CRPR)

# Then general plant ranks
lit_seq <- lit_seq %>%
  mutate(general.status = dplyr::case_when(
    CRPR == "native" ~ "Natives",
    CRPR == "weed" ~ "Weeds"
  ))
lit_seq$general.status[is.na(lit_seq$general.status)] = "Rare"

### Summary Statistics ###
lit_seq %>%
  group_by(CRPR) %>%
  tally()
# 5173 native species
# 1027 WEEDY SPECIES FROM THE CAL-IPC WEEDY PLANT LIST

# Write Complete data
write_csv(lit_seq, paste0('Output/',Sys.Date(),'-CA_lit_seq_complete.csv'))
