# Clean Data Dictionaries

# create list of data files
files <- list.files(
  path = 'Data/Hist_Data',
  pattern = '*.xlsx',
  full.names = T,
)

# Create load Xwalk data
for (file in 1:length(files)) {
  # create year of data
  year = 2000 + as.numeric(ifelse(file < 7, substr(files[file], 31, 32), substr(files[file], 32, 33)))
  # load datadictionary
  df <- read.xlsx(files[file], sheet = 'Drug Use Information', startRow = 5) %>%
    mutate(Year = year)
  
  colnames(df) = c('Brand.Name', 'Generic.Name', 'Drug.Uses', 'Year')
  
  if (year == 2016) {
    df_all = df
    next
  }
  df_all <- rbind(df_all, df)
}

# clean Xwalk
df_use <- df_all %>% 
  mutate(
    # remove all characters after 'NOTE:' strings from drug uses which acts
    # as a legal notice of the summary not containing all information 
    Drug.Uses = toupper(str_squish(str_remove_all(Drug.Uses, '(?s)NOTE:.*$'))),
    # remove stars from brand and generic
    Generic.Name = toupper(str_squish(Generic.Name)),
    Brand.Name = toupper(str_remove_all(str_squish(Brand.Name), "\\*")),
    key = paste(Brand.Name, Generic.Name, sep = "_")
  ) %>% 
  group_by(key) %>% 
  arrange(key, Year) %>%
  select(-Year) %>% 
  distinct() %>% 
  # fill missing drug uses (mostly from 2016)
  fill(Drug.Uses, .direction = 'updown') %>% 
  distinct() %>% 
  rowwise() %>% 
  # add in easy to identify use cases
  mutate(Drug.Uses = case_when(
    is.na(Drug.Uses) ~ 'Missing',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  any(str_detect(Generic.Name, c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  any(str_detect(Brand.Name, c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    str_detect(Brand.Name, toupper('Ulticare|Ultra Comfort|INSULIN')) ~ 'Diabetic Equipment', # Insulin needle brands
    str_detect(Generic.Name, toupper('PRENATAL')) ~ 'Prenatal',
    str_detect(Generic.Name, toupper('SODIUM CHLORIDE')) ~ 'Saline Solution',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('ANTISEPTIC')) ~ 'Steralization Pad',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('GAUZE')) ~ 'Gauze',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('NON-ADHERENT BANDAGE')) ~ 'Non-Adherent Bandage',
    (Drug.Uses == toupper('Drug uses not available') | is.na(Drug.Uses)) &  any(str_detect(Generic.Name,c('VACCINE', 'VACC'))) ~ 'Vaccine',
    Drug.Uses == toupper('Drug uses not available') ~ 'Missing',
    TRUE ~ Drug.Uses)
    ) %>% 
  group_by(key) %>% 
  # filter for distinct observations
  distinct() %>% 
  mutate(count = n()) %>% 
  rowwise() %>% 
  mutate(
    keep = case_when(
      count == 1 ~ 1,
      count > 1 & Drug.Uses != 'Missing' ~ 1,
      TRUE ~ 0)
    ) %>% 
  group_by(key) %>% 
  filter(keep == 1) %>%
  slice(1) %>% 
  ungroup() %>% 
  select(-c(count, keep))
  
# Add Treatment Category  -------------------------------------------------

# Create flat hierarchy Map
hier_lookup <- imap_dfr(hierarchical_map, function(sublist, class_name) {
  imap_dfr(sublist, function(terms, subgroup_name) {
    tibble(Med_Cat = class_name,
           Med_Sub_Cat = subgroup_name,
           Term     = terms)
  })
}) %>%
  # Add Priority
  mutate(Priority = if_else(Term %in% catch_all_terms, 1L, 2L)) %>%
  arrange(desc(Priority))

df_xwalk <- f_add_hierarchical_class_all(df_use, Drug.Uses)


# remove later ------------------------------------------------------------
# TODO 
# go through Missing and update or change to missing
# go through NA and update or change to missing
# roll up small groups into Other or different group such as dentistry w/ 3

# 17 missing with Drug Uses
mis <- df_xwalk %>% filter(is.na(Med_Cat)) %>% group_by(Drug.Uses) %>% 
  summarise(count = n())
mis2 <- df_xwalk %>% filter(is.na(Med_Cat))

# 208 Missing with no Drug Use
mis3 <- df_xwalk %>% 
  filter(Med_Cat == 'Missing') %>% 
  group_by(Generic.Name) %>% 
  summarise(count = n())
mis4 <- df_xwalk %>% 
  filter(Med_Cat == 'Missing') %>% 
  group_by(Brand.Name) %>% 
  summarise(count = n())
mis5 <- df_xwalk %>% filter(Med_Cat == 'Missing')

# Category counts
Cat_counts <- df_xwalk %>% group_by(Med_Cat) %>% 
  summarise(count = n())
# SubCategory counts
S_Cat_counts <- df_xwalk %>% group_by(Med_Sub_Cat) %>% 
  summarise(count = n())



df_xwalk <- df_xwalk %>% 
  select(-c("All_Terms_Matched", "All_Med_Cat", "All_Med_Sub_Cat")) %>% 
  mutate(Med_Cat = if_else(is.na(Med_Cat), 'Missing', Med_Cat),
         Med_Sub_Cat = if_else(is.na(Med_Sub_Cat), 'Missing', Med_Sub_Cat))

# Save Data ---------------------------------------------------------------

write.csv(df_xwalk, file = path_xwalk, row.names=FALSE)
print('Xwalk Cleaned')
