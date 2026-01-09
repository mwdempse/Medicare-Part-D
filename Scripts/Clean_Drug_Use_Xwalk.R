# Clean Data Dictionaries

# create list of data files
files <- list.files(
  path = 'Data/Hist_Data',
  pattern = '*.xlsx',
  full.names = T,
)

# List of strings which should be removed for easier processing 
xwalk_remove <- 'NOTE:  This is a summary and does NOT have all possible information about this product. This information does not assure that this product is safe, effective, or appropriate for you. This information is not individual medical advice and does not substitute for the advice of your health care professional. Always ask your health care professional for complete information about this product and your specific health needs.'
xwalk_remove2 <- 'This information does not assure that this product is safe, effective, or appropriate for you. This information is not individual medical advice and does not substitute for the advice of your health care professional. Always ask your health care professional for complete information about this product and your specific health needs.'
xwalk_remove3 <- 'NOTE: This is a summary and does not contain all possible information about this product. For complete information about this product or your specific health needs, ask your healthcare professional. Always seek the advice of your healthcare professional if you have any questions about this product or your medical condition. This information is not intended as individual medical advice and does not substitute for the knowledge and judgment of your healthcare professional.'
xwalk_remove4 <- 'NOTE: This is a summary and does NOT have all possible information about this product. This information does not assure that this product is safe, effective, or appropriate for you. This information is not individual medical advice and does not substitute for the advice of your health care professional. Always ask your health care professional for complete information about this product and your specific health needs.'
xwalk_remove5 <- 'NOTE: This is a summary and does NOT have all possible information about this product. This information does not assure that this product is safe, effective, or appropriate for you. This information is not individual medical advice and does not substitute for the advice of your health care professional. Always ask your health care professional for complete information about this product and your specific health needs.'
xwalk_remove6 <- 'NOTE: This is a summary and does NOT have all possible information about this product.'

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
    # remove strings from drug uses which simply add length
    Drug.Uses = str_remove_all(str_squish(Drug.Uses), xwalk_remove),
    Drug.Uses = str_remove_all(Drug.Uses, xwalk_remove2),
    Drug.Uses = str_remove_all(Drug.Uses, xwalk_remove3),
    Drug.Uses = str_remove_all(Drug.Uses, xwalk_remove4),
    Drug.Uses = str_remove_all(Drug.Uses, xwalk_remove5),
    Drug.Uses = str_remove_all(Drug.Uses, xwalk_remove6),
    Drug.Uses = str_squish(Drug.Uses),
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
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  any(str_detect(Generic.Name,c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  any(str_detect(Brand.Name,c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    Brand.Name %in% c('Ulticare', 'Ultra Comfort') ~ 'Diabetic Equipment', # Insulin needle brands
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('ANTISEPTIC')) ~ 'Steralization Pad',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('GAUZE')) ~ 'Gauze',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(Generic.Name,c('NON-ADHERENT BANDAGE')) ~ 'Non-Adherent Bandage',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  any(str_detect(Generic.Name,c('VACCINE', 'VACC'))) ~ 'Vaccine',
    Drug.Uses == 'Drug uses not available' ~ 'Missing',
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

mis <- df_xwalk %>% filter(is.na(Med_Cat)) %>% group_by(Drug.Uses) %>% 
  summarise(count = n())

test <- df_xwalk %>% group_by(Med_Cat) %>% 
  summarise(count = n())
test2 <- df_xwalk %>% group_by(Med_Sub_Cat) %>% 
  summarise(count = n())

mis2 <- df_xwalk %>% filter(Med_Cat == 'Missing')

mis3 <- df_xwalk %>% filter(is.na(Med_Cat))

df_xwalk <- df_xwalk %>% select(-c("All_Terms_Matched", "All_Med_Cat", "All_Med_Sub_Cat"))

# Save Data ---------------------------------------------------------------

write.csv(df_xwalk, file = path_xwalk, row.names=FALSE)
print('Xwalk Cleaned')
