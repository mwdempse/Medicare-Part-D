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
  year = 2000 + as.numeric(ifelse(file < 7,
                                  substr(files[file],31,32),
                                  substr(files[file],32,33)))
  # load datadictionary
    df <- read.xlsx(files[file],
                    sheet = 'Drug Use Information',
                    startRow = 5
    ) %>% 
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
    Brand.Name = str_remove_all(str_squish(Brand.Name), "\\*"),
    Generic.Name = str_remove_all(str_squish(Generic.Name), "\\*"),
    key = paste(str_squish(Brand.Name),str_squish(Generic.Name), sep = "_")
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
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  any(str_detect(toupper(Generic.Name),c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  any(str_detect(toupper(Brand.Name),c('DIABETIC','DIABETC','INSUL', 'INSULN', 'INSULIN'))) ~ 'Diabetic Equipment',
    Brand.Name %in% c('Ulticare', 'Ultra Comfort') ~ 'Diabetic Equipment', # Insulin needle brands
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(toupper(Generic.Name),c('ANTISEPTIC')) ~ 'Steralization Pad',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(toupper(Generic.Name),c('GAUZE')) ~ 'Gauze',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(toupper(Generic.Name),c('NON-ADHERENT BANDAGE')) ~ 'Non-Adherent Bandage',
    (Drug.Uses == 'Drug uses not available' | is.na(Drug.Uses)) &  str_detect(toupper(Generic.Name),c('VACCINE')) ~ 'Vaccine',
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
  select(-c(key, count, keep))
  

# Add Treatment Category  -------------------------------------------------
df_xwalk <- df_use %>%
  mutate(
    Med_Class = f_classify_drug_use(Drug.Uses),
    Brand.Name = str_squish(Brand.Name),
    Generic.Name = str_squish(Generic.Name),
    Brand.Name = str_remove_all(Brand.Name, "\\*"),
    key = paste(Brand.Name, Generic.Name, sep = "_")
  )


# test <- df_xwalk %>% group_by(Med_Class) %>% summarise(n())
# test2 <- df_xwalk %>% filter(Drug.Uses != 'Missing')%>% group_by(Med_Class) %>% summarise(n())
# other <- df_xwalk %>% filter(Med_Class == 'Other')


# Save Data ---------------------------------------------------------------

write.csv(df_xwalk, file = path_xwalk, row.names=FALSE)
print('Xwalk Cleaned')
