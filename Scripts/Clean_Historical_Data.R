# Clean Historical Data

# create list of data files
files <- list.files(
  path = 'Data/Hist_Data',
  pattern = '*.xlsx',
  full.names = T,
)

for (file in 1:length(files)) {
  
  # create year of data
  year = 2000 + as.numeric(ifelse(file < 7,
                                  substr(files[file],31,32),
                                  substr(files[file],32,33)))
  # years 2016 and 2017 data do not have the outlier variable and need special treatment
  if (year < 2018) {
    # ignore 2017 as all data from 2017 is in other data sets
    if (year == 2017) {
      next
    }
    
    # load data
    df <- read.xlsx(files[file],
                    sheet = 'Manufacturer Summary',
                    startRow = ifelse(year %in% c(2017),6,5)
    )
    # remove unneeded columns 
    df <- df[,1:(length(df) - 2)]
    # rename with appropriate year
    colnames(df) <- c(colnames(df)[1:3],
                      paste(colnames(df)[4:10], year-4, sep = '_'),
                      paste(gsub('.{2}$', '', colnames(df)[(4:10)+7]), year-3, sep = '_'),
                      paste(gsub('.{2}$', '', colnames(df)[(4:10)+7*2]), year-2, sep = '_'),
                      paste(gsub('.{2}$', '', colnames(df)[(4:10)+7*3]), year-1, sep = '_'),
                      paste(gsub('.{2}$', '', colnames(df)[(4:10)+7*4]), year, sep = '_')
    )
    # convert to numeric and pivot data
    df <- df %>% 
      mutate(across(!colnames(df)[1:3], as.numeric)) %>% 
      pivot_longer(
        cols = matches("\\d{4}$"), #-c(Brand.Name, Generic.Name, Manufacturer),
        names_to = c(".value", "Year"),
        names_sep = '_'
      )
    
    # add to df_all
    df_all <- df %>% 
      # keep only 2012 and 2014 as 2018 has data from 2014-2018
      filter(Year %in% c(2012,2013)) %>% 
      mutate(Outlier.Flag = NA)
    next
  }
  
  # load data
  df <- read.xlsx(files[file],
                  sheet = 'Manufacturer Summary',
                  startRow = ifelse(year %in% c(2017),6,5)
  )
  # remove unneeded columns 
  df <- df[,1:(length(df) - 2)]
  # rename with appropriate year
  colnames(df) <- c(colnames(df)[1:3],
                    paste(colnames(df)[4:11], year-4, sep = '_'),
                    paste(gsub('.{2}$', '', colnames(df)[(4:11)+8]), year-3, sep = '_'),
                    paste(gsub('.{2}$', '', colnames(df)[(4:11)+8*2]), year-2, sep = '_'),
                    paste(gsub('.{2}$', '', colnames(df)[(4:11)+8*3]), year-1, sep = '_'),
                    paste(gsub('.{2}$', '', colnames(df)[(4:11)+8*4]), year, sep = '_')
  )
  # convert to numeric and pivot data
  df <- df %>% 
    mutate(across(!colnames(df)[1:3], as.numeric)) %>% 
    pivot_longer(
      cols = matches("\\d{4}$"), #-c(Brand.Name, Generic.Name, Manufacturer),
      names_to = c(".value", "Year"),
      names_sep = '_'
    )
  
  if (year == 2018) {
    df_all <- rbind(df_all,df)
    next
  }
    df_all <- rbind(df_all,
                    df %>% filter(Year == year))
  
}

df_all <- df_all %>% 
  mutate(
    Generic.Name = toupper(str_squish(Generic.Name)),
    Brand.Name = toupper(str_remove_all(str_squish(Brand.Name), "\\*")),
    Manufacturer = toupper(str_squish(str_remove_all(Manufacturer, "\\*"))),
    key = paste(Brand.Name, Generic.Name, sep = "_")
  ) %>%
  rename(
    Avg_Spend_Dsg = `Average.Spending.Per.Dosage.Unit.(Weighted)`,
    Avg_Spend_Clm = Average.Spending.Per.Claim,
    Avg_Spend_Ben = Average.Spending.Per.Beneficiary
  )

# Save Data ---------------------------------------------------------------

write.csv(df_all, file = path_df, row.names=FALSE)
print('Historical Data Cleaned')