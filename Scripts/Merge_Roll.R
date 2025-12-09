
# load data ---------------------------------------------------------------
df_hist <- read.csv(path_df)
df_xwalk <- read.csv(path_xwalk)


# Merge Classifications -----------------------------------------------------

df_all <- df_hist %>% 
  left_join(df_xwalk %>% 
              select(-c(Brand.Name, Generic.Name)),
            by = 'key') %>% 
  mutate(
    # remove all * in names
    Brand.Name = str_remove_all(Brand.Name, "\\*"),
    Manufacturer = str_remove_all(Manufacturer, "\\*"),
    # update generic names
    Generic.Name = case_when(
      Generic.Name == 'Sodium Fluoride' ~ 'Fluoride (Sodium)',
      T ~ Generic.Name
      ),
    # add maude_count for rollups
    maude_count = 1
  )

# check missings
colSums(is.na(df_all))

# Rollups -----------------------------------------------------------------

df_maude_rollup <- rbind(
  df_all %>%
    group_by(Year, Manufacturer) %>%
    summarise(
      maude_count = sum(maude_count, na.rm = T),
      Total.Spending = sum(Total.Spending, na.rm = T),
      Total.Dosage.Units = sum(Total.Dosage.Units, na.rm = T),
      Total.Claims = sum(Total.Claims, na.rm = T),
      Total.Beneficiaries = sum(Total.Beneficiaries, na.rm = T),
      Avg_Spend_Dsg = Total.Spending / Total.Dosage.Units,
      Avg_Spend_Clm = Total.Spending / Total.Claims,
      Avg_Spend_Ben = Total.Spending / Total.Beneficiaries,
      Med_Class = 'Overall',
      Brand.Name = 'Overall',
      Generic.Name = 'Overall',
      Outlier.Flag = NA,
      
      # Year to Year average change in spending
      YtY_Avg_Spend_Dsg = (Avg_Spend_Dsg - lag(Avg_Spend_Dsg)) / lag(Avg_Spend_Dsg),
      YtY_Avg_Spend_Clm = (Avg_Spend_Clm - lag(Avg_Spend_Clm)) / lag(Avg_Spend_Clm),
      YtY_Avg_Spend_Ben = (Avg_Spend_Ben - lag(Avg_Spend_Ben)) / lag(Avg_Spend_Ben),
      YtY_Maude_Count = (maude_count - lag(maude_count)) / lag(maude_count),
      
      # Calculate Compound Annual Growth Rate by year,
      CAG_Spend = f_cagr(Total.Spending, 11),
      CAG_Dsg = f_cagr(Total.Dosage.Units, 11),
      CAG_Clm = f_cagr(Total.Claims, 11),
      CAG_Ben = f_cagr(Total.Beneficiaries, 11),
      CAG_Maude_Count = f_cagr(maude_count, 11),
      
      CAG_Spend_Dsg = f_cagr(Avg_Spend_Dsg, 11),
      CAG_Spend_Clm = f_cagr(Avg_Spend_Clm, 11),
      CAG_Spend_Ben = f_cagr(Avg_Spend_Ben, 11),
      
      # Calculate Growth Base to current
      Grow_Spend = (Total.Spending - lag(Total.Spending, 11)) / lag(Total.Spending, 11),
      Grow_Dsg = (Total.Dosage.Units - lag(Total.Dosage.Units, 11)) / lag(Total.Dosage.Units, 11),
      Grow_Clm = (Total.Claims - lag(Total.Claims, 11)) / lag(Total.Claims, 11),
      Grow_Ben = (Total.Beneficiaries - lag(Total.Beneficiaries, 11)) / lag(Total.Beneficiaries, 11),
      Grow_Maude_Count = (maude_count - lag(maude_count, 11)) / lag(maude_count, 11),
      
      Grow_Avg_Spend_Dsg = (Avg_Spend_Dsg - lag(Avg_Spend_Dsg, 11)) / lag(Avg_Spend_Dsg, 11),
      Grow_Avg_Spend_Clm = (Avg_Spend_Clm - lag(Avg_Spend_Clm, 11)) / lag(Avg_Spend_Clm, 11),
      Grow_Avg_Spend_Ben = (Avg_Spend_Ben - lag(Avg_Spend_Ben, 11)) / lag(Avg_Spend_Ben, 11),
      
    ) %>%
    ungroup() %>% 
    # Replace Infinite (divided by 0) And NaN to NA
    mutate_all(~if_else(is.nan(.), NA, .)) %>% 
    mutate_all(~if_else(is.infinite(.), NA, .)),
  df_all %>%
    mutate(
      
    )
    select(-c(key, Drug.Uses))
) 

df_generic_rollup <- df_all %>%
  f_rollup(group = c('Med_Group', 'Med_Use','Gnrc_Name'))


# Save Data ---------------------------------------------------------------

list_df <- list(
  'MAUDE' = df_maude_rollup,
  'Gerneric' = df_generic_rollup,
  'Med Use' = df_Med_Use_rollup,
  'Med Group' = df_Med_Group_rollup
)


print('Merged and Rolluped')
