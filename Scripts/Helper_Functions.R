# Helper Functions and other useful code ----------------------------------


# Paths -------------------------------------------------------------------


path_df <- paste0("Data/Medi_D_Data.csv")
path_xwalk <- paste0("Data/Medi_D_Xwalk.csv")
# path_save <- paste0()


# Classification Map ------------------------------------------------------

classification_map <- list(
  Anesthesiology = c("anesthesia", "sedation", "analgesia", "perioperative", "anesthetic"),
  Cardiology = c("cardio", "heart", "arrhythmia", "hypertension", "bp", "cholesterol", "lipid", "statin", "vascular"),
  Dermatology = c("derma", "skin", "acne", "psoriasis", "eczema", "rash",
                  "alopecia", 'isotretinoin','retinoid', 'psoriasis', 'dermatitis',
                  'rosacea', 'fungal skin infection', 'onychomycosis'),
  Endocrinology = c("diabetes", "hyperglycemia", "hypoglycemia", "thyroid", "weight", "obesity", "endocrine"),
  Gastroenterology = c("gastro", "liver", "hepatic", "gi ", "crohn", "ulcer", "ibs", "colitis"),
  Hematology = c("blood", "anemia", "coagulation", "clotting", "hematologic"),
  Hospital_Medicine = c("critical care", "icu", "life support", "sepsis", "shock"),
  Infectious_Disease = c("infection", "antibiotic", "virus", "viral", "bacterial",
                         "fungal", "antifungal", "hiv", "hepatitis",
                         'antiretroviral', 'tuberculosis','\\btb\\b', 'parasitic',
                         'malaria','helminth','antiparasitic'),
  Neurology = c("neuro", "seizure", "epilepsy", "migraine", "neuropathy", "parkinson", "multiple sclerosis"),
  Obstetrics = c("pregnancy", "postpartum", "obstetric", "fertility", "ivf"),
  Oncology = c("cancer", "tumor", "chemotherapy", "oncology"),
  Ophthalmology = c("eye", "ocular", "glaucoma", "ophthal"),
  Orthopedics = c("bone", "joint", "arthritis", "fracture", "musculoskeletal"),
  Otorhinolaryngology = c("ent", "ear", "nose", "throat", "sinus", "otitis"),
  Pain_Management = c("pain", "analgesic", "opioid", "nsaid"),
  Psychiatry = c("adhd", "alzheimer", "anxiety", "anxiolytic", "anticonvulsant",
                 "antipsychotic", "attention", "benzodiazepine", "bipolar",
                 "chorea", "deficit", "dementia", "depression", "epilepsy",
                 "huntington", "insomnia", "maoi", "mental", "migraine",
                 "monoclonal", "motor tics", "narcolepsy", "parkinson", "psych",
                 "psychosis", "restless leg", "schizophrenia", "seizure",
                 "sleep disorder", "snri", "ssri", "tca", "tremor", "tricyclic"),
  Pulmonology = c("lung", "pulmonary", "asthma", "copd", "respiratory",
                  'bronchodilator', 'emphysema', 'ipf', 'cystic fibrosis',
                  'lung disease', 'lung infection'),
  Radiology = c("contrast", "radiologic", "imaging"),
  Rheumatology = c("rheumatoid", "autoimmune", "lupus", "gout"),
  Urology = c("urinary", "kidney", "renal", "prostate", "urologic"),
  Vaccine = c('vaccine'),
  Dentistry = c("dental", "tooth", "oral", "periodontal"),
  Vitamin = c("vitamin","deficiency", "supplement", "nutritional", "replacement")
  # Other = c(),
)


# Generic Renaming --------------------------------------------------------



# Classify Drug Use -------------------------------------------------------

f_classify_drug_use <- function(text_vec) {
  
  text_vec_lower <- tolower(text_vec)
  
  # class name if matched, else NA
  mapped <- imap(classification_map, function(keywords, class_name) {
    pattern <- paste(keywords, collapse = "|")
    str_detect(text_vec_lower, pattern)
  })
  
  # Convert list of logical vectors to a matrix
  match_matrix <- do.call(cbind, mapped)
  
  # Get the first TRUE per row -> corresponding class name
  apply(match_matrix, 1, function(row) {
    idx <- which(row)
    if (length(idx) == 0) {
      return("Other")
    } else {
      return(names(classification_map)[idx[1]])
    }
  })
}

# Compound Annual Growth Rate ---------------------------------------------

f_cagr <- function(col_name, n) {
  cagr = ((col_name / lag(col_name, n)) ^ (1 / n)) - 1
  return(cagr)
}

# Rollup Calculations -----------------------------------------------------

f_rollup <- function(df, group = "", n_years = 11) {
  
  # create Overall
  part1 <- df %>%
    group_by_at(c(group, 'Year'))%>%
    summarise(
      T_ManFac_Count = sum(T_ManFac_Count, na.rm = T),
      Tot_Spndng = sum(Tot_Spndng, na.rm = T),
      Tot_Dsg_Unts = sum(Tot_Dsg_Unts, na.rm = T),
      Tot_Clms = sum(Tot_Clms, na.rm = T),
      Tot_Benes = sum(Tot_Benes, na.rm = T),
      Avg_Spnd_Dsg = Tot_Spndng / Tot_Dsg_Unts,
      Avg_Spnd_Clm = Tot_Spndng / Tot_Clms,
      Avg_Spnd_Ben = Tot_Spndng / Tot_Benes,
    ) %>%
    group_by_at(group) %>%
    mutate(
      # Year to Year change in Total Manfac
      YtY_Change_ManFac = (T_ManFac_Count - lag(T_ManFac_Count)) / lag(T_ManFac_Count),
      
      # Year to Year average change in spending
      YtY_Avg_Spnd_Per_Dsg = (Avg_Spnd_Dsg - lag(Avg_Spnd_Dsg)) / lag(Avg_Spnd_Dsg),
      YtY_Avg_Spnd_Per_Clm = (Avg_Spnd_Clm - lag(Avg_Spnd_Clm)) / lag(Avg_Spnd_Clm),
      YtY_Avg_Spnd_Per_Ben = (Avg_Spnd_Ben - lag(Avg_Spnd_Ben)) / lag(Avg_Spnd_Ben),
      
      # Calculate Growth Base to current
      Grow_Over_Spnd_12_23 = (Tot_Spndng - lag(Tot_Spndng,n_years))/lag(Tot_Spndng,n_years),
      Grow_Over_Dsg_12_23 = (Tot_Dsg_Unts - lag(Tot_Dsg_Unts,n_years))/lag(Tot_Dsg_Unts,n_years),
      Grow_Over_Clm_12_23 = (Tot_Clms - lag(Tot_Clms,n_years))/lag(Tot_Clms,n_years),
      Grow_Over_Ben_12_23 = (Tot_Benes - lag(Tot_Benes,n_years))/lag(Tot_Benes,n_years),
      Grow_Over_Man_12_23 = (T_ManFac_Count - lag(T_ManFac_Count,n_years))/lag(T_ManFac_Count,n_years),
      Grow_Over_Avg_Spnd_Dsg_12_23 = (Avg_Spnd_Dsg - lag(Avg_Spnd_Dsg, n_years))/lag(Avg_Spnd_Dsg,n_years),
      Grow_Over_Avg_Spnd_Clm_12_23 = (Avg_Spnd_Clm - lag(Avg_Spnd_Clm, n_years))/lag(Avg_Spnd_Clm,n_years),
      Grow_Over_Avg_Spnd_Ben_12_23 = (Avg_Spnd_Ben - lag(Avg_Spnd_Ben, n_years))/lag(Avg_Spnd_Ben,n_years),
      
      # Calculate CoBen_12_23mpound Annual Growth Rate by year
      Grow_Spnd_12_23 = f_cagr(Tot_Spndng,n_years),
      Grow_Dsg_12_23 = f_cagr(Tot_Dsg_Unts,n_years),
      Grow_Clm_12_23 = f_cagr(Tot_Clms,n_years),
      Grow_Ben_12_23 = f_cagr(Tot_Benes,n_years),
      Grow_Spnd_Per_Dsg_12_23 = f_cagr(Avg_Spnd_Dsg, n_years),
      Grow_Spnd_Per_Clm_12_23 = f_cagr(Avg_Spnd_Clm, n_years),
      Grow_Spnd_Per_Ben_12_23 = f_cagr(Avg_Spnd_Ben, n_years),
      Grow_ManFac_12_23 = f_cagr(T_ManFac_Count, n_years)
    ) %>% 
    # Replace Infinite (divided by 0) And NaN to NA
    mutate_all(~if_else(is.nan(.), NA, .)) %>% 
    mutate_all(~if_else(is.infinite(.), NA, .))
  
  part2 <- df  %>% 
    group_by(Year) %>% 
    summarise(
      Med_Group = 'Overall',
      T_ManFac_Count = sum(T_ManFac_Count, na.rm = T),
      Tot_Spndng = sum(Tot_Spndng, na.rm = T),
      Tot_Dsg_Unts = sum(Tot_Dsg_Unts, na.rm = T),
      Tot_Clms = sum(Tot_Clms, na.rm = T),
      Tot_Benes = sum(Tot_Benes, na.rm = T),
      Avg_Spnd_Dsg = Tot_Spndng / Tot_Dsg_Unts,
      Avg_Spnd_Clm = Tot_Spndng / Tot_Clms,
      Avg_Spnd_Ben = Tot_Spndng / Tot_Benes,
    ) %>%
    mutate(
      # Year to Year change in Total Manfac
      YtY_Change_ManFac = (T_ManFac_Count - lag(T_ManFac_Count)) / lag(T_ManFac_Count),
      
      # Year to Year average change in spending
      YtY_Avg_Spnd_Per_Dsg = (Avg_Spnd_Dsg - lag(Avg_Spnd_Dsg)) / lag(Avg_Spnd_Dsg),
      YtY_Avg_Spnd_Per_Clm = (Avg_Spnd_Clm - lag(Avg_Spnd_Clm)) / lag(Avg_Spnd_Clm),
      YtY_Avg_Spnd_Per_Ben = (Avg_Spnd_Ben - lag(Avg_Spnd_Ben)) / lag(Avg_Spnd_Ben),
      
      # Calculate Growth Base to current
      Grow_Over_Spnd_12_23 = (Tot_Spndng - lag(Tot_Spndng,n_years))/lag(Tot_Spndng,n_years),
      Grow_Over_Dsg_12_23 = (Tot_Dsg_Unts - lag(Tot_Dsg_Unts,n_years))/lag(Tot_Dsg_Unts,n_years),
      Grow_Over_Clm_12_23 = (Tot_Clms - lag(Tot_Clms,n_years))/lag(Tot_Clms,n_years),
      Grow_Over_Ben_12_23 = (Tot_Benes - lag(Tot_Benes,n_years))/lag(Tot_Benes,n_years),
      Grow_Over_Man_12_23 = (T_ManFac_Count - lag(T_ManFac_Count,n_years))/lag(T_ManFac_Count,n_years),
      Grow_Over_Avg_Spnd_Dsg_12_23 = (Avg_Spnd_Dsg - lag(Avg_Spnd_Dsg, n_years))/lag(Avg_Spnd_Dsg,n_years),
      Grow_Over_Avg_Spnd_Clm_12_23 = (Avg_Spnd_Clm - lag(Avg_Spnd_Clm, n_years))/lag(Avg_Spnd_Clm,n_years),
      Grow_Over_Avg_Spnd_Ben_12_23 = (Avg_Spnd_Ben - lag(Avg_Spnd_Ben, n_years))/lag(Avg_Spnd_Ben,n_years),
      
      # Calculate CoBen_12_23mpound Annual Growth Rate by year
      Grow_Spnd_12_23 = f_cagr(Tot_Spndng, n_years),
      Grow_Dsg_12_23 = f_cagr(Tot_Dsg_Unts, n_years),
      Grow_Clm_12_23 = f_cagr(Tot_Clms, n_years),
      Grow_Ben_12_23 = f_cagr(Tot_Benes, n_years),
      Grow_Spnd_Per_Dsg_12_23 = f_cagr(Avg_Spnd_Dsg,  n_years),
      Grow_Spnd_Per_Clm_12_23 = f_cagr(Avg_Spnd_Clm,  n_years),
      Grow_Spnd_Per_Ben_12_23 = f_cagr(Avg_Spnd_Ben,  n_years),
      Grow_ManFac_12_23 = f_cagr(T_ManFac_Count, n_years)
    ) %>% 
    # Replace Infinite (divided by 0) And NaN to NA
    mutate_all(~if_else(is.nan(.), NA, .)) %>% 
    mutate_all(~if_else(is.infinite(.), NA, .))
  
  result = rbind(part1,part2)
  return(result)
  
}

# end note ----------------------------------------------------------------

print('Help has arrived')


