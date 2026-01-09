# Helper Functions and other useful code ----------------------------------

# Paths -------------------------------------------------------------------

path_df <- paste0("Data/Medi_D_Data.csv")
path_xwalk <- paste0("Data/Medi_D_Xwalk.csv")
# path_save <- paste0()

# Hierarchy Map -----------------------------------------------------------

hierarchical_map <- list(
  Anesthesiology = list(
    Perioperative_Anesthesia = c("anesthesia", "sedation", "analgesia", "perioperative", "anesthetic")
  ),
  Cardiology = list(
    Hypertension = c("hypertension", "bp"),
    Lipids = c("cholesterol", "lipid", "statin"),
    Arrhythmia = c("arrhythmia"),
    Vascular_Heart_Disease = c("cardio", "heart", "vascular", "anticoagulant", "anticoagulation", "stroke", "cva")
  ),
  Dermatology = list(
    Acne = c("acne", "isotretinoin", "retinoid"),
    Inflammatory_Dermatoses = c("psoriasis", "eczema", "dermatitis", "rosacea"),
    Fungal_Skin = c("fungal skin infection", "onychomycosis", "fungal infection"),
    Hair_Disorders = c("alopecia"),
    General_Skin = c("derma", "skin", "rash", "urticaria", "hives")
  ),
  Endocrinology = list(
    Diabetes = c("diabetes", 'diabetic', "hyperglycemia", "hypoglycemia"),
    Thyroid = c("thyroid"),
    Obesity_Metabolic = c("weight", "obesity", "endocrine")
  ),
  Gastroenterology = list(
    Inflammatory_Bowel = c("crohn", "ulcer", "ibs", "colitis"),
    Liver = c("liver", "hepatic"),
    Upper_GI = c("gastro", "gi ", "gerd", "reflux"),
    Pancreatobiliary = c("pancreatitis", "gallbladder", "bile duct")
  ),
  Hematology = list(
    General_Blood = c("blood"),
    Anemia = c("anemia"),
    Coagulation = c("coagulation", "clotting", "hematologic")
  ),
  Hospital_Medicine = list(Critical_Care = c("critical care", "icu", "life support", "sepsis", "shock")),
  Infectious_Disease = list(
    General_Infection = c(
      "infection", "bacterial", "viral", "fungal", "parasitic", "antibiotic", 
      "antifungal", "antiparasitic"),
    HIV_AIDS = c("hiv", "antiretroviral"),
    Hepatitis = c("hepatitis"),
    Tuberculosis = c("tuberculosis", "\\btb\\b"),
    Tropical_Infections = c("malaria", "helminth"),
    Respiratory_Infections = c("pneumonia", "influenza", "covid", "staph", "mrsa", "strep")
  ),
  Neurology = list(
    General_Neurology = c("neuro" ,'narcolepsy'),
    Seizure_Disorders = c("seizure", "epilepsy"),
    Headache = c("migraine"),
    Neuropathy = c("neuropathy"),
    Demyelinating_Disease = c("multiple sclerosis"),
    Cognitive_Disorders = c("alzheim"),
    Movement_Disorders = c("parkinson", "motor tics", "restless leg",
                           "dystonia", "ataxia", 'Lou Gehrig',
                           'cerebral palsy', 'muscle disorder',
                           'motor', 'muscle spasms')
  ),
  Obstetrics = list(
    Pregnancy_Peripartum = c("pregnancy", "postpartum", "obstetric", 'METHYLERGONOVINE', 'Prenatal'),
    Fertility_Reproduction = c("fertility", "ivf")
  ),
  Oncology = list(General_Oncology = c("cancer", "tumor", "chemotherapy", "oncology", 'myelogenous leukemia')),
  Ophthalmology = list(General_Ophthalmology = c("eye", "ocular", "glaucoma", "ophthal")),
  Orthopedics = list(
    Musculoskeletal = c("bone", "joint", "musculoskeletal"),
    Arthritis = c("arthritis", "gout"),
    Fracture_Trauma = c("fracture")
  ),
  Pain_Management = list(General_Pain = c("pain", "analgesic", "opioid", "nsaid")),
  Psychiatry = list(
    Mood_Disorders = c("depression", "bipolar", "tca", "maoi", "ssri", "snri"),
    Anxiety_Disorders = c("anxiety", "anxiolytic", "benzodiazepine"),
    Psychotic_Disorders = c("psychosis", "schizophrenia", "antipsychotic", "psych"),
    ADHD = c("adhd", "attention", "deficit"),
    Sleep_Disorders = c("insomnia"),
    General_Mental_Health = c("mental")
  ),
  Pulmonology = list(
    General_Lung = c("lung"),
    General_Respiratory = c("respiratory"),
    Obstructive_Lung_Disease = c("asthma", "copd", "bronchodilator", "emphysema"),
    Interstitial_Lung_Disease = c("ipf", "cystic fibrosis", "lung disease"),
    Lung_Infection = c("lung infection"),
    Bronchitis = c("bronchitis")
  ),
  Radiology = list(Imaging = c("contrast", "radiologic", "imaging")),
  Rheumatology = list(
    Inflammatory_Arthritis = c("rheumatoid", "psoriatic arthritis",
                               "ankylosing spondylitis"),
    Autoimmune_Systemic = c("autoimmune", "lupus", "vasculitis", 'sjogren', 
                            'myasthenia gravis'),
    Crystal_Arthropathy = c("gout")
  ),
  Urology = list(
    Urinary_Tract = c("urinary", "urologic", 'bladder'),
    Kidney_Renal = c("kidney", "renal"),
    Prostate = c("prostate")
  ),
  Vaccine = list(Vaccination = c("vaccine")),
  Dentistry = list(General_Dentistry = c("dental", "tooth", "oral", "periodontal")),
  Otorhinolaryngology = list(General_ENT = c("ent", "ear", "nose", "throat",
                                             "sinus", "otitis")),
  Vitamin = list(Nutritional_Support = c( "vitamin", "deficiency", "supplement",
                                          "nutritional", "replacement" )),
  Other = list(Medical_Equipment = c('Gauze', 'Steralization Pad',
                                     'sodium chloride', 'Saline Solution')),
  Missing = list(Missing = c('missing'))
)


# Catch All Classification Terms ------------------------------------------

catch_all_terms <- c(
  "gastro", "gi ",       # GI broad
  "neuro",               # neurology broad
  "derma", "skin", "rash",
  "cardio", "heart",
  "respiratory", "lung",
  "infection",           # very broad ID
  "cancer", "tumor",
  "blood",
  "pain",
  "mental",
  "dental", "oral",
  "urinary", "urologic",
  "eye",
  "ear", "nose", "throat", 'ent', 'missing'
  )

# Hierarchical Classification ---------------------------------------------

f_hierarchical_class_all <- function(text, lookup = hier_lookup) {
  
  txt <- tolower(ifelse(is.na(text), "", text))
  
  # detect all hits
  hits <- vapply(
    lookup$Term,
    function(p) str_detect(txt, regex(p, ignore_case = TRUE)),
    logical(1)
  )
  
  if (!any(hits)) {
    return(list(
      Med_Cat = NA_character_,
      Med_Sub_Cat = NA_character_,
      Term_Matched = NA_character_,
      All_Terms_Matched = NA,
      All_Med_Cat = NA,
      All_Med_Sub_Cat = NA
    ))
  }
  
  hit_idx <- which(hits)
  
  # Match based on priority
  primary_idx <- hit_idx[1]
  
  list(
    Med_Cat = lookup$Med_Cat[primary_idx],
    Med_Sub_Cat = lookup$Med_Sub_Cat[primary_idx],
    Term_Matched = lookup$Term[primary_idx],
    
    # return all matching terms and categories
    All_Terms_Matched = lookup$Term[hit_idx],
    All_Med_Cat = lookup$Med_Cat[hit_idx],
    All_Med_Sub_Cat = lookup$Med_Sub_Cat[hit_idx]
  )
}


# Add Hierarchical Class --------------------------------------------------

f_add_hierarchical_class_all <- function(data, text_col) {
  text_col <- enquo(text_col)
  
  data %>%
    mutate(
      tmp_text = as.character(!!text_col),
      res      = map(tmp_text, f_hierarchical_class_all),
      
      # primary classification
      Med_Cat     = map_chr(res, "Med_Cat"),
      Med_Sub_Cat     = map_chr(res, "Med_Sub_Cat"),
      Term_Matched = map_chr(res, "Term_Matched"),
      
      # all matched classifications (list columns)
      All_Terms_Matched = map(res, "All_Terms_Matched"),
      All_Med_Cat      = map(res, "All_Med_Cat"),
      All_Med_Sub_Cat      = map(res, "All_Med_Sub_Cat")
    ) %>%
    select(-tmp_text, -res)
}


# Compound Annual Growth Rate ---------------------------------------------

f_cagr <- function(col_name, n) {
  cagr = ((col_name / dplyr::lag(col_name, n)) ^ (1 / n)) - 1
  return(cagr)
}

# Rollup Calculations -----------------------------------------------------

f_rollup <- function(df,
                     groups = c("Manufacturer"),
                     lag_years = 11,
                     year_col = "Year",
                     drop_cols = c("key", "Drug.Uses"),
                     filters = NULL) {
  # 
  stopifnot(is.data.frame(df))
  stopifnot(year_col %in% names(df))
  
  # Missing groups
  missing_groups <- setdiff(groups, names(df))
  if (length(missing_groups) > 0) {
    stop("These `groups` columns are missing from df: ", paste(missing_groups, collapse = ", "))
  }
  
  # Optional filter
  apply_optional_filters <- function(.data, filters) {
    if (is.null(filters) || length(filters) == 0) return(.data)
    
    for (nm in names(filters)) {
      if (nm %in% names(.data) && !is.null(filters[[nm]]) && length(filters[[nm]]) > 0) {
        .data <- dplyr::filter(.data, .data[[nm]] %in% filters[[nm]])
      }
    }
    .data
  }
  
  # NaN/Inf into NA for numeric columns
  na_nan_inf <- function(.data) {
    dplyr::mutate(
      .data,
      dplyr::across(
        where(is.numeric),
        ~ dplyr::if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)
      )
    )
  }
  
  # compute lag-based metrics
  add_growth_metrics <- function(.data) {
    dplyr::mutate(
      .data,
      # Year-to-year deltas
      YtY_Avg_Spend_Dsg = (Avg_Spend_Dsg - dplyr::lag(Avg_Spend_Dsg)) / dplyr::lag(Avg_Spend_Dsg),
      YtY_Avg_Spend_Clm = (Avg_Spend_Clm - dplyr::lag(Avg_Spend_Clm)) / dplyr::lag(Avg_Spend_Clm),
      YtY_Avg_Spend_Ben = (Avg_Spend_Ben - dplyr::lag(Avg_Spend_Ben)) / dplyr::lag(Avg_Spend_Ben),
      YtY_Maude_Count   = (maude_count   - dplyr::lag(maude_count))   / dplyr::lag(maude_count),
      
      # CAGR over lag_years (expects your f_cagr(x, n) to exist)
      CAG_Spend = f_cagr(Total.Spending, lag_years),
      CAG_Dsg = f_cagr(Total.Dosage.Units, lag_years),
      CAG_Clm = f_cagr(Total.Claims, lag_years),
      CAG_Ben = f_cagr(Total.Beneficiaries, lag_years),
      CAG_Maude_Count = f_cagr(maude_count, lag_years),
      
      CAG_Spend_Dsg = f_cagr(Avg_Spend_Dsg, lag_years),
      CAG_Spend_Clm = f_cagr(Avg_Spend_Clm, lag_years),
      CAG_Spend_Ben = f_cagr(Avg_Spend_Ben, lag_years),
      
      # Growth from base (lag_years) to current
      Grow_Spend = (Total.Spending - dplyr::lag(Total.Spending, lag_years)) / dplyr::lag(Total.Spending, lag_years),
      Grow_Dsg = (Total.Dosage.Units - dplyr::lag(Total.Dosage.Units, lag_years)) / dplyr::lag(Total.Dosage.Units, lag_years),
      Grow_Clm = (Total.Claims - dplyr::lag(Total.Claims, lag_years)) / dplyr::lag(Total.Claims, lag_years),
      Grow_Ben = (Total.Beneficiaries - dplyr::lag(Total.Beneficiaries, lag_years)) / dplyr::lag(Total.Beneficiaries, lag_years),
      Grow_Maude_Count = (maude_count - dplyr::lag(maude_count, lag_years)) / dplyr::lag(maude_count, lag_years),
      
      Grow_Avg_Spend_Dsg = (Avg_Spend_Dsg - dplyr::lag(Avg_Spend_Dsg, lag_years)) / dplyr::lag(Avg_Spend_Dsg, lag_years),
      Grow_Avg_Spend_Clm = (Avg_Spend_Clm - dplyr::lag(Avg_Spend_Clm, lag_years)) / dplyr::lag(Avg_Spend_Clm, lag_years),
      Grow_Avg_Spend_Ben = (Avg_Spend_Ben - dplyr::lag(Avg_Spend_Ben, lag_years)) / dplyr::lag(Avg_Spend_Ben, lag_years)
    )
  }
  
  # Apply filters
  df <- apply_optional_filters(df, filters)
  
  # Set defaults for dimensions not in rollup groups
  rollup_defaults <- list(
    Med_Class = if (!("Med_Class" %in% groups) && "Med_Class" %in% names(df)) "Overall" else NULL,
    Brand.Name = if (!("Brand.Name" %in% groups) && "Brand.Name" %in% names(df)) "Overall" else NULL,
    Generic.Name = if (!("Generic.Name" %in% groups) && "Generic.Name" %in% names(df)) "Overall" else NULL,
    Outlier.Flag = if ("Outlier.Flag" %in% names(df)) NA else NULL
  )
  
  rollup_defaults <- rollup_defaults[!vapply(rollup_defaults, is.null, logical(1))]
  
  # Overall rollup at year & groups
  rollup_overall <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(year_col, groups)))) %>%
    dplyr::summarise(
      maude_count         = sum(maude_count, na.rm = TRUE),
      Total.Spending      = sum(Total.Spending, na.rm = TRUE),
      Total.Dosage.Units  = sum(Total.Dosage.Units, na.rm = TRUE),
      Total.Claims        = sum(Total.Claims, na.rm = TRUE),
      Total.Beneficiaries = sum(Total.Beneficiaries, na.rm = TRUE),
      Avg_Spend_Dsg       = Total.Spending / Total.Dosage.Units,
      Avg_Spend_Clm       = Total.Spending / Total.Claims,
      Avg_Spend_Ben       = Total.Spending / Total.Beneficiaries,
      .groups = "drop"
    ) %>%
    # only set "Overall" for dimensions NOT being rolled up on
    dplyr::mutate(!!!rollup_defaults) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, year_col)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    add_growth_metrics() %>%
    dplyr::ungroup() %>%
    na_nan_inf()
  
  
  # base data
  detail <- df %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, year_col)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    add_growth_metrics() %>%
    dplyr::ungroup() %>%
    na_nan_inf()
  
  dplyr::bind_rows(rollup_overall, detail)
}




# f_rollup <- function(df, group = "", n_years = 11) {
#   
#   # create Overall
#   part1 <- df %>%
#     group_by_at(c(group, 'Year'))%>%
#     summarise(
#       T_ManFac_Count = sum(T_ManFac_Count, na.rm = T),
#       Tot_Spndng = sum(Tot_Spndng, na.rm = T),
#       Tot_Dsg_Unts = sum(Tot_Dsg_Unts, na.rm = T),
#       Tot_Clms = sum(Tot_Clms, na.rm = T),
#       Tot_Benes = sum(Tot_Benes, na.rm = T),
#       Avg_Spnd_Dsg = Tot_Spndng / Tot_Dsg_Unts,
#       Avg_Spnd_Clm = Tot_Spndng / Tot_Clms,
#       Avg_Spnd_Ben = Tot_Spndng / Tot_Benes,
#     ) %>%
#     group_by_at(group) %>%
#     mutate(
#       # Year to Year change in Total Manfac
#       YtY_Change_ManFac = (T_ManFac_Count - dplyr::lag(T_ManFac_Count)) / dplyr::lag(T_ManFac_Count),
#       
#       # Year to Year average change in spending
#       YtY_Avg_Spnd_Per_Dsg = (Avg_Spnd_Dsg - dplyr::lag(Avg_Spnd_Dsg)) / dplyr::lag(Avg_Spnd_Dsg),
#       YtY_Avg_Spnd_Per_Clm = (Avg_Spnd_Clm - dplyr::lag(Avg_Spnd_Clm)) / dplyr::lag(Avg_Spnd_Clm),
#       YtY_Avg_Spnd_Per_Ben = (Avg_Spnd_Ben - dplyr::lag(Avg_Spnd_Ben)) / dplyr::lag(Avg_Spnd_Ben),
#       
#       # Calculate Growth Base to current
#       Grow_Over_Spnd_12_23 = (Tot_Spndng - dplyr::lag(Tot_Spndng,n_years))/dplyr::lag(Tot_Spndng,n_years),
#       Grow_Over_Dsg_12_23 = (Tot_Dsg_Unts - dplyr::lag(Tot_Dsg_Unts,n_years))/dplyr::lag(Tot_Dsg_Unts,n_years),
#       Grow_Over_Clm_12_23 = (Tot_Clms - dplyr::lag(Tot_Clms,n_years))/dplyr::lag(Tot_Clms,n_years),
#       Grow_Over_Ben_12_23 = (Tot_Benes - dplyr::lag(Tot_Benes,n_years))/dplyr::lag(Tot_Benes,n_years),
#       Grow_Over_Man_12_23 = (T_ManFac_Count - dplyr::lag(T_ManFac_Count,n_years))/dplyr::lag(T_ManFac_Count,n_years),
#       Grow_Over_Avg_Spnd_Dsg_12_23 = (Avg_Spnd_Dsg - dplyr::lag(Avg_Spnd_Dsg, n_years))/dplyr::lag(Avg_Spnd_Dsg,n_years),
#       Grow_Over_Avg_Spnd_Clm_12_23 = (Avg_Spnd_Clm - dplyr::lag(Avg_Spnd_Clm, n_years))/dplyr::lag(Avg_Spnd_Clm,n_years),
#       Grow_Over_Avg_Spnd_Ben_12_23 = (Avg_Spnd_Ben - dplyr::lag(Avg_Spnd_Ben, n_years))/dplyr::lag(Avg_Spnd_Ben,n_years),
#       
#       # Calculate CoBen_12_23mpound Annual Growth Rate by year
#       Grow_Spnd_12_23 = f_cagr(Tot_Spndng,n_years),
#       Grow_Dsg_12_23 = f_cagr(Tot_Dsg_Unts,n_years),
#       Grow_Clm_12_23 = f_cagr(Tot_Clms,n_years),
#       Grow_Ben_12_23 = f_cagr(Tot_Benes,n_years),
#       Grow_Spnd_Per_Dsg_12_23 = f_cagr(Avg_Spnd_Dsg, n_years),
#       Grow_Spnd_Per_Clm_12_23 = f_cagr(Avg_Spnd_Clm, n_years),
#       Grow_Spnd_Per_Ben_12_23 = f_cagr(Avg_Spnd_Ben, n_years),
#       Grow_ManFac_12_23 = f_cagr(T_ManFac_Count, n_years)
#     ) %>% 
#     # Replace Infinite (divided by 0) And NaN to NA
#     mutate_all(~if_else(is.nan(.), NA, .)) %>% 
#     mutate_all(~if_else(is.infinite(.), NA, .))
#   
#   part2 <- df  %>% 
#     group_by(Year) %>% 
#     summarise(
#       Med_Group = 'Overall',
#       T_ManFac_Count = sum(T_ManFac_Count, na.rm = T),
#       Tot_Spndng = sum(Tot_Spndng, na.rm = T),
#       Tot_Dsg_Unts = sum(Tot_Dsg_Unts, na.rm = T),
#       Tot_Clms = sum(Tot_Clms, na.rm = T),
#       Tot_Benes = sum(Tot_Benes, na.rm = T),
#       Avg_Spnd_Dsg = Tot_Spndng / Tot_Dsg_Unts,
#       Avg_Spnd_Clm = Tot_Spndng / Tot_Clms,
#       Avg_Spnd_Ben = Tot_Spndng / Tot_Benes,
#     ) %>%
#     mutate(
#       # Year to Year change in Total Manfac
#       YtY_Change_ManFac = (T_ManFac_Count - dplyr::lag(T_ManFac_Count)) / dplyr::lag(T_ManFac_Count),
#       
#       # Year to Year average change in spending
#       YtY_Avg_Spnd_Per_Dsg = (Avg_Spnd_Dsg - dplyr::lag(Avg_Spnd_Dsg)) / dplyr::lag(Avg_Spnd_Dsg),
#       YtY_Avg_Spnd_Per_Clm = (Avg_Spnd_Clm - dplyr::lag(Avg_Spnd_Clm)) / dplyr::lag(Avg_Spnd_Clm),
#       YtY_Avg_Spnd_Per_Ben = (Avg_Spnd_Ben - dplyr::lag(Avg_Spnd_Ben)) / dplyr::lag(Avg_Spnd_Ben),
#       
#       # Calculate Growth Base to current
#       Grow_Over_Spnd_12_23 = (Tot_Spndng - dplyr::lag(Tot_Spndng,n_years))/dplyr::lag(Tot_Spndng,n_years),
#       Grow_Over_Dsg_12_23 = (Tot_Dsg_Unts - dplyr::lag(Tot_Dsg_Unts,n_years))/dplyr::lag(Tot_Dsg_Unts,n_years),
#       Grow_Over_Clm_12_23 = (Tot_Clms - dplyr::lag(Tot_Clms,n_years))/dplyr::lag(Tot_Clms,n_years),
#       Grow_Over_Ben_12_23 = (Tot_Benes - dplyr::lag(Tot_Benes,n_years))/dplyr::lag(Tot_Benes,n_years),
#       Grow_Over_Man_12_23 = (T_ManFac_Count - dplyr::lag(T_ManFac_Count,n_years))/dplyr::lag(T_ManFac_Count,n_years),
#       Grow_Over_Avg_Spnd_Dsg_12_23 = (Avg_Spnd_Dsg - dplyr::lag(Avg_Spnd_Dsg, n_years))/dplyr::lag(Avg_Spnd_Dsg,n_years),
#       Grow_Over_Avg_Spnd_Clm_12_23 = (Avg_Spnd_Clm - dplyr::lag(Avg_Spnd_Clm, n_years))/dplyr::lag(Avg_Spnd_Clm,n_years),
#       Grow_Over_Avg_Spnd_Ben_12_23 = (Avg_Spnd_Ben - dplyr::lag(Avg_Spnd_Ben, n_years))/dplyr::lag(Avg_Spnd_Ben,n_years),
#       
#       # Calculate CoBen_12_23mpound Annual Growth Rate by year
#       Grow_Spnd_12_23 = f_cagr(Tot_Spndng, n_years),
#       Grow_Dsg_12_23 = f_cagr(Tot_Dsg_Unts, n_years),
#       Grow_Clm_12_23 = f_cagr(Tot_Clms, n_years),
#       Grow_Ben_12_23 = f_cagr(Tot_Benes, n_years),
#       Grow_Spnd_Per_Dsg_12_23 = f_cagr(Avg_Spnd_Dsg,  n_years),
#       Grow_Spnd_Per_Clm_12_23 = f_cagr(Avg_Spnd_Clm,  n_years),
#       Grow_Spnd_Per_Ben_12_23 = f_cagr(Avg_Spnd_Ben,  n_years),
#       Grow_ManFac_12_23 = f_cagr(T_ManFac_Count, n_years)
#     ) %>% 
#     # Replace Infinite (divided by 0) And NaN to NA
#     mutate_all(~if_else(is.nan(.), NA, .)) %>% 
#     mutate_all(~if_else(is.infinite(.), NA, .))
#   
#   result = rbind(part1,part2)
#   return(result)
#   
# }

# end note ----------------------------------------------------------------

print('Help has arrived')


