
# About this Script__________________________________________________________________

# Author: Afentu divine Tamaraebi
#.Purpose: Masters in Global Public Health Digital Health Examination 
# This script for for cleaning, visualizing and analysing a data on Obesity in Children



# Load Packages -------------------------------------------------------------------------

install.packages("pacman")
pacman::p_load(
  rio,          # for importing data
  here,         # for file paths
  janitor,      # for data cleaning
  lubridate,    # for working with dates
  tidyverse,    # for data management
  here,          # sets file path
  dplyr,
  ggplot2,
  shiny
)

# Import data using the here function
   here("data", "raw")
   here("data", "raw obesity_raw.csv", "")
   
 # import the file from the "data" and "raw" subfolders
   obesity_raw <- import(here("data", "raw", "obesity_raw.csv"))
   
 # View basic structure and summary of the dataset
   
   str(obesity_raw)
   names(obesity_raw)
 
   # Identify and print all character columns
   
   character_columns <- names(obesity_raw)[sapply(obesity_raw, is.character)]
   print(character_columns)
   
   # Identify and print all numeric columns
   numeric_columns <- names(obesity_raw)[sapply(obesity_raw, is.character)]
   

   # Check and loop column classes
 
  columns_to_check <- c("Gender", "Age", "Height", "Weight", "family_history_with_overweight",
                            "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC", "FAF", "TUE",
                            "CALC", "MTRANS")      
  for (column_name in columns_to_check) {
    print(paste(column_name, ":", class(obesity_raw[[column_name]])))
  }
  tabyl(obesity_raw, FAVC)

 
  obesity<-obesity_raw

# Clean the column names
  obesity <- obesity_raw %>% 
    clean_names() %>%  # Clean the column names
    rename(  # Manually edit column names
      fam_hist = family_history_with_overweight,
      high_cal_food = favc,
      veg_cons = fcvc,
      meal_count = ncp,
      snack_freq = caec,
      smoker = smoke,
      water_intake = ch2o,
      cal_mon = scc,
      phys_act = faf,
      tech_use = tue,
      alc_intake = calc,
      trans_mode = mtrans
    ) %>% 
  mutate(bmi = weight / (height ^ 2)) %>%
  mutate(bmi_category = case_when(
    bmi >= 18.5 & bmi < 25  ~ "Normal Weight",
    bmi >= 25   & bmi < 30  ~ "Overweight",
    bmi >= 30   & bmi < 35  ~ "Obesity Class I (Moderate)",
    bmi >= 35   & bmi < 40  ~ "Obesity Class II (Severe)",
    bmi >= 40              ~ "Obesity Class III (Morbid)",
    TRUE                   ~ "Underweight"  # Catch-all for BMI < 18.5
  )) %>%
  group_by(gender,fam_hist, bmi_category) %>%
  summarise(count = n(), groups = 'drops')
  n
  names(obesity)
obesity <-obesity_raw %>%             
  #de duplicate rows
distinct()


    
  #rlang::last_trace()
  class(obesity$bmi)
  
  obesity$age <- round(obesity$age)
  obesity$weight <- round(obesity$weight)
  obesity$bmi <- round(obesity$bmi)
  
  
  # Round certain numerical columns to 2 decimal places
  obesity$veg_cons <- round(obesity$veg_cons, digits = 2)
  obesity$meal_count <-round(obesity$meal_count, digits= 2)  
  obesity$water_intake <-round(obesity$water_intake,digits = 2)
  obesity$height <-round(obesity$height, digits = 2)
  obesity$phys_act <-round(obesity$phys_act, digits = 2)
  obesity$tech_use <-round(obesity$tech_use, digits = 2)
  obesity$bmi<-round(obesity$bmi)   
  
   #Round off age and weight to an integer
  obesity$weight<-round(obesity$weight)
  obesity$age<-round(obesity$age)
  
  summary(obesity$weight)  # Check weight
  summary(obesity$height)  # Check height summary(your_data$weight)  # Check weight
  
obesity_summary <- obesity %>% 
  group_by(gender, fam_hist, bmi_category) %>%
  summarise(count = n())
  
  obesity %>%
    tabyl(gender)
  


# Run the app
shinyApp(ui = ui, server = server)
  