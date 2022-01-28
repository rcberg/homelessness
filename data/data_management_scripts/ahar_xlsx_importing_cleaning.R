library(tidyverse)
library(readxl)
library(janitor)

coc_categories = 
  read_xlsx("data/raw/2007-2020-PIT-Estimates-by-CoC.xlsx", sheet = 1) %>%
  select( 1, 3 ) # CoC number and size category variables only

write_csv(coc_categories,
          "data/export/coc_size_category_crosswalk.csv")

names(coc_categories) =
  c("coc_number", "coc_category") # even clean_names can't fix these...

ahar_data_functionizer = 
  function( sh ){ # supply a sheet number 2-14 (corresponding to year)
  
  if(sh == 1){ 
    
    in_data = # because we are going to supply the size categories manually
      read_xlsx("data/raw/2007-2020-PIT-Estimates-by-CoC.xlsx", sheet = sh)[,-3] 
    
  }else{ 
    in_data = 
      read_xlsx("data/raw/2007-2020-PIT-Estimates-by-CoC.xlsx", sheet = sh) 
  }
  
  out_data = 
    in_data %>%
    pivot_longer(cols = !1:2 ) %>%
    separate( name, 
              into = c("category_group",
                       "year"), 
              sep = ", ") %>%
    separate( category_group, 
              into = c("homeless_category", ## homelessness category notes: 
                                            ## "TH" = transition housing, 
                                            ## "ES" = emergency shelter, 
                                            ## "SH" = safe havens
                                            ## --(from AHAR 2020 pt 1 report )
                       "subgroup"), 
              sep =  " - ") %>%
    mutate( subgroup  = replace_na(subgroup, "Total") 
    )
  
  names(out_data) = c("coc_number",
                      "coc_name",
                      "homeless_category",
                      "subgroup",
                      "year",
                      "count" )
  return(out_data)
}

ahar_all_years_data = 
  map_df(
    1:14, ahar_data_functionizer
  ) %>%
  left_join(coc_categories) %>%
  mutate( state_abb = 
            str_extract(
              coc_number, 
              boundary(("word")
                       )
              )
  )

write_csv(ahar_all_years_data,
          "data/export/ahar_coc_level_all_years_total.csv")
