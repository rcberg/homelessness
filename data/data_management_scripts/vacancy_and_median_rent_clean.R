library(ipumsr)
library(tidyverse)
library(tidycensus)
library(janitor)

ddi = read_ipums_ddi("data/raw/usa_00031.xml")

vacancy_codes = 
  ipums_val_labels( ddi, var = 'VACANCY') %>%
  transmute( vacancy = val,
             vacancy_type = lbl)

nationwide_vacancy_data = 
  read_ipums_micro(ddi) %>%
  clean_names() %>%
  filter( gq < 3 ) %>%
  select(-rectype) %>% #rectype is household always
  mutate( across( .fns = as.numeric ),
          vacant = if_else( gq == 0 , 1 , 0 )
  ) %>%
  group_by(
    year,
    vacant
  ) %>%
  summarize( units = sum(hhwt) ) %>%
  group_by( year ) %>%
  mutate( total_units = sum(units),
          vacancy_rate = sum(vacant*units)/total_units
          ) 


state_vacancy_data =
  read_ipums_micro(ddi) %>%
  clean_names() %>%
  filter( gq < 3 ) %>%
  select(-rectype) %>% #rectype is household always
  mutate( across( .fns = as.numeric ),
          vacant = if_else( gq == 0 , 1 , 0 )
  ) %>%
  group_by(
    year,
    statefip,
    vacant
  ) %>%
  summarize( units = sum(hhwt) ) %>%
  merge(
    ipums_val_labels( ddi, var = 'STATEFIP') %>%
      transmute( statefip = val,
                 state_name = lbl)
  ) %>%
  group_by( year, statefip ) %>%
  mutate( total_units = sum(units),
          vacancy_rate = sum(vacant*units)/total_units
          )

msa_vacancy_data = 
  read_ipums_micro(ddi) %>%
  clean_names() %>%
  filter( gq < 3 ) %>%
  select(-rectype) %>% #rectype is household always
  mutate( across( .fns = as.numeric ),
          vacant = if_else( gq == 0 , 1 , 0 )
  ) %>%
  group_by(
    year,
    met2013,
    vacant
  ) %>%
  summarize( units = sum(hhwt) ) %>%
  merge( ipums_val_labels( ddi, var = 'MET2013') %>%
           transmute( met2013 = val,
                      metarea_name = lbl) 
  ) %>%
  group_by( year, met2013, metarea_name ) %>%
  summarise( total_units = sum(units),
          vacancy_rate = sum(vacant*units)/total_units
          )

vacancies_only = 
  read_ipums_micro(ddi) %>%
  clean_names() %>%
  filter( gq == 0 ) %>%
  select( -c(rectype, gq) ) %>%
  mutate( across( .fns = as.numeric ) ) %>%
  group_by( year, vacancy) %>%
  summarize( vacancies = sum(hhwt) ) %>%
  merge( vacancy_codes ) %>%
  group_by( year ) %>%
  mutate( total_vacancies = sum(vacancies),
          vacancy_share = vacancies/total_vacancies )

#write_csv( msa_vacancy_data,
#           "data/export/msa_vacancy_data.csv")
#
#write_csv( state_vacancy_data,
#           "data/export/state_vacancy_data.csv")

msa_rent_function = 
  function(yr){
    
    out_data = 
      get_acs( geography = "metropolitan statistical area/micropolitan statistical area",
               variables = "B25064_001",
               survey = "acs1",
               year = yr ) %>%
      transmute( met2013 = GEOID,
                 year = yr,
                 median_rent = estimate,
                 rent_error = moe )
    
    return(out_data)
  }

msa_rent_data = map_dfr(2015:2019, msa_rent_function)

msa_rent_vacancy_data = 
  merge(
    unique(select(msa_vacancy_data, -c(vacant, units, vacancy_status))),
    msa_rent_data
  ) 

#write_csv( msa_rent_vacancy_data,
#           "data/export/integrated_rent_msa_vacancy_data.csv")
