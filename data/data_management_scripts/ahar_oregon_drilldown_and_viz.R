library(tidyverse)

oregon_ahar_pit = 
  read_csv(
    "data/export/ahar_coc_level_all_years_total.csv"
  ) |> 
  filter( state_abb == "OR" )

oregon_unsheltered = 
  oregon_ahar_pit %>%
  filter( homeless_category == "Unsheltered Homeless",
          subgroup == "Total",
          coc_number != "OR-504", # marion/polk lacks most years' data,
          coc_number != "OR-505" )%>%
  group_by( coc_number, coc_name ) %>%
  mutate( unsheltered_count = count ,
          w_2007 = if_else( year == 2007, 1, 0 ),
          w_2015 = if_else( year == 2015, 1, 0 ),
          count07 = sum(count*w_2007),
          count15 = sum(count*w_2015),
          unsheltered_index_07 = (count/count07 - 1)*100,
          unsheltered_index_15 = (count/count15 - 1)*100
  )   %>%
  select( coc_number, coc_category, coc_name, year, unsheltered_count, unsheltered_index_07, unsheltered_index_15 ) 

oregon_sheltered = 
  oregon_ahar_pit %>%
  filter( homeless_category == "Sheltered Total Homeless",
          subgroup == "Total",
          coc_number != "OR-504", # marion/polk lacks most years' data,
          coc_number != "OR-505" ) %>%
  select( coc_number, coc_category, coc_name, year, count ) %>%
  group_by( coc_number, coc_name ) %>%
  mutate( sheltered_count = count ,
          w_2007 = if_else( year == 2007, 1, 0 ),
          w_2015 = if_else( year == 2015, 1, 0 ),
          count07 = sum(count*w_2007),
          count15 = sum(count*w_2015),
          sheltered_index_07 = (count/count07 - 1)*100,
          sheltered_index_15 = (count/count15 - 1)*100
  ) %>%
  select( coc_number, coc_category, coc_name, year, sheltered_count, sheltered_index_07, sheltered_index_15 ) 

oregon_homeless = 
  merge( oregon_sheltered,
         oregon_unsheltered ) %>%
  mutate( unsheltered_ratio = sheltered_count/unsheltered_count )

oregon_unsheltered %>% 
  filter( year > 2014 ) %>%
  ggplot( 
    aes(x = year, y = unsheltered_count, color = coc_name)
  ) + 
  geom_line( size = 1 ) + 
  labs( x = "Year" , y = "PIT Count", color = "CoC name", title = "Oregon unsheltered homelessness") +
  theme_minimal()

oregon_sheltered %>% 
  filter( year > 2014 ) %>%
  ggplot( 
    aes(x = year, y = sheltered_count, color = coc_name)
  ) + 
  geom_line( size = 1 ) + 
  labs( x = "Year" , y = "PIT Count", color = "CoC name", title = "Oregon sheltered homelessness") +
  theme_minimal()

oregon_homeless %>% 
  filter( year > 2014 ) %>%
  ggplot( 
    aes( x = year, y = unsheltered_ratio )
  ) + 
  geom_line( size = 1) + 
  geom_hline( yintercept = 1, linetype = 2, size = 1, color = "#00BFC4"  ) +
  scale_y_continuous(labels = scales::percent_format() ) + 
  labs( x = "Year" , 
        y = "Sheltered/unsheltered ratio", 
        color = "CoC name", 
        title = "Oregon sheltered/unsheltered ratio",
        caption = "Lower percentage reflects higher unsheltered homelessness" ) +
  facet_wrap(~coc_name, ncol = 3) + 
  theme_minimal()
