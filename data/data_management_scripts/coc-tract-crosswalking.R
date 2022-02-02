library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

total_homeless_data = 
  read_csv("data/export/ahar_coc_level_all_years_total.csv") |> 
  filter( homeless_category == "Overall Homeless",
          subgroup == "Total" )



tract_function = 
  function( st ){
    data = tracts( state = st )
    return(data)
  }

us_census_tracts = 
  map_dfr(c(state.name, "District of Columbia"), tract_function) |> 
  transmute(  tract_id = GEOID,
              tract_name = NAMELSAD,
              statefips = STATEFP,
              countyfips = COUNTYFP,
              col.id = row_number() 
           )

coc_geography = 
  st_read("data/raw/coc-geography-2019.gdb",
          options = "METHOD=SKIP") |> 
  transmute( coc_number = COCNUM,
             coc_name = COCNAME,
             row.id = row_number() ) 

crosswalk_coc = 
  coc_geography |> st_drop_geometry() |> select(coc_number, coc_name, row.id)
crosswalk_tract = 
  us_census_tracts |> st_drop_geometry() |> select(tract_id, tract_name, statefips, countyfips, col.id)

coc_tract_crosswalk = 
  as.data.frame(st_intersects(x = coc_geography$Shape, y = us_census_tracts$geometry)) |> 
  merge(crosswalk_coc) |> 
  merge(crosswalk_tract) |> 
  select(coc_name, coc_number, tract_name, tract_id, statefips, countyfips)

write_csv(coc_tract_crosswalk, "data/export/coc_tract_crosswalk_2019.csv")
