library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

total_homeless_data = 
  read_csv("data/export/ahar_coc_level_all_years_total.csv") |> 
  filter( homeless_category == "Overall Homeless",
          subgroup == "Total" )

us_counties = 
  st_as_sf(counties()) |> 
  mutate( col.id = row_number(),
          countyfips = paste0(STATEFP, COUNTYFP),
          county_name = NAMELSAD )
  

coc_geography = 
  st_read("data/raw/coc-geography-2019.gdb",
          options = "METHOD=SKIP") |> 
  transmute( coc_number = COCNUM,
             coc_name = COCNAME,
             row.id = row_number() ) 

crosswalk_coc = 
  coc_geography |> st_drop_geometry() |> select(coc_number, coc_name, row.id)
crosswalk_county = 
  us_counties |> st_drop_geometry() |> select(county_name, countyfips, col.id)

coc_county_crosswalk = 
  as.data.frame(st_intersects(x = coc_geography$Shape, y = us_counties$geometry)) |> 
  merge(crosswalk_coc) |> 
  merge(crosswalk_county) |> 
  select(coc_name, coc_number, county_name, countyfips)

#write_csv(coc_county_crosswalk, "data/export/coc_county_crosswalk_2019.csv")
