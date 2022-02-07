library(opendatatoronto)
library(dplyr)


# get package
package <- show_package("4db2d9d9-6590-41e6-bf2b-b9188436d044")
package

# get all resources for this package
resources <- list_package_resources("4db2d9d9-6590-41e6-bf2b-b9188436d044")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

library(tidyverse)
library(janitor)
library(tidyr)
library(knitr)

cleaned_data <- clean_names(data)

cleaned_data <- cleaned_data %>% drop_na(neighbourhood)

cleaned_data <- 
  cleaned_data |> 
  select(neighbourhood
  )

counted_data <- 
  cleaned_data |> 
  count(neighbourhood)

counted_data <- 
  counted_data |> 
  separate(col = neighbourhood,
           into = c('neighbourhood', 'numeric'),
           sep = ', ') |> 
  select(-numeric)

counted_data |> 
  ggplot(mapping = aes(x = neighbourhood, y = n, fill = n)) +
  geom_bar(stat="identity") + 
  labs(title = "Substance Use Treatment in Toronto Neighbourhoods", 
       x = "Neighbourhood Name", 
       y = "Agency Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )


table <- counted_data |>
  kable(caption = "Substance Use Treatment in Toronto Neighbourhoods", 
        col.names = c("Neighbourhood Name", "Agency Count"),
        digits = 1,
        booktabs = TRUE,
        linesep = "")



library("ggplot2")
theme_set(theme_bw())
library("sf")

data_geo <- 
  data |> 
  select(geometry
  )

g_wells <- ggplot(data = data_geo) +
  geom_sf(aes(colour = "red"), size = 3)
g_wells


g_wells_new <- ggplot(data = data_geo) +
  geom_sf(aes(colour = "red"), size = 10) +
  geom_sf_label(
    data = data,
    aes(label = NEIGHBOURHOOD),
    size = 2,
    color = "blue"
  ) +
  theme(legend.position = "none")
g_wells_new 




library(opendatatoronto)
library(dplyr)

# get package
package_neigh <- show_package("6e19a90f-971c-46b3-852c-0c48c436d1fc")
package_neigh

# get all resources for this package
resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data_neigh <- filter(datastore_resources, row_number()==1) %>% get_resource()
data_neigh

clean_neigh <- clean_names(data_neigh)

clean_neigh <- 
  clean_neigh |> 
  select(category, topic, characteristic, niagara, church_yonge_corridor,
         trinity_bellwoods, annex, bay_street_corridor, cabbagetown_south_st_james_town, 
         moss_park, regent_park, roncesvalles, york_university_heights, bathurst_manor,
         blake_jones, dorset_park, downsview_roding_cfb, glenfield_jane_heights,
         high_park_swansea, ionview, islington_city_centre_west, junction_area,
         kensington_chinatown, mimico_includes_humber_bay_shores, mount_pleasant_west,
         north_riverdale, oakridge, steeles, waterfront_communities_the_island, west_humber_clairville,
         woburn, wychwood
  )


clean_neigh_fin <-
  clean_neigh[c(3, 12, 13, 14, 1153, 1151, 1711, 1891),]

clean_neigh_fin_2 <-
  tail(t(clean_neigh_fin) , -2)

neigh_table <- clean_neigh_fin_2 |>
  kable(caption = "Toronto Neighbourhood Characteristics", 
        col.names = c("Population, 2016", "Working Age (25-54 years", "Pre-retirement (55-64 years)",
                      "Seniors (65+ years)", "Immigrants", "Total immigrants",
                      "Bachelor's Degree", "Unemployment Rate"),
        booktabs = TRUE,
        linesep = "")
neigh_table

