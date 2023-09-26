library(tidyverse)
library(plotly)
library(scales)
library(httr)
library(jsonlite)
library(dataRetrieval)
library(sf) # for the map
library(mapview) # for making the interactive plot
library(lubridate)

### USGS `dataRetrieval` R package
# pulls USGS daily ('dv') stream flow data:
usgs <- dataRetrieval::readNWISdv(siteNumbers = c("06752260", "06752280"), # USGS site code for the Poudre River at the Lincoln Bridge and the ELC
                                  parameterCd = "00060", # USGS code for stream flow
                                  startDate = "1999-10-01", # YYYY-MM-DD formatting
                                  endDate = lubridate::ymd(Sys.Date())) %>% # YYYY-MM-DD formatting
  rename(q_cfs = X_00060_00003) %>% # USGS code for stream flow units in cubic feet per second (CFS)
  mutate(Date = lubridate::ymd(Date), # convert the Date column to "Date" formatting using the `lubridate` package
         Site = case_when(site_no == "06752260" ~ "Lincoln", 
                          site_no == "06752280" ~ "Boxelder"))


### CDWR's API



co_water_data <- function(site, start_year = 2000, end_year = lubridate::year(Sys.Date())){
  
  raw_data <- httr::GET(url = paste0("https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsday/?format=json&dateFormat=dateOnly&fields=abbrev%2CmeasDate%2Cvalue%2CmeasUnit&encoding=deflate&abbrev=",site,
                                     "&min-measDate=10%2F01%2F", start_year - 1,
                                     "&max-measDate=09%2F30%2F", end_year))
  
  extracted_data <- httr::content(raw_data, as = "text", encoding = "UTF-8") 
  
  # parse text from JSON to data frame
  final_data <- jsonlite::fromJSON(extracted_data)[["ResultList"]]
  
  return(final_data)
  
}




sites <- c("CLAFTCCO","CLARIVCO","CLANSECO")

cdwr <- sites %>% map(~ co_water_data(site = ., start_year = 2000, end_year = lubridate::year(Sys.Date()))) %>%
  bind_rows() %>%
  rename(q_cfs = value) %>%
  mutate(Date = lubridate::ymd(measDate),
         Site = case_when(abbrev == "CLAFTCCO" ~ "Canyon Mouth",
                          abbrev == "CLARIVCO" ~ "Timnath",
                          abbrev == "CLANSECO" ~ "NF Below Seaman"))





site_pull <- GET(url = "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationSummary=true")
site_content <- httr::content(site_pull)
final_meta <- tibble()
no_station_data <- tibble()
## Functionalize the loop below and return a df named final_meta with all the station data
## and a df named no_station_data with all the stations that don't have any data
for(i in 1:length(site_content[["stationSummaries"]])){
  
  column_names <- c("id", "numId", "name", "elevation", "lat", "long" )
  
  basics <- site_content[["stationSummaries"]][[i]] %>% 
    rbind() %>% 
    as_tibble() %>%
    dplyr::select(id, numId, name, elevation, lat = latitude, long = longitude)%>%
    ## make all data characters rather than lists or factors
    mutate_at(vars(id, numId, name, elevation, lat, long), as.character)
  ## write code to skip station that doesn't have any dataType data 
  if(length(site_content[["stationSummaries"]][[i]][["dataTypes"]]) == 0){
    no_station_data <- bind_rows(no_station_data, basics)
    next
  }
  station_meta <- as.data.frame(do.call(rbind, 
                                        site_content[["stationSummaries"]][[i]][["dataTypes"]] ))%>%
    mutate(dataType = as.character(name)) %>%
    distinct(dataType)%>%
    rowid_to_column()%>%
    cbind(basics)%>%
    select(dataType, numId, id, name, lat, long, rowid)%>%
    pivot_wider(., names_from = dataType, values_from = rowid )%>%
    as_tibble()
  
  final_meta <- bind_rows(final_meta, station_meta)
}
## Use  sf to transform at the locations of the sites in look final and no_station_data
## remove NAs from lat and long columns from look final and no_station_data 
final_meta <- final_meta%>%
  filter(!is.na(lat) & !is.na(long)& lat != "NULL")
no_station_data <- no_station_data%>%
  filter(!is.na(lat)& !is.na(long)& lat != "NULL")

## create columns that classify sites by agency using the name column going off the list "Larimer", "Fort Collins", "Greeley" and "USGS"
# Label other sites as "Other" in the name column
final_meta <- final_meta%>%
  mutate(agency = case_when(
    grepl("Larimer -", name) ~ "Larimer",
    grepl("Fort Collins -", name) ~ "Fort Collins",
    grepl("Greeley -", name) ~ "Greeley",
    grepl("USGS", name) ~ "USGS",
    grepl("Loveland -", name) ~ "Loveland",
    grepl("Weld - ", name) ~ "Weld",
    grepl("Windsor - ", name) ~ "Windsor",
    TRUE ~ "Other"
  ))
#remove the agency name from the name column
final_meta <- final_meta%>%
  mutate(name = gsub("Larimer - ", "", name))%>%
  mutate(name = gsub("Fort Collins - ", "", name))%>%
  mutate(name = gsub("Greeley - ", "", name))%>%
  mutate(name = gsub("USGS ", "", name))%>%
  mutate(name = gsub("Loveland - ", "", name))%>%
  mutate(name = gsub("Weld - ", "", name))%>%
  mutate(name = gsub("Windsor - ", "", name))
## Count the number of sites by agency
final_meta%>%
  group_by(agency)%>%
  count()%>%
  arrange(desc(n))
## Classify sites by data_available, if the site has data in the dischargeRiver and Precip columns, then it is classified as "both". If it has only dischargeRiver data, then it is classified as "discharge". If it has only Precipitation data, then it is classified as "precipitation". If it has neither, then it is classified as "none"
final_meta <- final_meta%>%
  mutate(data_available = case_when(
    !is.na(DischargeRiver) & !is.na(Precip) ~ "Precip and Q",
    !is.na(DischargeRiver) & is.na(Precip) ~ "Q",
    is.na(DischargeRiver) & !is.na(Precip) ~ "Precip",
    is.na(DischargeRiver) & is.na(Precip) ~ "none"
  ))%>%
  rename(Q = DischargeRiver, 
         stage = WaterLevelRiver)







data_downloader <- function(site_nums, parameter){
  
  url <- paste0("https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/",
                "stationSummaries?forOperatorStationDashboard=true&stationNumId=", site_nums,
                "&periodStart=2022-10-01T13:59:00-07:00&periodEnd=", Sys.Date(),
                "T13:59:00-07:00")
  
  request <- httr::GET(url = url)
  
  total_list <- httr::content(request)
  
  parameter_row <- final_meta%>%
    filter(numId == site_nums)%>%
    pull(parameter)%>%
    as.numeric()
  
  
  unlister <- function(parameter_row){
    
    unlisted <- total_list[["stationSummaries"]][[1]][["ts"]][[parameter_row]][["data"]] %>%
      bind_rows() %>%
      mutate(locId = site_nums) %>%
      as.data.frame()
    
    return(unlisted)
    
  }
  
  data <- parameter_row %>%
    map(~ unlister(parameter_row= .)) %>%
    dplyr::bind_rows()%>%
    mutate(param = parameter)
  
  return(data)
  
}

larimer_sites <- tibble(Site = c("Rustic", "Below Poudre Falls", "Whitewater Park", "NF @ Livermore"),
                        site_no = c("11004", "11531", "11009", "6751490"))

#Map over all Q sites and consolidate into single dataframe
larimer <- map2(larimer_sites$site_no, "Q", data_downloader) %>%
  list_rbind()%>%
  mutate(datetime = lubridate::ymd_hms(dt)) %>%
  mutate(Date = lubridate::as_date(datetime)) %>%
  group_by(Date, locId) %>%
  summarize(q_cfs = mean(v, na.rm = TRUE)) %>%
  select(Date, q_cfs, site_no = locId) %>%
  mutate(site_no = as.character(site_no)) %>%
  left_join(larimer_sites, by = "site_no")

data_1 <- bind_rows(usgs,cdwr,larimer) %>% filter(year(Date)==year(Sys.Date()))

data_2 <- bind_rows(usgs,cdwr,larimer) %>%
  mutate(Year = lubridate::year(Date),
         my = yday(Date))
  