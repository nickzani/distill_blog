library(lubridate)
library(httr)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)
library(furrr)
library(data.table)

# Get strava info

client_id_use <-"0438aaa207c8d755116b85687d9e4ecf"
client_secret_use <- "847e8aca-982a-481b-830b-20a633c9aae5"

get_all_metadata <- function(page, token_id){
  
  metadata_url <- "https://public-api.mintecanalytics.com/v2/export/series/mintec/?catMetadata=true"
  
  metadata <- GET(metadata_url, 
                  add_headers(Authorization = paste("Bearer", token_id, sep = " ")),
                  query = list(PageIndex = page))
  
  metadata_content <- content(metadata)[["content"]]
  
  metadata_items <- metadata_content[["items"]]
  
  metadata_df <- rbindlist(metadata_items) #convert from list to dataframe
  
  return(metadata_df)
  
}



metadata_url <- "https://www.strava.com/api/v3/segments/explore?bounds=51.47549147942766,-0.9974239901285384,51.482912826294786,-0.9872414035275116&activity_type=running"

metadata <- GET(metadata_url, 
                add_headers(Authorization = paste("Bearer", token_id, sep = " ")))

metadata_content <- content(metadata)[["segments"]]

metadata_items <- metadata_content[["items"]]

sapply(content(metadata)$segments, function(x) x[["distance"]])


seg_id <- "https://www.strava.com/api/v3/segments/30783645"

segs <- GET(seg_id, 
                add_headers(Authorization = paste("Bearer", token_id, sep = " ")))

seg_xom_dat <- content(segs)[["xoms"]]
seg_xom_dat$overall

# function to return segment IDs

token_id <- "14a27e2875229eb827407ffe6ebeb1e47985d219"

get_seg_ids <- function(start, stop){
  
  url <- paste0("https://www.strava.com/api/v3/segments/explore?bounds=",start,",",stop,"&activity_type=running")
  
  metadata <- GET(url, 
                  add_headers(Authorization = paste("Bearer", token_id, sep = " ")))
  
  metadata_content <- content(metadata)[["segments"]]
  
  data.frame(id = sapply(metadata_content, function(x) x[["id"]]),
             name = sapply(metadata_content, function(x) x[["name"]]),
             distance = sapply(metadata_content, function(x) x[["distance"]]),
             elev = sapply(metadata_content, function(x) x[["elev_difference"]]),
             grade = sapply(metadata_content, function(x) x[["avg_grade"]]))
  
}

segments <- bind_rows(get_seg_ids("51.473387573529315,-1.0046363072087103", "51.4781187198289,-0.9890580374003739"),
                      get_seg_ids("51.46927080973672,-0.987899323117109", "51.47680901668196,-0.9756684499955481"),
                      get_seg_ids("51.47656845485491,-1.0000014499441066", "51.48260884555432,-0.9826207356951362"),
                      get_seg_ids("51.47945511309934,-0.9989285662889237", "51.4882208810044,-0.9809041218825841"),
                      get_seg_ids("51.46523180818228,-0.9740116381924533", "51.47055192322787,-0.9648921276297219"),
                      get_seg_ids("51.47922585598578,-0.9764148974691765", "51.48816536372976,-0.9669520641494366")
                      ) %>% 
  unique()

segments <- bind_rows(get_seg_ids("51.47351816643064,-1.001819961613837", "51.4783675285437,-0.9907140769223064"))

# function to get koms

get_koms <- function(id){
  
  seg_id <- paste0("https://www.strava.com/api/v3/segments/",id)
  
  segs <- GET(seg_id, 
              add_headers(Authorization = paste("Bearer", token_id, sep = " ")))
  
  seg_dat <- content(segs)
  
  seg_xoms <- seg_dat["xoms"]
  
  data.frame(id = seg_dat$id,
             name = seg_dat$name,
             distance = seg_dat$distance,
             ave_grade = seg_dat$average_grade,
             efforts = seg_dat$effort_count,
             kom = seg_xoms$xoms$overall)
  
}

# get_koms("30783645")

seg_koms <- purrr::map_dfr(segments$id, get_koms)

# 2:30 per km = 150s per km

seg_koms <- seg_koms %>% 
  rowwise() %>% 
  mutate(mins = case_when(str_detect(kom, ":") == TRUE ~ as.double(str_match(kom, "(^[0-9]+):")[2]),
                             TRUE ~ 0),
         secs = case_when(str_detect(kom, ":") == TRUE ~ as.double(str_match(kom, ":([0-9]+)$")[2]),
                          TRUE ~ as.double(str_match(kom, "([0-9]+)s")[2]))) %>% 
  mutate(total_secs = (mins*60) + secs,
         speed_ms = distance/total_secs,
         secs_per_km = 1000/speed_ms
         ) %>% 
  select(-mins, -secs) %>% 
  arrange(secs_per_km)

