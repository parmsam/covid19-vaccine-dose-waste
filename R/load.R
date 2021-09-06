# load libraries ----
library(here)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(janitor)

# ensure sub-directory creation ----
check_dir_exist <- function(dir_name){
  library(here)
  if(!dir.exists(here(dir_name)) ){
    dir.create(here(dir_name))
    message(dir_name, "/ created.")
    return(TRUE)
  } else {
    message(dir_name, "/ already exists.")
    return(FALSE)
  }
}
check_proj_subdirs <- function(){
  library(purrr)
  directories_vec <- c("input", "output", "graphics", "test")
  map_chr(directories_vec, check_dir_exist)
}
check_proj_subdirs()

#download excel data ----
excel_url <- "https://adatascienti.st/cdc/wastage.xlsx"
if(!file.exists( here("input/wastage.xlsx") )){
  download.file(excel_url,destfile = here("input/wastage.xlsx") )
}

# get sheet names ----
sheets_vector <- excel_sheets( here("input/wastage.xlsx") )

# import data and ensure type matching ----
# note that some date fields from excel need to be converted to date from Excel numerics
sheet1_data <- readxl::read_xlsx( here("input/wastage.xlsx"), sheet = sheets_vector[1]) %>% 
  mutate(WASTAGE_ORDER_NUMBER = as.numeric(WASTAGE_ORDER_NUMBER),
         WASTAGE_ORDER_LINE_NUMBER = as.numeric(WASTAGE_ORDER_LINE_NUMBER),
         DOSES_SUBMITTED = as.numeric(DOSES_SUBMITTED), 
         LAST_REFRESH_DATE = as.character(janitor::convert_to_datetime(LAST_REFRESH_DATE)),
         WASTAGE_SUBMITTED_DATE = as.character(janitor::convert_to_date(WASTAGE_SUBMITTED_DATE))
         )
sheet2_data <- readxl::read_xlsx( here("input/wastage.xlsx"), sheet = sheets_vector[2]) %>%
  mutate(WASTAGE_ORDER_NUMBER = as.numeric(WASTAGE_ORDER_NUMBER),
         WASTAGE_ORDER_LINE_NUMBER = as.numeric(WASTAGE_ORDER_LINE_NUMBER),
         DOSES_SUBMITTED = as.numeric(DOSES_SUBMITTED), 
         LAST_REFRESH_DATE = as.character(janitor::convert_to_datetime(LAST_REFRESH_DATE)),
         WASTAGE_SUBMITTED_DATE = as.character(janitor::convert_to_date(WASTAGE_SUBMITTED_DATE))
  )

# join datasets ----
wastage0a <- sheet1_data %>%
  bind_rows(sheet2_data) 

# format date fields and add week field ----
wastage0b <- wastage0a %>%
  mutate(DOSES_SUBMITTED = as.numeric(DOSES_SUBMITTED), 
         LAST_REFRESH_DATE = as_datetime(LAST_REFRESH_DATE),
         WASTAGE_SUBMITTED_DATE = as_date(WASTAGE_SUBMITTED_DATE)
         ) %>%
  mutate(WASTAGE_SUBMITTED_WEEK = floor_date(WASTAGE_SUBMITTED_DATE, "week"))

# add estimated cost data ----
## cost based on news articles discussing US government cost in late 2020/early 2021 
cost_pfizer = 19.5
cost_moderna = 15
cost_janssen = 10

wastage = wastage0b %>% mutate(EST_WASTE_COST = case_when(
  str_detect(VAX_MANUFACTURER, regex("pfizer", ignore_case = TRUE)) ~   DOSES_SUBMITTED * cost_pfizer,
  str_detect(VAX_MANUFACTURER, regex("moderna", ignore_case = TRUE)) ~  DOSES_SUBMITTED * cost_moderna,
  str_detect(VAX_MANUFACTURER, regex("janssen", ignore_case = TRUE)) ~  DOSES_SUBMITTED * cost_moderna,
  TRUE ~ 0
))
