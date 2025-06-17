# LUCID/CINT TRACKER - BMW # 

pacman::p_load(tidyverse, janitor, here, glue, gt)

# PROCESSING CINT DATA ----------------------------------------------------

# 1. SPECIFY LOCATION OF THE DATA FILE
file_location <- "~/R/bmw/brand_tracker_v2/cint_data/bmw_brandtracker_may2025.xlsx"

# 2. CHOOSE YOUR BRAND (BMW, TESLA, MERCEDES, AUDI, LEXUS)
brand <- "BMW"

# 3. DO YOU WANT TO FILTER DOWN TO A SPECIFIC GROUP

# Growth Audience Groups: AAPI (demo_race_ethnicity); Women (demo_gender); Millennials & Gen Z (genz_millen)
my_groups <- NULL
my_groups <- "demo_race_ethnicity"

# 4. SOURCE THE WRAPPER FUNCTION 
source(here::here("R", "cint_wrapper.R"))

# 4b. IF JUST NEED THE DATA LOADED 
source(here::here("R", "read_cint.R"))
read_cint(file_location)

# 5. RUN THE FUNCTION (DATA IS PROCESS AND ALL DIRECTORIES ARE CREATED)
cint_wrapper(file_location, brand = brand, sig_thresh = .90, my_groups = my_groups) 

# CHECK ON WHERE RESPONDENTS ARE COMING FROM (CHANNEL)
df |> 
  count(matched_control_digital, map_adtype, map_campaign, map_channel, map_site) |> 
  tp()

df |> 
  count(matched_control_digital, map_channel, map_site) |> 
  tp()

df |> 
  count(matched_control_xmedia, platform_m_four) |> 
  filter(platform_m_four != "NULL") |> 
  group_by(matched_control_xmedia) |> 
  mutate(total = sum(n))
