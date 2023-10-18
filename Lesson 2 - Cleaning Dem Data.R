# Load Packages ----
library(tidyverse)

# Import Data ----
team_mem_dem <- read_csv("SF_Team Member_2023_09_20_scrubbed.csv")
view(team_mem_dem)

# Race/Ethnicity Cleaning ----

#exclusive race/ethnicity categories
  # 1 = White, Non-Hispanic
  # 2 = Asian, Non-Hispanic
  # 3 = Black/African American, Non-Hispanic
  # 4 = Hispanic
  # 5 = Multiracial/Another Race

team_mem_dem <- team_mem_dem |> 
  mutate(race_all = case_when(
    `Hispanic, Latino, or Spanish origin` != "Yes" & `Race - American Indian or Alaska Native` == 0 & `Race - Asian` == 0 & `Race - Black or African American` == 0 & `Race - Hawaiian or Other Pac. Islander` == 0 & `Race - Other` == 0 & `Race - White` == 1 ~ 1,
    `Hispanic, Latino, or Spanish origin` != "Yes" & `Race - American Indian or Alaska Native` == 0 & `Race - Asian` == 1 & `Race - Black or African American` == 0 & `Race - Hawaiian or Other Pac. Islander` == 0 & `Race - Other` == 0 & `Race - White` == 0 ~ 2,
    `Hispanic, Latino, or Spanish origin` != "Yes" & `Race - American Indian or Alaska Native` == 0 & `Race - Asian` == 0 & `Race - Black or African American` == 1 & `Race - Hawaiian or Other Pac. Islander` == 0 & `Race - Other` == 0 & `Race - White` == 0 ~ 3,
    `Hispanic, Latino, or Spanish origin` == "Yes" & (`Race - American Indian or Alaska Native` == 0 & `Race - Asian` == 0 & `Race - Black or African American` == 0 & `Race - Hawaiian or Other Pac. Islander` == 0) & ((`Race - Other` == 1 & `Race - White` == 0) | (`Race - Other` == 0 & `Race - White` == 1)) ~ 4,
    (`Race - American Indian or Alaska Native` + `Race - Asian` + `Race - Black or African American` + `Race - Hawaiian or Other Pac. Islander` + `Race - Other` + `Race - White` > 1) | (`Hispanic, Latino, or Spanish origin` == "Yes" & `Race - American Indian or Alaska Native` == 1) | (`Hispanic, Latino, or Spanish origin` == "Yes" & `Race - Asian` == 1) | (`Hispanic, Latino, or Spanish origin` == "Yes" & `Race - Black or African American` == 1) | (`Hispanic, Latino, or Spanish origin` == "Yes" & `Race - Hawaiian or Other Pac. Islander` == 1) | `Race - American Indian or Alaska Native` == 1 | `Race - Hawaiian or Other Pac. Islander` == 1 | (`Race - Other` == 1 & `Hispanic, Latino, or Spanish origin` != "Yes") ~ 5,
    `Race - American Indian or Alaska Native` == 0 & `Race - Asian` == 0 & `Race - Black or African American` == 0 & `Race - Hawaiian or Other Pac. Islander` == 0 & `Race - Other` == 0 & `Race - White` == 0 & (`Hispanic, Latino, or Spanish origin` == "Prefer not to answer") ~ -99
  ))

#problem  w/ -99 also if Hispanic == blank; calculations need refinement 
  
# gen URM ----
team_mem_dem <- team_mem_dem |> 
  mutate(URM = case_when(
    `Race - American Indian or Alaska Native` == 1 | `Race - Black or African American` == 1 | `Race - Hawaiian or Other Pac. Islander` == 1 | `Race - Other` == 1 | `Hispanic, Latino, or Spanish origin` == "Yes" ~ 1,
    `Hispanic, Latino, or Spanish origin` != "Yes" & `Race - American Indian or Alaska Native` == 0 & `Race - Black or African American` == 0 & `Race - Hawaiian or Other Pac. Islander` == 0 & `Race - Other` == 0 & (`Race - White` == 1 | `Race - Asian` == 1) ~ 0,
  ))

# problem w/ -99 -- need calculation

# gen URG ----
team_mem_dem <- team_mem_dem |> 
  mutate(URG = case_when(
    URM == 1 | Gender == "Female" ~ 1,
    URM == 0 & Gender != "Female" ~ 0
  ))
#problem - still needs missing calculations  
    
    
