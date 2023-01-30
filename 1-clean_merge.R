

## Program:   1-merge.R
## Task:      Download PopuList, V-Party, Parlgov, and CMP data; clean and merge analysis
##            variables; save new data sets.
##
## Project:   pop-class
## Author:    Martin Lukk / 2023-01-11 (created)


# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(manifestoR); mp_setapikey("manifesto_apikey.txt") # NOTE: Obtain API key at https://manifestoproject.wzb.eu/
library(vdemdata) # NOTE: install with devtools::install_github("vdeminstitute/vdemdata")


# 1. Load Data ------------------------------------------------------------
populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-version-2-20200626.xlsx") 
parlgov  <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv")
cmp      <- mp_maindataset(version = "2022a")
vparty   <- vdemdata::vparty


# 2. Clean and Merge Data -------------------------------------------------
populist <-
  populist %>%
  select(populist, populist_start, populist_end,
         # farright, farright_start, farright_end,
         ends_with("_id"), -partyfacts_id) %>% 
  rename(id_manifesto = manifesto_id,
         id_parlgov   = parlgov_id) %>% 
  relocate(starts_with("id")) %>% 
  # Recode variable start/end values to true calendar years
  mutate(across(contains(c("start", "end")),
                ~ case_when(. == 1900 ~ 1989,
                            . == 2100 ~ 2020,
                            TRUE      ~ .))) %>% 
  # Expand time series based on start/end values
  mutate(year = map2(populist_start, populist_end, seq)) %>% 
  select(-populist_start, -populist_end) %>% 
  unnest(cols = year)
  
  

parlgov <- 
  parlgov %>%
  # Harmonize ID variable
  rename(id_parlgov = party_id) %>% 
  select(id_parlgov, country_name, country_id,
         party_name_english, party_name_short,
         left_right, state_market, liberty_authority, eu_anti_pro)
vparty <- 
  vparty %>% 
  select(v2paenname, v2pashname,
         pf_party_id, country_name, e_regionpol, year,
         v2xpa_antiplural, v2xpa_popul, # Note: These variables are indices based on subsequent ones
         v2paanteli, v2papeople, v2paopresp, v2paplur, v2paminor, v2paviol,
         v2paimmig, v2palgbt, v2paculsup, v2parelig, v2pagender, v2pawomlab,
         v2pariglef, v2pawelf, v2paclient,
         starts_with(c("v2pasalie", "v2pagroup", "v2pafunds")),
         v2paactcom, v2pasoctie, v2panom, v2padisa, v2paind,
         ep_antielite_salience, ep_corrupt_salience, ep_members_vs_leadership,
         ep_people_vs_elite, ep_type_populism, ep_type_populist_values,
         ep_v8_popul_rhetoric, ep_v9_popul_saliency, ep_galtan, ep_galtan_salience,
         ep_v6_lib_cons, ep_v7_lib_cons_saliency) %>% 
  rename(id_partyfacts = pf_party_id) %>% 
  filter(year >= 1989, !is.na(id_partyfacts)) %>% 
  arrange(country_name, id_partyfacts, year)

vparty_train <-
  vparty %>% 
  filter(country_name %in% populist_countries) %>% 
  left_join(., populist, by = c("id_partyfacts", "year"), multiple = "first") %>% 
  relocate(v2paenname, country_name, id_partyfacts, year, populist) %>% 
  # Code all parties not featured in Popu-List as "not populist"
  mutate(populist = replace_na(populist, 0))

cmp <- 
  cmp %>% 
  select(countryname, edate, party, partyname, partyabbrev,
         per101:per706) %>% 
  rename(id_manifesto = party)
vparty_test <- 
  vparty %>% 
  filter(!country_name %in% populist_countries) %>% 
  relocate(v2paenname, country_name, id_partyfacts, year)
  

# TODO: Figure out how to incorporate time-varying party data from manifestos
# with time-invariant data from parlgov and populist. Avg manifesto values for
# each party within popu-list time period?

df <- 
  left_join(parlgov, populist, by = "id_parlgov") %>% 
  # Create variable indicating if is party not included in Populist
  mutate(not_in_populist = if_else(is.na(populist), 1, 0))


# 3. Save data file -------------------------------------------------------
saveRDS(df, here("data", "output", "01-populist_parlgov.Rds"))
