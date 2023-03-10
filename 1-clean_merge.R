

## Program:   1-clean_merge.R
## Task:      Download PopuList, V-Party, Parlgov, and CMP data; clean and merge analysis
##            variables; save new data sets.
##
## Project:   pop-class
## Author:    Sakeef Karim and Martin Lukk / 2023-01-11 (created)


# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(rio)
library(vdemdata) # NOTE: install with devtools::install_github("vdeminstitute/vdemdata")
# library(manifestoR); mp_setapikey("manifesto_apikey.txt") # NOTE: Obtain API key at https://manifestoproject.wzb.eu/


# 1. Load Data ------------------------------------------------------------
populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-version-2-20200626.xlsx") 
vparty   <- vdemdata::vparty
# parlgov  <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv")
# cmp      <- mp_maindataset(version = "2022a")


# 2. Clean and Merge Data -------------------------------------------------
# Get vector of countries included in Popu-List
populist_countries <- 
  populist %>% 
  select(country_name) %>% 
  group_by(country_name) %>% 
  slice_head() %>% 
  pull()

# Supply missing PartyFacts IDs for select Popu-List parties
missing_populist_ids <- 
  tribble(
    ~party_name_english, ~partyfacts_id,
    "Croatian Civic Party", 6620,                                                 
    "Fidesz - Hungarian Civic Alliance", 1691,                                     
    "Fidesz -- Hungarian Civic Party / Christian Democratic People's Party", 6366,
    "Southern Action League", 8647,                                                
    "Enough!", 8182,                                                               
    "Geneva Citizens' Movement", 8176,                                            
    "Respect -- The Unity Coalition", 1082,
    "Croatian Party of Rights -- Dr. Ante Starcevic", 3706,
    "Alternative Ecologists", 8645,
    "Lithuanian Communist Party on the CPSU Platform", 7420,
    "Aragonese Council", 1564
  )

populist <-
  populist %>%
  # Replace missing PartyFacts IDs for some parties coded populist
  rows_update(., missing_populist_ids, by = "party_name_english") %>% 
  # Drop duplicate entries with same PartyFacts ID (and remaining NAs)
  distinct(partyfacts_id, .keep_all = T) %>% 
  select(farright, farright_start, farright_end,
         farleft, farleft_start, farleft_end,
         ends_with("_id")) %>% 
  rename(id_manifesto = manifesto_id,
         id_parlgov   = parlgov_id,
         id_partyfacts = partyfacts_id) %>% 
  relocate(starts_with("id")) %>% 
  # Recode variable start/end values to true calendar years
  mutate(across(contains(c("start", "end")),
                ~ case_when(. == 1900 ~ 1989,
                            . == 2100 ~ 2020,
                            TRUE      ~ .))) %>% 
  # Expand time series based on start/end values
  mutate(
    year = if_else(farright == 1, map2(farright_start, farright_end, seq), NA),
    year = if_else(farleft  == 1 , map2(farleft_start, farleft_end, seq),  year)
  ) %>% 
  filter(!(farright == 0 & farleft == 0)) %>% 
  select(-ends_with("start"), -ends_with("end")) %>% 
  unnest(cols = year)

vparty <- 
  vparty %>% 
  select(v2paenname, v2pashname,
         pf_party_id, country_name, e_regionpol, year,
         v2pavote, v2panumbseat,
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

# Identify VParty parties that may not meet Popu-list inclusion criteria (either 1 seat of 2% of votes since 1989)
vparty_exclude <-   
  vparty %>% 
  filter(country_name %in% populist_countries) %>% 
  group_by(v2paenname) %>% 
  summarize(maxvote = max(v2pavote, na.rm = T),
            maxseat = max(v2panumbseat, na.rm = T)) %>% 
  filter((maxvote < 2 | is.na(maxvote)) & (maxseat < 1 | is.na(maxseat))) %>%
  ungroup() %>% 
  pull(v2paenname)

vparty_train <-
  vparty %>% 
  filter(country_name %in% populist_countries,
         !(v2paenname %in% vparty_exclude)) %>% 
  left_join(., populist, by = c("id_partyfacts", "year")) %>% 
  # Code all parties not featured in Popu-List as mainstream
  mutate(classification = case_when(
           farright == 1 ~ "farright",
           farleft  == 1 ~ "farleft",
           is.na(farright) ~ "mainstream"
         )) %>% 
  relocate(v2paenname, classification, country_name, id_partyfacts, year,
           farright, farleft)

vparty_test <- 
  vparty %>% 
  filter(!country_name %in% populist_countries) %>% 
  relocate(v2paenname, country_name, id_partyfacts, year)

# parlgov <- 
#   parlgov %>%
#   # Harmonize ID variable
#   rename(id_parlgov = party_id) %>% 
#   select(id_parlgov, country_name, country_id,
#          party_name_english, party_name_short,
#          left_right, state_market, liberty_authority, eu_anti_pro)
# 
# cmp <- 
#   cmp %>% 
#   select(countryname, edate, party, partyname, partyabbrev,
#          per101:per706) %>% 
#   rename(id_manifesto = party)

# TODO: Incorporate additional time-varying party data from manifestos
# and time-invariant data from parlgov.


# 3. Save data file -------------------------------------------------------
write_csv(vparty_train, here("data", "output", "01-vparty_train_frfl.csv"))
write_csv(vparty_test,  here("data", "output", "02-vparty_test_frfl.csv"))
