# Post-election analysis data cleaning
## Universal packages
library(tidyverse)
library(tidyr)
library(readxl)
library(janitor)

## Houston Mayor
clean_houmay_files <- list()

### Fort Bend
#### General
raw_file_mayor_19g <- read_csv('/Users/joshy/Downloads/tabula-ft_bend_houmay_2019.csv') 

raw_file_mayor_19g %>% 
  clean_names() %>% 
  mutate(precinct = ifelse(grepl('Precinct ', party_candidate),
                                 str_trim(substring(str_remove(party_candidate, 'Precinct '),1,4)),
                                 NA),
         contest = ifelse(grepl('City of Houston Mayor',party_candidate),
                                'Houston Mayor',
                                ifelse(grepl('City of Houston',party_candidate),
                                       party_candidate,
                                       NA))) %>% 
  fill(precinct,
       contest, 
       .direction = 'downup') %>% 
  select(precinct, contest, party_candidate, absentee, early, election, total) %>% 
  filter(contest == 'Houston Mayor',
         !is.na(party_candidate)) %>% 
  ##### Gotta export to Excel and clean by hand from here...
  write_csv('/Users/joshy/Downloads/houmay_19g_close_to_clean.csv')

##### Going to add this to the list() object above after joining in total reg from the runoff report
mayor_19g_with_manual_cleaning <- read_csv('/Users/joshy/Downloads/houmay_19g_clean.csv') 

#### Run-Off
mayor_19ro_with_manual_cleaning <- read_csv('/Users/joshy/Downloads/houmay_19ro_clean.csv') 

n_rvs_houston_ftbend <- distinct(mayor_19ro_with_manual_cleaning, 
                                 precinct, registered_voters)
mayor_19g_final <- inner_join(mayor_19g_with_manual_cleaning, n_rvs_houston_ftbend)

clean_houmay_files[[1]] <- mayor_19g_final

## Harris County
### General (cleaned in Excel)
harris_mayor_19g <- read_csv('/Users/joshy/Downloads/harris_houmay_19g.csv')

# ### Run-Off
# harris_mayor_19ro <- read_csv('/Users/joshy/Downloads/harris_houmay_19ro.csv')

# ## Montgomery
# ### General
# harris_mayor_19g <- read_csv('/Users/joshy/Downloads/monty_houmay_19g.csv')
# 
# ### Run-Off
# harris_mayor_19g <- read_csv('/Users/joshy/Downloads/monty_houmay_19g.csv')

## Crosswalk 2019 results to the new precinct lines
xwalk <- read_excel('/Users/joshy/Downloads/desert precincts and crosswalk (1).xlsx',
                    sheet = 2) %>% 
  clean_names() %>% 
  filter(x2020_county %in% c('Harris','Fort Bend')) %>% 
  arrange(x2022_fips) %>% 
  mutate(x2020_pr = gsub('157_','',gsub('201_','',x2020_pr)),
         x2022_pr = substring(x2022_pr,5))

full_mayor_19 <- mayor_19g_final %>% 
  mutate(county = 'Fort Bend',
         precinct = as.character(precinct)) %>% 
  select(precinct, registered_voters, county, party_candidate, total) %>% 
  union_all(harris_mayor_19g %>% 
              select(-early, -election_day, -total) %>% 
              mutate(county = 'Harris') %>% 
              pivot_longer(cols = c('buzzbee','turner','other_1','other_2'),
                           names_to = 'party_candidate',
                           values_to = 'total')) %>% 
  mutate(precinct = as.character(precinct)) %>% 
  pivot_wider(names_from = 'party_candidate',
              values_from = 'total', 
              id_cols = c('county','precinct','registered_voters'),
              values_fn = max) %>% 
  clean_names()  %>%
  mutate(turner_19g = coalesce(sylvester_turner,0)+coalesce(turner,0),
         buzbee_19g = coalesce(tony_buzbee,0)+ coalesce(buzzbee,0),
         others_19g = coalesce(dwight_a_boykins,0) + coalesce(bill_king,0) + 
           coalesce(demetria_smith,0) + coalesce(naoufal_houjami,0)+
           coalesce(victoria_romero,0)+coalesce(roy_j_vasquez,0)+
           coalesce(kendall_baker,0)+coalesce(derrick_broze,0)+
           coalesce(sue_lovell,0)+coalesce(johnny_j_t_taylor,0)+
           coalesce(other_1,0)+coalesce(other_2,0))%>% 
  left_join(xwalk, by = c('county' = 'x2020_county',
                          'precinct' = 'x2020_pr')) %>% 
  mutate(turner_19g_dist = turner_19g*coverage,
         buzbee_19g_dist = buzbee_19g*coverage,
         others_19g_dist = others_19g*coverage,
         registered_voters_dist = registered_voters*coverage) %>% 
  group_by(vb_vf_national_precinct_code = paste0('TX_',toupper(county),'_', str_pad(x2022_precinct, 4, 'left','0'))) %>% 
  summarise(turner_19g = sum(turner_19g_dist, na.rm = TRUE),
            buzbee_19g = sum(buzbee_19g_dist, na.rm = TRUE),
            others_19g = sum(others_19g_dist, na.rm = TRUE),
            registered_voters_19g = sum(registered_voters_dist, na.rm = TRUE))

# Combine with existing demo, 2020, and 2022 data on precinct and county
demo_data <- read_csv('/Users/joshy/Downloads/tfp_precinct_basefile_110723.csv')

deserts <- read_excel('/Users/joshy/Downloads/desert precincts and crosswalk (1).xlsx',
                      sheet = 1) %>% 
  clean_names() %>% 
  filter(county %in% c(157,201)) %>% 
  mutate(county_name = case_when(county == '157' ~ 'Fort Bend',
                                 county == '201' ~ 'Harris'),
         precinct = substring(precinct_county,5),
         vb_vf_national_precinct_code = paste0('TX_',toupper(county_name),'_',substring(precinct_county,5))) %>% 
  select(vb_vf_national_precinct_code, rice_neighborhood = new_geoname)


historic_data <- demo_data %>% 
  filter(grepl('_HARRIS_',vb_vf_national_precinct_code) | grepl('_FORT BEND_',vb_vf_national_precinct_code)) %>% 
  left_join(full_mayor_19, by = 'vb_vf_national_precinct_code') %>% 
  left_join(deserts, by = 'vb_vf_national_precinct_code')

## Merge in most recent mayor result
harris_ftb_23g <- read_csv('/Users/joshy/Downloads/houmay_23g_results.csv') %>% 
  clean_names() %>% 
  mutate(vb_vf_national_precinct_code = paste0('TX_',toupper(county ),'_',str_pad(precinct,4,'left','0')),
         n_sjl = sheila_jackson_lee,
         n_whitmire = john_whitmire,
         n_others = m_griff_griffin+kathy_lee_tatum+robert_gallegos+roy_vasquez+david_c_lowy+
           gaylon_s_caldwell+jack_christie+naoufal_houjami+ robin_williams) %>% 
  select(vb_vf_national_precinct_code, n_sjl, n_whitmire, n_others)

full_houston_mayor_data <- historic_data %>% 
  inner_join(harris_ftb_23g, by = "vb_vf_national_precinct_code")

write_csv(full_houston_mayor_data,
          paste0('/Users/joshy/Downloads/full_houston_mayor_data_',gsub('-','',Sys.Date()))
