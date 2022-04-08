

## "Dual Burden of Animal and Human zoonoses" findings

### GNI from https://data.worldbank.org/indicator/NY.GNP.PCAP.CD


#### 1. Cystic echinococcosis, Worldwide. DOI: 10.3201/eid1202.050499 
world_CE <- list(
  daly_unadjusted = c(218515,366133),                   # Unadjusted DALY: lower and upper bound 95% CI 
  daly_adjusted = c(862119,1175654),                    # Adjusted DALY: lower and upper bound 95% CI 
  animal_loss_unadjusted = c(942356157, 1622045957),    # No correction factor of animal loss
  animal_loss_adjusted = c(1572373055, 2951409989),     # Adjusted animal loss for under-reporting
  gni = 5510                                             # Gross National Income in the study period 
) 


#### 2. Echinococcosis, Shiqu County, China. DOI: https://doi.org/10.4269/ajtmh.2005.73.2
china_CE <- list(
  daly = 1100,         
  animal_loss_1 = c(240829,318249),    # Total losses (excluding losses in calf production, carcass weight, and yak hide)
  animal_loss_2 = c(384342,498447),    # Total losses (including losses in calf production, carcass weight, and yak hide)
  gni = 1133                                             
) 




#### 3. Cysticercosis (Taenia solium), Mozambique. DOI: https://doi.org/10.1186/s12879-018-3030-z
mozambique <- list(
  daly = c(1433,2762),           # Lower and upper bound 95% UI
  num_infected = c(2098,3133),   # Number of pigs with cysticercosis (0.025, 0.975) quantile
  animal_value = c(30,79),       # Value of an adult pig (0.025, 0.975) quantile
  sold = 0.33,                   # Proportion of adult pigs sold per year
  reduced_value = 0.5,           # Value reduction of infected pork
  gni = 460                      # Gross National Income in the year of the study
  )                        


#### 4. Cysticercosis (Taenia solium), Cameroon. DOI: https://doi.org/10.1371/journal.pntd.0000406
cameroon <- list(
  daly = c(14108,103469),              # lower and upper bound 95% CR
  animal_loss = c(566799, 922192),    # Pig losses in $  https://fxtop.com/en/historical-currency-converter.php
  gni = 1310                          # Gross National Income in the year of the study
)   


#### 5. Cystic echinococcosis, Peru. DOI: https://doi.org/10.1371/journal.pntd.0001179
peru <- list(
  daly_total = 1139,               
  daly_male = 491,
  daly_female = 648,
  animal_loss = c(2676181, 4911383),  # Total losses  
  gni = 3450                          # Gross National Income provided in the study
)   


#### 6. Rabies, Worldwide. DOI: https://doi.org/10.1371/journal.pntd.0003709

world_rabies <- list(
  daly = c(1316000,10519000),              # lower and upper bound 95% CI
  animal_loss = 2667195,
  #animal_loss = 512101465,              # Livestock losses  
  gni = 3450                          # Gross National Income provided in the study
)   

# Load libraries
library(dplyr)
library(tidyr)

# Dataset Worldbank
worldbank<- read.csv("C:\\Users\\lnogue\\Downloads\\worldbankdata.csv")


gni <- select(worldbank, c(1,55))
         
colnames(gni) <- c("country", "gni_2010")


# Dataset global rabies
world_rabies <- read.csv("6rabiesdata.csv")


# Select columns
livestock_loss <- select(world_rabies, c(3,5,32,36,37))
livestock_loss_subset <- select(world_rabies, c(32,36,37))


# Remove 'commas'
livestock_loss <- apply(livestock_loss, 2, function(x) gsub(",", "", x))
colnames(livestock_loss) <- c('country','cluster', 'livestock_loss', 'livestock_lower', 'livestock_upper')

# Convert character into numeric variables
livestock_loss_subset <- transform(livestock_loss, livestock_loss=as.numeric(livestock_loss),
                                   livestock_lower=as.numeric(livestock_lower),
                                   livestock_upper=as.numeric(livestock_upper))

# Select the min value between livestock_lower and livestock_upper for each row
livestock_loss_subset <- livestock_loss_subset %>% rowwise() %>% 
  mutate(livestock_loss_lower = min(livestock_lower, livestock_upper))


# Select the max value between livestock_lower and livestock_upper for each row
livestock_loss_subset <- livestock_loss_subset %>% 
  rowwise() %>% 
  mutate(livestock_loss_upper = max(livestock_lower, livestock_upper))


# Join dataset with specific condition
world_rabies_data <- left_join(livestock_loss_subset, gni, by = c('country' = 'country'))

# Replace NA by '0'
world_rabies_data <- world_rabies_data %>% mutate(across(where(is.numeric), coalesce, 0))

# Convert character into factor variables
world_rabies_data <- transform(world_rabies_data, cluster=as.factor(cluster))

# Filter countries by clusters
asia2 <- world_rabies_data %>% filter(cluster == 'Asia 2')
asia3 <- world_rabies_data %>% filter(cluster == 'Asia 3')
asia4 <- world_rabies_data %>% filter(cluster == 'Asia 4')
china <- world_rabies_data %>% filter(cluster == 'China')
india <- world_rabies_data %>% filter(cluster == 'India')
indonesia <- world_rabies_data %>% filter(cluster == 'Indonesia')
north_africa <- world_rabies_data %>% filter(cluster == 'North Africa')
congo_basin <- world_rabies_data %>% filter(cluster == 'Congo Basin')
west_africa <- world_rabies_data %>% filter(cluster == 'West Africa')
sadc <- world_rabies_data %>% filter(cluster == 'SADC')
andean <- world_rabies_data %>% filter(cluster == 'Andean')
brazil <- world_rabies_data %>% filter(cluster == 'Brazil')
caribbean <- world_rabies_data %>% filter(cluster == 'Caribbean')
central <- world_rabies_data %>% filter(cluster == 'Central America & Mexico')
southern_cone <- world_rabies_data %>% filter(cluster == 'Southern Cone')
eastern_europe <- world_rabies_data %>% filter(cluster == 'EasternEurope')
eurasia <- world_rabies_data %>% filter(cluster == 'Eurasia')
middle_east <- world_rabies_data %>% filter(cluster == 'Middle East')


#### 7. Q fever, Netherlands. DOI: https://doi.org/10.1016/j.prevetmed.2013.06.002

qfever_daly <- sort(c(2462, 2758, 2165, 2623, 2301,
                      2483, 2455, 2956, 1968, 2865,
                      2159, 2563, 2360, 2468, 2454,
                      2693, 1837, 2569, 2112, 4695, 
                      974), decreasing = F)

netherlands <- list(
  daly = c(974,4695),              
  animal_loss = c(357,429),     # Goat losses in $  https://fxtop.com/en/historical-currency-converter.php
  infected_animal_2007 = 7,
  infected_animal_2008 = 6,
  infected_animal_2009 = 61,
  infected_animal_2010 = 94,
  infected_animal_2011 = 98,
  infected_animal = mean(c(7, 6, 61, 94, 98)),            # Goats infected per year (period: 2007-2011)  
  gni_2007 = 49110,
  gni_2008 = 52670,
  gni_2009 = 53670,
  gni_2010 = 53910,
  gni_2011 = 54480,
  gni = mean(c(49110, 52670, 53670, 53910, 54480))       # GNI over 2007-2011
  )


#### 8. Rabies, Africa and Asia. DOI: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2626230/pdf/15976877.pdf

worldbank_groups <- read.csv("worldbank_groups.csv")
worldbank_groups <- select(worldbank_groups, c(2,5))
colnames(worldbank_groups) <- c("region", "country")

worldbank<- read.csv("C:\\Users\\lnogue\\Downloads\\worldbankdata.csv")

# Select GNI from 2002
gni_africa_asia <- select(worldbank, c(1,47))
colnames(gni_africa_asia) <- c("country", "gni_2002")


# Join dataset with specific condition
rabies_data_africa_asia <- left_join(worldbank_groups,
                                     gni_africa_asia, by = c('country' = 'country'))


# Filter countries 
south_asia_rabies <- rabies_data_africa_asia %>% filter(region == 'South Asia')
# Delete rows by position: selecting countries included in the study
south_asia_rabies <- south_asia_rabies %>% 
  filter(!row_number() %in% c(1, 6)) 


east_asia_pacific_rabies <- rabies_data_africa_asia %>% filter(region == 'East Asia & Pacific')
# Selecting countries included in the study
east_asia_pacific_rabies <- east_asia_pacific_rabies %>% 
  filter(row_number() %in% c(3, 4, 9, 11, 13, 14, 17, 18, 20, 24, 26, 31, 35)) 

# Bind rows of two dataframes for Africa
asia_rabies <- bind_rows(south_asia_rabies,east_asia_pacific_rabies)

#  Mean GNI for Asia
gni_asia <- mean(asia_rabies$gni_2002)


# Africa
africa_rabies <- rabies_data_africa_asia %>% filter(region == 'Middle East & North Africa')
africa_rabies <- africa_rabies %>% 
  filter(row_number() %in% c(4, 5, 12, 13, 20))

africa_subsaharan_rabies <- rabies_data_africa_asia %>% filter(region == 'Sub-Saharan Africa')
africa_subsaharan_rabies  <- africa_subsaharan_rabies  %>% 
  filter(!row_number() %in% c(11,12,27, 37,38, 39,40, 41)) 

# Bind rows of two dataframes for Africa
africa_rabies <- bind_rows(africa_rabies,africa_subsaharan_rabies)

# Convert character into numeric variables
gni_africa <- transform(africa_rabies, gni_2002 = as.numeric(gni_2002))


# Remove rows with NA value in any column
gni_africa <- gni_africa %>%
    na.omit(gni_2002)

# Mean GNI for Africa
gni_africa <- mean(gni_africa$gni_2002)

rabies_africa_asia <- list(
  daly_africa = c(217954, 1449114),
  daly_asia = c(302324, 1983646),
  daly_total = c(799615, 2984109),
  daly_no_tx = c(4848684,15264050),        # No post-exposure tx
  livestock_loss = c(11000000,13700000),
  livestock_loss_africa = c(1500000, 1900000),
  livestock_loss_asia = c(9400000,11800000),
  #gni_china = 1100,
  #gni_india = 460,
  gni_africa = 915,
  gni_asia = 2550,
  gni_total = 1684)
 


#### 9. Cysticercosis (Taenia solium), Tanzania. DOI: https://doi.org/10.1016/j.actatropica.2015.12.021
tanzania <- list(
  daly = c(9136,72078),           # Lower and upper bound 95% UI
  pig_loss = c(1095960, 5366038),      
  gni = 810)                     # Gross National Income in the year of the study



#### 10. Rabies, Viet Nam. DOI: 10.1371/journal.pntd.0006866
vietnam <-  list(
  daly_26 = c(4200,4100,6550,4550,3400,3900,5500,4900,5250,3350),  # yearly 2005-2014
  daly_31 = c(3780,3690,5895,4095,3060,3510,4950,4410,4725,3015),
  daly_36 = c(3360,3280,5240,3640,2720,3120,4400,3920,4200,2680),
  livestock_loss_mean = 1034422,
  gni = c(630,720,840,980,1110,1250,1370,1540,1720,1880)
  )


#### 11. Rabies, Kazakhstan. DOI: 10.1371/journal.pntd.0004889
kz_rabies <- list(
  daly = c(338,594),
  daly_without_PEP = c(4723,12041),
  cattle_loss = c(18996,22442),
  sheep_loss = c(515,755),
  camel_loss = c(59,193),
  horse_loss = c(1628,2878),
  total_animal_loss = c(21818,25512),
  gni_mean = 6903
)

#### 12. Brucellosis, Kazakhstan. DOI: 10.1111/zph.12582
kazakhstan_brucellosis <- list(
  daly = 713,
  livestock_loss = 19684800,
  gni = 11380
)

#### 13.Leptospirosis, New Zealand. DOI: https://doi.org/10.1111/zph.12668
nz <-  list(
  daly_pop_at_risk = 14.07, #median
  pi_daly_pop_at_risk = c(1.86, 80.73), # Prediction Interval 95% (lower, upper)
  daly_pop_not_at_risk = 3.69,
  pi_pop_not_at_risk = c(0.49,21.20),
  total_daly = 17.76,
  pi_total_daly = c(2.35,101.93),
  beef_cattle_loss = 1360000,
  pi_beef_cattle_loss = c(4e+05,3960000),
  sheep_loss = 3090000,
  pi_sheep_loss = c(810000,8290000),
  deer_loss = 2800000,
  pi_deer_loss = c(830000,8850000),
  total_loss = 7920000,
  pi_total_loss = c(3750000,15480000),
  gni_mean = 40737
)



#### 14. Brucella, Anthrax, Tularemia, CCHF, Rabies, Cystic Echinococcosis,
####     Toxoplasmosis; Turkey. DOI: 10.33988/auvfd.789598
       
turkey <- list(
  brucella_daly = c(860,1083,1262),
  anthrax_daly = c(30, 0, 119),
  tularemia_daly = c(1, 1, 1),
  cchf_daly = c(446, 429, 639),
  rabies_daly = c(171, 72, 95),
  ce_daly = c(10, 40, 22),
  toxoplasmosis_daly = c(1, 61, 2),
  total_daly = c(1519, 1686, 2140), # correct turkey
  brucella_large_ruminant_loss = c(9170180, 15388268, 36264523),
  brucella_small_ruminant_loss = c(1302000, 2889920, 9064147),
  brucella_total_animal_loss = c(10472179, 18278188, 45328670),
  anthrax_large_ruminant_loss = c(1063393, 
                                  1271022,
                                  2621348),
  anthrax_small_ruminant_loss = c(501068,
                                  609817,
                                  1211237),
  anthrax_total_animal_loss = c(1565946,
                                1885018,
                                3827419),
  total_animal_loss = c(12048065, 20203818, 49094994),
  gni_mean = 10913
)
  
