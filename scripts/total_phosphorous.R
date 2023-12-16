# soooo plan for determining a baseline total phosphorous value for each lake
# using the waterbase (Water Quality data from the european environment agency)
# 01.11.2023
# Rebecca Hies

# I have downloaded two datasets from the european environment agency
# the site code was filtered to just lakes in discodata (before downloading)
# the total.phosphorous dataset was filtered in discodata to just contain the 
# total phosphorous values and just lakes

# Next steps for R data manipulation----
# 1. Make sure the observed property is just total phosphorous 
# 2. Filter the sitecode dataset to just contain the lake name monitoring site
#    identifier and scheme?
# 3. Merge together the sitecodes and total.phosphorous datasets by the
#    monitoring site identifier (column)
# 4. Each monitoring site has total phosphorous values over several years;
#    Calculate the mean total phosphorous for each monitoring site
# 5. There are multiple monitoring sites for each lake so calculate the mean 
#    total phosphorous for each lakes

# Library----
library(tidyverse)
library(ggplot2)

# Import data
sitecodes <- read.csv("data/sitecodes.csv")
total.phosphorous <- read.csv("data/totalphosphorous.csv")

# 1. Make sure the observed property is just total phosphorous----
unique(total.phosphorous$observedPropertyDeterminandLabel)
# there is also "Total nitrogen to total phosphorous ratio"
# remove those rows
total.phosphorous2 <- filter(total.phosphorous, !observedPropertyDeterminandLabel == "Total nitrogen to total phosphorus ratio")

# 2. Filter the sitecode dataset to just contain the lake name monitoring site----
#    identifier and scheme?
sitecodes2 <- subset(sitecodes, select = c(monitoringSiteIdentifier, waterBodyName, monitoringSiteIdentifierScheme))

# 3. Merge together the sitecodes and total.phosphorous datasets by the----
#    monitoring site identifier (column)
df_merge <- merge(total.phosphorous2, sitecodes2, by = "monitoringSiteIdentifier", 
                  all.x = TRUE)

# 4. Each monitoring site has total phosphorous values over several years;----
#    Calculate the mean total phosphorous for each monitoring site
df_merge2 <- df_merge %>% group_by(monitoringSiteIdentifier) %>% 
  mutate(mean_p = mean(resultMeanValue))

# 5. There are multiple monitoring sites for each lake so calculate the mean----
#    total phosphorous for each lakes
df_merge3 <- df_merge2 %>% group_by(waterBodyName) %>% 
  mutate(mean_p_lake = mean(mean_p))
# problem: sometimes the same water body has several names

unique(df_merge3$phenomenonTimeReferenceYear)

# Make column do divide total phosphorous values into categories
df_merge3 <- df_merge3 %>%
  mutate(phos_classes = case_when(
    mean_p < 0.02                      ~ "<0.02",
    mean_p >= 0.02 & mean_p < 0.05  ~ "0.02-0.05",
    mean_p >= 0.05 & mean_p < 0.1   ~ "0.05-0.1",
    mean_p >= 0.1 & mean_p < 0.2    ~ "0.1-0.2",
    mean_p >= 0.2 & mean_p < 0.5    ~ "0.2-0.5",
    mean_p >= 0.5   ~ ">0.5",
    TRUE                             ~ "NA"
  ))

df_merge3 <- df_merge3 %>% 
  group_by(countryCode) %>%
  mutate(monitoring_sites = length(unique(monitoringSiteIdentifier)))

# Renaming water body names

# Bodensee
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("BODENSEE (UNTERSEE)", "BODENSEE", "BODENSEE (OBERSEE) - FREIWASSER INTERNATIONAL"), "Bodensee", waterBodyName))

# Paijanne (PÄIJÄNNE) Finland
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("PÄIJÄNNE (ETEL. N60+78.10)", "PÄIJÄNNE (KESK. N60+78.10)", "PÄIJÄNNE (POHJ. N60+78.10)"), "Paijanne", waterBodyName))

# Keitele Finland
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("KESKI-KEITELE (N60 99.50)", "ALA-KEITELE (N60+99.50)", "YLÄ-KEITELE (N60 99.50)"), "Keitele", waterBodyName))

# Vanajavesi Finland
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("VANAJAVESI (N60 79.40)X1", "VANAJAVESI (N60 79.40)X2"), "Vanajavesi", waterBodyName))

# Pyhäjärvi Finland
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("PYHÄJÄRVI (N60 77.20) ETELÄ", "PYHÄJÄRVI", "PYHÄJÄRVI (N60 77.20) POHJOINEN", "PYHÄJÄRVI PYHÄSELKÄ"), "Pyhäjärvi", waterBodyName))

# Müritz Germany
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("MÜRITZ, DAVON AUßENMÜRITZ", "MÜRITZ, DAVON BINNENMÜRITZ"), "Müritz", waterBodyName))

# Corrib Ireland
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("CORRIB LOWER", "CORRIB UPPER"), "Corrib", waterBodyName))

# Garda Italy
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("LAGO DI GARDA", "LAGO DI GARDA OCCIDENTALE", "LAGO DI GARDA SUDORIENTALE"), "Garda", waterBodyName))

# Como Italy
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("COMO (LAGO) - BACINO DI COMO", "COMO (LAGO) - BACINO DI LECCO"), "Como", waterBodyName))

# Vänern Schweden
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("VÄNERN - VÄRMLANDSSJÖN", "VÄNERN - DÄTTERN"), "Vänern", waterBodyName))

# Lower erne UK
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("LOWER LOUGH ERNE KESH", "LOWER LOUGH ERNE DEVENISH"), "Lower lough Erne", waterBodyName))

# Lower lomond UK
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("LOCH LOMOND (NORTH)", "LOCH LOMOND (SOUTH)"), "Loch Lomond", waterBodyName))

# Macnean uk
df_merge3 <- df_merge3 %>%
  mutate(waterBodyName = ifelse(waterBodyName %in% c("LOUGH MACNEAN UPPER", "LOUGH MACNEAN LOWER"), "Loch macnean", waterBodyName))

# There are two monitoring stations for prespa lake, Albania
# AL2 and ALLK_603
df_merge3 <- df_merge3 %>%
  mutate(monitoringSiteIdentifier = ifelse(monitoringSiteIdentifier %in% c("AL2", "ALLK_603"), "AL2 & ALLK_603", monitoringSiteIdentifier))

df_merge3 <- df_merge3 %>% group_by(waterBodyName) %>% 
  mutate(mean_p_lake = mean(mean_p))

as.factor(df_merge3$monitoringSiteIdentifier)

# Filter by countries
df_merge4 <- filter(df_merge3, countryCode == "AL"| countryCode == "AT" | countryCode == "EE" |
                    countryCode == "FI"| countryCode == "FR"| countryCode == "HU"|
                    countryCode == "DE"| countryCode == "IE"| countryCode == "IT"|
                    countryCode == "LV"|countryCode == "NL"| countryCode == "NO"|
                    countryCode == "RO"| countryCode == "SE"| countryCode == "UK")

