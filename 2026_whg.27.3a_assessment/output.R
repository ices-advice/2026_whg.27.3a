## Extract results of interest, write TAF output tables

## Before: 
## After:

library(icesTAF)
library(stockassessment)

mkdir("output")


# ------------------------------------------------------------------------------
# get official landings

# some output helpers
# ------------------------------------------------------------------------------

levels <- c("Denmark", "Sweden", "Netherlands", "Norway", "UK (England)", 
            "UK(Scotland)", "Germany", "UKS", "UKE", "Belgium", 
            "Germany, Fed. Rep. of", "UK - Eng+Wales+N.Irl.", 
            "UK - England & Wales", "GB")
labels <- c("DK",      "SE",     "NL",          "NO",     "UK",           
            "UK",           "DE",      "UK",  "UK",  "BE",
            "DE",                    "UK", 
            "UK",                   "UK")

vsum <- function(x, y) {sum(x, y, na.rm = TRUE ) }

# from Alex InterCatch package
get_numbers <- function(x) {as.numeric(regmatches(x, regexpr("[[:digit:]]+", x)))}

# ------------------------------------------------------------------------------
# official landings
olhist <-
  read.csv("data/ICES_1950-2010.csv",
           stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
  filter(Species == "Whiting", Division == "III a") %>%
  reshape2:::melt.data.frame(id.vars = c("Species", "Division",  "Country"), variable.name = "Year") %>%
  as_tibble() %>%
  filter(as.integer(get_numbers(Year)) >= 1950) %>%
  mutate(Year = as.integer(get_numbers(Year)),
         Country = ifelse(Country %in% levels, Country, "Other"),
         Country = factor(Country, levels = levels, labels = labels, ordered = TRUE)) %>%
  filter(! value %in% c("-", ".", "<0.5")) %>%
  group_by(Year, Country) %>%
  summarise(value = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") %>%
  ungroup %>%
  transmute(Species = "WHG", Area = "27.3.a", Units = "TLW", 
            ConfidentialityFlag = "N", Country, Year, Landings = value) %>%
  filter(Year < 2006, Landings > 0)

ol <- read.csv(paste0("data/ICESCatchDataset2006-",ly-2,".csv"),
               stringsAsFactors = FALSE, header = TRUE, na.strings = "c",fileEncoding="UTF-8-BOM") %>% as_tibble()

ol2nd <- read.csv(paste0("data/Preliminary_landings_allSpecies_",ly-1,".csv"),
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
  filter(Species.Latin.Name %in% c("Merlangius merlangus"),
         Area %in% c("27.3.a", "27.3.a.20", "27.3.a.21")) %>%
  mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
  (\(d) {
    if ("ConfidentialityFlag" %in% names(d)) {
      d %>% mutate(
        ConfidentialityFlag = if_else(ConfidentialityFlag == "Y", "Y", "N")
      )
    } else {
      d %>% mutate(
        ConfidentialityFlag = "N")
    }
  })()   %>% 
    group_by(Year, Country,ConfidentialityFlag) %>%
  summarise(Species = "WHG",
            Area = "27.3.a",
            Units = "TLW",
            X2nd = vsum(AMS.Catch.TLW., BMS.Catch.TLW.), .groups = "drop") %>%
  ungroup() %>%
  transmute(Species, Area, Units,ConfidentialityFlag, Country = factor(Country, levels = unique(labels), ordered = TRUE), Year = ly-1 , Landings = X2nd)

ol1st <- read.csv(paste0("data/Preliminary_landings_allSpecies_",ly,".csv"),
                  stringsAsFactors = FALSE, header = TRUE) %>% as_tibble() %>%
  filter(Species.Latin.Name %in% c("Merlangius merlangus"),
         Area %in% c("27.3.a", "27.3.a.20", "27.3.a.21")) %>%
  mutate(Country = ifelse(Country == "GB", "UK", Country)) %>%
  (\(d) {
    if ("ConfidentialityFlag" %in% names(d)) {
      d %>% mutate(
        ConfidentialityFlag = if_else(ConfidentialityFlag == "Y", "Y", "N")
      )
    } else {
      d %>% mutate(
        ConfidentialityFlag = "N")
    }
  })()   %>% 
  group_by(Year, Country,ConfidentialityFlag) %>%
  summarise(Species = "WHG",
            Area = "27.3.a",
            Units = "TLW",
            X1st = vsum(AMS.Catch.TLW., BMS.Catch.TLW.), .groups = "drop") %>%
  ungroup() %>%
  transmute(Species, Area, Units, ConfidentialityFlag, Country = factor(Country, levels = unique(labels), ordered = TRUE), Year = ly, Landings = X1st)

oltur <- ol %>% filter(Species == "WHG", Area %in% c("27.3.a")) %>%
  reshape2:::melt.data.frame(id.vars = c("Species", "Area", "Units", "Country"), variable.name = "Year") %>%
  as_tibble() %>%
  mutate(Country = ifelse(Country == "GB", "UK", Country),
         Year = as.integer(get_numbers(Year)),
         ConfidentialityFlag = "N",
         Country = factor(Country, levels = unique(labels), ordered = TRUE),
         value = as.numeric(value)) %>%
  group_by(Species, Area, Units, ConfidentialityFlag, Country, Year) %>%
  summarise(Landings = sum(value), .groups = "drop") %>%
  bind_rows(olhist, ., ol2nd, ol1st)

saveRDS(oltur, file = "output/official_landings_whg.27.3a.Rds")


