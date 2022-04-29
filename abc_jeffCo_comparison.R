# Notes:
# eBird best practices: https://cornelllabofornithology.github.io/ebird-best-practices/
# best practice to also take lists from neighboring areas of Adam
# may need to use the auk package if i want to work with larger datasets

# Libraries
library(dplyr)
library(tidyr)

#set working directory
setwd("C:/Users/Ben/Documents/code/birding/data/")
# setwd("C:/Users/Ben/Downloads/")

# set file
file <- "ebd_US-WI-055_relJul-2021.txt"

# read in data
df <- read.delim(file)

# Adam Conservancy may be under Locality or Locality.ID
length(unique(df$LOCALITY))
grep("Adam", df$LOCALITY)

#subset Adams Conservancy
df_adam <- df[grep("Adam", df$LOCALITY),]

# check for all adams locations
unique(df_adam$LOCALITY)

# get number of times species observed
df_groups <- df_adam %>%
    group_by(COMMON.NAME) %>%
    summarize(times_observed = n()) %>%
    # filter(grepl('sp'.'', COMMON.NAME))
    # filter(!COMMON.NAME %in% c('sp.')) %>%
    arrange(desc(times_observed))

# sampling events
length(unique(df_adam$SAMPLING.EVENT.IDENTIFIER))
total_sampling_events_ABC <- length(unique(df_adam$SAMPLING.EVENT.IDENTIFIER))

events_adam <- df_adam %>%
    group_by(COMMON.NAME) %>%
    summarize(sampling_events_adam = n()) %>%
    mutate(freq_adam = 100*round(sampling_events_adam/total_sampling_events_ABC, 4)) %>%
    arrange(desc(freq_adam))

# Repeat frequency process for Lake Mills - zeloski
df_zelo <- df[grep("Lake Mills SWA--Zeloski Marsh", df$LOCALITY),]

total_sampling_events_zelo <- length(unique(df_zelo$SAMPLING.EVENT.IDENTIFIER))

events_zelo <- df_zelo %>%
    group_by(COMMON.NAME) %>%
    summarise(sampling_events_zelo = n()) %>%
    mutate(freq_zelo = 100*round(sampling_events_zelo/total_sampling_events_zelo, 4)) %>%
    arrange(desc(freq_zelo))

# Find top 10 non-ABC hot spots adn repeat process
length(unique(df$LOCALITY))
length(unique(df$LOCALITY.ID))
unique(df$LOCALITY.TYPE)

# group by locality, then group by species count (common name) and arrange
# df %>%
#     group_by(LOCALITY, COMMON.NAME) %>%
#     summarize(s_count = n()) %>%
#     arrange(desc(s_count))

# going to be nrow(df_groups)
df_localities <- df %>%
    group_by(LOCALITY, COMMON.NAME) %>%
    summarize(s_count = n()) %>%
    arrange(desc(s_count))

# Group by locality, then count number of rows; each row is a separate species
df_locality_counts <- df_localities %>%
    group_by(LOCALITY) %>%
    summarize(n_species = n()) %>%
    arrange(desc(n_species))

# keep only localities with greater than 200 rows; if too few, keep top 10 localities
# remove ABC
df_top10 <- df_locality_counts %>%
    filter(LOCALITY != "Adam Birding Conservancy") %>%
    head(10)

# create a new dataframe with only observations from the top 10 birding localities,
# sans ABC
df_top10 <- df %>%
    right_join(df_top10, by='LOCALITY') %>%
    select(-c(n_species))

# sampling events
length(unique(df_top10$SAMPLING.EVENT.IDENTIFIER))
total_sampling_events_top10 <- length(unique(df_top10$SAMPLING.EVENT.IDENTIFIER))

# get frequencies of each species in this df
df_top10_counts <- df_top10 %>% 
    group_by(COMMON.NAME) %>%
    summarize(times_observed = n()) %>%
    mutate(freq_top10 = 100 * round(times_observed/total_sampling_events_top10, 4)) %>%
    arrange(desc(freq_top10))

# df_top10 %>%
#     group_by(COMMON.NAME) %>%
#     summarize(sampling_events_adam = n()) %>%
#     mutate(freq_adam = 100*round(sampling_events_adam/total_sampling_events_ABC, 4)) %>%
#     arrange(desc(freq_adam))

#
length(which(df_localities$LOCALITY == "Adam Birding Conservancy"))

# df_localities %>%
#     group_by(LOCALITY) %>%
#     summarize(n_count = nrow(LOCALITY))

# Repeat frequency process for the rest of Jefferson County
df_jeff <- df %>%
    anti_join(df_adam) %>%
    anti_join(df_zelo)

total_sampling_events_jeff <- length(unique(df_jeff$SAMPLING.EVENT.IDENTIFIER))

events_jeff <- df_jeff %>%
    group_by(COMMON.NAME) %>%
    summarize(sampling_events_jeff = n()) %>%
    mutate(freq_jeff = 100*round(sampling_events_jeff/total_sampling_events_jeff, 4)) %>%
    arrange(desc(freq_jeff))

# join df's and compare
df_comp <- events_adam %>%
    full_join(events_zelo, by='COMMON.NAME') %>%
    full_join(events_jeff, by='COMMON.NAME')

# impute missing values with zeroes
df_comp[is.na(df_comp)] <- 0

# Create difference columns
df_comp <- df_comp %>%
    mutate(adam_zelo_diff = freq_adam - freq_zelo, adam_jeff_diff = freq_adam - freq_jeff)

df_comp[grep("sp.", df_comp$COMMON.NAME),]

# Get diff tables by different arrangements
adam_zelo_hi <- df_comp %>%
    select(COMMON.NAME, adam_zelo_diff, freq_adam) %>%
    arrange(desc(adam_zelo_diff))

adam_zelo_lo <- df_comp %>%
    select(COMMON.NAME, adam_zelo_diff, freq_adam) %>%
    arrange(adam_zelo_diff)

adam_jeff_hi <- df_comp %>%
    select(COMMON.NAME, adam_jeff_diff, freq_adam) %>%
    arrange(desc(adam_jeff_diff))

adam_jeff_lo <- df_comp %>%
    select(COMMON.NAME, adam_jeff_diff, freq_adam) %>%
    arrange(adam_jeff_diff)

# get months and years
df_adam$OBSERVATION.DATE <- as.Date(df_adam$OBSERVATION.DATE)
df_adam$YEAR <- format(df_adam$OBSERVATION.DATE, "%Y")
df_adam$MONTH <- format(df_adam$OBSERVATION.DATE, "%b")

# species by month
species_by_month <- df_adam %>%
    group_by(COMMON.NAME, MONTH) %>%
    summarize(total_events = n()) %>%
    pivot_wider(names_from = MONTH, values_from = total_events, values_fill = 0) %>%
    select("Species: Common Name" = COMMON.NAME, Jan, Mar, Apr, May, Jun,
           Jul, Aug, Sep, Oct, Nov, Dec)

# adam_months_totals <- species_by_month %>%
#   rename(species_by_month, "Species: Common Name" = COMMON.NAME, Jan = `01`,
#        Mar = `03`, Apr = `04`, May = `05`, Jun = `06`, Jul = `07`, Aug = `08`,
#        Sep = `09`, Oct = `10`, Nov = `11`, Dec = `12`)

# species by year
species_by_year <- df_adam %>%
    group_by(COMMON.NAME, YEAR) %>%
    summarize(total_events = n()) %>%
    pivot_wider(names_from = YEAR, values_from = total_events, values_fill = 0) %>%
    select("Species: Common Name" = COMMON.NAME, `2012`, `2014`, `2015`,
           `2016`, `2017`, `2018`, `2020`, `2021`)
# mutate(percent_2020 = `2020`/sum(`2020`))
#%>%

# write to file
# write.csv(species_by_year, file = "C:/Users/Ben/Documents/code/birding/adam_conservancy/ac_ben_exploratory_yearly_2020_0824.csv")

# Look at rose lake
df_rose <- df[grep("Rose", df$LOCALITY),]
unique(df_rose$LOCALITY)

# look at 9 springs
df[grep("Springs", df$LOCALITY),]

# look at lake mills
df[grep("Lake Mills", df$LOCALITY),]

# group by locality, then keep only localities with 200+ species types

