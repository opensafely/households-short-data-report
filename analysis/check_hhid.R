################################################################################
# Description: Household variable sense checks
#
# + Completeness of:
#   - household ID
#   - percent TPP coverage
#   - household size
#   - household type
#   
# + Sense check and consistency of household size
#   - By record count
#   - By pre-defined household size variable
#   - Total size (household size)/(percent TPP)
#   
# + Consistency of household characteristics across residents
# 
################################################################################

################################################################################

#----------------------#
#  SETUP ENVIRONMENT   #
#----------------------#

library(tidyverse)
library(data.table)

sink("./output/check_hhid_log.txt", type = "output")

theme_set(theme_minimal())

options(datatable.old.fread.datetime.character = TRUE)

# Function: calculate mode value across residents in household
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ---------------------------------------------------------------------------- #

#----------------------#
#      LOAD DATA       #
#----------------------#

# args <- c("./output/input.csv")
args = commandArgs(trailingOnly = TRUE)

dat <- fread(args[1], data.table = FALSE, na.strings = "")

# Redefine missing value codes in IMD and rural index
dat$imd <- na_if(na_if(dat$imd, -1),0)
dat$rural_urban <- na_if(dat$rural_urban, -1)

print(paste0("Total records: N = ", nrow(dat)))

# ---------------------------------------------------------------------------- #

#--------------------------#
#      COMPLETENESS        #
#--------------------------#

# Household ID

print("No. with missing household id (excluded):")
sum(dat$household_id == 0)
sum(is.na(dat$household_id))

dat <- filter(dat, household_id > 0 & !is.na(household_id))


# TPP coverage

print("No. with missing/0 TPP coverage:")
sum(is.na(dat$percent_tpp))
sum(dat$percent_tpp == 0)

print("Percent TPP = 0 set to NA.")
dat$percent_tpp <- na_if(dat$percent_tpp,0)

ggplot(dat, aes(percent_tpp)) +
  geom_histogram(bins = 50) -> percent_tpp_hist

ggsave("./output/household_tpp_coverage.png",
       percent_tpp_hist,
       height = 4, width = 6, units = "in")

print("No. with missing/0 HH size:")
sum(is.na(dat$household_size))
sum(dat$household_size == 0)

# print("Patients with household size = 0 set to mode across other household residents or NA if mode is also 0.")
# dat %>%
#   group_by(household_id) %>%
#   mutate(mode_hh_size = getmode(household_size)) %>%
#   ungroup() %>%
#   mutate(household_size = case_when(household_size == 0 ~ mode_hh_size,
#                                     household_size != 0 ~ household_size)) -> dat

print("Household size = 0 set to NA.")
dat$household_size <- na_if(dat$household_size,0)

print("Summary: cleaned household size")
summary(dat$household_size)

print("No. with missing HH type:")
sum(is.na(dat$care_home_type))

#------------------------------------------------------------------------------#

#-------------------------#
#     HOUSEHOLD SIZE      #
#---------------- --------#

# SIZE WITHIN TPP

# By individual:

dat %>%
  group_by(household_id) %>%
  mutate(household_n = n()) %>%
  ungroup() %>%
  mutate(diff_size_count = household_size - household_n) -> dat

print("Individuals with non-missing and discrepant household sizes (size - no. records):")
sum(na.omit(dat$diff_size_count) != 0)


# By household:

dat %>%
  group_by(household_id) %>%
  summarise(household_n = n(),
            household_size_distinct = n_distinct(household_size, na.rm = T),
            household_size_mode = getmode(household_size),
            household_size_pmiss = sum(is.na(household_size))/household_n,
            diff_size_count = household_size_mode - household_n) -> hh_size_check

print("Summary of household size, no. records and discrepancies, by household:")
summary(hh_size_check)

print("Households with discrepant sizes:")
sum(na.omit(hh_size_check$diff_size_count != 0))


# Distribution of household size and discrepancies:

hh_size_check %>%
  pivot_longer(-household_id, names_to = "variable") %>%
  ggplot(aes(value)) +
    geom_histogram(bins = 100) +
    facet_wrap(~variable, scales = "free")

ggsave("./output/household_size_tpp.png",
       height = 6, width = 8)


## -- TOTAL SIZE  -- ##

# Adjusted for TPP coverage

dat %>%
  mutate(household_size_tot = household_size/(percent_tpp/100)) -> dat

summary(dat$household_size_tot)

ggplot(dat, aes(household_size_tot)) +
  geom_histogram(bins = 100)

ggsave("./output/household_size_tot.png",
       height = 4, width = 6, units = "in")


## -- ALL SIZE METRICS BY CARE HOME TYPE  -- ##

print("TPP household size by care home type:")
dat %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_size, na.rm = T),
            median = median(household_size, na.rm = T),
            quants = paste(quantile(household_size, c(0.25, 0.75), na.rm = T), collapse = ", "),
            minmax = paste(min(household_size, na.rm = T), max(household_size, na.rm = T), sep = ", "),
            missing = sum(is.na(household_size)))

print("Number of records by care home type:")
dat %>%
  group_by(care_home_type, household_id) %>%
  summarise(household_n = n()) %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_n),
            median = median(household_n),
            quants = paste(quantile(household_n, c(0.25, 0.75)), collapse = ", "),
            minmax = paste(min(household_n), max(household_n), sep = ", "))

print("Total household size by care home type:")
dat %>%
  group_by(care_home_type) %>%
  summarise(mean = mean(household_size_tot, na.rm = T),
            median = median(household_size_tot, na.rm = T),
            quants = paste(round(quantile(household_size_tot, c(0.25, 0.75), na.rm = T)), collapse = ", "),
            minmax = paste(round(min(household_size_tot, na.rm = T)), round(max(household_size_tot, na.rm = T)), sep = ", "),
            missing = sum(is.na(household_size_tot)))

#------------------------------------------------------------------------------#

#-------------------------------------------#
#      UNIQUENESS OF CHARACTERISTICS        #
#-------------------------------------------#

print("Uniqueness of household characteristics over all residents:")
dat %>%
  group_by(household_id) %>%
  summarise(msoa = n_distinct(msoa, na.rm = T),
            household_size = n_distinct(household_size, na.rm = T),
            percent_tpp = n_distinct(percent_tpp, na.rm = T),
            care_home_type = n_distinct(care_home_type, na.rm = T),
            imd = n_distinct(imd, na.rm = T),
            rural_urban = n_distinct(rural_urban, na.rm = T)) -> n_distinct_chars

# Should be one distinct value for every household
summary(n_distinct_chars)

print("No. households with non-unique characteristics across residents:")
n_distinct_chars %>%
  dplyr::select(-household_id) %>%
  summarise(across(everything(), function(x) sum(x > 1)))


################################################################################

sink()

################################################################################
