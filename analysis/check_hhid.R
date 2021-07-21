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

library(data.table)

sink("./output/check_hhid_log.txt", type = "output")

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

dat <- fread(args[1], data.table = TRUE, na.strings = "")

# Redefine missing value codes in rural index
dat$rural_urban[dat$rural_urban <= 0] <- NA

dat <- dat[, `:=`(imd = as.factor(imd),
                  rural_urban = as.factor(rural_urban))]

print(paste0("Total records: N = ", nrow(dat)))

# ---------------------------------------------------------------------------- #

#--------------------------#
#      COMPLETENESS        #
#--------------------------#

# Household ID

print("No. with missing household id (excluded):")
table(dat$household_id == 0)

dat <- dat[dat$household_id > 0,]

# TPP coverage

print("No. with missing TPP coverage:")
table(is.na(dat$percent_tpp))

print("No. with 0% TPP coverage:")
table(dat$percent_tpp == 0)

print("Percent TPP = 0 set to NA.")
dat$percent_tpp[dat$percent_tpp == 0] <- NA

print("Summary: TPP percent coverage")
summary(dat$percent_tpp)

print("Summary: TPP percent coverage < 100%")
summary(dat$mixed_household)

print("No. with missing HH size:")
table(is.na(dat$household_size))

print("No. with HH size = 0:")
table(dat$household_size == 0)

print("Household size = 0 set to NA.")
dat$household_size[dat$household_size == 0] <- NA

print("No. with missing HH type:")
table(is.na(dat$care_home_type))

print("Define individual care home indicator")
dat <- dat[,care_home := (care_home_type != "U")]
table(dat$care_home)

#------------------------------------------------------------------------------#

#-------------------------#
#     HOUSEHOLD SIZE      #
#---------------- --------#

# Define count and summarise age by household
dat <- dat[, `:=`(household_n = .N,
                  household_mean_age = mean(age, na.rm = TRUE),
                  household_n_gt65 = sum(age >= 65)), 
           by = household_id]

# Define count/size discrepancy and total size
dat <- dat[,`:=`(diff_size_n = abs(household_size - household_n),
                 household_size_tot = household_size/(percent_tpp/100))]

#------------------------------------------------------------------------------#

print("# BY INDIVIDUAL #")

print("TPP household count")
summary(dat$household_n)

print("TPP household size")
summary(dat$household_size)

print("Discrepancy household size vs count")
summary(dat$diff_size_n)

print("Total household size")
summary(dat$household_size_tot)

print("Individuals with discrepant household sizes")
table(dat$diff_size_n != 0)

print("Size of non-zero discrepancies")
summary(dat$diff_size_n[dat$diff_size_n != 0])

#------------------------------------------------------------------------------#

print("CARE HOME RESIDENTS ONLY:")

ch <- dat[care_home == TRUE]

print("TPP household count")
summary(ch$household_n)

print("TPP household size")
summary(ch$household_size)

print("Discrepancy household size vs count")
summary(ch$diff_size_n)

print("Total household size")
summary(ch$household_size_tot)

#------------------------------------------------------------------------------#

print("# BY HOUSEHOLD #")

hh_size_check <- dat[!duplicated(household_id)][,c("age","patient_id") := NULL]

print("Total number of households:")
nrow(hh_size_check)

print("Mixed households:")
table(hh_size_check$mixed_household)

print("Care homes:")
table(hh_size_check$care_home)

print("Size, no. records and discrepancies, by household:")
summary(hh_size_check)

png("./output/household_tpp_coverage.png",height = 400, width = 600)
hist(hh_size_check$percent_tpp, breaks = 50)
dev.off()

print("No. households with discrepant sizes:")
sum(na.omit(hh_size_check$diff_size_n != 0))

print("Summary of non-zero discrepancies, by household:")
summary(hh_size_check$diff_size_n[hh_size_check$diff_size_n != 0])

print("No. households with discrepant sizes > 10")
sum(na.omit(hh_size_check$diff_size_n > 100))

#------------------------------------------------------------------------------#

print("100% COVERAGE ONLY:")

hh_size_tpp100 <- hh_size_check[percent_tpp == 100]
summary(hh_size_tpp100)

print("No. non-mixed households with discrepant sizes:")
sum(na.omit(hh_size_tpp100$diff_size_n != 0))

#------------------------------------------------------------------------------#

print("CARE HOME RESIDENTS ONLY:")

ch_size_check <- ch[!duplicated(household_id)][,c("age","patient_id") := NULL]
summary(ch_size_check)

#------------------------------------------------------------------------------#

#-------------------------------------------#
#      UNIQUENESS OF CHARACTERISTICS        #
#-------------------------------------------#

# Uniqueness of household characteristics over all residents:")
n_distinct_chars <- dat[, lapply(.SD, 
                                 function(x) {length(unique(x))}),
                        .SDcols = c("msoa","household_size","percent_tpp",
                                             "care_home","imd","rural_urban"),
                        by = household_id]

# Should be one distinct value for every household
summary(n_distinct_chars)

print("No. households with non-unique characteristics across residents:")
n_distinct_chars[,lapply(.SD, 
                         function(x)sum(x > 1)),
                 .SDcols = c("msoa","household_size","percent_tpp",
                             "care_home","imd","rural_urban")]


print("100% COVERAGE ONLY:")
n_distinct_chars2 <- dat[percent_tpp == 100, lapply(.SD, 
                                 function(x) {length(unique(x))}),
                        .SDcols = c("msoa","household_size",
                                    "care_home","imd","rural_urban"),
                        by = household_id]

# Should be one distinct value for every household
summary(n_distinct_chars2)

print("No. households with non-unique characteristics across residents:")
n_distinct_chars2[,lapply(.SD, 
                         function(x)sum(x > 1)),
                 .SDcols = c("msoa","household_size",
                             "care_home","imd","rural_urban")]


################################################################################

sink()

################################################################################
