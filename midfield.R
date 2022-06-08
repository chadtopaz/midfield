# Load libraries
library(midfieldr)
library(midfielddata)
library(data.table)
library(tidyverse)
library(readxl)
library(caret)
library(MatchIt)
library(multcomp)
library(broom)

# Load data
data(midfieldcourses, midfielddegrees, midfieldstudents, midfieldterms)

# Change name of cip field in each data set so as not to conflict
midfielddegrees <- midfielddegrees %>%
  dplyr::rename("degreecip" = "cip6")
midfieldstudents <- midfieldstudents %>%
  dplyr::rename("incomingcip" = "cip6")
  
# Get rid of transfer students
midfieldstudents <- midfieldstudents %>%
  dplyr::filter(transfer == "N") %>%
  dplyr::select(-transfer, -hours_transfer)

# Keep only relevant time period
midfieldstudents <- midfieldstudents %>%
  dplyr::filter(term_enter >= 20051 & term_enter <= 20124)

# Drop students with race Other, International, or Unknown
midfieldstudents <- midfieldstudents %>%
  dplyr::filter(!race %in% c("Other", "International", "Unknown")) %>%
  dplyr::filter(!is.na(race))

# Recode race
# Keep only white and urm
midfieldstudents <- midfieldstudents %>%
  dplyr::mutate(race = dplyr::case_when(
    race == "White" ~ "nonurm",
    race == "Asian" ~ "nonurm",
    TRUE ~ "urm")) %>%
  mutate(race = factor(race)) %>%
  mutate(race = relevel(race, ref = "nonurm"))

# Add code for no STEM intending
midfieldstudents <- midfieldstudents %>%
  dplyr::mutate(nostemintent = incomingcip %in% cip_stem)

# Add six year horizon for degree
midfieldstudents <- midfieldstudents %>%
  dplyr::mutate(timehorizon = term_enter + 60)

# Merge in other data sets
data <- merge(midfieldstudents, midfielddegrees, by = c("id", "institution"), all.x = TRUE, all.y = FALSE)
data <- merge(data, midfieldcourses,by = c("id", "institution"), all.x = TRUE, all.y = FALSE)

# Add flag for STEM degree
data <- data %>%
  dplyr::mutate(stemdegree = degreecip %in% cip_stem & term_degree <= timehorizon)

# Keep only courses during each student's first term
data <- data %>%
  filter(term_enter == term_course)

# Drop some unused variables
data <- data %>%
  dplyr::select(-incomingcip, -term_enter, -age, -us_citizen, -home_zip, -high_school, -sat_math, 
                -sat_verbal, -timehorizon, -degreecip, -term_degree, -degree,
                -term_course, -course, -number, -section, -hours_course, -type,
                -pass_fail, -faculty_rank)

# Choose institutions
# Some seem to have very few URM students which is suspect
# Eliminate these: D, J, M
# Rename remaining schools with real names
badInstitutions <- paste0("Institution ", c("D", "J", "M"))
data <- data %>%
  dplyr::filter(!institution %in% badInstitutions) %>%
  dplyr::mutate(institution = dplyr::case_when(
    institution == "Institution B" ~ "University of Colorado",
    institution == "Institution C" ~ "Colorado State University",
    institution == "Institution D" ~ "Elizabethtown College",
    institution == "Institution L" ~ "University of Oklahoma",
    TRUE ~ institution)) %>%
  dplyr::mutate(institution = factor(institution))

# Read in prefixes, merge into data
# Make nice flag for STEM classes
prefixes <- read_excel("courseAbbrev.xlsx")
prefixes <- prefixes %>%
  dplyr::select(institution, abbrev, classification)
data <- merge(data, prefixes, all.x = TRUE, all.y = FALSE)
data <- data %>%
  dplyr::mutate(stemcourse = dplyr::case_when(
    classification == "Non-NSTEM" ~ FALSE,
    !is.na(classification) ~ TRUE,
    TRUE ~ NA))

# Set DFW flag
data <- data %>%
  dplyr::mutate(dfw = dplyr::case_when(
    grade %in% c("D+", "D", "D-", "F","U", "W") ~ TRUE,
    TRUE ~ FALSE
  ))

# Drop some more variables
data <- data %>%
  dplyr::select(-abbrev, -classification, -grade)

# Filter out students with no STEM classes
stemcoursestudents <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(stemcount = sum(stemcourse)) %>%
  dplyr::select(id, stemcount) %>%
  dplyr::filter(stemcount > 0) %>%
  dplyr::pull(id)
data <- data %>%
  dplyr::filter(id %in% stemcoursestudents)

# Summarize by count of STEM DFW classes and count of STEM DWF classes
stemdfwcount <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(stemdfw = sum(stemcourse & dfw)) %>%
  dplyr::mutate(stemdfw = stemdfw > 0)

# Merge in
data <- merge(data, stemdfwcount)

# Select relevant variables
# Reduce to unique rows
# Clean up variables
# Rename/reorder variables
# Throw away incomplete records
data <- data %>%
  dplyr::select(institution, sex, race, act_comp, nostemintent, stemdegree, stemdfw) %>%
  dplyr::mutate(sex = dplyr::case_when(
    sex == "Male" ~ "male",
    sex == "Female" ~ "female",
    TRUE ~ NA_character_)) %>%
  dplyr::mutate(sex = factor(sex)) %>%
  dplyr::mutate(sex = relevel(sex, ref = "male")) %>%
  dplyr::rename("act" = "act_comp") %>%
  unique %>%
  na.omit %>%
  droplevels

# Matching to balance data
mat <- matchit(stemdfw ~ sex*race*nostemintent*stemdegree, data = data, method = "cem")
mat.sum <- summary(mat)
plot(mat.sum)
modeldata <- match.data(mat, drop.unmatched = TRUE) %>%
  dplyr::mutate(subclass = factor(subclass)) %>%
  dplyr::mutate(subclass = relevel(subclass, ref = "2"))

# Create model
M <- glm(stemdegree ~ act + nostemintent + race*stemdfw,
          weights = weights, data = modeldata, family = "binomial")
S <- step(M)
summary(S)

# Subclasses
# 1 = male urm
# 2 = male nonurm
# 3 = female urm
# 4 = female nonurm
# Subclass1  = intercept
# Subclass2 = intercept + subclass2
# Subclass3 = intercept + subclass3
# Subclass4 = intercept + subclass 4
# Subclass1dfw = intercept + dfw
# Subclass2dfw = intercept + dfw + sunclass2 + subclass2:dfw
# Subclass3dfw = intercept + dfw + sunclass3 + subclass3:dfw
# Subclass4dfw = intercept + dfw + sunclass4 + subclass4:dfw
# Subclass1change = -dfw
# Subclass2change = -dfw - subclass2:dfw
# Subclass3change = -dfw - subclass3:dfw
# Subclass4change = -dfw - subclass4:dfw
# Contrasts: 2 - 1, 1 - 3, 2 - 3, 2 - 4, 3 - 4
L <- matrix(rep(0,5*length(coef(M))),nrow = 5)
colnames(L) <- names(coef(M))
L[1,c("subclass2:stemdfwTRUE")] <- -1
L[2,c("subclass3:stemdfwTRUE")] <- 1
L[3,c("subclass3:stemdfwTRUE","subclass2:stemdfwTRUE")] <- c(1,-1)
L[4,c("subclass4:stemdfwTRUE","subclass2:stemdfwTRUE")] <- c(1,-1)
L[5,c("subclass4:stemdfwTRUE","subclass3:stemdfwTRUE")] <- c(1,-1)
rownames(L) <- c("2 - 1","1 - 3","2 - 3","2 - 4","3 - 4")
glht(M, linfct = L) %>%
  tidy %>%
  dplyr::select(contrast,estimate,adj.p.value) %>%
  rename("disparity" = "contrast")

  


