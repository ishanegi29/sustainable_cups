library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)
library(knitr)

# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

# Import raw data
f1 <- read_csv(here("data", "FinalSurvey1.csv"))
f2 <- read_csv(here("data", "FinalSurvey2.csv"))
f3 <- read_csv(here("data", "FinalSurvey3.csv"))


# Format and join the three surveys -------

# Variables common to each survey: 
# session  = a unique ID for the Run - should be the same across all surveys
# created  = time stamp when survey was started
# modified = time stamp when survey was last modified by respondent
# ended    = time stamp when survey ended
# expired  = time stamp when survey expired (if respondent didn't reach end)

# Compute time values for each part
f1 <- f1 %>% 
    mutate(
        created = ymd_hms(created, tz = "EST"),
        ended =  ymd_hms(ended, tz = "EST"),
        time_sec_p1 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p1, consentUnderstand, screenout)

f2 <- f2 %>% 
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_p2 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p2, respondentID, starts_with("cbc"), cbcAllSame)

f3 <- f3 %>% 
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_p3 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p3, yearOfBirth:feedback)

# Join all parts together using the session variable
data <- f1 %>% 
    left_join(f2, by = "session") %>% 
    left_join(f3, by = "session") %>% 
    # No longer need session variable
    select(-session)
head(data)

view(data)



# Filter out bad responses ---------

nrow(data)

# Drop people who got screened out
data <- data %>% 
    filter(!is.na(screenout), screenout == 1)
nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>% 
    filter(!is.na(cbc1)) %>% 
    filter(!is.na(cbc2)) %>% 
    filter(!is.na(cbc3)) %>% 
    filter(!is.na(cbc4)) %>% 
    filter(!is.na(cbc5)) %>% 
    filter(!is.na(cbc6)) %>% 
    filter(!is.na(cbc7)) %>% 
    filter(!is.na(cbc8))
nrow(data)

# Drop respondents who went too fast
data <- data %>% 
    mutate(
        # First replace NA values with 0 seconds
        time_sec_p1 = ifelse(is.na(time_sec_p1), 0, time_sec_p1),
        time_sec_p2 = ifelse(is.na(time_sec_p2), 0, time_sec_p2),
        time_sec_p3 = ifelse(is.na(time_sec_p3), 0, time_sec_p3),
        # Now compute the total time
        time_min_total = (time_sec_p1 + time_sec_p2 + time_sec_p3) / 60
    )
# Look at summary of completion times
summary(data$time_min_total)
# Drop anyone who finished in under the 10th percentile of completion times
time_10 <- quantile(data$time_min_total, 0.1)
nrow(data)
data <- data %>% 
    filter(time_min_total >= time_10)
nrow(data)

# Drop respondents that got the attention check question wrong
data <- data %>% 
    filter(cbcPractice == 2)
nrow(data)

data <- data %>% 
    filter(yearOfBirth != "prefer_not_say") %>% 
    transform(yearOfBirth = as.numeric(yearOfBirth))%>% 
    mutate(group = ifelse((2021-yearOfBirth <= 25), "A", "B"))

# Create choice data ---------

# First gather the data
choiceData <- data %>% 
    pivot_longer(
        cols = cbc1:cbc8,
        names_to = "qID",
        values_to = "choice") %>% 
    # Convert the qID variable to a number
    mutate(qID = parse_number(qID))

head(choiceData)

# Read in choice questions and join it to the choiceData
survey <- read_csv("https://formr.org/assets/tmp/admin/t9LU04_6qJo68GFcg1tHgfW4f2ekG-ZWKPZXHQVGtHeJ.txt?v1636593399")
choiceData <- choiceData %>% 
    rename(respID = respondentID) %>% 
    left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData <- choiceData %>% 
    mutate(choice = ifelse(choice == altID, 1, 0)) %>% 
    # Drop unused variables
    select(-image, -cbcPractice, -cbcAllSame)

head(choiceData)

# Create new values for respID & obsID
nRespondents <- nrow(data)
nAlts <- max(survey$altID)
nQuestions <- max(survey$qID)
choiceData$respID <- rep(seq(nRespondents), each = nAlts*nQuestions)
choiceData$obsID <- rep(seq(nRespondents*nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choiceData <- choiceData %>% 
    select(ends_with("ID"), "choice", everything())

head(choiceData)

# Save cleaned data for modeling
write_csv(choiceData, here("data", "choiceData.csv"))

choice <- read_csv(here('data', 'choiceData.csv'))
head(choice)
