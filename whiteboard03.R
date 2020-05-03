## Whiteboard03.R
## Whiteboard #2

## install.packages(c("dplyr", "rio", "purrr"))
library(dplyr)
library(tidyr)
library(rio)
library(purrr)

gitHubURL <- "https://raw.githubusercontent.com/prosumer1001/ProgrammingAssignment3/master/outcome.csv"

outcome_df <- import(gitHubURL)
outcome_df <- outcome_df %>% select(-City)
str(outcome_df)
outcome_df$State <- as.factor(outcome_df$State)
str(outcome_df)

outcome = "heart attack"
num = 20


example <- outcome_df %>%
        filter(deathCause == outcome) %>%
        filter(!is.na(deathRate)) %>%
        group_by(State) %>% # Group each by the State
        mutate(rank = row_number(deathRate)) %>% # generate a ranking variable
        filter(rank == num)%>% # filter by choosing the rank number
        select(Hospital, State, deathCause, deathRate, rank); example

example %>%
        group_by(State) %>%
        mutate(prev = lag(State), order_by(State)) %>%
        filter(State <= prev | is.na(prev))
