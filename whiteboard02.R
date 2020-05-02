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

outcome = "heart attack"
num = 20

rankState <- outcome_df %>%
        filter(!is.na(deathCause)) %>% # Remove all NA in outcome
        filter(deathCause == outcome) %>% # Include only outcome
        group_by(State) %>% # Group each by the State
        mutate(rank = row_number(deathRate)) %>% # generate a ranking variable
        filter(rank == num)%>% # filter by choosing the rank number
        select(Hospital, State, deathCause, deathRate, rank)

str(rankState)


## How many hospitals does each state have? (Top)
window <- rankState %>%
        group_by(State) %>%
        summarise(rank_n = n())
View(window)

## Texas has the most hospitals at 370.
## How many hospitals does each state have? (Top)

