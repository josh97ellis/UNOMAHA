# Import Library for Cleaning and Visualizing
library(tidyverse)


# Set Working Directory
setwd('C:\\Users\\Josh Ellis\\OneDrive - University of Nebraska at Omaha\\COURSES\\SPRING_2022\\STAT8416_INTRO_TO_DATA_SCIENCE\\MISC')


# Global Function to Import data, renames columns, and drop null fields  
import_function <- function(df, file_name){
  file_name <- 'ZOOM_SESSION_FORMS_DATA.csv'
  # Step 1: Import Dataframe
  df <- read_csv(file = file_name)
  
  # Step 2: Rename Columns
  df <- df %>% 
    rename(
      c(
        'id' = 'ID',
        'StartDateTime' = 'Start time',
        'CompleteTime' = 'Completion time',
        'Points' = 'Total points',
        'Feedback' = 'Quiz feedback',
        'Answer' = "Would you be interested in joining a meet and greet zoom meeting to get to know each other?" ,
        'QPoints' = "Points - Would you be interested in joining a meet and greet zoom meeting to get to know each other?",
        'QFeedback' = "Feedback - Would you be interested in joining a meet and greet zoom meeting to get to know each other?"
        )
      )
  
  # Step 3: Drop null Fields
  df <- df %>%
    select(
      -c(
        'Email',
        'Name',
        'Points',
        'Feedback',
        'QPoints',
        'QFeedback'
      )
    )
}

df <- import_function(df, file_name)

df$Answer <- df$Answer %>% str_replace_all(';', '')


# Analysis
n_students = 29
n_respondents = nrow(df)
n_no_response = n_students - n_respondents

results_summary <- df %>%
  group_by(Answer) %>%
  summarise(Count = n()) %>%
  add_row(
    Answer = 'No Response',
    Count = n_no_response) %>%
  mutate(
    PercentOfClass = round((Count / n_students)*100, 0)
  )

results_summary %>%
  ggplot(aes(x=Answer, y=Count, fill=Answer)) +
  geom_col() +
  geom_text(aes(label = paste0(PercentOfClass, "%")), position = position_stack(vjust=0.5)) +
  scale_y_continuous(breaks = round(seq(0, 15, by = 1),1)) +
  labs(
    title = 'Results of Zoom Meeting Quiz',
    y = 'Number of Students',
    x = 'Answer')

