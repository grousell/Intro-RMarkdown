
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(rmarkdown)

increase <- "increase in percentage"
decrease <- "decrease in percetnage"
no_change <- "no change"

# ggplot theme ------------------------------------------------------------

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),                 ##requires grid package starts at top, right, bottom,left
  title = element_text (colour="black", size=15),                  ##Colour and size of chart title
  
  panel.background = element_rect(fill="NA"),                     ##Background colour of chart
  panel.border = element_blank(),                                 ##No border around chart panel
  panel.grid.major.y = element_line(colour="grey90"),             ##Major y-axis gridline colour
  panel.grid.minor.y = element_line(colour="NA"),                 ##Minor y-axis gridline colour
  panel.grid.major.x = element_line(colour="NA"),             ##Major y-axis gridline colour
  panel.grid.minor.x = element_line(colour="NA"),                 ##Minor y-axis gridline colour
  
  axis.text.y = element_text (colour="black", size=10, hjust=1),  ##Colour and size of y-axis text
  axis.title.y = element_text (colour="black", size=12, angle=90), ##Colour, size and angle of y axis title
  
  axis.text.x = element_text (colour="black", size=8,angle=0),   ##Colour, size, angle of x-axis text
  axis.title.x = element_text (colour="black", size=12),          ##Colour and size of x-axis title
  
  
  legend.text = element_text (colour="black", size = 12),          ##Colour and size of legend text
  legend.position = ("bottom"),                                   ##Position of legend
  legend.title = element_text(colour = "black"),                                  ##Removes title from legend box
  
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)
  )
  

# Load Data ---------------------------------------------------------------

df <- read_csv ("https://raw.githubusercontent.com/grousell/Intro-RMarkdown/main/data/sample_data.csv")

factor_levels <- c ("1 - Not at All", 
                    "2",
                    "3", 
                    "4", 
                    "5 - Significantly")

df <- df %>% 
  mutate_at(vars (starts_with("session")),
            ~(parse_factor(., levels = factor_levels)
            )
  )

# Summary Table

summary_table <- df %>% 
  pivot_longer(cols = starts_with("session"),
               names_to = "question",
               values_to = "response") %>%
  group_by (school, question) %>% 
  count (response) %>% 
  mutate (Percent = n / sum(n),
          question = dplyr::recode(question, 
                                   session_applicable = "The session was applicable to my work",  
                                   session_changed = "The session changes my thinking",  
                                   session_reinforced  = "The session reinforced my thinking",  
                                   session_useful = "The session was useful")
          ) %>% 
  drop_na (response)
  

# Loop --------------------------------------------------------------------

for (sch in unique(df$school)) {
  rmarkdown::render("PDF_Example_Advanced.Rmd",
                    output_file =  paste(sch, "_report.pdf", sep=''), 
                    output_dir = './reports/')
}


