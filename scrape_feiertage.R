
# file scrapes the Austrian public holidays from the website of the Austrian government
# the data is stored in a list of dataframes, which are then combined into one dataframe
# the dates are converted to the Date format

# load libraries
library(rvest)
library(XML)
library(dplyr)


feiertage_list <- list()

for(i in 1:4){
  
  html <- paste0("https://edm.gv.at/edm_portal/redaList.do?d-49520-s=0&seqCode=c47h8ycd24ybf3&d-49520-p=", i, "&d-49520-o=2")
  
  html_text <- read_html(html) %>% html_node(xpath = '//*[@id="list"]')
  
  feiertage_list[[i]] <- html_table(html_text)
  
  
}

feiertage_df <- bind_rows(feiertage_list)

feiertage_df <- feiertage_df %>% mutate(Datum = as.Date(Datum)) %>%
  rename(Feiertag = Bezeichnung)

# save file
readr::write_rds(feiertage_df, file = "./feiertage.rds")
