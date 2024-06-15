
datatable_kalender <- function(kalender){
  
  
  # create datatable
  datatable(kalender, 
            class = list(stripe = FALSE),
            rownames = FALSE,
            escape = FALSE) %>% 
    
    # grey if Fri - Sun
    formatStyle("Tag", target = "row", 
                backgroundColor = styleEqual(as.character(wday(c(1:7), label = TRUE)), 
                                             c("#f1f1f1f1", rep("#fff",4), rep("#f1f1f1f1",2)))) %>%
    # grey if Gleittag
    formatStyle("FMA", target = "row",
                backgroundColor = styleEqual("Gleittag", c("#f1f1f1f1"))) %>%
    
    # grey if Feiertag
    formatStyle("Feiertag", target = "row",
                backgroundColor = styleEqual(feiertage_df$Feiertag, c("#f1f1f1f1")))
  
  
}