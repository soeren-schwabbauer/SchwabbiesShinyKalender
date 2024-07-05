

# This function prepare the calender. No input needed

prepare_kalender <- function(termine, feiertage_df){
  
  # empty calendar -------------------------------------------------------------
  date_today <- as.Date(Sys.Date(), format = "%Y-%m-%d")
  seq_date   <- seq.Date(from = date_today, to = date_today+999, by = "day")
  
  kalender_empty <- data.frame(KW = strftime(seq_date, format = "%V"),
                               Datum = as.Date(seq_date, format = "%Y-%m-%d"),
                               Tag = wday(seq_date, label = TRUE))
  
  
  # split termine df into lists ------------------------------------------------
  termine$Datum <- as.Date(termine$Datum, format = "%Y-%m-%d")
  termine <- split(termine, termine$Typ)
  
  # Edit Geburtstage -----------------------------------------------------------
  
  # Alter berechnen
  jahr_heute <- gsub("-.*", "", Sys.Date() )
  jahr_heute <- as.numeric(jahr_heute)
  
  termine[["Geburtstage"]] <- termine[["Geburtstage"]] %>%
    mutate(jahr_geburtstag = year(Datum),
           jahr_heute = as.numeric(gsub("-.*", "", Sys.Date())),
           # Datum bekommt aktuelles Jahr
           Datum = as.Date(paste0(jahr_heute, "-", month(Datum), "-", day(Datum))),
           # Alter anfügen
           alter = jahr_heute - jahr_geburtstag,
           Termin = paste0(Termin, " (", alter, ")")) %>%
    select(Datum, Termin, Typ)
  
  # Geburtstage für nächstes Jahr anfügen (wichtig bei Jahreswechsel)
  geburtstag_naechtesjahr <- termine[["Geburtstage"]]  %>%
    mutate(Datum = as.Date(Datum) + lubridate::years(1))
  termine[["Geburtstage"]] <- bind_rows(termine[["Geburtstage"]], geburtstag_naechtesjahr)
  
  
  # join all termine by date to a sinlge dataframe -----------------------------
  termine %<>% bind_rows(.) %>% 
    group_by(Datum, Typ) %>% 
    summarise(Termin = paste(Termin, collapse = "<br/>")) %>%
    
    tidyr::pivot_wider(names_from = Typ, values_from = Termin)
  
  # join empty calendar with termine and feiertage -----------------------------
  kalender <- kalender_empty %>% left_join(termine, by = "Datum") %>%
    
    left_join(feiertage_df, by = "Datum") %>%
    
    # KW ganz ans Ende
    select(Datum, Tag, Privat, FMA, Uni, FMA, Karo, Geburtstage, Feiertag, KW)
  
  # output ---------------------------------------------------------------------
  return(kalender)
}
