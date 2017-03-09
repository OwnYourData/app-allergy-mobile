# UI for selecting a date-range in the table view
# last update: 2017-03-09

appTableSelect <- function(){
        tagList(
                selectInput('dateTableSelect',
                           label = 'Auswahl',
                           choices = c('letzte Woche'='1',
                                       'letztes Monat'='2',
                                       'letzten 2 Monate'='3',
                                       'letzten 6 Monate'='4',
                                       'aktuelles Jahr'='5',
                                       'letztes Jahr'='6',
                                       'alle Daten'='10',
                                       'individuell'='7'),
                           selected = 1),
                selectInput('pollTableSelect',
                            label = NA,
                            choices = c('keine'),
                            selected = 'keine'),
                hr()
        )
}
