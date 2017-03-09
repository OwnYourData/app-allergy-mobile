# application specific logic
# last update: 2017-03-01

source('srvScheduler.R', local=TRUE)

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# anything that should run only once during startup
appStart <- function(){
        app <- currApp()
        if(length(app) > 0){
                # polliation options in chart
                allItems <- readPlzItems()
                plz <- allItems$plzCode
                if(length(plz) > 0){
                        plzPollList <- as.character(sort(mapply(
                                paste0, 
                                rep(plz, length(pollenList)), 
                                ': ',
                                rep(pollenList, length(plz)))))
                        pollSelectDefault <- plzPollList[1]
                        updateSelectInput(
                                session,
                                'pollSelect',
                                choices = c('Pollen ausblenden', plzPollList),
                                selected = pollSelectDefault)
                        updateSelectInput(
                                session,
                                'pollTableSelect',
                                choices = c('persönliche Aufzeichnungen', plzPollList),
                                selected = 'persönliche Aufzeichnungen')
                }
                
                # Pollination selection in chart
                url <- itemsUrl(app[['url']],
                                paste0(appKey, '.mobilechart_config'))
                cfg <- readItems(app, url)
                if(nrow(cfg) == 1){
                        updateSelectInput(session, 'pollSelect',
                                          selected = cfg$pollSelect)
                }
        }
}

# get stored PLZs
readPlzItems <- function(){
        app <- currApp()
        plzItems <- data.frame()
        if(length(app) > 0){
                retVal <- readSchedulerItemsFunction()
                if(nrow(retVal) > 0){
                        retVal <- retVal[retVal$task == 'Rscript', , drop = FALSE]
                        if(nrow(retVal) > 0){
                                plzItems <- retVal
                                rownames(plzItems) <- plzItems$name
                                plzItems <- plzItems[, c('parameters.replace.plz', 'id')]
                                colnames(plzItems) <- c('plzCode', 'id')
                        }
                }
        }
        plzItems
}

observeEvent(input$saveAllergyInput, {
        app <- currApp()
        if(length(app) > 0){
                myDate <- as.character(input$dateInput)
                
                # Befinden
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], '.condition'))
                data <- list(
                        date = myDate,
                        value = input$conditionInput,
                        '_oydRepoName' = 'Befinden')
                writeItem(app, url, data)
                
                # Medikamenteneinnahme
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], '.medintake'))
                data <- list(
                        date = myDate,
                        value = input$medInput,
                        '_oydRepoName' = 'Medikamenteneinnahme')
                writeItem(app, url, data)
                
                # Notiz
                if(!is.na(input$diaryInput) &
                   (nchar(as.character(input$diaryInput)) > 0))
                {
                        url <- itemsUrl(app[['url']],
                                        paste0(app[['app_key']], '.diary'))
                        data <- list(
                                date = myDate,
                                value = input$diaryInput,
                                '_oydRepoName' = 'Tagebuch')
                        writeItem(app, url, data)
                }
                
                output$allergyInputStatus <- renderUI('Daten wurden erfolgreich gespeichert')
        }
})

dateRangeSelect <- function(data, ts = TRUE){
        if(nrow(data) > 0){
                myRange <- c(mymin = as.Date(Sys.Date()),
                             mymax = as.Date(Sys.Date()))
                switch(input$dateSelect,
                       '1' = { myRange <- c(mymin = as.Date(Sys.Date()-7),
                                            mymax = as.Date(Sys.Date()+2)) },
                       '2' = { myRange <- c(mymin = as.Date(Sys.Date() - months(1)),
                                            mymax = as.Date(Sys.Date()+2)) },
                       '3' = { myRange <- c(mymin = as.Date(Sys.Date() - months(2)),
                                            mymax = as.Date(Sys.Date()+2)) },
                       '4' = { myRange <- c(mymin = as.Date(Sys.Date() - months(6)),
                                            mymax = as.Date(Sys.Date()+2)) },
                       '5' = { myRange <- c(mymin = as.Date(paste(year(Sys.Date()),'1','1',sep='-')),
                                            mymax = as.Date(paste(year(Sys.Date()),'12','31',sep='-'))) },
                       '6' = { myRange <- c(mymin = as.Date(Sys.Date() - months(12)),
                                            mymax = as.Date(Sys.Date()+2)) },
                       '10'= { myRange <- c(mymin = as.Date('1970-01-01'),
                                            mymax = as.Date('2070-01-01')) },
                       {})
                mymin <- myRange['mymin']
                mymax <- myRange['mymax']
                daterange <- seq(mymin, mymax, 'days')
                if(ts){
                        data[as.Date(as.POSIXct(data$timestamp, origin = '1970-01-01')) 
                             %in% daterange, ]
                } else {
                        data[as.Date(data$date) %in% daterange, ]
                }
        } else {
                data.frame()
        }
}

pollData <- function(pollSelect){
        if(pollSelect == 'keine'){
                data.frame()
        } else {
                plz <- strsplit(pollSelect, ':')[[1]][1]
                typ <- trimws(strsplit(pollSelect, ':')[[1]][2])
                repo <- paste0(app_id, '.pollination', plz)
                data <- repoData(repo)
                data <- data[data$pollType == typ, ]
                if(nrow(data) > 0){
                        data$belastungTxt <- lapply(data$value, function(x){
                                switch(as.character(x), 
                                       '0'='keine Belastung', 
                                       '1'='niedrige Belastung',
                                       '2'='mittlere Belastung',
                                       '3'='hohe Belastung',
                                       '4'='sehr hohe Belastung')})
                        data
                } else {
                        data.frame()
                }
        }
}

observeEvent(input$pollSelect, {
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                paste0(appKey, '.mobilechart_config'))
                cfg <- readItems(app, url)
                if(nrow(cfg) > 1){
                        deleteRepo(app, url)
                        cfg <- data.frame()
                }
                data <- list(
                        pollSelect = input$pollSelect,
                        '_oydRepoName' = 'Konfiguration (Mobil)')
                if(nrow(cfg) == 0){
                        writeItem(app, url, data)
                } else {
                        updateItem(app, url, data, cfg$id)
                }
        }
})

output$trendChart <- renderPlotly({
        pdf(NULL)
        pollSelect <- input$pollSelect
        pollData <- dateRangeSelect(pollData(pollSelect))
        condData <- dateRangeSelect(
                repoData('eu.ownyourdata.allergy.condition'), FALSE)
        medData <- dateRangeSelect(
                repoData('eu.ownyourdata.allergy.medintake'), FALSE)
        # medData$medTxt <- ''
        # medData[medData$value, 'medTxt'] <- 'Medikament eingenommen'
        diaryData <- dateRangeSelect(
                repoData('eu.ownyourdata.allergy.diary'), FALSE)
        
        plotly_output <- FALSE
        outputPlot <- plot_ly()
        if(nrow(medData) > 0){
                plotly_output <- TRUE
                outputPlot <- plot_ly(x = as.POSIXct(medData$date),
                                      y = as.integer(medData$value)*6,
                                      type = 'bar',
                                      name = 'Medikament',
                                      text = 'Medikament eingenommen',
                                      marker = list(color = 'rgb(158,202,225)'))
        }
        if(nrow(pollData) > 0){
                plotly_output <- TRUE
                outputPlot <- outputPlot %>%
                        add_lines(x = as.POSIXct(pollData$timestamp,
                                                 origin = '1970-01-01'),
                                  y = as.numeric(pollData$value),
                                  yaxis = 'y2',
                                  name = paste0(substr(pollSelect, 1, 12), "\u2026"),
                                  line=list(
                                          width = 2,
                                          color = 'red',
                                          shape = 'spline')) %>%
                        add_markers(x = as.POSIXct(pollData$timestamp,
                                                   origin = '1970-01-01'),
                                    y = as.numeric(pollData$value),
                                    yaxis = 'y2',
                                    name = '',
                                    marker = list(
                                            color='red',
                                            size = 6),
                                    text = paste0(
                                            pollSelect, ' - ',
                                            pollData$belastungTxt),
                                    showlegend = FALSE)
        }
        if(nrow(condData) > 0){
                plotly_output <- TRUE
                outputPlot <- outputPlot %>%
                        add_lines(x = as.POSIXct(paste(as.Date(condData$date), "0:00")),
                                  y = as.numeric(condData$value),
                                  name = 'Befinden',
                                  hoverinfo = 'text',
                                  text = '',
                                  line=list(
                                          width = 3,
                                          color = 'blue',
                                          shape = 'spline'))
        }
        if(nrow(diaryData) > 0){
                plotly_output <- TRUE
                diaryCondData <- merge.data.frame(condData,
                                                  diaryData,
                                                  by='date',
                                                  all = TRUE)
                colnames(diaryCondData) <- c('date', 'id', 'value', 'id2', 'diary')
                diaryCondData$size <- 10
                diaryCondData[is.na(diaryCondData$diary), 'size'] <- 6
                outputPlot <- outputPlot %>%
                        add_markers(x = ~as.POSIXct(paste(as.Date(diaryCondData$date), "0:00")),
                                    y = ~as.numeric(diaryCondData$value),
                                    text = diaryCondData$diary,
                                    marker = list(
                                            color= 'blue',
                                            size = diaryCondData$size),
                                    name = 'Befinden',
                                    showlegend = FALSE)
        }
        
        if(plotly_output){
                outputPlot <- outputPlot %>%
                        layout( title = '',
                                xaxis = list(
                                        title = '',
                                        type = 'date',
                                        tickformat = '%Y-%m-%d'),
                                yaxis = list(
                                        title = '< besser - Befinden - schlechter >',
                                        titlefont = list(
                                                color='blue'),
                                        range = c(0,6.5),
                                        nticks = 1),
                                yaxis2 = list(
                                        overlaying = 'y',
                                        side = 'right',
                                        title = 'Pollenbelastung',
                                        range = c(-0.5, 4.5),
                                        nticks = 6)
                        )
        } else {
                outputPlot <- plotly_empty()
        }
        dev.off()
        outputPlot
})

output$tableList <- DT::renderDataTable(datatable({
        switch(as.character(input$pollTableSelect),
               'persönliche Aufzeichnungen'={
                       condData <- dateRangeSelect(
                               repoData('eu.ownyourdata.allergy.condition'), FALSE)
                       diaryData <- dateRangeSelect(
                               repoData('eu.ownyourdata.allergy.diary'), FALSE)
                       medData <- dateRangeSelect(
                               repoData('eu.ownyourdata.allergy.medintake'), FALSE)
                       data <- combineData(combineData(condData, diaryData), medData)
                       if(nrow(data) > 0){
                               data$Medikament <- 'Nein'
                               data[data$value, 'Medikament'] <- 'Ja'
                               data <- data[, c('date', 'value.x', 'Medikament', 'value.y')]
                               colnames(data) <- c('Datum', 'Befinden', 'Medikament', 'Notiz')
                               data$Datum <- format(as.Date(data$Datum), '%y-%m-%d')
                               data[data$Medikament, ]
                       }
                       data },
               { pollData <- dateRangeSelect(pollData(input$pollTableSelect))
               if(nrow(pollData) > 0){
                       pollData$Datum <- format(as.POSIXct(pollData$timestamp, 
                                                           origin='1970-01-01'), 
                                                '%Y-%m-%d %k Uhr')
                       pollData <- pollData[, c('Datum', 'value', 'belastungTxt')]
                       colnames(pollData) <- c('Datum', 'Pollenbelastung', 'Beschreibung')
                       pollData
               } else {
                       data.frame()
               }})
}, options = list(
        language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'),
                searching = FALSE,
                lengthChange = FALSE,
                pageLength = 10
        )
))
