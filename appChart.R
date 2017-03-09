# mobile UI to show chart
# last update: 2017-03-09

source('appSelect.R')

appChart<- function(){
        tabPanel('Diagramm',
                 appSelect(),
                 bsAlert('dataStatus'),
                 plotlyOutput('trendChart')
        )
}