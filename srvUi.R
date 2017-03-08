# server-side mobile UI logic
# last update: 2017-01-03

output$hdrImageLinkMobile <- renderUI({
        # fix for "Raspberry Navigation"
        renderHdrImageLink()
})

headerTitle <- function() {
        app <- currApp()
        if(length(all.equal(app, logical(0)))>1){
                app <- currApp()
                url <- itemsUrl(app[['url']], 'eu.ownyourdata.info')
                infoRec <- readItems(app, url)
                if(nrow(infoRec) == 1){
                        infoRec$name
                } else {
                        "OwnYourData"                        
                }
        } else {
                "OwnYourData"
        }
}

renderHdrImageLink <- function() {
        tags$div(
                tags$a(href=input$store$pia_url,
                       tags$img(height='25px', style='margin-top:-5px',
                                src=oydLogo)),
                tags$a(href=input$store$pia_url, headerTitle())
        )
}
