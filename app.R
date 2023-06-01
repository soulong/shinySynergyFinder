

library(shiny)


source('synergyFinder.R')



server <- function(input, output) {
  
  data <- eventReactive(input$submit, {
    tmpdir <- tempdir()
    withProgress(
      read_csv(input$input_file$datapath) %>%
        run_synergyfinder(type=input$type, save_dir=tmpdir),
      message='Calculating ...', value=0.4)
    res_files <- list.files(tmpdir, pattern='(.pdf)|(.xlsx)', full.names=F)
    # print(res_files)
    return(list(tmpdir=tmpdir, res_files=res_files))
  })
  
  output$res_show <- renderPrint({
    data()
  })
  
  output$download <- downloadHandler(
    filename = 'result.zip',
    content = function(fname) {
      setwd(data()$tmpdir)
      zip(zipfile=fname, files=data()$res_files)
    },
    contentType = "application/zip"
  )
}

ui <- shinyUI(
  fluidPage(
    title='SynergyFinder',
    titlePanel('SynergyFinder from Bioconductor Version'),
    sidebarLayout(
      sidebarPanel(
        fileInput('input_file', 'Upload formated csv file', accept='.csv'),
        radioButtons('type', 'Effect type', c('viability','inhibition'), selected='viability', inline=F),
        actionButton('submit', 'Submit'),
        downloadButton('download', 'Download', icon=icon("file-download"))
      ),
      mainPanel(
        verbatimTextOutput('res_show')
      )
    )
  )
)


app <- shinyApp(ui, server)


# run locally
if(interactive()) {
  runApp(app, host='0.0.0.0', port=5000L)
}

# run with GitHub
if(F) {
  runGitHub('shinySynergyFinder', 'soulong', host='0.0.0.0', port=5000L)
}




