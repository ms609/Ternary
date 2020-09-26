library("shiny")
library("colourpicker")
library("Ternary")

palettes <- list("#91aaa7",
                 c("#969660", "#c3dfca"),
                 c("#be83ae", "#2ea7af", "#fbcdcf"),
                 c("#72c5a9", "#b7c5ff", "#dbdabb", "#a28bac"),
                 c("#59c4c0", "#ea9a9a", "#7998a6", "#e9d7a9", "#9c9379"),
                 c("#e8b889", "#67c6eb", "#e5d5c2", "#938fba", "#64b69a", "#779c7b"),
                 c("#c4808f", "#5ba08f", "#f0a693", "#ccd7fe", "#cdb87e", "#c6aae2", "#d2dad8"),
                 c("#d0847f", "#63a5d7", "#d7b981", "#5a9bbb", "#9bb67e", "#dea6d5", "#91967e", "#ca7f96"),
                 c("#8b93a8", "#ccb97e", "#8e9dd7", "#57a384", "#dbb1e7", "#2da7af", "#d68986", "#75d2f9", "#e4d1f0"),
                 c("#dfcf92", "#40b3cb", "#b88a61", "#ecb2e0", "#d6dbbc", "#a28bae", "#edcfeb", "#7498ab", "#b187a0", "#8f939c"),
                 c("#a98f70", "#7be5e2", "#d295c0", "#9ae2bd", "#d3b7f1", "#eca88d", "#8993cd", "#ffc7bb", "#8695a8", "#b3e1df", "#b6878a"),
                 c("#eaa9d3", "#7ac09b", "#fdaca8", "#8ce7e4", "#eed69b", "#70a4d9", "#e8d6ba", "#589bbb", "#959672", "#d0dbd1", "#9b9282", "#d9d9c6"),
                 c("#7498ab", "#e5bd8a", "#7ed8ff", "#de8f8e", "#46bac6", "#ffc0d3", "#57ae96", "#f7cddd", "#50a098", "#b58a6d", "#add49d", "#a18da1", "#cedad9"),
                 c("#8097a4", "#d0dea9", "#a78cc3", "#aee4bf", "#bb82a8", "#5dc9c6", "#b88690", "#26a3b9", "#ad8e6f", "#a4e2ef", "#869a65", "#efcfdd", "#60a089", "#9e927b"),
                 c("#b9aae5", "#bbd69c", "#e2adde", "#77a777", "#f8abc8", "#8ee7ce", "#f2a1a5", "#81bdf1", "#f2bb91", "#b8dcfe", "#aeb276", "#f2cdef", "#e8d6b2", "#8d92b0", "#b7878d"),
                 c("#c3d79b", "#b28cc0", "#64a985", "#e3a7d4", "#2ea2aa", "#e69892", "#85c6f9", "#fbd1a0", "#7696be", "#89996c", "#ddcdff", "#719d89", "#f5cde6", "#b6e0da", "#e8d4cd", "#b5ddfa"),
                 c("#a98d83", "#84e1ff", "#bb8964", "#46b1d1", "#ffbfa5", "#6199c0", "#bbcb8f", "#bf82ab", "#85ddc4", "#eea0ba", "#c1d8ff", "#c3818b", "#c5c6ff", "#999388", "#e8cbff", "#ffb5b6", "#d2dad7"),
                 c("#faccde", "#60a987", "#c6abe4", "#6f9e77", "#c48093", "#a5e5d3", "#cc8f6f", "#499fae", "#d9dca6", "#7796b8", "#bee1ba", "#b4daff", "#919583", "#e2d3e9", "#47a19b", "#ebd4bc", "#7c9993", "#a9e3e0"),
                 c("#739e6e", "#ffbfd9", "#43b6bb", "#e8ad88", "#5e9bce", "#c2af75", "#a8e0fe", "#fad0a8", "#679e8d", "#ffc7b1", "#abe5c0", "#ac8d78", "#c5dddc", "#a48f84", "#cadfb0", "#899694", "#fdcdc1", "#d1dad5", "#dfd8c4"),
                 c("#6e9c93", "#ffb4b3", "#7ec6a2", "#eeccfe", "#cddb9d", "#8a90c5", "#dcb983", "#77bff0", "#f0ab92", "#90ddff", "#f1d3a9", "#b5c2fe", "#c1e1b7", "#7596ba", "#bce1c4", "#a88c96", "#5a9daf", "#b18b80", "#d4d6f3", "#949577"),
                 c("#e7d6bb", "#749ed5", "#f9d29d", "#67b3e2", "#d09773", "#65ccec", "#d38585", "#7fe8ef", "#cf8190", "#94e8cd", "#ae8cc1", "#b3cf95", "#cbc0fc", "#94a66c", "#eeccff", "#ada368", "#e9a6ce", "#48a297", "#ffc1df", "#799c7a", "#facbe0", "#5d9e9a", "#ffc6c1", "#619bb0", "#fccdcb", "#7197bb", "#b1e4c3", "#9390b1", "#c3e0c0", "#a98c90", "#ade3ce", "#9c927d", "#c2dafe", "#869881", "#e6d3dc", "#6e9ba4", "#bde0d0", "#8196a4", "#b2e1df", "#b9deea")
)

ltyInput <- function (id, name, val) {
  selectInput(id, paste(name, 'line type'),
              list('None' = 'none', 'Solid' = 'solid', 'Dotted' = 'dotted',
                   'Dashed' = 'dashed', 'Dot-Dash' = 'dotdash',
                   'Long-dash' = 'longdash', 'Two-dash' = 'twodash'),
              val)
}
cexInput <- function (id, name, val) {
  sliderInput(id, name, 0, 4, val, step = 0.01)
}
lwdInput <- function (id, name, val) {
  sliderInput(id, paste(name, 'line width'), 0, 6, val, step = 0.01)
}
fontInput <- function (id, name, val) {
  selectInput(id, paste0(name, ' font style'),
              list('Plain' = 1, 'Bold' = 2, 'Italic' = 3, 'Bold-italic' = 4),
              val)
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(title = 'Ternary plotter',

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Load data',
           fileInput("datafile", "Data", placeholder = "No data file selected"),
           textOutput(outputId = "dataStatus"),
           textInput('dim1', 'Column one', ''),
           textInput('dim2', 'Column two', ''),
           textInput('dim3', 'Column three', ''),
           ),
        tabPanel('Plot display',
           
           sliderInput('lab.offset', 'Label offset', -0.3, 0.5, 0.16, step = 0.005),
           colourInput('lab.col', 'Label colour', 'black'),
           selectInput('point', 'Plot direction', 
                       list('Up' = 'up', 'Right' = 'right', 'Down' = 'down',
                            'Left' = 'left'), 'up'),
           checkboxGroupInput('display', 'Display options', 
                              list('Clockwise' = 'clockwise',
                                   'Isometric' = 'isometric',
                                   'Tip labels' = 'show.tip.labels',
                                   'Axis labels' = 'show.axis.labels',
                                   'Axis tick labels' = 'axis.labels',
                                   'Axis tick marks' = 'axis.tick',
                                   'Rotate tick labels' = 'axis.rotate'), 
                              c('clockwise', 'isometric', 'axis.labels',
                                'show.axis.labels', 'axis.tick', 'axis.rotate')),
          
          #xlim = NULL,
          #ylim = NULL,
          
          #atip.rotate = NULL,
          #btip.rotate = NULL,
          #ctip.rotate = NULL,
          #atip.pos = NULL,
          #btip.pos = NULL,
          #ctip.pos = NULL,
          #padding = 0.08,
          #col = NA,
          cexInput('lab.cex', 'Label size', 1),
          fontInput('lab.font', 'Label', 1),
          
          cexInput('tip.cex', 'Tip label size', 1),
          fontInput('tip.font', 'Tip' , 1),
          
          colourInput('tip.col', 'Tip colour', 'black'),
          ),
        tabPanel('Grids',
           sliderInput('grid.lines' , 'Main grid lines', 1, 42, 10),
           sliderInput('grid.minor.lines', 'Minor grid lines:', 0, 10, 4),
           colourInput('grid.col', 'Grid colour', "darkgrey"),
           colourInput('grid.minor.col', 'Grid secondary colour', "lightgrey"),
           ltyInput('grid.lty', 'Grid', 'solid'),
           ltyInput('grid.minor.lty', 'Secondary grid', 'solid'),
           lwdInput('grid.lwd', 'Grid', par("lwd")),
           lwdInput('grid.minor.lwd', 'Secondary grid', par("lwd")),
           ),
        tabPanel('Axes',
           
           
           ltyInput('axis.lty', 'Axis line type', 'solid'),
           cexInput('axis.cex', 'Axis character size', 0.8),
           fontInput('axis.font', 'Axis', par("font")),
           lwdInput('axis.lwd', 'Axis', 1),
           colourInput('axis.col', 'Axis colour', "black"),
           
           #axis.pos = NULL,
           
           lwdInput('ticks.lwd', 'Axis ticks', 1),
           sliderInput('ticks.length', 'Axis tick length', 0, 0.1, 0.025),
           colourInput('ticks.col', 'Axis tick colour', "darkgrey"),
         )
      ),
    ),

  # Sidebar layout with input and output definitions ----
  
    mainPanel(
      fluidRow(plotOutput(outputId = "plot")),
      fluidRow(textOutput(outputId = "footer")),
    )
  )

  
  # References and notes
)

server <- function(input, output, session) {
  
  displaySetting <- function(id) id %in% input$display
  
  myData <- reactive({
    fileInput <- input$treefile
    exampleFile <- system.file('inst', 'plot', 'example.csv', package = 'Ternary')
    if (is.null(fileInput)) {
      output$dataStatus <- renderText({"Data file not found; using example."})
      tmpFile <- exampleFile
    } else {
      tmpFile <- fileInput$datapath
      if (is.null(tmpFile)) {
        output$dataStatus <- renderText({"Data file not found; using example."})
      }
    }
    
    ret <- read.csv(tmpFile)
    
    if (dim(ret)[2] < 3L) {
      ret <- read.table(tmpFile)
    }
    if (dim(ret)[2] < 3L) {
      ret <- xlsx::read.xlsx(tmpFile)
    }
    if (dim(ret)[2] < 3L) {
      ret <- read.csv(exampleFile)
      output$dataStatus <- renderText({"Could not parse data file; using example."})
    }
    cn <- colnames(ret)
    updateTextInput(session, 'dim1', value = cn[1])
    updateTextInput(session, 'dim2', value = cn[2])
    updateTextInput(session, 'dim3', value = cn[3])
    
    ret
  })
  
  dataLabels <- reactive({
    candidates <- colnames(ret)
    if (is.null(candidates)) rep('', 3L) else candidates
  })
  
  axisLabels <- reactive({
    if (displaySetting('show.axis.labels')) {
      c(input$dim1, input$dim2, input$dim3)
    } else rep(NULL, 3)
  })
  
  
  tipLabels <- reactive({
    if (displaySetting('show.tip.labels')) {
      c(input$dim1, input$dim2, input$dim3)
    } else rep(NULL, 3)
  })
  
  output$plot <- renderPlot({
    par(mar = rep(0, 4))
    TernaryPlot(
      atip = tipLabels()[1],
      btip = tipLabels()[2],
      ctip = tipLabels()[3],
      alab = axisLabels()[1],
      blab =  axisLabels()[2],
      clab = axisLabels()[3],
      lab.offset = input$lab.offset,
      lab.col = input$lab.col,
      point = input$point,
      clockwise = displaySetting('clockwise'),
      xlim = NULL,
      ylim = NULL,
      lab.cex = input$lab.cex,
      lab.font = as.numeric(input$lab.font),
      tip.cex = input$tip.cex,
      tip.font = as.numeric(input$tip.font),
      tip.col = input$tip.col,
      isometric = displaySetting('isometric'),
      atip.rotate = NULL,
      btip.rotate = NULL,
      ctip.rotate = NULL,
      atip.pos = NULL,
      btip.pos = NULL,
      ctip.pos = NULL,
      padding = 0.08,
      col = NA,
      grid.lines = input$grid.lines,
      grid.col = input$grid.col,
      grid.lty = input$grid.lty,
      grid.lwd = input$grid.lwd,
      grid.minor.lines = input$grid.minor.lines,
      grid.minor.col = input$grid.minor.col,
      grid.minor.lty = input$grid.minor.lty,
      grid.minor.lwd = input$grid.minor.lwd,
      axis.lty = input$axis.lty,
      axis.labels = displaySetting('axis.labels'),
      axis.cex = input$axis.cex,
      axis.font = as.numeric(input$axis.font),
      axis.rotate = displaySetting('axis.rotate'),
      #axis.pos = input$axis.pos,
      axis.tick = displaySetting('axis.tick'),
      axis.lwd = input$axis.lwd,
      ticks.lwd = input$ticks.lwd,
      ticks.length = input$ticks.length,
      axis.col = input$axis.col,
      ticks.col = input$ticks.col
    )
    TernaryPoints(myData()[, 1:3])
    
  })
  
}

shinyApp(ui = ui, server = server)