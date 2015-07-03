#' Shiny interface to visualise anomalous output
#'
#' Opens a shiny GUI to facilitate interaction with the anomalous package
#'
#' @param x a matrix returned by `tsmeasures` function
#'
#' @import shiny
#'
#' @examples
#' data(iris)
#' \dontrun{
#' shinypairs(iris)
#' }
#'
#' @export
shinyanomalous = function(x){
  calls = match.call()
  shinyApp(
    ui=fluidPage(
      titlePanel(""),
      fluidRow(
        column(3,
               wellPanel(
                 conditionalPanel(condition="input.tabs=='biplot'",
                                  uiOutput("biplotmethod")),
                 conditionalPanel(condition="input.tabs=='outlierdetect'",
                                  uiOutput("outliermethod"))
               )
        ),
        column(9,
               tabsetPanel(id="tabs",
                           tabPanel(title = "Biplot", value = "biplot",
                                    plotOutput("biplot")#,verbatimTextOutput("test")
                                    ),
                           tabPanel(title = "Outlier detection", value="outlierdetect",
                                    plotOutput("outlierplot",
                                               click = "out_click"),
                                    plotOutput("tsplot"),
                                    verbatimTextOutput("pointID"))
               )
        )
      )
    ),
    shinyServer(function(input, output) {
      
      output$outliermethod <- renderUI({
        list(
          selectInput("anomaly_method", "Method:",
                      choices="hdr", selected="hdr", multiple=FALSE),
          selectInput("anomaly_robust", "Robust:",
                      choices=c("TRUE","FALSE"), selected="TRUE", multiple=FALSE)
        )
      })
      
      output$biplotmethod <- renderUI({
        selectInput("biplot_robust", "Robust:",
                    choices=c("TRUE","FALSE"), selected="TRUE", multiple=FALSE)
      })
      
      
      output$outlierplot <- renderPlot({
        anomaly(x,method = input$anomaly_method,
                robust = as.logical(input$anomaly_robust))
      })
      
#       output$test <- renderPrint({
#         input$biplot_robust
#       })
      
      output$biplot <- renderPlot({
        biplot.features(x,robust = as.logical(input$biplot_robust))
      })
      
      output$tsplot <- renderPlot({
        nearPoints(x, input$plot_click, xvar = "PC1", yvar = "PC2")
        NULL
      })
      output$pointID <- renderPrint({
        nearPoints(x, input$plot_click, xvar = "PC1", yvar = "PC2")
      })
      
    })
  )}
