#' mathsquare
#'
#' Drag numbers into the grid until all rows, columns, and diagonals sum to the same value.  To move a value from the grid back to the starting area, drag a new number to the cell containing the number you wish to remove.  The undesirable number will be replaced and sent back to the starting area.
#' @keywords mathsquare
#' @import shiny
#' @import shinyjqui
#' @import rstudioapi
#' @export mathsquare

mathsquare <- function() {

  shiny::runApp(shiny::shinyApp(

    # UI ----
    ui <- shiny::fluidPage(
                           shiny::tags$div(
                             shiny::tags$style(HTML("
                                             #numbers #numbers {
                                                  font-family: Arial;
                                                  border: 1px solid #E5E8E8;
                                              }

                                              .ui-sortable {
                                                  font-family: Arial;
                                                  border-style: solid;
                                                  border-color: #464646;
                                                  height: 40px;
                                              }

                                              #title {
                                                  font-size: 36px;
                                              }

                                              #goal span {
                                                  font-size: 16px;
                                                  color: #464646;
                                              }

                                              #message {
                                                  font-family: Arial Black;
                                                  font-style: bold;
                                                  color: #464646;
                                              }
                                             "))
                             ),

                           title = 'Math Square',
                           shiny::tags$div(shiny::uiOutput('title'), style = 'margin-left: 1.45cm;'),
                           shiny::tags$div(shiny::uiOutput('goal'), style = 'margin-left: 1cm;'),
                           shiny::fluidRow(shiny::uiOutput('numbers')),
                           shiny::fluidRow(
                             shiny::tags$div(shiny::uiOutput('Cell11'), style = 'display: inline-block; margin-left: 3cm'),
                             shiny::tags$div(shiny::uiOutput('Cell12'), style = 'display: inline-block;'),
                             shiny::tags$div(shiny::uiOutput('Cell13'), style = 'display: inline-block;')
                           ),
                           shiny::fluidRow(
                             shiny::tags$div(shiny::uiOutput('Cell21'), style = 'display: inline-block; margin-left: 3cm'),
                             shiny::tags$div(shiny::uiOutput('Cell22'), style = 'display: inline-block;'),
                             shiny::tags$div(shiny::uiOutput('Cell23'), style = 'display: inline-block;')
                           ),
                           shiny::fluidRow(
                             shiny::tags$div(shiny::uiOutput('Cell31'), style = 'display: inline-block; margin-left: 3cm'),
                             shiny::tags$div(shiny::uiOutput('Cell32'), style = 'display: inline-block;'),
                             shiny::tags$div(shiny::uiOutput('Cell33'), style = 'display: inline-block;')
                           ),
                           shiny::tags$br(),
                           shiny::tags$div(shiny::textOutput('message'), style = 'margin-left: 0.45cm;'),
                           shiny::tags$br()
    ),

    # Server ----
    server <- function(input, output, session) {

      # Selected numbers ----
      selected.numbers <- shiny::reactive({
        c(input$cell11_order, input$cell12_order, input$cell13_order,
          input$cell21_order, input$cell22_order, input$cell23_order,
          input$cell31_order, input$cell32_order, input$cell33_order
        )
      })

      # Available numbers ----
      output$numbers <- shiny::renderUI({
        shinyjqui::orderInput(inputId = 'numbers',
                              as_source = T,
                              label = NULL,
                              items = c(1:9)[!(c(1:9) %in% selected.numbers())],
                              placeholder = '',
                              item_class = colors$d,
                              connect = c('cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                              width = '330px',
                              style = 'margin-left: 1cm;'
        )
      })

      # Cells ----
      # Cell11
      output$Cell11 <- shiny::renderUI({
        if(length(input[['cell11_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell11',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell11_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell11',
                                label = NULL,
                                items = input[['cell11_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell11',
                                label = NULL,
                                items = input[['cell11_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell12
      output$Cell12 <- shiny::renderUI({
        if(length(input[['cell12_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell12',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell12_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell12',
                                label = NULL,
                                items = input[['cell12_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell12',
                                label = NULL,
                                items = input[['cell12_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell13
      output$Cell13 <- shiny::renderUI({
        if(length(input[['cell13_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell13',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell13_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell13',
                                label = NULL,
                                items = input[['cell13_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell13',
                                label = NULL,
                                items = input[['cell13_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell21
      output$Cell21 <- shiny::renderUI({
        if(length(input[['cell21_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell21',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell21_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell21',
                                label = NULL,
                                items = input[['cell21_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell21',
                                label = NULL,
                                items = input[['cell21_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell22', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell22
      output$Cell22 <- shiny::renderUI({
        if(length(input[['cell22_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell22',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell22_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell22',
                                label = NULL,
                                items = input[['cell22_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell22',
                                label = NULL,
                                items = input[['cell22_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell23', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell23
      output$Cell23 <- shiny::renderUI({
        if(length(input[['cell23_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell23',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell23_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell23',
                                label = NULL,
                                items = input[['cell23_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell23',
                                label = NULL,
                                items = input[['cell23_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell31', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell31
      output$Cell31 <- shiny::renderUI({
        if(length(input[['cell31_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell31',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell31_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell31',
                                label = NULL,
                                items = input[['cell31_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell32', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell31',
                                label = NULL,
                                items = input[['cell31_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell32', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell32
      output$Cell32 <- shiny::renderUI({
        if(length(input[['cell32_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell32',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell33'),
                                width = '40px'
          )
        } else if(length(input[['cell32_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell32',
                                label = NULL,
                                items = input[['cell32_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell33'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell32',
                                label = NULL,
                                items = input[['cell32_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell33'),
                                width = '40px'
          )
        }
      })

      # Cell33
      output$Cell33 <- shiny::renderUI({
        if(length(input[['cell33_order']]) == 0) {
          shinyjqui::orderInput(inputId = 'cell33',
                                label = NULL,
                                items = NULL,
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32'),
                                width = '40px'
          )
        } else if(length(input[['cell33_order']]) == 1) {
          shinyjqui::orderInput(inputId = 'cell33',
                                label = NULL,
                                items = input[['cell33_order']],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32'),
                                width = '40px'
          )
        } else {
          shinyjqui::orderInput(inputId = 'cell33',
                                label = NULL,
                                items = input[['cell33_order']][c(1)],
                                item_class = colors$d,
                                placeholder = '.',
                                connect = c('numbers', 'cell11', 'cell12', 'cell13', 'cell21', 'cell22', 'cell23', 'cell31', 'cell32'),
                                width = '40px'
          )
        }
      })

      # Sums ----
      r1 <- shiny::reactive({
        sum(as.numeric(input$cell11_order),
            as.numeric(input$cell12_order),
            as.numeric(input$cell13_order)
        )
      })

      r2 <- shiny::reactive({
        sum(as.numeric(input$cell21_order),
            as.numeric(input$cell22_order),
            as.numeric(input$cell23_order)
        )
      })

      r3 <- shiny::reactive({
        sum(as.numeric(input$cell31_order),
            as.numeric(input$cell32_order),
            as.numeric(input$cell33_order)
        )
      })

      c1 <- shiny::reactive({
        sum(as.numeric(input$cell11_order),
            as.numeric(input$cell21_order),
            as.numeric(input$cell31_order)
        )
      })

      c2 <- shiny::reactive({
        sum(as.numeric(input$cell12_order),
            as.numeric(input$cell22_order),
            as.numeric(input$cell32_order)
        )
      })

      c3 <- shiny::reactive({
        sum(as.numeric(input$cell13_order),
            as.numeric(input$cell23_order),
            as.numeric(input$cell33_order)
        )
      })

      d1 <- shiny::reactive({
        sum(as.numeric(input$cell11_order),
            as.numeric(input$cell22_order),
            as.numeric(input$cell33_order)
        )
      })

      d2 <- shiny::reactive({
        sum(as.numeric(input$cell31_order),
            as.numeric(input$cell22_order),
            as.numeric(input$cell13_order)
        )
      })

      # Reactive Values ----
      colors <- shiny::reactiveValues(d = 'primary')
      message <- shiny::reactiveValues(d = '')

      # Events ----
      shiny::observeEvent(c(input$cell11_order, input$cell12_order, input$cell13_order,
                            input$cell21_order, input$cell22_order, input$cell23_order,
                            input$cell31_order, input$cell32_order, input$cell33_order, message), {

                              if(
                                length(input$cell11_order) == 1 &
                                length(input$cell12_order) == 1 &
                                length(input$cell13_order) == 1 &
                                length(input$cell21_order) == 1 &
                                length(input$cell22_order) == 1 &
                                length(input$cell23_order) == 1 &
                                length(input$cell31_order) == 1 &
                                length(input$cell32_order) == 1 &
                                length(input$cell33_order) == 1 &
                                r1() == r2() &
                                r2() == r3() &
                                r3() == c1() &
                                c1() == c2() &
                                c2() == c3() &
                                c3() == d1() &
                                d1() == d2()
                              ) {

                                colors$d <- 'success'
                                message$d <- 'Congratulations, you solver of puzzles!!'

                              } else if (
                                length(input$cell11_order) == 1 &
                                length(input$cell12_order) == 1 &
                                length(input$cell13_order) == 1 &
                                length(input$cell21_order) == 1 &
                                length(input$cell22_order) == 1 &
                                length(input$cell23_order) == 1 &
                                length(input$cell31_order) == 1 &
                                length(input$cell32_order) == 1 &
                                length(input$cell33_order) == 1 &
                                (r1() != r2() |
                                 r2() != r3() |
                                 r3() != c1() |
                                 c1() != c2() |
                                 c2() != c3() |
                                 c3() != d1() |
                                 d1() != d2())
                              ) {

                                message$d <- "Nice try, but you're not quite there yet"
                                colors$d <- 'primary'

                              } else {
                                colors$d <- 'primary'
                                message$d <- ""
                              }
                            })

      # Title ----
      output$title <- shiny::renderUI({
        HTML(paste0("<strong>",
                    "<span style = 'color: #286090;'>math</span>",
                    "<span style = 'color: #464646;'>square</span>",
                    "</strong>"))
      })

      # Instuctions ----
      output$goal <- shiny::renderUI({
        shiny::HTML(paste0("<span><strong>Goal: </strong>Drag each number to one of the boxes</span>",
                           "<br>",
                           "<span><strong></strong>below so that each row, column, and diagonal</span>",
                           "<br>",
                           "<span><strong></strong>sum to the same value.</span>"
        )
        )
      })

      # Message ----
      output$message <- shiny::renderText({message$d})

    }
  ), launch.browser = rstudioapi::viewer)
}
