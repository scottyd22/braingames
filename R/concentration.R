#' concentration
#' 
#' Click on pairs of cards until all matches are found.
#' @param level  An integer (1, 2, or 3) denoting the difficutly of the board. Defaults to 1 (easiest).
#' @keywords concentration
#' @import dplyr
#' @import shiny
#' @importFrom shinyjs useShinyjs disable enable
#' @import rstudioapi
#' @export concentration
#' @examples 
#' concentration(2)

concentration <- function(level = 1) {
  
  if(!(level %in% c(1, 2, 3))) {
    
    stop(paste0("Only three levels exist. Please choose level 1, 2, or 3."))
    
  } else {
    
    # Global ----
    # Level
    n <- 2 * (level + 1)
    
    # Icon selection
    icons <- c('android', 'ambulance', 'apple', 'asterisk', 
               'bath', 'bed', 'bell', 'birthday-cake',
               'calculator', 'camera', 'car', 'check', 'child', 'coffee', 'credit-card', 'cube',
               'download',
               'edit', 'envelope', 'eye',
               'female', 'fire', 'flag', 'folder', 'font',
               'gift', 'globe',
               'heart', 'home', 'hourglass',
               'id-card', 'image',
               'key',
               'laptop', 'leaf', 'link', 'lock', 
               'magnet', 'male', 'map-marker', 'microphone', 'mobile', 'money', 'mouse-pointer', 'music',
               'outdent', 
               'paperclip', 'pause', 'paste', 'pencil', 'percent', 'phone', 'plane', 'play', 'plug', 'plus', 'power-off',
               'quote-right',
               'recycle', 'registered', 'rocket', 'rss',
               'save', 'search', 'share', 'shopping-cart', 'shower', 'signal', 'sitemap', 'sort', 'star', 'stop-circle', 'subway', 
               'tablet', 'tag', 'taxi', 'thumbs-up', 'toggle-off', 'train', 'truck', 'tv',
               'undo', 'university', 'unlock', 'upload', 'user', 'users',
               'volume-up', 'volume-down',
               'wheelchair', 'wifi', 'wrench',
               # Brands
               'windows', 'stack-overflow', 'amazon', 'github', 'google', 'twitter', 'facebook-square', 'linkedin-square')
    
    # Scramble icons
    icons1 <- sample(icons, n^2/2, replace = F)
    icons2 <- sample(icons1, n^2/2, replace = F)
    icons.list <- sample(c(icons1, icons2), replace = F); rm(icons1, icons2)
    
    shiny::runApp(shiny::shinyApp(
      
      # UI
      ui = shiny::fluidPage(
        tags$div(
          tags$style(HTML("
                          
                          .shiny-bound-output {
                          display: inline-block;
                          }
                          
                          input:disabled {
                          background: #333333;
                          }
                          
                          #grid .shiny-bound-output:nth-child(1) .shiny-bound-input {
                          margin-left: 1cm;
                          }
                          
                          "))
          ),
        
        title = 'Concentration',
        shinyjs::useShinyjs(),
        div(shiny::uiOutput('title'), style = 'margin-left: 0.25cm; font-size: 250%;'),
        shiny::uiOutput('grid'),
        br(),
        br(),
        shiny::fluidRow(div(shiny::uiOutput('message'), style = 'margin-left: 0.5cm; font-family: Arial Black')),
        br(),
        br()
          ),
      
      # Server
      server = function(input, output, session) {
        
        # Matches/Selected ----
        rx.matches <- shiny::reactiveValues(d = NULL)
        rx.selected <- shiny::reactiveValues(d = NULL)
        
        # Rx Clicks ----
        rx.input1 <- shiny::reactiveValues(d = 0)
        rx.input2 <- shiny::reactiveValues(d = 0)
        rx.input3 <- shiny::reactiveValues(d = 0)
        rx.input4 <- shiny::reactiveValues(d = 0)
        rx.input5 <- shiny::reactiveValues(d = 0)
        rx.input6 <- shiny::reactiveValues(d = 0)
        rx.input7 <- shiny::reactiveValues(d = 0)
        rx.input8 <- shiny::reactiveValues(d = 0)
        rx.input9 <- shiny::reactiveValues(d = 0)
        rx.input10 <- shiny::reactiveValues(d = 0)
        rx.input11 <- shiny::reactiveValues(d = 0)
        rx.input12 <- shiny::reactiveValues(d = 0)
        rx.input13 <- shiny::reactiveValues(d = 0)
        rx.input14 <- shiny::reactiveValues(d = 0)
        rx.input15 <- shiny::reactiveValues(d = 0)
        rx.input16 <- shiny::reactiveValues(d = 0)
        
        if(n > 4) {
          rx.input17 <- shiny::reactiveValues(d = 0)
          rx.input18 <- shiny::reactiveValues(d = 0)
          rx.input19 <- shiny::reactiveValues(d = 0)
          rx.input20 <- shiny::reactiveValues(d = 0)
          rx.input21 <- shiny::reactiveValues(d = 0)
          rx.input22 <- shiny::reactiveValues(d = 0)
          rx.input23 <- shiny::reactiveValues(d = 0)
          rx.input24 <- shiny::reactiveValues(d = 0)
          rx.input25 <- shiny::reactiveValues(d = 0)
          rx.input26 <- shiny::reactiveValues(d = 0)
          rx.input27 <- shiny::reactiveValues(d = 0)
          rx.input28 <- shiny::reactiveValues(d = 0)
          rx.input29 <- shiny::reactiveValues(d = 0)
          rx.input30 <- shiny::reactiveValues(d = 0)
          rx.input31 <- shiny::reactiveValues(d = 0)
          rx.input32 <- shiny::reactiveValues(d = 0)
          rx.input33 <- shiny::reactiveValues(d = 0)
          rx.input34 <- shiny::reactiveValues(d = 0)
          rx.input35 <- shiny::reactiveValues(d = 0)
          rx.input36 <- shiny::reactiveValues(d = 0)
        }
        
        if(n > 6) {
          rx.input37 <- shiny::reactiveValues(d = 0)
          rx.input38 <- shiny::reactiveValues(d = 0)
          rx.input39 <- shiny::reactiveValues(d = 0)
          rx.input40 <- shiny::reactiveValues(d = 0)
          rx.input41 <- shiny::reactiveValues(d = 0)
          rx.input42 <- shiny::reactiveValues(d = 0)
          rx.input43 <- shiny::reactiveValues(d = 0)
          rx.input44 <- shiny::reactiveValues(d = 0)
          rx.input45 <- shiny::reactiveValues(d = 0)
          rx.input46 <- shiny::reactiveValues(d = 0)
          rx.input47 <- shiny::reactiveValues(d = 0)
          rx.input48 <- shiny::reactiveValues(d = 0)
          rx.input49 <- shiny::reactiveValues(d = 0)
          rx.input50 <- shiny::reactiveValues(d = 0)
          rx.input51 <- shiny::reactiveValues(d = 0)
          rx.input52 <- shiny::reactiveValues(d = 0)
          rx.input53 <- shiny::reactiveValues(d = 0)
          rx.input54 <- shiny::reactiveValues(d = 0)
          rx.input55 <- shiny::reactiveValues(d = 0)
          rx.input56 <- shiny::reactiveValues(d = 0)
          rx.input57 <- shiny::reactiveValues(d = 0)
          rx.input58 <- shiny::reactiveValues(d = 0)
          rx.input59 <- shiny::reactiveValues(d = 0)
          rx.input60 <- shiny::reactiveValues(d = 0)
          rx.input61 <- shiny::reactiveValues(d = 0)
          rx.input62 <- shiny::reactiveValues(d = 0)
          rx.input63 <- shiny::reactiveValues(d = 0)
          rx.input64 <- shiny::reactiveValues(d = 0)
        }
        
        # Rx Background Colors ----
        rx.button1 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button2 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button3 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button4 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button5 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button6 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button7 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button8 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button9 <- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button10<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button11<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button12<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button13<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button14<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button15<- shiny::reactiveValues(d = 'background-color: #333333;')
        rx.button16<- shiny::reactiveValues(d = 'background-color: #333333;')
        
        if(n > 4) {
          rx.button17 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button18 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button19 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button20 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button21 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button22 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button23 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button24 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button25 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button26 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button27 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button28 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button29 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button30 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button31 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button32 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button33 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button34 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button35 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button36 <- shiny::reactiveValues(d = 'background-color: #333333;')
        }
        
        if(n > 6) {
          rx.button37 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button38 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button39 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button40 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button41 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button42 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button43 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button44 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button45 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button46 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button47 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button48 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button49 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button50 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button51 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button52 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button53 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button54 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button55 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button56 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button57 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button58 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button59 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button60 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button61 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button62 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button63 <- shiny::reactiveValues(d = 'background-color: #333333;')
          rx.button64 <- shiny::reactiveValues(d = 'background-color: #333333;')
        }
        
        # Buttons
        output$button1 <- shiny::renderUI({shiny::actionButton('val1', NULL, icon = icon(icons.list[1], 'fa-3x'), width = '70px', style = rx.button1$d)})
        output$button2 <- shiny::renderUI({shiny::actionButton('val2', NULL, icon = icon(icons.list[2], 'fa-3x'), width = '70px', style = rx.button2$d)})
        output$button3 <- shiny::renderUI({shiny::actionButton('val3', NULL, icon = icon(icons.list[3], 'fa-3x'), width = '70px', style = rx.button3$d)})
        output$button4 <- shiny::renderUI({shiny::actionButton('val4', NULL, icon = icon(icons.list[4], 'fa-3x'), width = '70px', style = rx.button4$d)})
        output$button5 <- shiny::renderUI({shiny::actionButton('val5', NULL, icon = icon(icons.list[5], 'fa-3x'), width = '70px', style = rx.button5$d)})
        output$button6 <- shiny::renderUI({shiny::actionButton('val6', NULL, icon = icon(icons.list[6], 'fa-3x'), width = '70px', style = rx.button6$d)})
        output$button7 <- shiny::renderUI({shiny::actionButton('val7', NULL, icon = icon(icons.list[7], 'fa-3x'), width = '70px', style = rx.button7$d)})
        output$button8 <- shiny::renderUI({shiny::actionButton('val8', NULL, icon = icon(icons.list[8], 'fa-3x'), width = '70px', style = rx.button8$d)})
        output$button9 <- shiny::renderUI({shiny::actionButton('val9', NULL, icon = icon(icons.list[9], 'fa-3x'), width = '70px', style = rx.button9$d)})
        output$button10 <- shiny::renderUI({shiny::actionButton('val10', NULL, icon = icon(icons.list[10], 'fa-3x'), width = '70px', style = rx.button10$d)})
        output$button11 <- shiny::renderUI({shiny::actionButton('val11', NULL, icon = icon(icons.list[11], 'fa-3x'), width = '70px', style = rx.button11$d)})
        output$button12 <- shiny::renderUI({shiny::actionButton('val12', NULL, icon = icon(icons.list[12], 'fa-3x'), width = '70px', style = rx.button12$d)})
        output$button13 <- shiny::renderUI({shiny::actionButton('val13', NULL, icon = icon(icons.list[13], 'fa-3x'), width = '70px', style = rx.button13$d)})
        output$button14 <- shiny::renderUI({shiny::actionButton('val14', NULL, icon = icon(icons.list[14], 'fa-3x'), width = '70px', style = rx.button14$d)})
        output$button15 <- shiny::renderUI({shiny::actionButton('val15', NULL, icon = icon(icons.list[15], 'fa-3x'), width = '70px', style = rx.button15$d)})
        output$button16 <- shiny::renderUI({shiny::actionButton('val16', NULL, icon = icon(icons.list[16], 'fa-3x'), width = '70px', style = rx.button16$d)})
        
        if(n > 4) {
          output$button17 <- shiny::renderUI({shiny::actionButton('val17', NULL, icon = icon(icons.list[17], 'fa-3x'), width = '70px', style = rx.button17$d)})
          output$button18 <- shiny::renderUI({shiny::actionButton('val18', NULL, icon = icon(icons.list[18], 'fa-3x'), width = '70px', style = rx.button18$d)})
          output$button19 <- shiny::renderUI({shiny::actionButton('val19', NULL, icon = icon(icons.list[19], 'fa-3x'), width = '70px', style = rx.button19$d)})
          output$button20 <- shiny::renderUI({shiny::actionButton('val20', NULL, icon = icon(icons.list[20], 'fa-3x'), width = '70px', style = rx.button20$d)})
          output$button21 <- shiny::renderUI({shiny::actionButton('val21', NULL, icon = icon(icons.list[21], 'fa-3x'), width = '70px', style = rx.button21$d)})
          output$button22 <- shiny::renderUI({shiny::actionButton('val22', NULL, icon = icon(icons.list[22], 'fa-3x'), width = '70px', style = rx.button22$d)})
          output$button23 <- shiny::renderUI({shiny::actionButton('val23', NULL, icon = icon(icons.list[23], 'fa-3x'), width = '70px', style = rx.button23$d)})
          output$button24 <- shiny::renderUI({shiny::actionButton('val24', NULL, icon = icon(icons.list[24], 'fa-3x'), width = '70px', style = rx.button24$d)})
          output$button25 <- shiny::renderUI({shiny::actionButton('val25', NULL, icon = icon(icons.list[25], 'fa-3x'), width = '70px', style = rx.button25$d)})
          output$button26 <- shiny::renderUI({shiny::actionButton('val26', NULL, icon = icon(icons.list[26], 'fa-3x'), width = '70px', style = rx.button26$d)})
          output$button27 <- shiny::renderUI({shiny::actionButton('val27', NULL, icon = icon(icons.list[27], 'fa-3x'), width = '70px', style = rx.button27$d)})
          output$button28 <- shiny::renderUI({shiny::actionButton('val28', NULL, icon = icon(icons.list[28], 'fa-3x'), width = '70px', style = rx.button28$d)})
          output$button29 <- shiny::renderUI({shiny::actionButton('val29', NULL, icon = icon(icons.list[29], 'fa-3x'), width = '70px', style = rx.button29$d)})
          output$button30 <- shiny::renderUI({shiny::actionButton('val30', NULL, icon = icon(icons.list[30], 'fa-3x'), width = '70px', style = rx.button30$d)})
          output$button31 <- shiny::renderUI({shiny::actionButton('val31', NULL, icon = icon(icons.list[31], 'fa-3x'), width = '70px', style = rx.button31$d)})
          output$button32 <- shiny::renderUI({shiny::actionButton('val32', NULL, icon = icon(icons.list[32], 'fa-3x'), width = '70px', style = rx.button32$d)})
          output$button33 <- shiny::renderUI({shiny::actionButton('val33', NULL, icon = icon(icons.list[33], 'fa-3x'), width = '70px', style = rx.button33$d)})
          output$button34 <- shiny::renderUI({shiny::actionButton('val34', NULL, icon = icon(icons.list[34], 'fa-3x'), width = '70px', style = rx.button34$d)})
          output$button35 <- shiny::renderUI({shiny::actionButton('val35', NULL, icon = icon(icons.list[35], 'fa-3x'), width = '70px', style = rx.button35$d)})
          output$button36 <- shiny::renderUI({shiny::actionButton('val36', NULL, icon = icon(icons.list[36], 'fa-3x'), width = '70px', style = rx.button36$d)})
        }
        
        if(n > 6) {
          output$button37 <- shiny::renderUI({shiny::actionButton('val37', NULL, icon = icon(icons.list[37], 'fa-3x'), width = '70px', style = rx.button37$d)})
          output$button38 <- shiny::renderUI({shiny::actionButton('val38', NULL, icon = icon(icons.list[38], 'fa-3x'), width = '70px', style = rx.button38$d)})
          output$button39 <- shiny::renderUI({shiny::actionButton('val39', NULL, icon = icon(icons.list[39], 'fa-3x'), width = '70px', style = rx.button39$d)})
          output$button40 <- shiny::renderUI({shiny::actionButton('val40', NULL, icon = icon(icons.list[40], 'fa-3x'), width = '70px', style = rx.button40$d)})
          output$button41 <- shiny::renderUI({shiny::actionButton('val41', NULL, icon = icon(icons.list[41], 'fa-3x'), width = '70px', style = rx.button41$d)})
          output$button42 <- shiny::renderUI({shiny::actionButton('val42', NULL, icon = icon(icons.list[42], 'fa-3x'), width = '70px', style = rx.button42$d)})
          output$button43 <- shiny::renderUI({shiny::actionButton('val43', NULL, icon = icon(icons.list[43], 'fa-3x'), width = '70px', style = rx.button43$d)})
          output$button44 <- shiny::renderUI({shiny::actionButton('val44', NULL, icon = icon(icons.list[44], 'fa-3x'), width = '70px', style = rx.button44$d)})
          output$button45 <- shiny::renderUI({shiny::actionButton('val45', NULL, icon = icon(icons.list[45], 'fa-3x'), width = '70px', style = rx.button45$d)})
          output$button46 <- shiny::renderUI({shiny::actionButton('val46', NULL, icon = icon(icons.list[46], 'fa-3x'), width = '70px', style = rx.button46$d)})
          output$button47 <- shiny::renderUI({shiny::actionButton('val47', NULL, icon = icon(icons.list[47], 'fa-3x'), width = '70px', style = rx.button47$d)})
          output$button48 <- shiny::renderUI({shiny::actionButton('val48', NULL, icon = icon(icons.list[48], 'fa-3x'), width = '70px', style = rx.button48$d)})
          output$button49 <- shiny::renderUI({shiny::actionButton('val49', NULL, icon = icon(icons.list[49], 'fa-3x'), width = '70px', style = rx.button49$d)})
          output$button50 <- shiny::renderUI({shiny::actionButton('val50', NULL, icon = icon(icons.list[50], 'fa-3x'), width = '70px', style = rx.button50$d)})
          output$button51 <- shiny::renderUI({shiny::actionButton('val51', NULL, icon = icon(icons.list[51], 'fa-3x'), width = '70px', style = rx.button51$d)})
          output$button52 <- shiny::renderUI({shiny::actionButton('val52', NULL, icon = icon(icons.list[52], 'fa-3x'), width = '70px', style = rx.button52$d)})
          output$button53 <- shiny::renderUI({shiny::actionButton('val53', NULL, icon = icon(icons.list[53], 'fa-3x'), width = '70px', style = rx.button53$d)})
          output$button54 <- shiny::renderUI({shiny::actionButton('val54', NULL, icon = icon(icons.list[54], 'fa-3x'), width = '70px', style = rx.button54$d)})
          output$button55 <- shiny::renderUI({shiny::actionButton('val55', NULL, icon = icon(icons.list[55], 'fa-3x'), width = '70px', style = rx.button55$d)})
          output$button56 <- shiny::renderUI({shiny::actionButton('val56', NULL, icon = icon(icons.list[56], 'fa-3x'), width = '70px', style = rx.button56$d)})
          output$button57 <- shiny::renderUI({shiny::actionButton('val57', NULL, icon = icon(icons.list[57], 'fa-3x'), width = '70px', style = rx.button57$d)})
          output$button58 <- shiny::renderUI({shiny::actionButton('val58', NULL, icon = icon(icons.list[58], 'fa-3x'), width = '70px', style = rx.button58$d)})
          output$button59 <- shiny::renderUI({shiny::actionButton('val59', NULL, icon = icon(icons.list[59], 'fa-3x'), width = '70px', style = rx.button59$d)})
          output$button60 <- shiny::renderUI({shiny::actionButton('val60', NULL, icon = icon(icons.list[60], 'fa-3x'), width = '70px', style = rx.button60$d)})
          output$button61 <- shiny::renderUI({shiny::actionButton('val61', NULL, icon = icon(icons.list[61], 'fa-3x'), width = '70px', style = rx.button61$d)})
          output$button62 <- shiny::renderUI({shiny::actionButton('val62', NULL, icon = icon(icons.list[62], 'fa-3x'), width = '70px', style = rx.button62$d)})
          output$button63 <- shiny::renderUI({shiny::actionButton('val63', NULL, icon = icon(icons.list[63], 'fa-3x'), width = '70px', style = rx.button63$d)})
          output$button64 <- shiny::renderUI({shiny::actionButton('val64', NULL, icon = icon(icons.list[64], 'fa-3x'), width = '70px', style = rx.button64$d)})
        }
        
        # Events
        shiny::observeEvent(c(rx.selected$d, rx.matches$d), {
          
          if('input$val1' %in% c(rx.selected$d, rx.matches$d)) {rx.button1$d <- 'background-color: #FFFFFF;'} else {rx.button1$d <- 'background-color:#333333;'}
          if('input$val2' %in% c(rx.selected$d, rx.matches$d)) {rx.button2$d <- 'background-color: #FFFFFF;'} else {rx.button2$d <- 'background-color:#333333;'}
          if('input$val3' %in% c(rx.selected$d, rx.matches$d)) {rx.button3$d <- 'background-color: #FFFFFF;'} else {rx.button3$d <- 'background-color:#333333;'}
          if('input$val4' %in% c(rx.selected$d, rx.matches$d)) {rx.button4$d <- 'background-color: #FFFFFF;'} else {rx.button4$d <- 'background-color:#333333;'}
          if('input$val5' %in% c(rx.selected$d, rx.matches$d)) {rx.button5$d <- 'background-color: #FFFFFF;'} else {rx.button5$d <- 'background-color:#333333;'}
          if('input$val6' %in% c(rx.selected$d, rx.matches$d)) {rx.button6$d <- 'background-color: #FFFFFF;'} else {rx.button6$d <- 'background-color:#333333;'}
          if('input$val7' %in% c(rx.selected$d, rx.matches$d)) {rx.button7$d <- 'background-color: #FFFFFF;'} else {rx.button7$d <- 'background-color:#333333;'}
          if('input$val8' %in% c(rx.selected$d, rx.matches$d)) {rx.button8$d <- 'background-color: #FFFFFF;'} else {rx.button8$d <- 'background-color:#333333;'}
          if('input$val9' %in% c(rx.selected$d, rx.matches$d)) {rx.button9$d <- 'background-color: #FFFFFF;'} else {rx.button9$d <- 'background-color:#333333;'}
          if('input$val10' %in% c(rx.selected$d, rx.matches$d)) {rx.button10$d <- 'background-color: #FFFFFF;'} else {rx.button10$d <- 'background-color:#333333;'}
          if('input$val11' %in% c(rx.selected$d, rx.matches$d)) {rx.button11$d <- 'background-color: #FFFFFF;'} else {rx.button11$d <- 'background-color:#333333;'}
          if('input$val12' %in% c(rx.selected$d, rx.matches$d)) {rx.button12$d <- 'background-color: #FFFFFF;'} else {rx.button12$d <- 'background-color:#333333;'}
          if('input$val13' %in% c(rx.selected$d, rx.matches$d)) {rx.button13$d <- 'background-color: #FFFFFF;'} else {rx.button13$d <- 'background-color:#333333;'}
          if('input$val14' %in% c(rx.selected$d, rx.matches$d)) {rx.button14$d <- 'background-color: #FFFFFF;'} else {rx.button14$d <- 'background-color:#333333;'}
          if('input$val15' %in% c(rx.selected$d, rx.matches$d)) {rx.button15$d <- 'background-color: #FFFFFF;'} else {rx.button15$d <- 'background-color:#333333;'}
          if('input$val16' %in% c(rx.selected$d, rx.matches$d)) {rx.button16$d <- 'background-color: #FFFFFF;'} else {rx.button16$d <- 'background-color:#333333;'}
          
          if(n > 4) {
            if('input$val17' %in% c(rx.selected$d, rx.matches$d)) {rx.button17$d <- 'background-color: #FFFFFF;'} else {rx.button17$d <- 'background-color:#333333;'}
            if('input$val18' %in% c(rx.selected$d, rx.matches$d)) {rx.button18$d <- 'background-color: #FFFFFF;'} else {rx.button18$d <- 'background-color:#333333;'}
            if('input$val19' %in% c(rx.selected$d, rx.matches$d)) {rx.button19$d <- 'background-color: #FFFFFF;'} else {rx.button19$d <- 'background-color:#333333;'}
            if('input$val20' %in% c(rx.selected$d, rx.matches$d)) {rx.button20$d <- 'background-color: #FFFFFF;'} else {rx.button20$d <- 'background-color:#333333;'}
            if('input$val21' %in% c(rx.selected$d, rx.matches$d)) {rx.button21$d <- 'background-color: #FFFFFF;'} else {rx.button21$d <- 'background-color:#333333;'}
            if('input$val22' %in% c(rx.selected$d, rx.matches$d)) {rx.button22$d <- 'background-color: #FFFFFF;'} else {rx.button22$d <- 'background-color:#333333;'}
            if('input$val23' %in% c(rx.selected$d, rx.matches$d)) {rx.button23$d <- 'background-color: #FFFFFF;'} else {rx.button23$d <- 'background-color:#333333;'}
            if('input$val24' %in% c(rx.selected$d, rx.matches$d)) {rx.button24$d <- 'background-color: #FFFFFF;'} else {rx.button24$d <- 'background-color:#333333;'}
            if('input$val25' %in% c(rx.selected$d, rx.matches$d)) {rx.button25$d <- 'background-color: #FFFFFF;'} else {rx.button25$d <- 'background-color:#333333;'}
            if('input$val26' %in% c(rx.selected$d, rx.matches$d)) {rx.button26$d <- 'background-color: #FFFFFF;'} else {rx.button26$d <- 'background-color:#333333;'}
            if('input$val27' %in% c(rx.selected$d, rx.matches$d)) {rx.button27$d <- 'background-color: #FFFFFF;'} else {rx.button27$d <- 'background-color:#333333;'}
            if('input$val28' %in% c(rx.selected$d, rx.matches$d)) {rx.button28$d <- 'background-color: #FFFFFF;'} else {rx.button28$d <- 'background-color:#333333;'}
            if('input$val29' %in% c(rx.selected$d, rx.matches$d)) {rx.button29$d <- 'background-color: #FFFFFF;'} else {rx.button29$d <- 'background-color:#333333;'}
            if('input$val30' %in% c(rx.selected$d, rx.matches$d)) {rx.button30$d <- 'background-color: #FFFFFF;'} else {rx.button30$d <- 'background-color:#333333;'}
            if('input$val31' %in% c(rx.selected$d, rx.matches$d)) {rx.button31$d <- 'background-color: #FFFFFF;'} else {rx.button31$d <- 'background-color:#333333;'}
            if('input$val32' %in% c(rx.selected$d, rx.matches$d)) {rx.button32$d <- 'background-color: #FFFFFF;'} else {rx.button32$d <- 'background-color:#333333;'}
            if('input$val33' %in% c(rx.selected$d, rx.matches$d)) {rx.button33$d <- 'background-color: #FFFFFF;'} else {rx.button33$d <- 'background-color:#333333;'}
            if('input$val34' %in% c(rx.selected$d, rx.matches$d)) {rx.button34$d <- 'background-color: #FFFFFF;'} else {rx.button34$d <- 'background-color:#333333;'}
            if('input$val35' %in% c(rx.selected$d, rx.matches$d)) {rx.button35$d <- 'background-color: #FFFFFF;'} else {rx.button35$d <- 'background-color:#333333;'}
            if('input$val36' %in% c(rx.selected$d, rx.matches$d)) {rx.button36$d <- 'background-color: #FFFFFF;'} else {rx.button36$d <- 'background-color:#333333;'}
          }
          
          if(n > 6) {
            if('input$val37' %in% c(rx.selected$d, rx.matches$d)) {rx.button37$d <- 'background-color: #FFFFFF;'} else {rx.button37$d <- 'background-color:#333333;'}
            if('input$val38' %in% c(rx.selected$d, rx.matches$d)) {rx.button38$d <- 'background-color: #FFFFFF;'} else {rx.button38$d <- 'background-color:#333333;'}
            if('input$val39' %in% c(rx.selected$d, rx.matches$d)) {rx.button39$d <- 'background-color: #FFFFFF;'} else {rx.button39$d <- 'background-color:#333333;'}
            if('input$val40' %in% c(rx.selected$d, rx.matches$d)) {rx.button40$d <- 'background-color: #FFFFFF;'} else {rx.button40$d <- 'background-color:#333333;'}
            if('input$val41' %in% c(rx.selected$d, rx.matches$d)) {rx.button41$d <- 'background-color: #FFFFFF;'} else {rx.button41$d <- 'background-color:#333333;'}
            if('input$val42' %in% c(rx.selected$d, rx.matches$d)) {rx.button42$d <- 'background-color: #FFFFFF;'} else {rx.button42$d <- 'background-color:#333333;'}
            if('input$val43' %in% c(rx.selected$d, rx.matches$d)) {rx.button43$d <- 'background-color: #FFFFFF;'} else {rx.button43$d <- 'background-color:#333333;'}
            if('input$val44' %in% c(rx.selected$d, rx.matches$d)) {rx.button44$d <- 'background-color: #FFFFFF;'} else {rx.button44$d <- 'background-color:#333333;'}
            if('input$val45' %in% c(rx.selected$d, rx.matches$d)) {rx.button45$d <- 'background-color: #FFFFFF;'} else {rx.button45$d <- 'background-color:#333333;'}
            if('input$val46' %in% c(rx.selected$d, rx.matches$d)) {rx.button46$d <- 'background-color: #FFFFFF;'} else {rx.button46$d <- 'background-color:#333333;'}
            if('input$val47' %in% c(rx.selected$d, rx.matches$d)) {rx.button47$d <- 'background-color: #FFFFFF;'} else {rx.button47$d <- 'background-color:#333333;'}
            if('input$val48' %in% c(rx.selected$d, rx.matches$d)) {rx.button48$d <- 'background-color: #FFFFFF;'} else {rx.button48$d <- 'background-color:#333333;'}
            if('input$val49' %in% c(rx.selected$d, rx.matches$d)) {rx.button49$d <- 'background-color: #FFFFFF;'} else {rx.button49$d <- 'background-color:#333333;'}
            if('input$val50' %in% c(rx.selected$d, rx.matches$d)) {rx.button50$d <- 'background-color: #FFFFFF;'} else {rx.button50$d <- 'background-color:#333333;'}
            if('input$val51' %in% c(rx.selected$d, rx.matches$d)) {rx.button51$d <- 'background-color: #FFFFFF;'} else {rx.button51$d <- 'background-color:#333333;'}
            if('input$val52' %in% c(rx.selected$d, rx.matches$d)) {rx.button52$d <- 'background-color: #FFFFFF;'} else {rx.button52$d <- 'background-color:#333333;'}
            if('input$val53' %in% c(rx.selected$d, rx.matches$d)) {rx.button53$d <- 'background-color: #FFFFFF;'} else {rx.button53$d <- 'background-color:#333333;'}
            if('input$val54' %in% c(rx.selected$d, rx.matches$d)) {rx.button54$d <- 'background-color: #FFFFFF;'} else {rx.button54$d <- 'background-color:#333333;'}
            if('input$val55' %in% c(rx.selected$d, rx.matches$d)) {rx.button55$d <- 'background-color: #FFFFFF;'} else {rx.button55$d <- 'background-color:#333333;'}
            if('input$val56' %in% c(rx.selected$d, rx.matches$d)) {rx.button56$d <- 'background-color: #FFFFFF;'} else {rx.button56$d <- 'background-color:#333333;'}
            if('input$val57' %in% c(rx.selected$d, rx.matches$d)) {rx.button57$d <- 'background-color: #FFFFFF;'} else {rx.button57$d <- 'background-color:#333333;'}
            if('input$val58' %in% c(rx.selected$d, rx.matches$d)) {rx.button58$d <- 'background-color: #FFFFFF;'} else {rx.button58$d <- 'background-color:#333333;'}
            if('input$val59' %in% c(rx.selected$d, rx.matches$d)) {rx.button59$d <- 'background-color: #FFFFFF;'} else {rx.button59$d <- 'background-color:#333333;'}
            if('input$val60' %in% c(rx.selected$d, rx.matches$d)) {rx.button60$d <- 'background-color: #FFFFFF;'} else {rx.button60$d <- 'background-color:#333333;'}
            if('input$val61' %in% c(rx.selected$d, rx.matches$d)) {rx.button61$d <- 'background-color: #FFFFFF;'} else {rx.button61$d <- 'background-color:#333333;'}
            if('input$val62' %in% c(rx.selected$d, rx.matches$d)) {rx.button62$d <- 'background-color: #FFFFFF;'} else {rx.button62$d <- 'background-color:#333333;'}
            if('input$val63' %in% c(rx.selected$d, rx.matches$d)) {rx.button63$d <- 'background-color: #FFFFFF;'} else {rx.button63$d <- 'background-color:#333333;'}
            if('input$val64' %in% c(rx.selected$d, rx.matches$d)) {rx.button64$d <- 'background-color: #FFFFFF;'} else {rx.button64$d <- 'background-color:#333333;'}
          }
          
        }, ignoreNULL = T)
        
        # Track clicks
        shiny::observeEvent(input$val1, {rx.input1$d <- rx.input1$d + 1})
        shiny::observeEvent(input$val2, {rx.input2$d <- rx.input2$d + 1})
        shiny::observeEvent(input$val3, {rx.input3$d <- rx.input3$d + 1})
        shiny::observeEvent(input$val4, {rx.input4$d <- rx.input4$d + 1})
        shiny::observeEvent(input$val5, {rx.input5$d <- rx.input5$d + 1})
        shiny::observeEvent(input$val6, {rx.input6$d <- rx.input6$d + 1})
        shiny::observeEvent(input$val7, {rx.input7$d <- rx.input7$d + 1})
        shiny::observeEvent(input$val8, {rx.input8$d <- rx.input8$d + 1})
        shiny::observeEvent(input$val9, {rx.input9$d <- rx.input9$d + 1})
        shiny::observeEvent(input$val10, {rx.input10$d <- rx.input10$d + 1})
        shiny::observeEvent(input$val11, {rx.input11$d <- rx.input11$d + 1})
        shiny::observeEvent(input$val12, {rx.input12$d <- rx.input12$d + 1})
        shiny::observeEvent(input$val13, {rx.input13$d <- rx.input13$d + 1})
        shiny::observeEvent(input$val14, {rx.input14$d <- rx.input14$d + 1})
        shiny::observeEvent(input$val15, {rx.input15$d <- rx.input15$d + 1})
        shiny::observeEvent(input$val16, {rx.input16$d <- rx.input16$d + 1})
        
        if(n > 4) {
          shiny::observeEvent(input$val17, {rx.input17$d <- rx.input17$d + 1})
          shiny::observeEvent(input$val18, {rx.input18$d <- rx.input18$d + 1})
          shiny::observeEvent(input$val19, {rx.input19$d <- rx.input19$d + 1})
          shiny::observeEvent(input$val20, {rx.input20$d <- rx.input20$d + 1})
          shiny::observeEvent(input$val21, {rx.input21$d <- rx.input21$d + 1})
          shiny::observeEvent(input$val22, {rx.input22$d <- rx.input22$d + 1})
          shiny::observeEvent(input$val23, {rx.input23$d <- rx.input23$d + 1})
          shiny::observeEvent(input$val24, {rx.input24$d <- rx.input24$d + 1})
          shiny::observeEvent(input$val25, {rx.input25$d <- rx.input25$d + 1})
          shiny::observeEvent(input$val26, {rx.input26$d <- rx.input26$d + 1})
          shiny::observeEvent(input$val27, {rx.input27$d <- rx.input27$d + 1})
          shiny::observeEvent(input$val28, {rx.input28$d <- rx.input28$d + 1})
          shiny::observeEvent(input$val29, {rx.input29$d <- rx.input29$d + 1})
          shiny::observeEvent(input$val30, {rx.input30$d <- rx.input30$d + 1})
          shiny::observeEvent(input$val31, {rx.input31$d <- rx.input31$d + 1})
          shiny::observeEvent(input$val32, {rx.input32$d <- rx.input32$d + 1})
          shiny::observeEvent(input$val33, {rx.input33$d <- rx.input33$d + 1})
          shiny::observeEvent(input$val34, {rx.input34$d <- rx.input34$d + 1})
          shiny::observeEvent(input$val35, {rx.input35$d <- rx.input35$d + 1})
          shiny::observeEvent(input$val36, {rx.input36$d <- rx.input36$d + 1})
        }
        
        if(n > 6) {
          shiny::observeEvent(input$val37, {rx.input37$d <- rx.input37$d + 1})
          shiny::observeEvent(input$val38, {rx.input38$d <- rx.input38$d + 1})
          shiny::observeEvent(input$val39, {rx.input39$d <- rx.input39$d + 1})
          shiny::observeEvent(input$val40, {rx.input40$d <- rx.input40$d + 1})
          shiny::observeEvent(input$val41, {rx.input41$d <- rx.input41$d + 1})
          shiny::observeEvent(input$val42, {rx.input42$d <- rx.input42$d + 1})
          shiny::observeEvent(input$val43, {rx.input43$d <- rx.input43$d + 1})
          shiny::observeEvent(input$val44, {rx.input44$d <- rx.input44$d + 1})
          shiny::observeEvent(input$val45, {rx.input45$d <- rx.input45$d + 1})
          shiny::observeEvent(input$val46, {rx.input46$d <- rx.input46$d + 1})
          shiny::observeEvent(input$val47, {rx.input47$d <- rx.input47$d + 1})
          shiny::observeEvent(input$val48, {rx.input48$d <- rx.input48$d + 1})
          shiny::observeEvent(input$val49, {rx.input49$d <- rx.input49$d + 1})
          shiny::observeEvent(input$val50, {rx.input50$d <- rx.input50$d + 1})
          shiny::observeEvent(input$val51, {rx.input51$d <- rx.input51$d + 1})
          shiny::observeEvent(input$val52, {rx.input52$d <- rx.input52$d + 1})
          shiny::observeEvent(input$val53, {rx.input53$d <- rx.input53$d + 1})
          shiny::observeEvent(input$val54, {rx.input54$d <- rx.input54$d + 1})
          shiny::observeEvent(input$val55, {rx.input55$d <- rx.input55$d + 1})
          shiny::observeEvent(input$val56, {rx.input56$d <- rx.input56$d + 1})
          shiny::observeEvent(input$val57, {rx.input57$d <- rx.input57$d + 1})
          shiny::observeEvent(input$val58, {rx.input58$d <- rx.input58$d + 1})
          shiny::observeEvent(input$val59, {rx.input59$d <- rx.input59$d + 1})
          shiny::observeEvent(input$val60, {rx.input60$d <- rx.input60$d + 1})
          shiny::observeEvent(input$val61, {rx.input61$d <- rx.input61$d + 1})
          shiny::observeEvent(input$val62, {rx.input62$d <- rx.input62$d + 1})
          shiny::observeEvent(input$val63, {rx.input63$d <- rx.input63$d + 1})
          shiny::observeEvent(input$val64, {rx.input64$d <- rx.input64$d + 1})
        }
        
        # Reset selected after no match
        shiny::observeEvent(rx.selected$d, {
          f <- function(){
            a <- quote(paste0('rx.input', substr(rx.selected$d[1], 10, nchar(rx.selected$d[1])),
                              '$d <- rx.input', substr(rx.selected$d[1], 10, nchar(rx.selected$d[1])), '$d + 1'))
            
            b <- quote(paste0('rx.input', substr(rx.selected$d[2], 10, nchar(rx.selected$d[2])),
                              '$d <- rx.input', substr(rx.selected$d[2], 10, nchar(rx.selected$d[2])), '$d + 1'))
            
            eval(parse(text = eval(a)))
            eval(parse(text = eval(b)))
            
            shinyjs::enable('val1'); shinyjs::enable('val2'); shinyjs::enable('val3'); shinyjs::enable('val4'); 
            shinyjs::enable('val5'); shinyjs::enable('val6'); shinyjs::enable('val7'); shinyjs::enable('val8'); 
            shinyjs::enable('val9'); shinyjs::enable('val10'); shinyjs::enable('val11'); shinyjs::enable('val12'); 
            shinyjs::enable('val13'); shinyjs::enable('val14'); shinyjs::enable('val15'); shinyjs::enable('val16')
            
            if(n > 4) {
              shinyjs::enable('val17'); shinyjs::enable('val18'); shinyjs::enable('val19'); shinyjs::enable('val20'); 
              shinyjs::enable('val21'); shinyjs::enable('val22'); shinyjs::enable('val23'); shinyjs::enable('val24'); 
              shinyjs::enable('val25'); shinyjs::enable('val26'); shinyjs::enable('val27'); shinyjs::enable('val28'); 
              shinyjs::enable('val29'); shinyjs::enable('val30'); shinyjs::enable('val31'); shinyjs::enable('val32');
              shinyjs::enable('val33'); shinyjs::enable('val34'); shinyjs::enable('val35'); shinyjs::enable('val36')
            }
            
            if(n > 6) {
              shinyjs::enable('val37'); shinyjs::enable('val38'); shinyjs::enable('val39'); shinyjs::enable('val40'); 
              shinyjs::enable('val41'); shinyjs::enable('val42'); shinyjs::enable('val43'); shinyjs::enable('val44'); 
              shinyjs::enable('val45'); shinyjs::enable('val46'); shinyjs::enable('val47'); shinyjs::enable('val48'); 
              shinyjs::enable('val49'); shinyjs::enable('val50'); shinyjs::enable('val51'); shinyjs::enable('val52');
              shinyjs::enable('val53'); shinyjs::enable('val54'); shinyjs::enable('val55'); shinyjs::enable('val56');
              shinyjs::enable('val57'); shinyjs::enable('val58'); shinyjs::enable('val59'); shinyjs::enable('val60'); 
              shinyjs::enable('val61'); shinyjs::enable('val62'); shinyjs::enable('val63'); shinyjs::enable('val64')
            }
          }
          
          if(length(rx.selected$d) == 2) {
            shinyjs::disable('val1'); shinyjs::disable('val2'); shinyjs::disable('val3'); shinyjs::disable('val4'); 
            shinyjs::disable('val5'); shinyjs::disable('val6'); shinyjs::disable('val7'); shinyjs::disable('val8'); 
            shinyjs::disable('val9'); shinyjs::disable('val10'); shinyjs::disable('val11'); shinyjs::disable('val12'); 
            shinyjs::disable('val13'); shinyjs::disable('val14'); shinyjs::disable('val15'); shinyjs::disable('val16')
            
            if(n > 4) {
              shinyjs::disable('val17'); shinyjs::disable('val18'); shinyjs::disable('val19'); shinyjs::disable('val20'); 
              shinyjs::disable('val21'); shinyjs::disable('val22'); shinyjs::disable('val23'); shinyjs::disable('val24'); 
              shinyjs::disable('val25'); shinyjs::disable('val26'); shinyjs::disable('val27'); shinyjs::disable('val28'); 
              shinyjs::disable('val29'); shinyjs::disable('val30'); shinyjs::disable('val31'); shinyjs::disable('val32');
              shinyjs::disable('val33'); shinyjs::disable('val34'); shinyjs::disable('val35'); shinyjs::disable('val36')
            }
            
            if(n > 6) {
              shinyjs::disable('val37'); shinyjs::disable('val38'); shinyjs::disable('val39'); shinyjs::disable('val40'); 
              shinyjs::disable('val41'); shinyjs::disable('val42'); shinyjs::disable('val43'); shinyjs::disable('val44'); 
              shinyjs::disable('val45'); shinyjs::disable('val46'); shinyjs::disable('val47'); shinyjs::disable('val48'); 
              shinyjs::disable('val49'); shinyjs::disable('val50'); shinyjs::disable('val51'); shinyjs::disable('val52');
              shinyjs::disable('val53'); shinyjs::disable('val54'); shinyjs::disable('val55'); shinyjs::disable('val56');
              shinyjs::disable('val57'); shinyjs::disable('val58'); shinyjs::disable('val59'); shinyjs::disable('val60'); 
              shinyjs::disable('val61'); shinyjs::disable('val62'); shinyjs::disable('val63'); shinyjs::disable('val64')
            }
            shinyjs::delay(1000, f())
          }
        })
        
        # Input group ----
        input.group <- shiny::reactive({
          unlist(lapply(1:(n^2), function(i) {paste0('input$val', i)}))
        })
        
        
        # Trigger group ----
        trigger.group <- shiny::reactive({
          trigger <- c(rx.input1$d, rx.input2$d, rx.input3$d, rx.input4$d, rx.input5$d, rx.input6$d, rx.input7$d, rx.input8$d,
                       rx.input9$d, rx.input10$d, rx.input11$d, rx.input12$d, rx.input13$d, rx.input14$d, rx.input15$d, rx.input16$d)
          
          if(n > 4) {
            trigger <- c(trigger,
                         rx.input17$d, rx.input18$d, rx.input19$d, rx.input20$d, rx.input21$d, rx.input22$d, rx.input23$d, rx.input24$d,
                         rx.input25$d, rx.input26$d, rx.input27$d, rx.input28$d, rx.input29$d, rx.input30$d, rx.input31$d, rx.input32$d,
                         rx.input33$d, rx.input34$d, rx.input35$d, rx.input36$d)
          }
          
          if(n > 6) {
            trigger <- c(trigger,
                         rx.input37$d, rx.input38$d, rx.input39$d, rx.input40$d, rx.input41$d, rx.input42$d, rx.input43$d, rx.input44$d,
                         rx.input45$d, rx.input46$d, rx.input47$d, rx.input48$d, rx.input49$d, rx.input50$d, rx.input51$d, rx.input52$d,
                         rx.input53$d, rx.input54$d, rx.input55$d, rx.input56$d, rx.input57$d, rx.input58$d, rx.input59$d, rx.input60$d,
                         rx.input61$d, rx.input62$d, rx.input63$d, rx.input64$d)
          }
          trigger
        })
        
        # Icons group ----
        icons.group <- shiny::reactive({
          ig <- c(icons.list[1], icons.list[2], icons.list[3], icons.list[4], icons.list[5], icons.list[6], icons.list[7], icons.list[8],
                  icons.list[9], icons.list[10], icons.list[11], icons.list[12], icons.list[13], icons.list[14], icons.list[15], icons.list[16])
          if(n > 4) {
            ig <- c(ig,
                    icons.list[17], icons.list[18], icons.list[19], icons.list[20], icons.list[21], icons.list[22], icons.list[23], icons.list[24],
                    icons.list[25], icons.list[26], icons.list[27], icons.list[28], icons.list[29], icons.list[30], icons.list[31], icons.list[32],
                    icons.list[33], icons.list[34], icons.list[35], icons.list[36])
          }
          if(n > 6) {
            ig <- c(ig,
                    icons.list[37], icons.list[38], icons.list[39], icons.list[40], icons.list[41], icons.list[42], icons.list[43], icons.list[44],
                    icons.list[45], icons.list[46], icons.list[47], icons.list[48], icons.list[49], icons.list[50], icons.list[51], icons.list[52],
                    icons.list[53], icons.list[54], icons.list[55], icons.list[56], icons.list[57], icons.list[58], icons.list[59], icons.list[60],
                    icons.list[61], icons.list[62], icons.list[63], icons.list[64])
          }
          ig
        })
        
        # Current status of all buttons ----
        shiny::observeEvent(trigger.group(), {
          
          df <- data.frame(Input = input.group(),
                           Value = trigger.group() %% 2,
                           Pic = icons.group(),
                           stringsAsFactors = F)
          
          matches <- dplyr::group_by(df, Pic)
          matches <- dplyr::mutate(matches, Total = sum(Value))
          matches <- dplyr::ungroup(matches)
          matches <- dplyr::filter(matches, Total == 2)
          matches <- dplyr::pull(matches, Input)
          rx.matches$d <- matches
          
          selected <- dplyr::filter(df, Value == 1)
          if(length(matches) > 0) {selected <- dplyr::filter(selected, !(Input %in% rx.matches$d))}
          selected <- dplyr::pull(selected, Input)
          rx.selected$d <- selected
          
        }, ignoreNULL = T)
        
        # UI Grid ----
        output$grid <- shiny::renderUI({
          if(n == 4) {
            shiny::fluidRow(
              shiny::fluidRow(shiny::uiOutput('button1'), shiny::uiOutput('button2'), shiny::uiOutput('button3'), shiny::uiOutput('button4')),
              shiny::fluidRow(shiny::uiOutput('button5'), shiny::uiOutput('button6'), shiny::uiOutput('button7'), shiny::uiOutput('button8')),
              shiny::fluidRow(shiny::uiOutput('button9'), shiny::uiOutput('button10'), shiny::uiOutput('button11'), shiny::uiOutput('button12')),
              shiny::fluidRow(shiny::uiOutput('button13'), shiny::uiOutput('button14'), shiny::uiOutput('button15'), shiny::uiOutput('button16'))
            )
          } else if(n == 6) {
            shiny::fluidRow(
              shiny::fluidRow(shiny::uiOutput('button1'), shiny::uiOutput('button2'), shiny::uiOutput('button3'), 
                              shiny::uiOutput('button4'), shiny::uiOutput('button5'), shiny::uiOutput('button6')),
              shiny::fluidRow(shiny::uiOutput('button7'), shiny::uiOutput('button8'), shiny::uiOutput('button9'), 
                              shiny::uiOutput('button10'), shiny::uiOutput('button11'), shiny::uiOutput('button12')),
              shiny::fluidRow(shiny::uiOutput('button13'), shiny::uiOutput('button14'), shiny::uiOutput('button15'), 
                              shiny::uiOutput('button16'), shiny::uiOutput('button17'), shiny::uiOutput('button18')),
              shiny::fluidRow(shiny::uiOutput('button19'), shiny::uiOutput('button20'), shiny::uiOutput('button21'), 
                              shiny::uiOutput('button22'), shiny::uiOutput('button23'), shiny::uiOutput('button24')),
              shiny::fluidRow(shiny::uiOutput('button25'), shiny::uiOutput('button26'), shiny::uiOutput('button27'), 
                              shiny::uiOutput('button28'), shiny::uiOutput('button29'), shiny::uiOutput('button30')),
              shiny::fluidRow(shiny::uiOutput('button31'), shiny::uiOutput('button32'), shiny::uiOutput('button33'), 
                              shiny::uiOutput('button34'), shiny::uiOutput('button35'), shiny::uiOutput('button36'))
            )
          } else if(n == 8) {
            shiny::fluidRow(
              shiny::fluidRow(shiny::uiOutput('button1'), shiny::uiOutput('button2'), shiny::uiOutput('button3'), shiny::uiOutput('button4'), 
                              shiny::uiOutput('button5'), shiny::uiOutput('button6'), shiny::uiOutput('button7'), shiny::uiOutput('button8')),
              shiny::fluidRow(shiny::uiOutput('button9'), shiny::uiOutput('button10'), shiny::uiOutput('button11'), shiny::uiOutput('button12'), 
                              shiny::uiOutput('button13'), shiny::uiOutput('button14'), shiny::uiOutput('button15'), shiny::uiOutput('button16')),
              shiny::fluidRow(shiny::uiOutput('button17'), shiny::uiOutput('button18'), shiny::uiOutput('button19'), shiny::uiOutput('button20'), 
                              shiny::uiOutput('button21'), shiny::uiOutput('button22'), shiny::uiOutput('button23'), shiny::uiOutput('button24')),
              shiny::fluidRow(shiny::uiOutput('button25'), shiny::uiOutput('button26'), shiny::uiOutput('button27'), shiny::uiOutput('button28'), 
                              shiny::uiOutput('button29'), shiny::uiOutput('button30'), shiny::uiOutput('button31'), shiny::uiOutput('button32')),
              shiny::fluidRow(shiny::uiOutput('button33'), shiny::uiOutput('button34'), shiny::uiOutput('button35'), shiny::uiOutput('button36'), 
                              shiny::uiOutput('button37'), shiny::uiOutput('button38'), shiny::uiOutput('button39'), shiny::uiOutput('button40')),
              shiny::fluidRow(shiny::uiOutput('button41'), shiny::uiOutput('button42'), shiny::uiOutput('button43'), shiny::uiOutput('button44'), 
                              shiny::uiOutput('button45'), shiny::uiOutput('button46'), shiny::uiOutput('button47'), shiny::uiOutput('button48')),
              shiny::fluidRow(shiny::uiOutput('button49'), shiny::uiOutput('button50'), shiny::uiOutput('button51'), shiny::uiOutput('button52'), 
                              shiny::uiOutput('button53'), shiny::uiOutput('button54'), shiny::uiOutput('button55'), shiny::uiOutput('button56')),
              shiny::fluidRow(shiny::uiOutput('button57'), shiny::uiOutput('button58'), shiny::uiOutput('button59'), shiny::uiOutput('button60'), 
                              shiny::uiOutput('button61'), shiny::uiOutput('button62'), shiny::uiOutput('button63'), shiny::uiOutput('button64'))
            )
          } else {
            NULL
          }
        })
        
        # Title ----
        output$title <- shiny::renderUI({
          shiny::HTML(paste0("<strong>",
                             "<span style = 'color: #464646;'>c</span>",
                             "<span style = 'color: #286090;'>o</span>",
                             "<span style = 'color: #464646;'>ncentrati</span>",
                             "<span style = 'color: #286090;'>o</span>",
                             "<span style = 'color: #464646;'>n</span>",
                             "</strong>"))
        })
        
        # Winning Message ----
        output$message <- shiny::renderUI({
          if(length(rx.matches$d) == n^2 & n == 4) {
            'Mission accomplished!!'
          } else if(length(rx.matches$d) == n^2 & n == 6) {
            'Your focus is superb.  Laser-like.  Well done!'
          } else if(length(rx.matches$d) == n^2 & n == 8) {
            'Definitely add problem-solving to your resume.'
          } else {
            NULL
          }
        })
      }
    ), launch.browser = rstudioapi::viewer)
  }
  }