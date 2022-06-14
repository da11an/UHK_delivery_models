#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinythemes)

ui <- function(request) {fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("UHK Delivery Status Predictor"),
  
  tabsetPanel(
    tabPanel(
      title = "Prediction",
     h3("Order Delivery Prediction"),
      numericInput("your_order",
                   "Enter Order Number:",
                   value = logs %>% filter(type=="last-order") %>% pull(order) %>% max()),
      
      plotOutput("deliveryDates"),
      
      DTOutput("order_pred")
    ),
    tabPanel(
      title = "Data",
     h3("Reference table"),
      DTOutput("refTable"),
      
     h3("Dataset"),
      DTOutput("dataset"),
      
      p("Data from:",
        tags$ul(
          tags$li("https://ultimatehackingkeyboard.com/delivery-status"),
          tags$li("https://github.com/hxv/uhk-shipping-progress"),
          tags$li("UHK via https://archive.org/"),
          tags$li("@danpalmer (twitter)")
      )),
      
      p("For UHK last-order model, predictions for a given month were assigned
        to the 28th of that month.")
    ),
    tabPanel(
      title = "Code & Details",
      helpText("Linear model fits are presented for (1a) UHK-reported `shipping-next`
      dates; (1b) non-black variant keyboards;
      and (2) UHK-estimated shipping dates for the `last-order` received.
      The `shipping-next` model bases expectations for the future on past
      shipping info. The `last-order` model reflects UHK's historical delivery
      predictions for new orders."),
      helpText("95% prediction intervals are given for both models providing lower and
      upper estimates. 19 times out of 20, new data would fall between those 
      bounds if deviations from the mean were normally distributed (they're not)."),
      
      hr(),
      
      tags$a(href="https://github.com/da11an/UHK_delivery_models", "Code available on Github"),
      br(),
      tags$a(href="https://blog.hxv.me/uhk-shipping-progress/", "Also check out UHK Shipping progress site"),
      
      hr(),
      
      helpText("App updated 2022-06-14")
    )
  )
)}

server <- function(input, output, session) {
  
  output$deliveryDates <- renderPlot({delivery_dates_plus(logs, input$your_order)})
  
  output$order_pred <- renderDT({
    DT::datatable(
      preds(input$your_order) %>% rename(model = type),
      options = list(dom = 't', scrollX = TRUE)
    )
  })
  
  output$refTable <- renderDT({
    datatable(ref_table(logs),
              options = list(scrollX = TRUE))
  })
  
  output$dataset <- renderDT({
    datatable(logs %>% select(-post_num, -ship_num) %>% arrange(desc(post_date)),
              options = list(scrollX = TRUE))})
  
  # Automatically bookmark every time an input changes
  observe({
    # whitelist approach
    toExclude <- setdiff(names(input), "your_order")
    setBookmarkExclude(toExclude)
    
    # update bookmarks
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
