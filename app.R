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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("UHK Delivery Status Predictor"),
  
  
  h2("Order Delivery Prediction"),
  numericInput("your_order",
               "Enter Order Number:",
               value = logs %>% filter(type=="last-order") %>% pull(order) %>% max()),
  
  DTOutput("order_pred"),
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
  h2("General Data"),
  plotOutput("deliveryDates"),
  
  h2("Reference table"),
  DTOutput("refTable"),
  
  h2("Dataset"),
  DTOutput("dataset"),
  
  hr(),
  tags$a(href="https://blog.hxv.me/uhk-shipping-progress/", "Also check out UHK Shipping progress site")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$deliveryDates <- renderPlot({delivery_dates(logs)})
  
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
    datatable(logs %>% select(-post_num, -ship_num) %>% arrange(post_date),
              options = list(scrollX = TRUE))})
}

# Run the application 
shinyApp(ui = ui, server = server)
