source("setup.R")

ui <- dashboardPage(
  dashboardHeader(title = "The Market Board"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      modify_stop_propagation(
        menuItem("Calendars", startExpanded = T, 
                 menuSubItem("Earning calendar", tabName = "earning_calendar"),
                 menuSubItem("IPO calendar", tabName = "ipo_calendar"),
                 menuSubItem("Stock split calendar", tabName = "stock_split_calendar"),
                 menuSubItem("Stock dividend calendar", tabName = "stock_dividend_calendar"),
                 menuSubItem("Economic calendar", tabName = "economic_calendar"))),
      modify_stop_propagation(
        menuItem("Feeds", startExpanded = T,
                 menuSubItem("General feed", tabName = "general_feed"),
                 menuSubItem("Stock news", tabName = "stock_news_feed"),
                 menuSubItem("Insider trading feed", tabName = "insider_trading_feed"))),
      modify_stop_propagation(
        menuItem("Securities", startExpanded = T,
                 menuSubItem("Stocks", tabName = "stocks"),
                 menuSubItem("ETFs", tabName = "etfs"))),
      modify_stop_propagation(
        menuItem("Settings", startExpanded = T,
                 menuSubItem("API", tabName = "api"),
                 menuSubItem("Dummy", tabName = "dummy", selected = T)))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    # tags$head(tags$script(type="text/javascript", src="custom.js")),
    tabItems(
      tabItem(tabName = "earning_calendar",
              fluidRow(
                DTOutput("tbl_earning_calendar", height = "auto")
              )),
      tabItem(tabName = "ipo_calendar",
              fluidRow(
                DTOutput("tbl_ipo_calendar", height = "auto")
              )),
      tabItem(tabName = "stock_split_calendar",
              fluidRow(
                DTOutput("tbl_stock_split_calendar", height = "auto")
              )),
      tabItem(tabName = "stock_dividend_calendar",
              fluidRow(
                DTOutput("tbl_stock_dividend_calendar", height = "auto")
              )),
      tabItem(tabName = "economic_calendar",
              fluidRow(
                DTOutput("tbl_economic_calendar", height = "auto")
              )),
      tabItem(tabName = "general_feed",
              fluidRow(
                DTOutput("tbl_general_feed", height = "auto")
              )),
      tabItem(tabName = "stock_news_feed",
              fluidRow(
                DTOutput("tbl_stock_news", height = "auto")
              )),
      tabItem(tabName = "insider_trading_feed",
              fluidRow(
                DTOutput("tbl_insider_trading_rss_feed", height = "auto")
              )),
      tabItem(tabName = "stocks",
              fluidRow(
                column(2,
                       textInput("input_stock_search", "Stock search"),
                       actionButton("input_stock_search_confirm", "Search")),
                column(6,
                       DTOutput("tbl_stock_search"))
              ),
              fluidRow(
                verbatimTextOutput("stockname")
              )),
      tabItem(tabName = "etfs",
              fluidRow(
              )),
      tabItem(tabName = "api",
              fluidRow(
                box(textInput("input_apikey", 
                              label = "fmpcloud.io API key"),
                    actionButton("input_apikey_confirm",
                                 label = "Confirm"))
              ))
      )
  )
)

server <- function(input, output, session) {
  apikey <- reactiveVal(apikey_handler())
  apikey_hidden <- reactiveVal(apikey_handler(hide = T))
  ticker <- reactiveVal(NULL)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["tab"]])) {
      if (query[["tab"]] == "earning_calendar") {
        updateTabItems(session, "tabs", selected = "earning_calendar")
      } else if (query[["tab"]] == "ipo_calendar") {
        updateTabItems(session, "tabs", selected = "ipo_calendar")
      } else if (query[["tab"]] == "stocks") {
        updateTabItems(session, "tabs", selected = "stocks")
      } 
    }
    
    if (!is.null(query[["ticker"]])) {
      ticker(query[["ticker"]])
      updateTabItems(session, "tabs", selected = "stocks")
    }
  })
  
  observe({
    updateQueryString(paste0("?tab=", input$tabs), mode = "push")
  })
  
  # earning_calendar ----
  prep_earning_calendar <- reactive({
    get_fmp("earning_calendar", apikey = apikey())
  })
  
  output$tbl_earning_calendar <- renderDT({
    prep_earning_calendar()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
      
  })
  
  # ipo_calendar ----
  prep_ipo_calendar <- reactive({
    get_fmp("ipo_calendar", apikey = apikey())
  })
  
  output$tbl_ipo_calendar <- renderDT({
    prep_ipo_calendar()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # stock_split_calendar ----
  prep_stock_split_calendar <- reactive({
    get_fmp("stock_split_calendar", apikey = apikey())
  })
  
  output$tbl_stock_split_calendar <- renderDT({
    prep_stock_split_calendar()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # stock_dividend_calendar ----
  prep_stock_dividend_calendar <- reactive({
    get_fmp("stock_dividend_calendar", apikey = apikey())
  })
  
  output$tbl_stock_dividend_calendar <- renderDT({
    prep_stock_dividend_calendar()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # economic_calendar ----
  prep_economic_calendar <- reactive({
    get_fmp("economic_calendar", apikey = apikey())
  })
  
  output$tbl_economic_calendar <- renderDT({
    prep_economic_calendar()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # general_feed ----
  prep_general_feed <- reactive({
    get_fmp("rss_feed", apikey = apikey())
  })
  
  output$tbl_general_feed <- renderDT({
    prep_general_feed()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # stock_news ----
  prep_stock_news <- reactive({
    get_fmp("stock_news", apikey = apikey())
  })
  
  output$tbl_stock_news <- renderDT({
    prep_stock_news()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # insider_trading_rss_feed ----
  prep_insider_trading_rss_feed <- reactive({
    get_fmp("insider-trading-rss-feed", apikey = apikey(), version = "v4")
  })
  
  output$tbl_insider_trading_rss_feed <- renderDT({
    prep_insider_trading_rss_feed()$df %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
    
  })
  
  # stock_search ----
  prep_stock_search <- eventReactive(input$input_stock_search_confirm, {
    req(input$input_stock_search)
    
    fmp_search(input$input_stock_search, apikey = apikey())
  })
  
  output$tbl_stock_search <- renderDT({
    prep_stock_search() %>% 
      datatable(extensions = 'Scroller', options = list(dom = "t",
                                                        deferRender = T,
                                                        scrollY = 1000,
                                                        scroller = T,
                                                        autoHeight = T),
                style = "bootstrap")
  })
  
  output$stockname <- renderText({
    ticker()
  }) 
  
  
  
  
  

  # api ----  
  observe({
    updateTextInput(session,
                    "input_apikey",
                    value = apikey_hidden())
  })
  
  observeEvent(input$input_apikey_confirm, {
    req(input$input_apikey)
    new_apikey <- input$input_apikey

    if (new_apikey != apikey_hidden()) {
      apikey(new_apikey)
      apikey_hidden(apikey_hider(new_apikey))
      updateTextInput(session,
                      "input_apikey",
                      value = apikey_hidden())
      apikey_handler(new_apikey)
    }
  })
}

enableBookmarking("url")

shinyApp(ui, server)