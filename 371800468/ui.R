library(shiny)

ui <- fluidPage(
  titlePanel("Chatbot Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("sidebarUI"),
      width = 3
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.page == 'page1'",
        plotOutput("plot", height = "600px") %>% 
          shinycssloaders::withSpinner(color = "#0dc5c1"),  # Add spinner
        wellPanel(actionButton("go_page2", "Go to Head-to-Head Matchups"))
      ),
      conditionalPanel(
        condition = "output.page == 'page2'",
        h2("Head-to-Head Matchups"),
        plotOutput("plot2", height = "600px") %>% 
          shinycssloaders::withSpinner(color = "#0dc5c1"),  # Add spinner
        wellPanel(actionButton("go_page1", "Back to Overall Comparisons"))
      ),
      width = 9
    )
  )
)