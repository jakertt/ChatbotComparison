library(shiny)

server <- function(input, output, session) {
  
  # Track which page is active
  current_page <- reactiveVal("page1")
  observeEvent(input$go_page1, current_page("page1"))
  observeEvent(input$go_page2, current_page("page2"))
  
  output$page <- renderText(current_page())
  outputOptions(output, "page", suspendWhenHidden = FALSE)
  
  # Precompute results_ability once when app starts
  results_ability <- analyze_chatbot_data(final_data, ranking_method = "ability", 
                                          top_n = 10, plot_title = "Everything")
  
  # Dynamic sidebar
  output$sidebarUI <- renderUI({
    if (current_page() == "page1") {
      tagList(
        selectInput("querytopic", "Query Topic:", categorytags),
        selectInput("plottype", "Type of Plot:", plottypes),
        hr(),
        helpText(HTML('Data is collected from <a href="https://www.lmarena.ai">LMArena</a>.'))
      )
    } else {
      tagList(
        selectInput("modelname1", "Model 1:", models),
        selectInput("modelname2", "Model 2:", models),
        hr(),
        helpText(HTML('Data is collected from <a href="https://www.lmarena.ai">LMArena</a>.'))
      )
    }
  })
  
  # Cached reactive expression with bindCache
  current_results <- reactive({
    req(input$querytopic)
    
    filtered_data <- switch(input$querytopic,
                            "Everything" = final_data,
                            "Coding Help" = filter(final_data, is_code == TRUE),
                            "Math Help" = filter(final_data, category_tag_math_v0.1_math == TRUE),
                            "Real World Topics" = filter(final_data, category_tag_criteria_v0.1_real_world == TRUE),
                            "Problem Solving" = filter(final_data, category_tag_criteria_v0.1_problem_solving == TRUE),
                            "Creative Thinking" = filter(final_data, category_tag_criteria_v0.1_creativity == TRUE),
                            "Complex Knowledge" = filter(final_data, category_tag_criteria_v0.1_complexity == TRUE),
                            "Creative Writing" = filter(final_data, category_tag_creative_writing_v0.1_creative_writing == TRUE)
    )
    
    analyze_chatbot_data(filtered_data, ranking_method = "ability", 
                         top_n = 10, plot_title = input$querytopic)
  }) %>% bindCache(input$querytopic)  # Cache based on query topic
  
  # Overall plots
  output$plot <- renderPlot({
    results_object <- current_results()
    
    if(input$plottype == "Bar Plot"){
      plot_ability_scores(results_object, input$querytopic, results_ability)
    } else {
      results_object$heatmap
    }
  }) %>% bindCache(input$querytopic, input$plottype)  # Cache the plot too
  
  # Head-to-head comparison
  output$plot2 <- renderPlot({
    comparison_table <- get_model_comparison(input$modelname1, input$modelname2, results_ability)
    comparison_data <- comparison_table %>%
      pivot_longer(cols = -Type, names_to = "Model", values_to = "WinRate")
    
    ggplot(comparison_data, aes(x = Model, y = WinRate, fill = Type)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.2f", WinRate)), 
                position = position_dodge(width = 0.9), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_viridis(discrete = TRUE, option = "plasma") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = paste0("Head-to-Head: ", input$modelname1, " vs ", input$modelname2),
           x = "Model", y = "Win Rate") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            legend.position = "right")
  }) %>% bindCache(input$modelname1, input$modelname2)  # Cache head-to-head plots
}