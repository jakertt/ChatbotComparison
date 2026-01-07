analyze_chatbot_data <- function(data, ranking_method = "ability", 
                                 top_n = 10, plot_title = "Everything") {
  # ranking_method: 
  # "ability" for Bradley-Terry model, 
  # "win_rate" for raw win percentage
  
  # plot_title:
  # This string will go into the heat map title
  
  # Output has the following:
  # $win_rates - Outputs the win rate table
  # $abilities - Outputs the ability ranking table
  # $win_pct_matrix - Head to head percentages
  # $heatmap - Creates the heat map
  
  # ============================================================================
  # Start with Raw Win rates
  # ============================================================================
  
  processed_data <- data %>%
    mutate(model_a_orig = model_a, model_b_orig = model_b) %>%
    pivot_longer(
      cols = c(model_a, model_b),
      names_to = "model_type",
      values_to = "model"
    ) %>%
    mutate(
      model_outcome = case_when(
        winner == model_type ~ "won",
        winner %in% c("tie", "both_bad") ~ winner,
        TRUE ~ "lost"
      ),
      opponent = if_else(model_type == "model_a", model_b_orig, model_a_orig)
    ) %>%
    select(-model_a_orig, -model_b_orig, -winner)
  
  processed_data <- processed_data[seq(1, nrow(processed_data), by = 2), ]
  processed_data <- filter(processed_data, !(model == opponent))
  
  model_perspective <- processed_data %>%
    rename(model_name = model) %>%
    select(model_name, model_outcome)
  
  opponent_perspective <- processed_data %>%
    rename(model_name = opponent) %>%
    mutate(model_outcome = case_when(
      model_outcome == "won" ~ "lost",
      model_outcome == "lost" ~ "won",
      model_outcome == "tie" ~ "tie",
      model_outcome == "both_bad" ~ "both_bad"
    )) %>%
    select(model_name, model_outcome)
  
  all_games <- bind_rows(model_perspective, opponent_perspective)
  
  win_rates <- all_games %>%
    group_by(model_name) %>%
    summarise(
      total_games = n(),
      wins = sum(model_outcome == "won") + sum(model_outcome == "tie"),
      losses = sum(model_outcome == "lost") + sum(model_outcome == "both_bad"),
      win_rate = (sum(model_outcome == "won") + sum(model_outcome == "tie")) / n()
    ) %>%
    arrange(desc(win_rate))
  
  # ============================================================================
  # The following section finds the head to head percentages
  # ============================================================================
  
  final_data_awin <- data %>% filter(winner == 'model_a')
  final_data_bwin <- data %>% filter(winner == 'model_b')
  
  results <- list()
  for (model in unique(final_data_awin$model_a)) {
    temp <- final_data_awin %>%
      filter(model_a == model) %>%
      count(model_b) %>%
      pivot_wider(
        names_from = model_b,
        values_from = n,
        values_fill = 0
      ) %>%
      mutate(model = model)
    results[[model]] <- temp
  }
  loss_data <- bind_rows(results)
  
  results_b <- list()
  for (model in unique(final_data_bwin$model_b)) {
    temp <- final_data_bwin %>%
      filter(model_b == model) %>%
      count(model_a) %>%
      pivot_wider(
        names_from = model_a,
        values_from = n,
        values_fill = 0
      ) %>%
      mutate(model = model)
    results_b[[model]] <- temp
  }
  loss_data_b <- bind_rows(results_b)
  
  loss_data <- loss_data %>%
    relocate(model, .before = everything())
  loss_data_b <- loss_data_b %>%
    relocate(model, .before = everything())
  
  loss_data <- loss_data %>%
    select(model, all_of(loss_data$model))
  loss_data_b <- loss_data_b %>%
    select(model, all_of(loss_data$model))
  loss_data_b <- loss_data_b %>%
    slice(match(loss_data$model, loss_data_b$model))
  
  models <- loss_data$model
  loss_data <- loss_data %>%
    select(-model)
  loss_data <- as.data.frame(sapply(loss_data, as.numeric))
  
  loss_data_b <- loss_data_b %>%
    select(-model)
  loss_data_b <- as.data.frame(sapply(loss_data_b, as.numeric))
  
  loss_data <- loss_data + loss_data_b
  
  # Covvert to percentages
  n <- nrow(loss_data)
  win_pct <- matrix(NA, n, n, dimnames = list(models, models))
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j & !is.na(loss_data[i, j]) & !is.na(loss_data[j, i])) {
        total_matches <- loss_data[i, j] + loss_data[j, i]
        if (total_matches > 0) {
          win_pct[i, j] <- loss_data[i, j] / total_matches
        } else {
          win_pct[i, j] <- NA
        }
      }
    }
  }
  
  win_pct_df <- as.data.frame(win_pct)
  win_pct_df <- cbind(models, win_pct_df)
  
  bt_data <- data %>%
    filter(winner %in% c("model_a", "model_b")) %>%
    mutate(result = ifelse(winner == "model_a", 1, 0)) %>%
    select(model_a, model_b, result)
  
  models_all <- sort(unique(c(bt_data$model_a, bt_data$model_b)))
  safe_names <- make.names(models_all)
  n_models <- length(models_all)
  n_matches <- nrow(bt_data)
  
  design_mat <- matrix(0, nrow = n_matches, ncol = n_models,
                       dimnames = list(NULL, safe_names))
  
  for (i in seq_len(n_matches)) {
    a <- bt_data$model_a[i]
    b <- bt_data$model_b[i]
    design_mat[i, make.names(a)] <- 1
    design_mat[i, make.names(b)] <- -1
  }
  
  bt_design_df <- as.data.frame(design_mat, stringsAsFactors = FALSE)
  bt_design_df$result <- as.numeric(bt_data$result)
  
  bt_manual <- glm(result ~ ., data = bt_design_df, family = binomial)
  
  coefs <- coef(bt_manual)
  coef_no_intercept <- coefs[names(coefs) != "(Intercept)"]
  
  abilities <- data.frame(
    model = models_all,
    coef_name = safe_names,
    ability = as.numeric(coef_no_intercept[safe_names])
  )
  
  abilities$ability[is.na(abilities$ability)] <- 0
  abilities$ability <- abilities$ability - mean(abilities$ability)
  abilities <- abilities %>% arrange(desc(ability))
  abilities <- select(abilities, -c(coef_name))
  
  # ============================================================================
  # Heat map creation now
  # ============================================================================
  
  if (ranking_method == "ability") {
    top_models <- abilities %>%
      head(top_n) %>%
      pull(model)
  } else {
    top_models <- win_rates %>%
      head(top_n) %>%
      pull(model_name)
  }
  
  row_indices <- which(rownames(win_pct) %in% top_models)
  col_indices <- which(colnames(win_pct) %in% top_models)
  
  top_win_pct_mat <- win_pct[row_indices, col_indices]
  
  heatmap_data <- as.data.frame(top_win_pct_mat) %>%
    mutate(model = rownames(.)) %>%
    pivot_longer(
      cols = -model,
      names_to = "opponent",
      values_to = "win_pct"
    )
  
  if (ranking_method == "ability") {
    heatmap_data$model <- factor(heatmap_data$model, levels = rev(top_models))
    heatmap_data$opponent <- factor(heatmap_data$opponent, levels = top_models)
    title_text <- paste0("Head-to-Head Win Rates: Top ", top_n, 
                         " Models (by Ability). Category: ", plot_title)
  } else {
    heatmap_data$model <- factor(heatmap_data$model, levels = rev(top_models))
    heatmap_data$opponent <- factor(heatmap_data$opponent, levels = top_models)
    title_text <- paste0("Head-to-Head Win Rates: Top ", top_n, 
                         " Models (by Win Rate). Category: ", plot_title)
  }
  
  p <- ggplot(heatmap_data, aes(x = opponent, y = model, fill = win_pct)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(is.na(win_pct), "", 
                                 sprintf("%.2f", win_pct))),
              color = "white", size = 3, fontface = "bold") +
    scale_fill_viridis(option = "plasma", na.value = "grey90",
                       limits = c(0, 1),
                       labels = scales::percent) +
    labs(
      title = title_text,
      x = "Opponent",
      y = "Model (Row Wins)",
      fill = "Win %"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "right"
    ) +
    coord_fixed()
  
  return(list(
    win_rates = win_rates,
    abilities = abilities,
    win_pct_matrix = win_pct_df,
    heatmap = p
  ))
}

get_model_comparison <- function(model1, model2, results_object) {
  win_matrix <- results_object$win_pct_matrix
  abilities  <- results_object$abilities
  
  if (!(model1 %in% win_matrix$models) | !(model2 %in% colnames(win_matrix))) {
    stop("One or both of the specified models are not found in the results object.")
  }
  
  m1_over_m2 <- win_matrix[win_matrix$models == model1, model2]
  m2_over_m1 <- win_matrix[win_matrix$models == model2, model1]
  
  ability1 <- abilities$ability[abilities$model == model1]
  ability2 <- abilities$ability[abilities$model == model2]
  
  p_m1_win <- exp(ability1) / (exp(ability1) + exp(ability2))
  p_m2_win <- 1 - p_m1_win
  
  tibble::tibble(
    Type = c("Empirical Win Rate", "Predicted (BT) Win Rate"),
    !!model1 := c(m1_over_m2, p_m1_win),
    !!model2 := c(m2_over_m1, p_m2_win)
  )
}


plot_ability_scores <- function(results_object, category_name, overall_results) {
  # Get top 10 models from the overall ranking
  top_10_models <- overall_results$abilities %>%
    head(10) %>%
    pull(model)
  
  plot_data <- results_object$abilities %>%
    filter(model %in% top_10_models)
  
  plot_data <- plot_data %>%
    mutate(model = factor(model, levels = top_10_models[top_10_models %in% plot_data$model]))
  
  p <- ggplot(plot_data, aes(x = model, y = ability, fill = ability)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.3f", ability)), 
              vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_fill_viridis(option = "plasma") +
    labs(
      title = paste0("Bradley-Terry Ability Scores: ", category_name),
      subtitle = "Top 10 Models (based on overall ranking)",
      x = "Model",
      y = "Ability Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 8),
      axis.text.x = element_text(angle = 60, hjust = 1, size = 11),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
  
  return(p)
}