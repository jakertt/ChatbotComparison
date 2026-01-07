# Run locally, then upload the compressed file
library(data.table)
library(readr)

# Read and select only necessary columns
final_data <- read.csv("data/combined_clean_flat.csv")

# Keep only essential columns
essential_cols <- c("model_a", "model_b", "winner", "is_code",
                    "category_tag_math_v0.1_math",
                    "category_tag_criteria_v0.1_real_world",
                    "category_tag_criteria_v0.1_problem_solving",
                    "category_tag_criteria_v0.1_creativity",
                    "category_tag_criteria_v0.1_complexity",
                    "category_tag_creative_writing_v0.1_creative_writing")

final_data_slim <- final_data %>% select(all_of(essential_cols))

# Save as compressed RDS (much smaller than CSV)
saveRDS(final_data_slim, "data/combined_clean_flat.rds", compress = TRUE)