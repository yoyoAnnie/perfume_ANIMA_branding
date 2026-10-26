#1. Common Ingredient & Note Distributions (Text Analysis)

library(tidyverse)

# Force dplyr namespace and use backticks for the space in the column name
top_notes_freq <- perfume_data %>%
  separate_rows(`Top Notes`, sep = ",\\s*") %>% 
  dplyr::count(`Top Notes`, sort = TRUE)

# View the clean frequency table
print(top_notes_freq)


# Visualize the top 10 most common notes 
ggplot(top_notes_freq %>% head(10), aes(x = reorder(`Top Notes`, n), y = n)) +
  geom_col(fill = "#785ba7", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Top 10 Most Common Top Notes",
    x = "Fragrance Note",
    y = "Number of Perfumes"
  ) +
  theme_minimal(base_size = 14)


#2. Price per Milliliter Comparisons by Brand

library(tidyverse)

# Fix the random number generator so the dots stay in the exact same place every run
set.seed(42)

ggplot(perfume_data, aes(x = Brand, y = `Price per ml ($/ml)`)) +
  # Use color instead of fill to guarantee the borders/boxes outline vividly 
  geom_boxplot(aes(color = Brand), alpha = 0.2, outlier.shape = NA, size = 1) +
  # Freeze the jittered positions permanently using a locked seed
  geom_jitter(
    aes(color = Brand), 
    position = position_jitter(width = 0.15, height = 0, seed = 42), 
    size = 2.5, 
    alpha = 0.85
  ) +
  # Apply a robust pre-made color scheme that maps automatically
  scale_color_brewer(palette = "Set2") + 
  labs(
    title = "Price Distribution per ml Across Fragrance Houses",
    x = "Brand",
    y = "Price ($ / ml)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


#3. Scent Profile Co-Occurrence (Brand Signatures)
library(tidyverse)

# 1. Clean, separate, and group the scent profiles into broad categories
consolidated_matrix <- perfume_data %>%
  # Split multi-profile entries (e.g., "Green, Woody Aromatic" -> separate rows)
  separate_rows(`Scent Profile`, sep = ",\\s*") %>%
  # Standardize text to lowercase to prevent case mismatches
  mutate(profile_clean = tolower(`Scent Profile`)) %>%
  # Group sub-categories into major parent families
  mutate(Broad_Category = case_when(
    str_detect(profile_clean, "wood")      ~ "Woody",
    str_detect(profile_clean, "floral")    ~ "Floral",
    str_detect(profile_clean, "gourmand")  ~ "Gourmand",
    str_detect(profile_clean, "citrus")    ~ "Citrus",
    str_detect(profile_clean, "musk")      ~ "Musky",
    str_detect(profile_clean, "green")     ~ "Green",
    str_detect(profile_clean, "spicy")     ~ "Spicy",
    TRUE                                    ~ "Other"  # Catch-all for any outliers
  )) %>%
  # Force dplyr namespace to count the consolidated parent categories by brand
  dplyr::count(Brand, Broad_Category)

# 2. Draw the clean, grouped matrix heatmap
ggplot(consolidated_matrix, aes(x = Brand, y = Broad_Category, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  # Add numerical text labels directly inside the tiles for immediate clarity
  geom_text(aes(label = n), color = "black", fontface = "bold", size = 4.5) +
  # Use a distinct, elegant purple sequential color scheme
  scale_fill_distiller(palette = "Purples", direction = 1) +
  labs(
    title = "Fragrance Family Distribution by Brand",
    subtitle = "Sub-categories consolidated into major parent profiles",
    x = "Brand",
    y = "Scent Profile Family",
    fill = "Perfume Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilts brand names if they crowd
  )

