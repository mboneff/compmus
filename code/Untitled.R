library(tidyverse)
library(compmus)
library(tidymodels)
library(ggdendro)
library(heatmaply)

exportify <- read_csv("~/Downloads/Computational_Musicology.csv")

exportify_juice <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = exportify
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep(training = exportify |> mutate(`Track Name` = make.unique(str_trunc(`Track Name`, 36)))) |>
  juice() |>
  column_to_rownames("Track Name")

exportify_dist <- dist(exportify_juice, method = "euclidean")

hc <- hclust(exportify_dist, method = "average")
plot(hc)

p <- hc |>
  dendro_data() |>
  ggdendrogram()

print(p)
dev.off()
plot(hc)
ggdendrogram(hc)
heatmaply(
  exportify_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)
set.seed(1)

exportify_small <- exportify %>%
  slice_sample(n = 40)

exportify_juice <-
  recipe(
    `Track Name` ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = exportify_small
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep(exportify_small |> mutate(`Track Name` = make.unique(str_trunc(`Track Name`, 20)))) |>
  juice() |>
  column_to_rownames("Track Name")

exportify_dist <- dist(exportify_juice, method = "euclidean")
hc <- hclust(exportify_dist, method = "average")

plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering of 40 Tracks")

heatmaply(
  exportify_juice,
  Rowv = as.dendrogram(hc),
  Colv = NA,
  scale = "none"
)
library(tidyverse)
library(tidymodels)

# Load data
exportify <- read_csv("~/Downloads/Computational_Musicology.csv")

# Sample 20 tracks
set.seed(1)
exportify_small <- exportify %>%
  drop_na() %>%
  slice_sample(n = 20) %>%
  mutate(`Track Name` = make.unique(str_trunc(`Track Name`, 25)))

# Prepare numeric data
exportify_scaled <- exportify_small %>%
  select(
    Danceability,
    Energy,
    Loudness,
    Speechiness,
    Acousticness,
    Instrumentalness,
    Liveness,
    Valence,
    Tempo,
    `Duration (ms)`
  ) %>%
  scale()

# Add row names
rownames(exportify_scaled) <- exportify_small$`Track Name`

# Distance + clustering
dist_mat <- dist(exportify_scaled, method = "euclidean")
hc <- hclust(dist_mat, method = "average")

# Plot dendrogram
plot(
  hc,
  main = "Dendrogram (20 Tracks)",
  cex = 0.7,   # text size
  hang = -1
)
library(tidyverse)
library(heatmaply)

# Load data
exportify <- read_csv("~/Downloads/Computational_Musicology.csv")

# Sample 20 tracks
set.seed(1)
exportify_small <- exportify %>%
  drop_na() %>%
  slice_sample(n = 20) %>%
  mutate(`Track Name` = make.unique(str_trunc(`Track Name`, 25)))

# Prepare numeric data
exportify_scaled <- exportify_small %>%
  select(
    Danceability,
    Energy,
    Loudness,
    Speechiness,
    Acousticness,
    Instrumentalness,
    Liveness,
    Valence,
    Tempo,
    `Duration (ms)`
  ) %>%
  scale()

rownames(exportify_scaled) <- exportify_small$`Track Name`

# Hierarchical clustering
dist_mat <- dist(exportify_scaled)
hc <- hclust(dist_mat, method = "average")

# Heatmap
heatmaply(
  exportify_scaled,
  Rowv = as.dendrogram(hc),  # same clustering as dendrogram
  Colv = NA,                 # don't cluster features
  scale = "none",
  margins = c(80, 120),      # space for labels
  fontsize_row = 8,
  fontsize_col = 10
)