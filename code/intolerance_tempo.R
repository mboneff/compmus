library(tidyverse)
library(compmus)
intolerance <- read_csv("~/Downloads/intolerance.csv")
intolerance |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(20, 50) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")
intolerance_dft <- read_csv("~/Downloads/intolerance_dft.csv")
intolerance_dft |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
intolerance_act <- read_csv("~/Downloads/intolerance_act.csv")
intolerance_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
pata_pata <- read_csv("~/Downloads/pata-pata-novelty.csv")
pata_pata |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits to the desired time range
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")
graveola_act <- read_csv("~/Downloads/graveola-act.csv")
graveola_dft <- read_csv("~/Downloads/graveola-dft.csv")
graveola_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
graveola_dft |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
intolerance_act2 <- read_csv("~/Downloads/intolerance_act2.csv")
intolerance_act2 |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
intolerance_dft2 <- read_csv("~/Downloads/intolerance_dft2.csv")
intolerance_dft2 |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
