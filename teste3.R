# Pacotes
library(ggplot2)
library(dplyr)

# ------------------------
# 1. Definir parâmetros
# ------------------------
n_pins <- 200          # número de pinos
n_linhas <- 500        # número de linhas que vamos desenhar
circle_radius <- 100   # raio do círculo
circle_center <- c(0, 0)

# ------------------------
# 2. Calcular posições dos pinos
# ------------------------
theta <- seq(0, 2*pi, length.out = n_pins + 1)[-1] # ângulos
pins <- data.frame(
  id = 1:n_pins,
  x = circle_center[1] + circle_radius * cos(theta),
  y = circle_center[2] + circle_radius * sin(theta)
)

# ------------------------
# 3. Criar linhas aleatórias entre pinos
# ------------------------
set.seed(123) # reprodutibilidade
linhas <- data.frame(
  from = sample(1:n_pins, n_linhas, replace = TRUE),
  to   = sample(1:n_pins, n_linhas, replace = TRUE)
) %>%
  filter(from != to) %>% # evitar linha sobre o mesmo pino
  left_join(pins, by = c("from" = "id")) %>%
  rename(x1 = x, y1 = y) %>%
  left_join(pins, by = c("to" = "id")) %>%
  rename(x2 = x, y2 = y)

# ------------------------
# 4. Plotar o String Art simples
# ------------------------
ggplot() +
  geom_point(data = pins, aes(x, y), color="red", size=1) +
  geom_segment(data = linhas, aes(x = x1, y = y1, xend = x2, yend = y2),
               color="black", alpha=0.2, linewidth=0.2) +
  coord_equal() +
  theme_void() +
  ggtitle("String Art - Primeira Versão (Linhas Aleatórias)")
