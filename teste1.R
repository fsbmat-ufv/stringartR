# Pacotes necessários
library(imager)
library(tidyverse)

# 1. Ler e preparar a imagem
img <- load.image("sua_imagem.jpg") %>%
  grayscale() %>%
  resize(200, 200) # ajuste o tamanho

# Converter para matriz
mat <- as.matrix(img)

# 2. Definir pinos em um círculo
n_pins <- 200
theta <- seq(0, 2*pi, length.out = n_pins + 1)[-1]
circle_radius <- 100
circle_center <- c(100, 100)

pins <- data.frame(
  x = circle_center[1] + circle_radius * cos(theta),
  y = circle_center[2] + circle_radius * sin(theta)
)

# 3. Exemplo: desenhar os pinos
ggplot() +
  geom_point(data = pins, aes(x, y), color="red") +
  coord_equal() +
  theme_void()

# 4. (Próximo passo) - Implementar heurística para escolher linhas entre pinos
# -> Aqui entra a lógica de "qual linha escurece mais a imagem"

