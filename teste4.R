library(R6)
library(imager)
library(ggplot2)
library(dplyr)

StringArtGenerator <- R6Class("StringArtGenerator",
                              public = list(
                                img = NULL,
                                data = NULL,
                                nails = NULL,
                                seed = NULL,
                                iterations = NULL,
                                
                                initialize = function() {
                                  # Construtor vazio
                                },
                                
                                load_image = function(path) {
                                  if (!file.exists(path)) stop("Arquivo não encontrado: ", path)
                                  self$img <- load.image(path) %>% grayscale() %>% resize(200,200)
                                },
                                
                                preprocess = function() {
                                  arr <- as.array(self$img)
                                  if (length(dim(arr)) == 4) {
                                    self$data <- arr[,,1,1]
                                  } else if (length(dim(arr)) == 3) {
                                    self$data <- arr[,,1]
                                  } else {
                                    self$data <- arr
                                  }
                                },
                                
                                set_nails = function(n) {
                                  theta <- seq(0, 2*pi, length.out = n+1)[-1]
                                  r <- min(dim(self$data))/2
                                  cx <- ncol(self$data)/2
                                  cy <- nrow(self$data)/2
                                  self$nails <- data.frame(
                                    id = 1:n,
                                    x = cx + r*cos(theta),
                                    y = cy + r*sin(theta)
                                  )
                                },
                                
                                set_seed = function(s) {
                                  self$seed <- s
                                  set.seed(s)
                                },
                                
                                set_iterations = function(it) {
                                  self$iterations <- it
                                },
                                
                                generate = function() {
                                  # Por enquanto: conecta pinos de forma aleatória
                                  pattern <- sample(self$nails$id, self$iterations, replace = TRUE)
                                  coords <- self$nails[pattern, ]
                                  return(coords)
                                },
                                
                                plot_pattern = function(coords, filename = NULL) {
                                  lines <- data.frame(
                                    x1 = head(coords$x, -1),
                                    y1 = head(coords$y, -1),
                                    x2 = tail(coords$x, -1),
                                    y2 = tail(coords$y, -1)
                                  )
                                  
                                  p <- ggplot() +
                                    geom_segment(data = lines,
                                                 aes(x = x1, y = y1, xend = x2, yend = y2),
                                                 color = "black", linewidth = 0.1, alpha = 0.2) +
                                    coord_equal() +
                                    theme_void()
                                  
                                  if (!is.null(filename)) {
                                    ggsave(filename, p, width = 8, height = 8, dpi = 300, bg = "white")
                                  } else {
                                    print(p)
                                  }
                                }
                              )
)
# Criar objeto
generator <- StringArtGenerator$new()

# Carregar e preparar imagem
generator$load_image("demo/input/Sample_ML.jpg")
generator$preprocess()

# Configurar parâmetros
generator$set_nails(180)
generator$set_seed(42)
generator$set_iterations(4000)

# Gerar padrão
pattern <- generator$generate()

# Plotar na tela
generator$plot_pattern(pattern)

# Ou salvar em PNG
generator$plot_pattern(pattern, filename = "demo/result_ml.png")
