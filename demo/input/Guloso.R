# Pacotes necessários
library(imager)
library(tidyverse)

# 1. Ler e preparar a imagem


img_edges <- load.image("sample_ml.png") %>%
  grayscale() %>%
  resize(200,200) %>%
  cannyEdges(sigma = 2)

img_matrix <- as.matrix(img_edges[,,1,1])
img_matrix <- (img_matrix - min(img_matrix)) /
  (max(img_matrix) - min(img_matrix))



string_art_greedy <- function(target_matrix, n_pegs = 220, n_lines = 2000, line_strength = 0.03) {
  
  size <- nrow(target_matrix)
  
  theta <- seq(0, 2*pi, length.out = n_pegs + 1)[-(n_pegs+1)]
  radius <- size/2 - 2
  
  pegs <- data.frame(
    x = round(radius * cos(theta) + size/2),
    y = round(size/2 - radius * sin(theta))
  )
  
  approx_img <- matrix(1, size, size)  # começa branca
  
  draw_line <- function(x0, y0, x1, y1) {
    n <- max(abs(x1 - x0), abs(y1 - y0))
    xs <- round(seq(x0, x1, length.out = n))
    ys <- round(seq(y0, y1, length.out = n))
    cbind(xs, ys)
  }
  
  current_peg <- sample(1:n_pegs, 1)
  connections <- list()
  
  for (iter in 1:n_lines) {
    
    best_gain <- -Inf
    best_peg <- NULL
    best_coords <- NULL
    
    for (j in 1:n_pegs) {
      if (j == current_peg) next
      
      coords <- draw_line(
        pegs$x[current_peg], pegs$y[current_peg],
        pegs$x[j], pegs$y[j]
      )
      
      valid <- coords[,1] > 0 & coords[,1] <= size &
        coords[,2] > 0 & coords[,2] <= size
      
      coords <- coords[valid, , drop = FALSE]
      
      if (nrow(coords) < 2) next
      
      rows <- coords[,2]
      cols <- coords[,1]
      
      # ganho local = quanto reduz diferença
      before <- approx_img[cbind(rows, cols)]
      after  <- pmax(0, before - line_strength)
      
      gain <- sum((before - target_matrix[cbind(rows, cols)])^2 -
                    (after  - target_matrix[cbind(rows, cols)])^2)
      
      if (gain > best_gain) {
        best_gain <- gain
        best_peg <- j
        best_coords <- coords
      }
    }
    
    if (is.null(best_coords)) break
    
    rows <- best_coords[,2]
    cols <- best_coords[,1]
    
    approx_img[cbind(rows, cols)] <- 
      pmax(0, approx_img[cbind(rows, cols)] - line_strength)
    
    connections[[iter]] <- c(current_peg, best_peg)
    current_peg <- best_peg
  }
  
  list(image = approx_img,
       connections = connections,
       pegs = pegs)
}

img_matrix <- as.matrix(img[,,1,1])  # NÃO inverter agora
result <- string_art_greedy(img_matrix)
image(result$image, col = gray.colors(256))


rotated_img <- result$image[nrow(result$image):1, 
                            ncol(result$image):1]
image(rotated_img, col = gray.colors(256))
