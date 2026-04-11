rm(list = ls())
cat("\014")

stringart_circle <- function(n, k, r, col = "blue", lwd = 1, plot = TRUE) {
  if (n < 3) stop("É necessário pelo menos 3 pregos.")
  
  # Ângulos para os pregos
  theta <- seq(0, 2 * pi, length.out = n + 1)[- (n + 1)]
  
  # Coordenadas dos pregos
  x <- r * cos(theta)
  y <- r * sin(theta)
  
  # Vetores para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )
  
  # Plotar o círculo e os pregos
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = paste("String Art com", n, "pregos"))
    points(x, y, pch = 19, col = "black")
  }
  
  # Conectar os pregos e calcular comprimento do barbante
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    
    # Desenha segmento se solicitado
    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }
    
    # Calcula comprimento do barbante (distância euclidiana)
    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len
    
    # Armazena a conexão
    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }
  
  # Mensagem com o comprimento total de barbante
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))
  
  # Retorna lista com os dados
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
  
  # Vetor para armazenar os índices dos pregos
  conexoes <- character(n)
  
  # Construir as conexões com fio contínuo
  for (i in 1:n) {
    from <- i
    to <- (i + k - 1) %% n + 1
    conexoes[i] <- paste0("Prego ", from, " -> Prego ", to)
  }
  
  # Imprimir resultado linha a linha
  cat(paste(conexoes, collapse = "\n"), "\n")
  
  # Retornar invisivelmente para uso posterior
  invisible(conexoes)
}

#Elipse

stringart_ellipse <- function(n = 100, k = 2, a = 1, b = 0.5,
                              col = "blue", lwd = 1, plot = TRUE) {
  if (n < 3) stop("É necessário pelo menos 3 pregos.")
  
  # Ângulos para distribuição dos pregos
  theta <- seq(0, 2 * pi, length.out = n + 1)[- (n + 1)]
  
  # Coordenadas dos pregos ao longo da elipse
  x <- a * cos(theta)
  y <- b * sin(theta)
  
  # Vetores para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )
  
  # Plotagem
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = sprintf("String Art com %d pregos em uma Elipse", n))
    points(x, y, pch = 19, col = "black")
  }
  
  # Conectar os pregos com passo k e calcular comprimento
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }
    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len
    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }
  
  # Mensagem com o comprimento total de barbante
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))
  
  # Retornar informações
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

## Hipérbole

stringart_hyperbola <- function(n = 40, k = 5, a = 1, b = 0.5,
                                t_max = 2, col = "blue", lwd = 1, plot = TRUE) {
  if (n %% 4 != 0) stop("O número de pregos deve ser múltiplo de 4 para simetria total.")
  
  m <- n / 4  # pregos por quadrante
  t_vals <- seq(0.1, t_max, length.out = m)
  
  # 1º quadrante: (+x, +y)
  x1 <-  a * cosh(t_vals)
  y1 <-  b * sinh(t_vals)
  
  # 2º quadrante: (-x, +y)
  x2 <- -a * cosh(t_vals)
  y2 <-  b * sinh(t_vals)
  
  # 3º quadrante: (-x, -y)
  x3 <- -a * cosh(t_vals)
  y3 <- -b * sinh(t_vals)
  
  # 4º quadrante: (+x, -y)
  x4 <-  a * cosh(t_vals)
  y4 <- -b * sinh(t_vals)
  
  # Unir coordenadas
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  
  # Inicializar comprimento e conexões
  total_length <- 0
  conexoes <- data.frame(x1 = numeric(n), y1 = numeric(n),
                         x2 = numeric(n), y2 = numeric(n),
                         comprimento = numeric(n))
  
  # Plotagem
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = sprintf("String Art - Hipérbole (%d pregos)", n))
    points(x, y, pch = 19, col = "black")
  }
  
  # Conectar os pregos com passo k
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }
    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len
    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }
  
  # Mostrar resultado
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))
  
  # Retorno
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

## Parabola

stringart_parabola <- function(n = 100, k = 2, a = 0.2, x_max = 5,
                               col = "blue", lwd = 1, plot = TRUE) {
  if (n < 3) stop("É necessário pelo menos 3 pregos.")
  
  # Geração dos pontos ao longo da parábola: x em [-x_max, x_max]
  x <- seq(-x_max, x_max, length.out = n)
  y <- a * x^2
  
  # Vetores para armazenar comprimento de barbante
  total_length <- 0
  conexoes <- data.frame(
    x1 = numeric(n),
    y1 = numeric(n),
    x2 = numeric(n),
    y2 = numeric(n),
    comprimento = numeric(n)
  )
  
  # Plotagem
  if (plot) {
    plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE,
         main = paste("String Art - Parábola com", n, "pregos"))
    points(x, y, pch = 19, col = "black")
  }
  
  # Conectar pregos
  for (i in 1:n) {
    j <- (i + k - 1) %% n + 1
    if (plot) {
      segments(x[i], y[i], x[j], y[j], col = col, lwd = lwd)
    }
    len <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
    total_length <- total_length + len
    conexoes[i, ] <- c(x[i], y[i], x[j], y[j], len)
  }
  
  # Exibir comprimento total do barbante
  message(sprintf("Comprimento total de barbante: %.2f unidades", total_length))
  
  # Retorno
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}



# Exemplo de uso
stringart_circle(n = 5, k = 2, r = 1, col = "red", lwd = 1)
stringart_ellipse(n = 50, k = 9, a = 2, b = 1, col = "darkorange", lwd = 1.2)
stringart_hyperbola(n = 40, k = 8, a = 1.2, b = 0.6, t_max = 2.5,
                    col = "red", lwd = 1.2)
stringart_parabola(n = 50, k = 18, a = 0.4, x_max = 5,
                   col = "darkgreen", lwd = 1.2)

stringart_fio_continuo <- function(n = 100, k = 2) {
  if (n < 3) stop("É necessário pelo menos 3 pregos.")
  
  # Vetor para armazenar os índices dos pregos
  conexoes <- character(n)
  
  # Construir as conexões com fio contínuo
  for (i in 1:n) {
    from <- i
    to <- (i + k - 1) %% n + 1
    conexoes[i] <- paste0("Prego ", from, " -> Prego ", to)
  }
  
  # Imprimir resultado linha a linha
  cat(paste(conexoes, collapse = "\n"), "\n")
  
  # Retornar invisivelmente para uso posterior
  invisible(conexoes)
}
stringart_fio_continuo(n = 100, k = 37)
