library(shiny)

# --- Função auxiliar: String Art de um círculo ---
stringart_circle <- function(n = 100, k = 2, r = 1, col = "blue", lwd = 1, plot = TRUE) {
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
  
  # Retorna dados (sem plotar nada extra)
  invisible(list(
    pregos = data.frame(x = x, y = y),
    conexoes = conexoes,
    comprimento_total = total_length
  ))
}

# --- Server do Shiny ---
function(input, output, session) {
  
  output$distPlot <- renderPlot({
    
    # Garante que input$bins é um número inteiro e mínimo de 3
    n_pregos <- max(3, round(input$bins))
    
    # Chama a função de String Art
    stringart_circle(n = n_pregos, k = 8, r = 1, col = "red", lwd = 1)
    
  })
  
}
