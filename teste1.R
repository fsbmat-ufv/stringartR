#' Gera uma figura de String Art circular
#'
#' A função `stcircle()` constrói uma figura de *String Art* sobre uma
#' circunferência, posicionando `n` pregos igualmente espaçados e conectando
#' cada prego ao prego `k` posições à frente, segundo uma regra modular fixa.
#' A função também calcula o comprimento total do barbante e retorna,
#' invisivelmente, os dados completos da construção.
#'
#' @param n Inteiro maior ou igual a 3. Número de pregos igualmente espaçados
#'   sobre a circunferência.
#' @param k Inteiro entre 1 e `n - 1`. Salto modular da conexão.
#'   A conexão é dada por `j <- (i + k - 1) %% n + 1`.
#' @param r Número positivo. Raio da circunferência.
#' @param col Cor das conexões.
#' @param lwd Número positivo. Espessura das conexões.
#' @param plot Lógico. Se `TRUE`, desenha a figura.
#' @param show_points Lógico. Se `TRUE`, mostra os pregos.
#' @param cex_pregos Número positivo. Tamanho dos pregos no gráfico.
#' @param col_pregos Cor dos pregos.
#' @param show_labels Lógico. Se `TRUE`, mostra os rótulos dos pregos.
#' @param cex_labels Número positivo. Tamanho dos rótulos.
#' @param label_col Cor dos rótulos.
#' @param verbose Lógico. Se `TRUE`, informa o comprimento total do barbante
#'   e uma observação sobre a estrutura da figura.
#'
#' @details
#' A construção posiciona os pregos sobre uma circunferência de raio `r`,
#' centrada em `(0, 0)`, numerando-os de `1` a `n` no sentido anti-horário,
#' a partir do ponto `(r, 0)`.
#'
#' A regra de conexão é auditável e dada por
#' `j <- (i + k - 1) %% n + 1`,
#' ou seja, cada prego `i` é ligado ao prego `k` posições à frente.
#'
#' Quando `mdc(n, k) = 1`, a figura percorre todos os pregos em um único ciclo.
#' Quando `mdc(n, k) > 1`, a construção se decompõe em ciclos independentes,
#' ainda obedecendo exatamente à mesma regra modular.
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{pregos}{`data.frame` com as colunas `indice`, `x` e `y`.}
#'   \item{conexoes}{`data.frame` com as colunas `prego_inicial`,
#'   `prego_final`, `x_inicial`, `y_inicial`, `x_final`, `y_final`
#'   e `comprimento`.}
#'   \item{comprimento_total}{Número com o comprimento total do barbante.}
#' }
#'
#' @examples
#' # Exemplo básico
#' stcircle(n = 20, k = 3, r = 1, col = "blue", lwd = 1.2)
#'
#' # Exemplo com rótulos dos pregos
#' stcircle(
#'   n = 12, k = 5, r = 1,
#'   col = "firebrick", lwd = 1,
#'   show_labels = TRUE
#' )
#'
#' # Exemplo sem plot
#' res <- stcircle(
#'   n = 10, k = 2, r = 1,
#'   col = "darkgreen", lwd = 1,
#'   plot = FALSE, verbose = FALSE
#' )
#' res$comprimento_total
#' head(res$pregos)
#' head(res$conexoes)
#'
#' @importFrom graphics plot points segments lines text
#' @export
stcircle <- function(n = 20, k = 3, r = 1,
                     col = "blue", lwd = 1,
                     plot = TRUE,
                     show_points = TRUE,
                     cex_pregos = 0.8,
                     col_pregos = "black",
                     show_labels = FALSE,
                     cex_labels = 0.7,
                     label_col = "black",
                     verbose = TRUE) {
  
  #---------------------------------------------------------------------------
  # Input checks
  #---------------------------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n != as.integer(n) || n < 3L) {
    stop("`n` must be a single integer greater than or equal to 3.")
  }
  
  if (!is.numeric(k) || length(k) != 1L || is.na(k) ||
      k != as.integer(k) || k < 1L) {
    stop("`k` must be a single positive integer.")
  }
  
  if (!is.numeric(r) || length(r) != 1L || is.na(r) || r <= 0) {
    stop("`r` must be a single positive number.")
  }
  
  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive number.")
  }
  
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("`plot` must be TRUE or FALSE.")
  }
  
  if (!is.logical(show_points) || length(show_points) != 1L || is.na(show_points)) {
    stop("`show_points` must be TRUE or FALSE.")
  }
  
  if (!is.logical(show_labels) || length(show_labels) != 1L || is.na(show_labels)) {
    stop("`show_labels` must be TRUE or FALSE.")
  }
  
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.")
  }
  
  if (!is.numeric(cex_pregos) || length(cex_pregos) != 1L ||
      is.na(cex_pregos) || cex_pregos <= 0) {
    stop("`cex_pregos` must be a single positive number.")
  }
  
  if (!is.numeric(cex_labels) || length(cex_labels) != 1L ||
      is.na(cex_labels) || cex_labels <= 0) {
    stop("`cex_labels` must be a single positive number.")
  }
  
  n <- as.integer(n)
  k <- as.integer(k)
  
  if (k >= n) {
    stop("`k` must satisfy 1 <= k <= n - 1.")
  }
  
  #---------------------------------------------------------------------------
  # Auxiliary function
  #---------------------------------------------------------------------------
  gcd_int <- function(a, b) {
    a <- abs(as.integer(a))
    b <- abs(as.integer(b))
    while (b != 0L) {
      tmp <- b
      b <- a %% b
      a <- tmp
    }
    a
  }
  
  #---------------------------------------------------------------------------
  # Nail positions
  #---------------------------------------------------------------------------
  theta <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  
  pregos <- data.frame(
    indice = seq_len(n),
    x = r * cos(theta),
    y = r * sin(theta)
  )
  
  #---------------------------------------------------------------------------
  # Connections following the modular jump rule
  #---------------------------------------------------------------------------
  prego_inicial <- seq_len(n)
  prego_final <- ((prego_inicial + k - 1L) %% n) + 1L
  
  conexoes <- data.frame(
    prego_inicial = prego_inicial,
    prego_final = prego_final,
    x_inicial = pregos$x[prego_inicial],
    y_inicial = pregos$y[prego_inicial],
    x_final = pregos$x[prego_final],
    y_final = pregos$y[prego_final]
  )
  
  conexoes$comprimento <- sqrt(
    (conexoes$x_final - conexoes$x_inicial)^2 +
      (conexoes$y_final - conexoes$y_inicial)^2
  )
  
  comprimento_total <- sum(conexoes$comprimento)
  
  #---------------------------------------------------------------------------
  # Plot
  #---------------------------------------------------------------------------
  if (plot) {
    margem <- 0.15 * r
    lims <- c(-r - margem, r + margem)
    
    graphics::plot(
      NA, NA,
      xlim = lims, ylim = lims,
      asp = 1,
      xlab = "", ylab = "",
      axes = FALSE
    )
    
    # Circumference outline
    tt <- seq(0, 2 * pi, length.out = 500L)
    graphics::lines(r * cos(tt), r * sin(tt), col = "grey80", lty = 3)
    
    # String connections
    graphics::segments(
      x0 = conexoes$x_inicial,
      y0 = conexoes$y_inicial,
      x1 = conexoes$x_final,
      y1 = conexoes$y_final,
      col = col,
      lwd = lwd
    )
    
    # Nails
    if (show_points) {
      graphics::points(
        pregos$x, pregos$y,
        pch = 19,
        cex = cex_pregos,
        col = col_pregos
      )
    }
    
    # Labels
    if (show_labels) {
      fator_rotulo <- 1.08
      graphics::text(
        x = fator_rotulo * pregos$x,
        y = fator_rotulo * pregos$y,
        labels = pregos$indice,
        cex = cex_labels,
        col = label_col
      )
    }
  }
  
  #---------------------------------------------------------------------------
  # Verbose output
  #---------------------------------------------------------------------------
  if (verbose) {
    message(sprintf(
      "Total string length: %.4f units.",
      comprimento_total
    ))
    
    d <- gcd_int(n, k)
    if (d == 1L) {
      message("The modular rule generates a single cycle through all nails.")
    } else {
      message(sprintf(
        "The modular rule generates %d independent cycles (gcd(n, k) = %d).",
        d, d
      ))
    }
  }
  
  #---------------------------------------------------------------------------
  # Invisible return
  #---------------------------------------------------------------------------
  invisible(list(
    pregos = pregos,
    conexoes = conexoes,
    comprimento_total = comprimento_total
  ))
}
stcircle(n = 20, k = 15, r = 1,
         col = "blue", lwd = 1,
         plot = TRUE,
         show_points = TRUE,
         cex_pregos = 0.8,
         col_pregos = "black",
         show_labels = T,
         cex_labels = 0.7,
         label_col = "black",
         verbose = TRUE)
