library(imager)   # Para carregar e processar imagens
library(magrittr) # Para %>% encadeamento
library(ggplot2)  # Para visualização

StringArtGenerator <- function() {
  env <- new.env()
  
  # Atributos
  env$iterations <- 1000
  env$shape <- "circle"
  env$image <- NULL
  env$data <- NULL
  env$residual <- NULL
  env$seed <- 0
  env$nails <- 100
  env$weight <- 20
  env$nodes <- list()
  env$paths <- list()
  
  # ---- Métodos ----
  
  set_seed <- function(seed) env$seed <- seed
  set_weight <- function(weight) env$weight <- weight
  set_shape <- function(shape) env$shape <- shape
  set_iterations <- function(iter) env$iterations <- iter
  
  set_nails <- function(n) {
    env$nails <- n
    if (env$shape == "circle") {
      set_nodes_circle()
    } else if (env$shape == "rectangle") {
      set_nodes_rectangle()
    }
  }
  
  set_nodes_circle <- function() {
    spacing <- 2*pi/env$nails
    radius <- get_radius()
    steps <- 0:(env$nails-1)
    x <- radius + radius*cos(steps*spacing)
    y <- radius + radius*sin(steps*spacing)
    env$nodes <- mapply(function(xx,yy) c(xx,yy), x,y, SIMPLIFY=FALSE)
  }
  
  set_nodes_rectangle <- function() {
    perimeter <- get_perimeter()
    spacing <- perimeter/env$nails
    dims <- dim(env$data)
    width <- dims[1]; height <- dims[2]
    pnails <- seq(0, perimeter, length.out=env$nails+1)[-1]
    xarr <- c(); yarr <- c()
    for (p in pnails) {
      if (p < width) {
        x <- p; y <- 0
      } else if (p < width + height) {
        x <- width; y <- p - width
      } else if (p < 2*width + height) {
        x <- width - (p - width - height)
        y <- height
      } else {
        x <- 0; y <- height - (p - 2*width - height)
      }
      xarr <- c(xarr, x)
      yarr <- c(yarr, y)
    }
    env$nodes <- mapply(function(xx,yy) c(xx,yy), xarr,yarr, SIMPLIFY=FALSE)
  }
  
  get_radius <- function() {
    0.5*max(dim(env$data))
  }
  
  get_perimeter <- function() {
    2*sum(dim(env$data))
  }
  
  load_image <- function(path) {
    img <- load.image(path)
    env$image <- img
    arr <- as.array(img)
    
    # Lida com 4D (x,y,canal,frame), 3D (x,y,canal) ou 2D (x,y)
    if (length(dim(arr)) == 4) {
      env$data <- arr[,,1,1]
    } else if (length(dim(arr)) == 3) {
      env$data <- arr[,,1]
    } else {
      env$data <- arr
    }
  }
  
  preprocess <- function() {
    img <- grayscale(env$image)
    img <- imager::renorm(img)  # normaliza para [0,1]
    img <- imager::imrotate(img, 0) # placeholder, pode ser removido
    img <- 1 - img # inverter cores
    env$image <- img
    
    arr <- as.array(img)
    if (length(dim(arr)) == 3) {
      env$data <- arr[,,1]
    } else {
      env$data <- arr
    }
  }
  
  
  bresenham_path <- function(start, end) {
    # Algoritmo de Bresenham simplificado
    x1 <- round(start[1]); y1 <- round(start[2])
    x2 <- round(end[1]);   y2 <- round(end[2])
    dx <- abs(x2 - x1)
    dy <- abs(y2 - y1)
    sx <- ifelse(x1 < x2, 1, -1)
    sy <- ifelse(y1 < y2, 1, -1)
    err <- dx - dy
    
    path <- list()
    while(TRUE) {
      path <- append(path, list(c(x1,y1)))
      if (x1 == x2 && y1 == y2) break
      e2 <- 2*err
      if (e2 > -dy) { err <- err - dy; x1 <- x1 + sx }
      if (e2 <  dx) { err <- err + dx; y1 <- y1 + sy }
    }
    return(path)
  }
  
  calculate_paths <- function() {
    env$paths <- list()
    for (i in seq_along(env$nodes)) {
      env$paths[[i]] <- list()
      for (j in seq_along(env$nodes)) {
        env$paths[[i]][[j]] <- bresenham_path(env$nodes[[i]], env$nodes[[j]])
      }
    }
  }
  
  choose_darkest_path <- function(nail) {
    max_dark <- -1
    darkest_path <- NULL
    darkest_nail <- NULL
    
    for (idx in seq_along(env$paths[[nail]])) {
      coords <- env$paths[[nail]][[idx]]
      rows <- sapply(coords, function(c) c[1])
      cols <- sapply(coords, function(c) c[2])
      rows <- pmax(1, pmin(rows, nrow(env$data)))
      cols <- pmax(1, pmin(cols, ncol(env$data)))
      dark <- sum(env$data[cbind(rows,cols)])
      if (dark > max_dark) {
        darkest_path <- coords
        darkest_nail <- idx
        max_dark <- dark
      }
    }
    list(nail=darkest_nail, path=darkest_path)
  }
  
  generate <- function() {
    calculate_paths()
    pattern <- list()
    nail <- env$seed
    datacopy <- env$data
    
    for (i in 1:env$iterations) {
      chosen <- choose_darkest_path(nail)
      pattern <- append(pattern, list(env$nodes[[chosen$nail]]))
      coords <- chosen$path
      for (c in coords) {
        x <- c[1]; y <- c[2]
        env$data[x,y] <- max(0, env$data[x,y] - env$weight)
      }
      if (sum(env$data) <= 0) {
        message("Parando: sem mais dados")
        break
      }
      nail <- chosen$nail
    }
    env$residual <- env$data
    env$data <- datacopy
    return(pattern)
  }
  
  # Retorna lista de métodos
  list(
    set_seed = set_seed,
    set_weight = set_weight,
    set_shape = set_shape,
    set_nails = set_nails,
    set_iterations = set_iterations,
    load_image = load_image,
    preprocess = preprocess,
    generate = generate
  )
}

art <- StringArtGenerator()
art$load_image("demo/input/Sample_ML.jpg")
art$preprocess()
art$set_shape("circle")
art$set_nails(150)
art$set_seed(1)
art$set_iterations(500)

pattern <- art$generate()

# Visualizar pontos escolhidos
df <- do.call(rbind, pattern)
df <- as.data.frame(df)
colnames(df) <- c("x","y")

ggplot(df, aes(x,y)) +
  geom_path() +
  coord_equal() +
  theme_minimal()
