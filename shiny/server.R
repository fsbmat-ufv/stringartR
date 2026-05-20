##################################
# StringArt Shiny App
# Standardized Server Version
##################################

library(shiny)
library(stringArt)
library(markdown)
library(DT)

shinyServer(function(input, output, session) {
  
  # --------------------------------------------------
  # Operador auxiliar
  # --------------------------------------------------
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # --------------------------------------------------
  # Lista de figuras disponíveis no pacote
  # --------------------------------------------------
  available_figures <- c(
    "Círculo",
    "Cardioide",
    "Elipse",
    "Triângulo",
    "Polígono regular",
    "Estrela",
    "Parábola",
    "Rede",
    "Hexaflower",
    "Radial",
    "Lótus",
    "Rosa",
    "Espiral",
    "Lissajous",
    "Região",
    "Grade retangular",
    "Decimal"
  )
  
  # --------------------------------------------------
  # Parâmetros internos fixos do app
  # --------------------------------------------------
  fixed_params <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      
      "Círculo" = list(
        r = 5
      ),
      
      "Cardioide" = list(
        r = 4,
        rotate = 0
      ),
      
      "Elipse" = list(
        a = 4,
        b = 2.5
      ),
      
      "Triângulo" = list(
        side = 5
      ),
      
      "Polígono regular" = list(
        sides = 5,
        radius = 4,
        rotate = pi / 2
      ),
      
      "Estrela" = list(
        radius = 4,
        rotate = pi / 2,
        draw_polygon = FALSE
      ),
      
      "Parábola" = list(
        width = 6,
        height = 6,
        show_envelope = FALSE
      ),
      
      "Rede" = list(
        length1 = 6,
        length2 = 6,
        angle = pi / 2,
        rotate = 0,
        show_envelope = FALSE
      ),
      
      "Hexaflower" = list(
        r = 3,
        scale_mid = 0.72,
        scale_inner = 0.42,
        offset_mid = 0,
        offset_inner = 0
      ),
      
      "Radial" = list(
        m = 6,
        r = 1.2,
        spread = pi / 5,
        rotate = 0,
        show_center = TRUE,
        center_col = "black",
        center_cex = 0.9
      ),
      
      "Lótus" = list(
        petals = 5,
        outer_radius = 4,
        petal_radius = 1.35,
        petal_center_radius = 1.35,
        inner_radius = 0.7,
        rotate = 0
      ),
      
      "Rosa" = list(
        petals = 6,
        amplitude = 4,
        rotate = 0
      ),
      
      "Espiral" = list(
        turns = 3,
        spacing = 1.2,
        inner_radius = 0,
        rotate = 0
      ),
      
      "Lissajous" = list(
        a = 3,
        b = 2,
        phase = pi / 2,
        amplitude_x = 4,
        amplitude_y = 4,
        rotate = 0
      ),
      
      "Região" = list(
        contour = NULL
      ),
      
      "Grade retangular" = list(
        width = 6,
        height = 4,
        rotate = 0
      ),
      
      "Decimal" = list(
        numerator = 1,
        denominator = 7,
        radius = 4,
        rotate = pi / 2,
        include_integer_part = TRUE
      )
    )
  })
  
  # --------------------------------------------------
  # Limites de n por figura
  # --------------------------------------------------
  min_n_by_figure <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      "Decimal" = 2L,
      "Região" = 4L,
      "Hexaflower" = 6L,
      "Polígono regular" = 5L,
      "Estrela" = 5L,
      3L
    )
  })
  
  max_n_by_figure <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      "Decimal" = 10L,
      300L
    )
  })
  
  # --------------------------------------------------
  # Valores iniciais sugeridos por figura
  # --------------------------------------------------
  observeEvent(input$figura, {
    if (input$figura == "Estrela") {
      updateNumericInput(session, "n", min = 5, max = 300, value = 5)
      updateNumericInput(session, "k", min = 2, max = 2, value = 2)
      
    } else if (input$figura == "Polígono regular") {
      updateNumericInput(session, "n", min = 5, max = 300, value = 60)
      updateNumericInput(session, "k", min = 1, max = 59, value = 7)
      
    } else if (input$figura == "Decimal") {
      updateNumericInput(session, "n", min = 2, max = 10, value = 10)
      updateNumericInput(session, "k", min = 1, max = 20, value = 2)
    }
  }, ignoreInit = TRUE)
  
  # --------------------------------------------------
  # Número efetivo de pregos
  # --------------------------------------------------
  effective_n <- reactive({
    req(input$n, input$figura)
    
    n0 <- as.integer(input$n)
    
    if (input$figura == "Decimal") {
      return(max(2L, min(10L, n0)))
    }
    
    if (input$figura == "Região") {
      return(max(4L, n0))
    }
    
    if (input$figura == "Polígono regular") {
      return(max(5L, n0))
    }
    
    if (input$figura == "Estrela") {
      return(max(5L, n0))
    }
    
    if (input$figura == "Hexaflower") {
      n1 <- max(6L, as.integer(round(n0 / 6) * 6))
      return(n1)
    }
    
    max(3L, n0)
  })
  
  # --------------------------------------------------
  # Limites e valor efetivo de k
  # --------------------------------------------------
  k_limits <- reactive({
    req(input$figura, effective_n())
    
    if (input$figura == "Decimal") {
      list(min = 1L, max = 20L)
      
    } else if (input$figura == "Estrela") {
      # Para a visualização pedagógica de polígonos estrelados, evitamos k = 1,
      # que gera apenas o polígono regular, e evitamos passos equivalentes
      # maiores que n/2.
      list(
        min = 2L,
        max = max(2L, floor((effective_n() - 1L) / 2L))
      )
      
    } else {
      list(
        min = 1L,
        max = max(1L, effective_n() - 1L)
      )
    }
  })
  
  effective_k <- reactive({
    req(input$k, input$figura, effective_n())
    
    limits <- k_limits()
    k0 <- as.integer(round(input$k))
    
    max(limits$min, min(k0, limits$max))
  })
  
  # --------------------------------------------------
  # Título padronizado das figuras
  # --------------------------------------------------
  plot_title <- reactive({
    req(input$figura, effective_n(), effective_k())
    
    paste0(
      input$figura,
      " - n = ", effective_n(),
      ", k = ", effective_k()
    )
  })
  
  # --------------------------------------------------
  # Ajuste dinâmico de n
  # --------------------------------------------------
  observe({
    req(input$figura, input$n)
    
    n_eff <- effective_n()
    
    updateNumericInput(
      session,
      "n",
      min = min_n_by_figure(),
      max = max_n_by_figure(),
      value = n_eff
    )
  })
  
  # --------------------------------------------------
  # Ajuste dinâmico do salto k
  # --------------------------------------------------
  observe({
    req(input$figura, effective_n(), input$k)
    
    limits <- k_limits()
    
    updateNumericInput(
      session,
      "k",
      min = limits$min,
      max = limits$max,
      value = effective_k()
    )
  })
  
  # --------------------------------------------------
  # Função auxiliar:
  # traduz e padroniza a saída das funções do pacote para o Shiny
  # --------------------------------------------------
  normalize_art_result <- function(res) {
    
    req(res)
    req(res$pegs)
    req(res$connections)
    req(res$total_length)
    
    pegs <- res$pegs
    
    if (!"index" %in% names(pegs)) {
      pegs$index <- seq_len(nrow(pegs))
    }
    
    names(pegs)[names(pegs) == "index"] <- "indice"
    names(pegs)[names(pegs) == "module"] <- "modulo"
    names(pegs)[names(pegs) == "group"] <- "grupo"
    names(pegs)[names(pegs) == "layer"] <- "camada"
    names(pegs)[names(pegs) == "local_index"] <- "indice_local"
    names(pegs)[names(pegs) == "axis"] <- "eixo"
    names(pegs)[names(pegs) == "ray"] <- "semirreta"
    names(pegs)[names(pegs) == "side"] <- "lado"
    names(pegs)[names(pegs) == "digit"] <- "digito"
    
    con <- res$connections
    
    if (!"connection_index" %in% names(con)) {
      con$connection_index <- seq_len(nrow(con))
    }
    
    names(con)[names(con) == "connection_index"] <- "indice_conexao"
    names(con)[names(con) == "from"] <- "prego_inicial"
    names(con)[names(con) == "to"] <- "prego_final"
    names(con)[names(con) == "x_from"] <- "x_inicial"
    names(con)[names(con) == "y_from"] <- "y_inicial"
    names(con)[names(con) == "x_to"] <- "x_final"
    names(con)[names(con) == "y_to"] <- "y_final"
    names(con)[names(con) == "length"] <- "comprimento"
    
    names(con)[names(con) == "module"] <- "modulo"
    names(con)[names(con) == "group"] <- "grupo"
    names(con)[names(con) == "block"] <- "bloco"
    names(con)[names(con) == "sector"] <- "setor"
    names(con)[names(con) == "layer"] <- "camada"
    names(con)[names(con) == "sweep"] <- "varredura"
    names(con)[names(con) == "offset"] <- "deslocamento"
    names(con)[names(con) == "local_from"] <- "indice_local_inicial"
    names(con)[names(con) == "local_to"] <- "indice_local_final"
    names(con)[names(con) == "local_index"] <- "indice_local"
    names(con)[names(con) == "digit_from"] <- "digito_inicial"
    names(con)[names(con) == "digit_to"] <- "digito_final"
    names(con)[names(con) == "position"] <- "posicao"
    names(con)[names(con) == "color"] <- "cor"
    
    # Aliases curtos para compatibilidade visual com versões anteriores do app.
    con$i <- con$prego_inicial
    con$j <- con$prego_final
    con$x1 <- con$x_inicial
    con$y1 <- con$y_inicial
    con$x2 <- con$x_final
    con$y2 <- con$y_final
    
    canonical_cols <- c(
      "indice_conexao",
      "prego_inicial", "prego_final",
      "x_inicial", "y_inicial",
      "x_final", "y_final",
      "comprimento"
    )
    
    alias_cols <- c("i", "j", "x1", "y1", "x2", "y2")
    extra_cols <- setdiff(names(con), c(canonical_cols, alias_cols))
    
    con <- con[, c(
      canonical_cols[canonical_cols %in% names(con)],
      alias_cols[alias_cols %in% names(con)],
      extra_cols
    ), drop = FALSE]
    
    res$pregos <- pegs
    res$conexoes <- con
    res$comprimento_total <- res$total_length
    
    res
  }
  
  # --------------------------------------------------
  # Funções auxiliares para regra e descrição
  # --------------------------------------------------
  rule_label <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      "Círculo" = "j = (i + k - 1) %% n + 1",
      "Cardioide" = "j = ((k * (i - 1)) %% n) + 1",
      "Elipse" = "j = (i + k - 1) %% n + 1",
      "Triângulo" = "j = (i + k - 1) %% n + 1",
      "Polígono regular" = "j = (i + k - 1) %% n + 1",
      "Estrela" = "{n/k}: j = (i + k - 1) %% n + 1",
      "Parábola" = "Prego i no eixo horizontal -> prego i deslocado no eixo vertical",
      "Rede" = "Prego i na primeira semirreta -> prego i deslocado na segunda semirreta",
      "Hexaflower" = paste(
        "Bloco 1: contorno externo;",
        "Blocos 2 e 3: j = (i + k - 1) %% n + 1;",
        "Bloco 4: vértices externos -> centro."
      ),
      "Radial" = "Em cada módulo: j = (i + k - 1) %% n + 1",
      "Lótus" = "Em cada módulo circular: j = (i + k - 1) %% n + 1",
      "Rosa" = "Pontos sobre curva polar; j = (i + k - 1) %% n + 1",
      "Espiral" = "Pontos sobre espiral de Arquimedes; j = (i + k - 1) %% n + 1",
      "Lissajous" = "Pontos sobre curva paramétrica; j = (i + k - 1) %% n + 1",
      "Região" = "j = (i + floor(n/2) + deslocamento) %% n",
      "Grade retangular" = "j = (i + k - 1) %% n + 1",
      "Decimal" = "Conecta dígitos consecutivos da expansão decimal da fração"
    )
  })
  
  rule_description <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      "Círculo" = "Os pregos são distribuídos uniformemente sobre uma circunferência e cada prego é conectado ao prego `k` posições à frente.",
      "Cardioide" = "Os pregos são distribuídos uniformemente sobre uma circunferência e cada prego é conectado por uma regra multiplicativa modular. Para `k = 2`, surge a figura clássica associada ao efeito de cardioide.",
      "Elipse" = "Os pregos são distribuídos uniformemente sobre o contorno de uma elipse e cada prego é conectado ao prego `k` posições à frente.",
      "Triângulo" = "Os pregos são distribuídos uniformemente ao longo do contorno de um triângulo equilátero e cada prego é conectado ao prego `k` posições à frente.",
      "Polígono regular" = "Os pregos são distribuídos ao longo do contorno de um polígono regular. A construção permite explorar ângulo central, ângulo interno, simetria e aritmética modular.",
      "Estrela" = "Os pregos são distribuídos sobre uma circunferência e conectados pelo salto `k`, formando polígonos estrelados. A auditoria permite observar ciclos, período e máximo divisor comum.",
      "Parábola" = "Os pregos são distribuídos em dois eixos perpendiculares. A família de segmentos gera uma envoltória visual associada à parábola clássica da String Art.",
      "Rede" = "Os pregos são distribuídos em duas semirretas com origem comum. A figura generaliza a construção parabólica para ângulos diferentes.",
      "Hexaflower" = "Os pregos são distribuídos em três circuitos hexagonais concêntricos e um centro. As conexões combinam contorno externo, saltos entre circuitos e ligações dos vértices externos ao centro.",
      "Radial" = "A figura é composta por módulos triangulares rotacionados em torno do centro. Em cada módulo, os pregos são distribuídos ao longo do contorno triangular e conectados localmente com salto `k`.",
      "Lótus" = "A figura é formada pela sobreposição de módulos circulares, incluindo círculo externo, pétalas e núcleo central.",
      "Rosa" = "Os pregos são posicionados sobre uma curva polar do tipo rosácea. A figura permite explorar simetria radial, trigonometria e periodicidade.",
      "Espiral" = "Os pregos são posicionados sobre uma espiral de Arquimedes. A figura permite explorar coordenadas polares, crescimento e parametrização.",
      "Lissajous" = "Os pregos são posicionados sobre uma curva de Lissajous. A figura permite explorar frequência, fase, razão entre frequências e curvas paramétricas.",
      "Região" = "Os pregos são distribuídos ao longo de um contorno fechado e conectados a pontos aproximadamente opostos, preenchendo visualmente a região.",
      "Grade retangular" = "Os pregos são distribuídos ao longo da borda de um retângulo e conectados por uma regra modular. A figura permite explorar coordenadas, inclinação e simetria.",
      "Decimal" = "A circunferência é dividida em dígitos e a figura conecta dígitos consecutivos da expansão decimal de uma fração racional."
    )
  })
  
  # --------------------------------------------------
  # Parâmetros geométricos em texto
  # --------------------------------------------------
  geometry_parameters_md <- reactive({
    req(input$figura)
    
    pars <- fixed_params()
    n_eff <- effective_n()
    
    switch(
      input$figura,
      
      "Círculo" = paste0(
        "- **Raio utilizado:** ", pars$r, "\n"
      ),
      
      "Cardioide" = paste0(
        "- **Raio utilizado:** ", pars$r, "\n",
        "- **Rotação aplicada:** ", sprintf("%.2f", pars$rotate), " rad\n"
      ),
      
      "Elipse" = paste0(
        "- **Semi-eixo maior (a):** ", pars$a, "\n",
        "- **Semi-eixo menor (b):** ", pars$b, "\n"
      ),
      
      "Triângulo" = paste0(
        "- **Comprimento do lado do triângulo:** ", pars$side, "\n"
      ),
      
      "Polígono regular" = paste0(
        "- **Número de lados:** ", pars$sides, "\n",
        "- **Raio circunscrito:** ", pars$radius, "\n"
      ),
      
      "Estrela" = paste0(
        "- **Notação do polígono estrelado:** {", n_eff, "/", effective_k(), "}\n",
        "- **Raio utilizado:** ", pars$radius, "\n"
      ),
      
      "Parábola" = paste0(
        "- **Largura:** ", pars$width, "\n",
        "- **Altura:** ", pars$height, "\n"
      ),
      
      "Rede" = paste0(
        "- **Comprimento da primeira semirreta:** ", pars$length1, "\n",
        "- **Comprimento da segunda semirreta:** ", pars$length2, "\n",
        "- **Ângulo entre as semirretas:** ", sprintf("%.2f", pars$angle), " rad\n"
      ),
      
      "Hexaflower" = paste0(
        "- **Raio externo:** ", pars$r, "\n",
        "- **Escala intermediária:** ", pars$scale_mid, "\n",
        "- **Escala interna:** ", pars$scale_inner, "\n",
        "- **Deslocamento intermediário:** ", pars$offset_mid, "\n",
        "- **Deslocamento interno:** ", pars$offset_inner, "\n",
        "- **Número efetivo de pregos por circuito:** ", n_eff, "\n"
      ),
      
      "Radial" = paste0(
        "- **Número de módulos (m):** ", pars$m, "\n",
        "- **Raio externo do módulo:** ", pars$r, "\n",
        "- **Abertura angular:** ", sprintf("%.2f", pars$spread), " rad\n"
      ),
      
      "Lótus" = paste0(
        "- **Número de pétalas:** ", pars$petals, "\n",
        "- **Raio externo:** ", pars$outer_radius, "\n",
        "- **Raio das pétalas:** ", pars$petal_radius, "\n",
        "- **Raio do núcleo central:** ", pars$inner_radius, "\n"
      ),
      
      "Rosa" = paste0(
        "- **Número de pétalas:** ", pars$petals, "\n",
        "- **Amplitude:** ", pars$amplitude, "\n"
      ),
      
      "Espiral" = paste0(
        "- **Número de voltas:** ", pars$turns, "\n",
        "- **Espaçamento radial:** ", pars$spacing, "\n",
        "- **Raio inicial:** ", pars$inner_radius, "\n"
      ),
      
      "Lissajous" = paste0(
        "- **Frequência em x (a):** ", pars$a, "\n",
        "- **Frequência em y (b):** ", pars$b, "\n",
        "- **Fase:** ", sprintf("%.2f", pars$phase), " rad\n"
      ),
      
      "Região" = paste0(
        "- **Contorno utilizado:** padrão interno do pacote\n"
      ),
      
      "Grade retangular" = paste0(
        "- **Largura:** ", pars$width, "\n",
        "- **Altura:** ", pars$height, "\n"
      ),
      
      "Decimal" = paste0(
        "- **Fração utilizada:** ", pars$numerator, "/", pars$denominator, "\n",
        "- **Base / número de dígitos:** ", n_eff, "\n",
        "- **Repetições exibidas:** ", effective_k(), "\n"
      )
    )
  })
  
  # --------------------------------------------------
  # Função auxiliar:
  # chama a função correta do pacote
  # --------------------------------------------------
  build_art <- function(plot_value = FALSE) {
    
    n_eff <- effective_n()
    pars <- fixed_params()
    template_value <- isTRUE(input$template)
    
    common <- list(
      n = n_eff,
      k = effective_k(),
      col = input$col,
      lwd = input$lwd,
      plot = plot_value,
      show_points = input$show_points,
      show_labels = input$show_labels,
      verbose = FALSE,
      template = template_value,
      main = plot_title()
    )
    
    if (input$figura == "Círculo") {
      args <- c(common, pars)
      res <- do.call(stcircle, args)
      
    } else if (input$figura == "Cardioide") {
      args <- c(common, pars)
      res <- do.call(stcardioid, args)
      
    } else if (input$figura == "Elipse") {
      args <- c(common, pars)
      res <- do.call(stellipse, args)
      
    } else if (input$figura == "Triângulo") {
      args <- c(common, pars)
      res <- do.call(sttriangle, args)
      
    } else if (input$figura == "Polígono regular") {
      args <- c(common, pars)
      res <- do.call(stpolygon, args)
      
    } else if (input$figura == "Estrela") {
      args <- c(common, pars)
      res <- do.call(ststar, args)
      
    } else if (input$figura == "Parábola") {
      args <- c(common, pars)
      res <- do.call(stparabola, args)
      
    } else if (input$figura == "Rede") {
      args <- c(common, pars)
      res <- do.call(stnet, args)
      
    } else if (input$figura == "Hexaflower") {
      args <- c(common, pars)
      res <- do.call(sthexaflower, args)
      
    } else if (input$figura == "Radial") {
      args <- c(common, pars)
      res <- do.call(stradial, args)
      
    } else if (input$figura == "Lótus") {
      args <- c(common, pars)
      res <- do.call(stlotus, args)
      
    } else if (input$figura == "Rosa") {
      args <- c(common, pars)
      res <- do.call(strose, args)
      
    } else if (input$figura == "Espiral") {
      args <- c(common, pars)
      res <- do.call(stspiral, args)
      
    } else if (input$figura == "Lissajous") {
      args <- c(common, pars)
      res <- do.call(stlissajous, args)
      
    } else if (input$figura == "Região") {
      args <- c(common, pars)
      res <- do.call(stregion, args)
      
    } else if (input$figura == "Grade retangular") {
      args <- c(common, pars)
      res <- do.call(stgrid, args)
      
    } else if (input$figura == "Decimal") {
      args <- c(common, pars)
      res <- do.call(stdecimal, args)
      
    } else {
      stop("Figura não reconhecida.", call. = FALSE)
    }
    
    res
  }
  
  # --------------------------------------------------
  # Função auxiliar:
  # constrói os dados sem plot
  # --------------------------------------------------
  build_art_data <- function() {
    normalize_art_result(build_art(plot_value = FALSE))
  }
  
  # --------------------------------------------------
  # Dados reativos
  # --------------------------------------------------
  art_data <- reactive({
    build_art_data()
  })
  
  # --------------------------------------------------
  # Função de desenho
  # --------------------------------------------------
  draw_stringart <- function() {
    invisible(build_art(plot_value = TRUE))
  }
  
  # --------------------------------------------------
  # Plot
  # --------------------------------------------------
  output$grafico <- renderPlot({
    draw_stringart()
  }, res = 96)
  
  # --------------------------------------------------
  # Auditoria textual
  # --------------------------------------------------
  auditoria_texto <- reactive({
    
    res <- art_data()
    req(res$conexoes)
    
    con <- res$conexoes
    
    conexoes_txt <- if ("modulo" %in% names(con) &&
                        all(c("indice_local_inicial", "indice_local_final") %in% names(con))) {
      
      paste0(
        "Módulo ", con$modulo,
        ": Prego ", con$indice_local_inicial,
        " -> Prego ", con$indice_local_final
      )
      
    } else if ("bloco" %in% names(con)) {
      
      paste0(
        "[", con$bloco, "] ",
        "Prego ", con$prego_inicial,
        " -> Prego ", con$prego_final
      )
      
    } else if (all(c("digito_inicial", "digito_final") %in% names(con))) {
      
      paste0(
        "Dígito ", con$digito_inicial,
        " -> Dígito ", con$digito_final
      )
      
    } else if (all(c("varredura", "indice_local_inicial", "indice_local_final") %in% names(con))) {
      
      paste0(
        "Varredura ", con$varredura,
        ": Prego ", con$indice_local_inicial,
        " -> Prego ", con$indice_local_final
      )
      
    } else {
      
      paste0(
        "Prego ", con$prego_inicial,
        " -> Prego ", con$prego_final
      )
    }
    
    max_show <- 250L
    conexoes_txt_show <- if (length(conexoes_txt) > max_show) {
      c(
        conexoes_txt[1:max_show],
        sprintf("... (%d conexões adicionais omitidas nesta visualização)", length(conexoes_txt) - max_show)
      )
    } else {
      conexoes_txt
    }
    
    audit_package <- if (!is.null(res$audit)) {
      paste(res$audit, collapse = "\n")
    } else {
      "Auditoria interna do pacote não disponível."
    }
    
    paste(
      "Auditoria da Construção",
      "",
      paste0("Figura: ", input$figura),
      paste0("Número de pregos efetivo (n): ", effective_n()),
      paste0("Parâmetro k: ", effective_k()),
      paste0("Comprimento total de barbante: ",
             sprintf("%.2f", res$comprimento_total), " unidades"),
      "",
      "Regra de ligação:",
      rule_label(),
      "",
      "Resumo retornado pelo pacote:",
      audit_package,
      "",
      "Ligações realizadas:",
      paste(conexoes_txt_show, collapse = "\n"),
      sep = "\n"
    )
  })
  
  output$auditoria_texto <- renderText({
    auditoria_texto()
  })
  
  # --------------------------------------------------
  # Auditoria detalhada no console
  # --------------------------------------------------
  observeEvent(
    {
      list(input$figura, input$n, input$k, input$col, input$lwd,
           input$show_points, input$show_labels, input$template, input$verbose)
    },
    {
      if (isTRUE(input$verbose)) {
        cat("\n============================\n")
        cat(auditoria_texto())
        cat("\n============================\n")
      }
    },
    ignoreInit = TRUE
  )
  
  # --------------------------------------------------
  # Tabela de conexões
  # --------------------------------------------------
  output$tabela_conexoes <- DT::renderDataTable({
    
    res <- art_data()
    req(res$conexoes)
    
    con <- res$conexoes
    
    canonical_cols <- c(
      "indice_conexao",
      "prego_inicial", "prego_final",
      "x_inicial", "y_inicial",
      "x_final", "y_final",
      "comprimento"
    )
    
    alias_cols <- c("i", "j", "x1", "y1", "x2", "y2")
    extra_cols <- setdiff(names(con), c(canonical_cols, alias_cols))
    
    display_cols <- c(
      canonical_cols[canonical_cols %in% names(con)],
      extra_cols
    )
    
    tabela <- con[, display_cols, drop = FALSE]
    
    DT::datatable(
      tabela,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "420px",
        searching = FALSE,
        lengthChange = FALSE
      )
    )
  })
  
  # --------------------------------------------------
  # Downloads
  # --------------------------------------------------
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("stringart_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 1200, res = 150)
      draw_stringart()
      dev.off()
    }
  )
  
  output$download_hd <- downloadHandler(
    filename = function() {
      paste0("stringart_HD_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 3000, height = 3000, res = 300)
      draw_stringart()
      dev.off()
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("stringart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      grDevices::pdf(file, width = 8, height = 8)
      draw_stringart()
      dev.off()
    }
  )
  
  observeEvent(input$print_plot, {
    session$sendCustomMessage("printPlot", list())
  })
  
  # --------------------------------------------------
  # Resumo técnico
  # --------------------------------------------------
  output$resumo_md <- renderUI({
    
    res <- art_data()
    req(res)
    
    n_eff <- effective_n()
    
    ideal_n <- switch(
      input$figura,
      
      "Círculo" = if (n_eff < 30) {
        "Para melhor definição visual recomenda-se utilizar pelo menos 40 pregos."
      } else if (n_eff >= 40 && n_eff <= 120) {
        "O número de pregos utilizado encontra-se em uma faixa adequada para boa definição geométrica."
      } else {
        "Número elevado de pregos gera alta densidade visual e maior complexidade estrutural."
      },
      
      "Cardioide" = if (n_eff < 60) {
        "Para padrões tipo cardioide recomenda-se utilizar pelo menos 80 pregos para uma envoltória visual mais suave."
      } else {
        "O número de pregos está adequado para boa percepção da envoltória produzida pela regra multiplicativa."
      },
      
      "Elipse" = "Para elipses recomenda-se utilizar entre 60 e 120 pregos para melhor percepção da curvatura.",
      
      "Triângulo" = if (n_eff < 24) {
        "Para triângulos recomenda-se utilizar ao menos 24 pregos para melhor distribuição ao longo do contorno."
      } else if (n_eff >= 24 && n_eff <= 90) {
        "O número de pregos está adequado para uma construção triangular equilibrada."
      } else {
        "Número elevado de pregos produz alta densidade de segmentos e uma malha triangular mais complexa."
      },
      
      "Polígono regular" = "Para polígonos regulares, recomenda-se equilibrar o número de pregos com o número de lados para preservar a legibilidade do contorno.",
      
      "Estrela" = "Para estrelas, a relação entre `n` e `k` é fundamental: se o máximo divisor comum for 1, a figura forma um ciclo único.",
      
      "Parábola" = "Para a parábola, valores entre 30 e 80 pregos costumam produzir boa visualização da envoltória.",
      
      "Rede" = "Para redes de String Art, valores moderados de `n` preservam a leitura dos segmentos e da envoltória.",
      
      "Hexaflower" = "Para Hexaflower, recomenda-se usar valores de `n` múltiplos de 6 com pelo menos 18 ou 24 pregos por circuito para melhor simetria.",
      
      "Radial" = "Para figuras radiais, a escolha de `n` deve equilibrar densidade visual e legibilidade dos módulos.",
      
      "Lótus" = "Para a Lótus, valores entre 30 e 80 pregos por módulo produzem bom equilíbrio entre suavidade e legibilidade.",
      
      "Rosa" = "Para rosáceas, recomenda-se usar valores maiores de `n` quando se deseja suavizar a curva trigonométrica.",
      
      "Espiral" = "Para espirais, valores maiores de `n` tornam a parametrização mais suave e a estrutura mais contínua.",
      
      "Lissajous" = "Para curvas de Lissajous, valores maiores de `n` ajudam a representar melhor a periodicidade da curva.",
      
      "Região" = "Para regiões, valores entre 80 e 150 pregos tendem a produzir preenchimento mais uniforme do contorno.",
      
      "Grade retangular" = "Para grades retangulares, o número de pregos deve permitir boa distribuição nas quatro bordas.",
      
      "Decimal" = "Na figura decimal, `n` representa a base e o número de dígitos. Para representação decimal usual, utiliza-se `n = 10`."
    )
    
    ideal_lwd <- if (input$lwd < 1) {
      "Espessuras menores produzem traçados mais delicados."
    } else if (input$lwd >= 1 && input$lwd <= 2) {
      "Espessura adequada para visualização equilibrada."
    } else {
      "Espessuras maiores produzem efeito visual mais intenso."
    }
    
    nota_hex <- if (input$figura == "Hexaflower" && !identical(as.integer(input$n), n_eff)) {
      paste0(
        "\n### ℹ️ Ajuste automático\n\n",
        "- O valor de `n` foi ajustado automaticamente para **", n_eff,
        "**, pois a figura Hexaflower exige `n` múltiplo de 6.\n\n"
      )
    } else {
      ""
    }
    
    nota_decimal <- if (input$figura == "Decimal") {
      paste0(
        "\n### ℹ️ Observação sobre a figura decimal\n\n",
        "- Nesta versão simplificada do Shiny, a fração usada é **1/7** e `k` controla o número de repetições do período exibidas.\n\n"
      )
    } else {
      ""
    }
    
    nota_template <- if (isTRUE(input$template)) {
      paste0(
        "\n### 📌 Modo gabarito\n\n",
        "- O modo **gabarito sem barbante** está ativado. Assim, o app mostra os pregos sem desenhar as conexões.\n\n"
      )
    } else {
      ""
    }
    
    HTML(markdown::markdownToHTML(
      text = paste0(
        "### 🔎 Especificação da Figura Gerada\n\n",
        "- **Figura construída:** ", input$figura, "\n",
        "- **Número de pregos efetivo (n):** ", n_eff, "\n",
        "- **Parâmetro k:** ", effective_k(), "\n",
        geometry_parameters_md(),
        "- **Espessura da linha (lwd):** ", input$lwd, "\n",
        "- **Comprimento total de barbante:** ", sprintf("%.2f", res$comprimento_total), " unidades\n\n",
        
        "### 🧵 Lógica de Construção\n\n",
        rule_description(), "\n\n",
        "A regra de ligação utilizada é dada por:\n\n",
        "`", rule_label(), "`\n\n",
        
        "### 📐 Recomendações Técnicas\n\n",
        "- ", ideal_n, "\n",
        "- ", ideal_lwd, "\n\n",
        
        nota_hex,
        nota_decimal,
        nota_template,
        
        "### 🎓 Créditos Acadêmicos\n\n",
        "Este aplicativo Shiny é resultado do trabalho desenvolvido por **Ivo Moreira Barbosa**, mestrando do PROFMAT.\n\n",
        "Orientação: **Fernando de Souza Bastos**.\n\n",
        "Defesa realizada no ano de **2026**."
      ),
      fragment.only = TRUE
    ))
  })
})
