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
  # Parâmetros internos fixos do app
  # --------------------------------------------------
  fixed_params <- reactive({
    switch(
      input$figura,
      "Círculo" = list(
        r = 5
      ),
      "Elipse" = list(
        a = 4,
        b = 2.5
      ),
      "Triângulo" = list(
        side = 5
      ),
      "Cardioide" = list(
        r = 4,
        rotate = 0
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
        show_center = TRUE,
        center_col = "black",
        cex_center = 0.9
      )
    )
  })
  
  # --------------------------------------------------
  # Número efetivo de pregos
  # --------------------------------------------------
  effective_n <- reactive({
    req(input$n, input$figura)
    
    n0 <- as.integer(input$n)
    
    if (input$figura == "Hexaflower") {
      n1 <- max(6L, as.integer(round(n0 / 6) * 6))
      return(n1)
    }
    
    max(3L, n0)
  })
  
  # --------------------------------------------------
  # Ajuste dinâmico de n para Hexaflower
  # --------------------------------------------------
  observe({
    req(input$figura, input$n)
    
    n_eff <- effective_n()
    min_n <- if (input$figura == "Hexaflower") 6 else 3
    
    updateSliderInput(
      session,
      "n",
      min = min_n,
      max = 240,
      value = n_eff
    )
  })
  
  # --------------------------------------------------
  # Ajuste dinâmico do salto k
  # --------------------------------------------------
  observe({
    req(effective_n())
    
    k_max <- max(1L, effective_n() - 1L)
    
    updateSliderInput(
      session,
      "k",
      min = 1,
      max = k_max,
      value = min(input$k, k_max)
    )
  })
  
  # --------------------------------------------------
  # Função auxiliar:
  # padroniza a saída das funções do pacote
  # --------------------------------------------------
  normalize_art_result <- function(res, k) {
    
    req(res)
    req(res$pregos)
    req(res$conexoes)
    req(res$comprimento_total)
    
    if (!"indice" %in% names(res$pregos)) {
      res$pregos$indice <- seq_len(nrow(res$pregos))
    }
    
    con <- res$conexoes
    
    if (!"indice_conexao" %in% names(con)) {
      con$indice_conexao <- seq_len(nrow(con))
    }
    
    if (!"x_inicial" %in% names(con) && "x1" %in% names(con)) con$x_inicial <- con$x1
    if (!"y_inicial" %in% names(con) && "y1" %in% names(con)) con$y_inicial <- con$y1
    if (!"x_final" %in% names(con) && "x2" %in% names(con)) con$x_final <- con$x2
    if (!"y_final" %in% names(con) && "y2" %in% names(con)) con$y_final <- con$y2
    
    if (!"prego_inicial" %in% names(con) && "i" %in% names(con)) {
      con$prego_inicial <- con$i
    }
    
    if (!"prego_final" %in% names(con) && "j" %in% names(con)) {
      con$prego_final <- con$j
    }
    
    if (!"prego_inicial" %in% names(con)) {
      con$prego_inicial <- seq_len(nrow(con))
    }
    
    if (!"prego_final" %in% names(con)) {
      con$prego_final <- ((seq_len(nrow(con)) + k - 1L) %% nrow(con)) + 1L
    }
    
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
    
    res$conexoes <- con
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
      "Elipse" = "j = (i + k - 1) %% n + 1",
      "Triângulo" = "j = (i + k - 1) %% n + 1",
      "Cardioide" = "j = ((k * (i - 1)) %% n) + 1",
      "Hexaflower" = paste(
        "Bloco 1: contorno externo;",
        "Blocos 2 e 3: j = (i + k - 1) %% n + 1;",
        "Bloco 4: vértices externos -> centro."
      ),
      "Radial" = "Em cada módulo: j = (i + k - 1) %% n + 1"
    )
  })
  
  rule_description <- reactive({
    req(input$figura)
    
    switch(
      input$figura,
      "Círculo" = "Os pregos são distribuídos uniformemente sobre uma circunferência e cada prego é conectado ao prego `k` posições à frente.",
      "Elipse" = "Os pregos são distribuídos uniformemente sobre o contorno de uma elipse e cada prego é conectado ao prego `k` posições à frente.",
      "Triângulo" = "Os pregos são distribuídos uniformemente ao longo do contorno de um triângulo equilátero e cada prego é conectado ao prego `k` posições à frente.",
      "Cardioide" = "Os pregos são distribuídos uniformemente sobre uma circunferência e cada prego é conectado por uma regra multiplicativa modular. Para `k = 2`, surge a figura clássica associada ao efeito de cardioide.",
      "Hexaflower" = "Os pregos são distribuídos em três circuitos hexagonais concêntricos e um centro. As conexões combinam contorno externo, saltos entre circuitos e ligações dos vértices externos ao centro.",
      "Radial" = "A figura é composta por módulos triangulares rotacionados em torno do centro. Em cada módulo, os pregos são distribuídos ao longo do contorno triangular e conectados localmente com salto `k`."
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
      "Elipse" = paste0(
        "- **Semi-eixo maior (a):** ", pars$a, "\n",
        "- **Semi-eixo menor (b):** ", pars$b, "\n"
      ),
      "Triângulo" = paste0(
        "- **Comprimento do lado do triângulo:** ", pars$side, "\n"
      ),
      "Cardioide" = paste0(
        "- **Raio utilizado:** ", pars$r, "\n",
        "- **Rotação aplicada:** ", sprintf("%.2f", pars$rotate), " rad\n"
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
      )
    )
  })
  
  # --------------------------------------------------
  # Função auxiliar:
  # constrói os dados sem plot
  # --------------------------------------------------
  build_art_data <- function() {
    
    n_eff <- effective_n()
    pars <- fixed_params()
    
    if (input$figura == "Círculo") {
      res <- stcircle(
        n = n_eff,
        k = input$k,
        r = pars$r,
        col = input$col,
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Elipse") {
      res <- stellipse(
        n = n_eff,
        k = input$k,
        a = pars$a,
        b = pars$b,
        col = input$col,
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Triângulo") {
      res <- sttriangle(
        n = n_eff,
        k = input$k,
        side = pars$side,
        col = input$col,
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Cardioide") {
      res <- stcardioid(
        n = n_eff,
        k = input$k,
        r = pars$r,
        col = input$col,
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        rotate = pars$rotate,
        verbose = FALSE
      )
      
    } else if (input$figura == "Hexaflower") {
      res <- sthexaflower(
        n = n_eff,
        k = input$k,
        r = pars$r,
        scale_mid = pars$scale_mid,
        scale_inner = pars$scale_inner,
        offset_mid = pars$offset_mid,
        offset_inner = pars$offset_inner,
        col = rep(input$col, 6),
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Radial") {
      res <- stradial(
        n = n_eff,
        k = input$k,
        m = pars$m,
        r = pars$r,
        spread = pars$spread,
        col = rep(input$col, pars$m),
        lwd = input$lwd,
        plot = FALSE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        show_center = pars$show_center,
        center_col = pars$center_col,
        cex_center = pars$cex_center,
        verbose = FALSE
      )
    }
    
    normalize_art_result(res, k = input$k)
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
    
    n_eff <- effective_n()
    pars <- fixed_params()
    
    if (input$figura == "Círculo") {
      stcircle(
        n = n_eff,
        k = input$k,
        r = pars$r,
        col = input$col,
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Elipse") {
      stellipse(
        n = n_eff,
        k = input$k,
        a = pars$a,
        b = pars$b,
        col = input$col,
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Triângulo") {
      sttriangle(
        n = n_eff,
        k = input$k,
        side = pars$side,
        col = input$col,
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Cardioide") {
      stcardioid(
        n = n_eff,
        k = input$k,
        r = pars$r,
        col = input$col,
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        rotate = pars$rotate,
        verbose = FALSE
      )
      
    } else if (input$figura == "Hexaflower") {
      sthexaflower(
        n = n_eff,
        k = input$k,
        r = pars$r,
        scale_mid = pars$scale_mid,
        scale_inner = pars$scale_inner,
        offset_mid = pars$offset_mid,
        offset_inner = pars$offset_inner,
        col = rep(input$col, 6),
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        verbose = FALSE
      )
      
    } else if (input$figura == "Radial") {
      stradial(
        n = n_eff,
        k = input$k,
        m = pars$m,
        r = pars$r,
        spread = pars$spread,
        col = rep(input$col, pars$m),
        lwd = input$lwd,
        plot = TRUE,
        show_points = input$show_points,
        show_labels = input$show_labels,
        show_center = pars$show_center,
        center_col = pars$center_col,
        cex_center = pars$cex_center,
        verbose = FALSE
      )
    }
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
  output$auditoria_texto <- renderText({
    
    res <- art_data()
    req(res$conexoes)
    
    con <- res$conexoes
    
    conexoes_txt <- if (
      input$figura == "Radial" &&
      all(c("grupo", "indice_local_inicial", "indice_local_final") %in% names(con))
    ) {
      
      paste0(
        "Módulo ", con$grupo,
        ": Prego ", con$indice_local_inicial,
        " -> Prego ", con$indice_local_final
      )
      
    } else if (input$figura == "Hexaflower" && "bloco" %in% names(con)) {
      
      paste0(
        "[", con$bloco, "] ",
        "Prego ", con$prego_inicial,
        " -> Prego ", con$prego_final
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
    
    paste(
      "Auditoria da Construção",
      "",
      paste0("Figura: ", input$figura),
      paste0("Número de pregos efetivo (n): ", effective_n()),
      paste0("Parâmetro k: ", input$k),
      paste0("Comprimento total de barbante: ",
             sprintf("%.2f", res$comprimento_total), " unidades"),
      "",
      "Regra de ligação:",
      rule_label(),
      "",
      "Ligações realizadas:",
      paste(conexoes_txt_show, collapse = "\n"),
      sep = "\n"
    )
  })
  
  # --------------------------------------------------
  # Auditoria detalhada no console
  # --------------------------------------------------
  observeEvent(
    {
      list(input$figura, input$n, input$k, input$col, input$lwd,
           input$show_points, input$show_labels, input$verbose)
    },
    {
      if (isTRUE(input$verbose)) {
        cat("\n============================\n")
        cat(output$auditoria_texto())
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
      
      "Elipse" = "Para elipses recomenda-se utilizar entre 60 e 120 pregos para melhor percepção da curvatura.",
      
      "Triângulo" = if (n_eff < 24) {
        "Para triângulos recomenda-se utilizar ao menos 24 pregos para melhor distribuição ao longo do contorno."
      } else if (n_eff >= 24 && n_eff <= 90) {
        "O número de pregos está adequado para uma construção triangular equilibrada."
      } else {
        "Número elevado de pregos produz alta densidade de segmentos e uma malha triangular mais complexa."
      },
      
      "Cardioide" = if (n_eff < 60) {
        "Para padrões tipo cardioide recomenda-se utilizar pelo menos 80 pregos para uma envoltória visual mais suave."
      } else {
        "O número de pregos está adequado para boa percepção da envoltória produzida pela regra multiplicativa."
      },
      
      "Hexaflower" = "Para Hexaflower, recomenda-se usar valores de `n` múltiplos de 6 com pelo menos 18 ou 24 pregos por circuito para melhor simetria.",
      
      "Radial" = "Para figuras radiais, a escolha de `n` deve equilibrar densidade visual e legibilidade dos módulos."
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
    
    HTML(markdown::markdownToHTML(
      text = paste0(
        "### 🔎 Especificação da Figura Gerada\n\n",
        "- **Figura construída:** ", input$figura, "\n",
        "- **Número de pregos efetivo (n):** ", n_eff, "\n",
        "- **Parâmetro k:** ", input$k, "\n",
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
        
        "### 🎓 Créditos Acadêmicos\n\n",
        "Este aplicativo Shiny é resultado do trabalho desenvolvido por **Ivo Moreira Barbosa**, mestrando do PROFMAT.\n\n",
        "Orientação: **Fernando de Souza Bastos**.\n\n",
        "Defesa realizada no ano de **2026**."
      ),
      fragment.only = TRUE
    ))
  })
})