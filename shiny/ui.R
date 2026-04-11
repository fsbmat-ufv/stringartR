##################################
# StringArt Shiny App
# UI simplificada
##################################

library(shiny)
library(shinydashboard)
library(colourpicker)

shinyUI(
  dashboardPage(
    
    skin = "green",
    
    dashboardHeader(
      title = "StringArt",
      titleWidth = 300
    ),
    
    dashboardSidebar(
      width = 300,
      
      sidebarMenu(
        
        tags$div(
          style = "text-align: center; padding: 15px;",
          img(src = "StringArt.png", width = 180)
        ),
        
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("String Art", tabName = "stringart", icon = icon("circle-nodes"))
      )
    ),
    
    dashboardBody(
      
      tags$script(HTML("
        Shiny.addCustomMessageHandler('printPlot', function(message) {
          window.print();
        });
      ")),
      
      tags$style(HTML("
        @media print {
          .main-sidebar,
          .main-header,
          .control-sidebar,
          .no-print {
            display: none !important;
          }

          .content-wrapper,
          .right-side,
          .main-footer {
            margin-left: 0 !important;
            padding: 0 !important;
          }

          .box {
            border: none !important;
            box-shadow: none !important;
          }

          #print-area {
            width: 100% !important;
            height: auto !important;
            margin: 0 auto !important;
          }

          #print-area img,
          #print-area canvas {
            display: block;
            margin: 0 auto;
          }
        }
      ")),
      
      tags$style(
        type = "text/css",
        "
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }

        .equal-box {
          height: 650px;
        }

        .equal-box .box-body {
          height: 580px;
          overflow-y: auto;
        }

        .audit-box {
          height: 500px;
          overflow-y: auto;
          overflow-x: auto;
          padding-right: 10px;
        }

        .audit-box pre {
          white-space: pre-wrap;
          word-break: break-word;
        }

        .table-box {
          height: 500px;
          overflow-y: auto;
          overflow-x: auto;
        }
        "
      ),
      
      tabItems(
        
        tabItem(
          tabName = "home",
          
          fluidRow(
            box(
              width = 12,
              title = "Bem-vindo ao StringArt",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              p("Aplicação interativa para geração de figuras em String Art."),
              p("As figuras disponíveis são: Círculo, Elipse, Triângulo, Cardioide, Hexaflower e Radial."),
              p("Escolha a figura e ajuste apenas os parâmetros principais para gerar a construção.")
            )
          )
        ),
        
        tabItem(
          tabName = "stringart",
          
          fluidRow(
            
            box(
              width = 3,
              class = "equal-box",
              title = "Parâmetros",
              status = "success",
              solidHeader = TRUE,
              
              selectInput(
                "figura",
                "Escolha a figura:",
                choices = c(
                  "Círculo",
                  "Elipse",
                  "Triângulo",
                  "Cardioide",
                  "Hexaflower",
                  "Radial"
                ),
                selected = "Círculo"
              ),
              
              sliderInput(
                "n",
                "Número de pregos:",
                min = 3, max = 240, value = 30, step = 1
              ),
              
              sliderInput(
                "k",
                "Salto / fator (k):",
                min = 1, max = 100, value = 5, step = 1
              ),
              
              colourInput(
                "col",
                "Cor da linha:",
                value = "blue"
              ),
              
              sliderInput(
                "lwd",
                "Espessura da linha:",
                min = 0.5, max = 5, value = 1.2, step = 0.1
              ),
              
              checkboxInput(
                "show_points",
                "Mostrar pregos",
                value = FALSE
              ),
              
              checkboxInput(
                "show_labels",
                "Mostrar rótulos",
                value = FALSE
              ),
              
              checkboxInput(
                "verbose",
                "Exibir auditoria detalhada no console",
                value = FALSE
              )
            ),
            
            box(
              width = 9,
              class = "equal-box",
              title = "Visualização e Auditoria",
              status = "primary",
              solidHeader = TRUE,
              
              fluidRow(
                column(
                  12,
                  align = "right",
                  class = "no-print",
                  downloadButton("download_png", "Baixar PNG"),
                  downloadButton("download_hd", "Baixar Alta Resolução"),
                  downloadButton("download_pdf", "Baixar PDF"),
                  actionButton("print_plot", "Imprimir")
                )
              ),
              
              br(),
              
              tabsetPanel(
                id = "abas_saida",
                
                tabPanel(
                  "Figura",
                  br(),
                  div(
                    id = "print-area",
                    plotOutput("grafico", height = "520px")
                  )
                ),
                
                tabPanel(
                  "Auditoria",
                  br(),
                  div(
                    class = "audit-box",
                    verbatimTextOutput("auditoria_texto")
                  )
                ),
                
                tabPanel(
                  "Tabela de conexões",
                  br(),
                  div(
                    class = "table-box",
                    DT::dataTableOutput("tabela_conexoes")
                  )
                )
              )
            )
          ),
          
          fluidRow(
            box(
              width = 12,
              title = "Resumo Técnico da Construção",
              status = "info",
              solidHeader = TRUE,
              htmlOutput("resumo_md")
            )
          )
        )
      )
    )
  )
)