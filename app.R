library(shiny)

ui <- fluidPage(
  titlePanel("Formulário Bancos Comunitários"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Nome", placeholder = "Digite seu nome"),
      textInput("email", "Email", placeholder = "Digite seu email"),
      textInput("bank", "Banco Comunitário", placeholder = "Digite o nome do banco"),
      actionButton("submit_button", "Enviar")
    ),
    mainPanel(
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$submit_button, {
    req(input$name, input$email, input$bank)
    
    # Dados do formulário
    new_entry <- data.frame(
      Nome = input$name,
      Email = input$email,
      Banco = input$bank,
      stringsAsFactors = FALSE
    )
    
    # Caminho para o arquivo CSV
    csv_path <- "dados.csv"
    
    # Adicionar dados ao CSV
    if (file.exists(csv_path)) {
      write.table(new_entry, csv_path, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      write.table(new_entry, csv_path, sep = ",", row.names = FALSE, col.names = TRUE)
    }
    
    # Operações Git
    system('git push https://MapaBancos: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@github.com/MapaBancos/MapaBancos.git')
    system("git add dados.csv")
    system('git commit -m "Atualizando dados do formulário"')
    # Push para o GitHub
    system('git push origin main')
    
    output$status <- renderText("Dados enviados com sucesso e salvos no GitHub!")
  })
}


shinyApp(ui, server)
