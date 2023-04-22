
  # Cria dfMatrizComp com base nos dados originais
  dfMatrizComp <- select(Respostas, Setor2, Processos)
  
  
  # cria df com agrupamento de setor e processos
  dfMatrizComp <- dfMatrizComp %>%
    separate_rows(Processos, sep = ";") %>%
    group_by(Setor2, Processos) %>%
    summarise(n = n())
  
  # Tirar valores não informados e com apenas 1 ocorrência, para deixar o plot 
  # mais limpo, apenas com dados mais relevantes
  dfMatrizComp <- filter(dfMatrizComp, 
                         !is.na(Setor2), 
                         !is.na(Processos),
                         Processos != "",
                         n > 1
                        )
  
  # Calcula e insere uma linha de total
  dfMatrizComp2 <- dfMatrizComp %>%
    group_by(Processos) %>%
    summarise(n = sum(n))
  
  dfMatrizComp2 <- mutate(dfMatrizComp2, Setor2 = "* TOTAL *")
  dfmatrizall <- union_all(dfMatrizComp, dfMatrizComp2)
  
  # Monta gráfico de matriz de comparação
  # consultar paletas de cores válidas com comando RColorBrewer::brewer.pal.info 
  fig <- plot_ly(dfmatrizall,
    x = ~Processos, y = ~Setor2,
    z = ~n, 
    type = "heatmap",
    colors = "Spectral"
  ) %>%
    add_annotations(
      data = dfmatrizall,
      x = ~Processos,
      y = ~Setor2,
      text = ~n,
      xref = 'x', 
      yref = 'y', 
      showarrow = FALSE, 
      font=list(color='black')
      ) %>%
    layout(title = "Setor vs Processos",
           xaxis = list(title = "Processos"),
           yaxis = list(title = "Setor"))

  fig



