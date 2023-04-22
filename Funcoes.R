# Substitui vírgulas e outros caracteres por espaço
subs_sinais <- function(x) {
  x <- gsub("[,;\\\\////]", " ", x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  return(x)
}

#Recebe um dataframe com ID e texto, faz os tratamentos do corpus com TM_MAP e devolve um dataframe tratado

tratadataframe <- function(df) {
    # Criar um objeto Corpus a partir do dataframe recebido
    
  dfs <- DataframeSource(df)
  corpusDfs <- Corpus(dfs)
  inspect(corpusDfs)
  meta(corpusDfs)
  
  corpusDfs <- tm_map(corpusDfs, content_transformer(subs_sinais))
  
  # Remover pontuações, números, espaços extras e transformar em letras minúsculas
  corpusDfs <- tm_map(corpusDfs, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpusDfs <- tm_map(corpusDfs, tolower)
  corpusDfs <- tm_map(corpusDfs, stripWhitespace)
  corpusDfs <- tm_map(corpusDfs, removeNumbers)
  
  # Remover stop words em português
  stop_words_pt <- stopwords("pt")
  corpusDfs <- tm_map(corpusDfs, removeWords, stop_words_pt)
  
  # Tokenizar o Corpus - não tem opção de ngrams no pacote TM
  corpusDfs <- tm_map(corpusDfs, Boost_tokenizer)
  
  
  # # Criar uma matriz de termos e documentos (Term-Document Matrix)
  # tdm <- TermDocumentMatrix(corpusDfs)
  # inspect(tdm)
  # findFreqTerms(tdm, 5)
  # findAssocs(tdm, "receita", 0.8)
  # 
  # # Criar uma matriz de termos e documentos (Document-term Matrix)
  # dtm <- DocumentTermMatrix(corpusDfs)
  # inspect(dtm)
  # findFreqTerms(dtm, 5)
  # findAssocs(dtm, "aumento", 0.8)
  
  # Converter os termos do corpus em uma tabela
  df <- data.frame(text = sapply(corpusDfs, paste, collapse = " "), stringsAsFactors = FALSE)
  
  # Resume a tabela com os termos contados
  df <- df %>% count(text, sort = TRUE)
  
  return (df)
}


# Monta tabela de sentimentos personalizada e faz 
# comando pra testar polaridade da palavra no lexiconPT::get_word_sentiment("virus")
termos <- c("amor", "ótimo", "familiar", "sustentavel", "corretamente",
            "neutro", "indiferente", "morno", "regular")
polaridades <- c(1, 1, 1, 1, 1,
                 0, 0, 0, 0)
tabela_sentimentos <- data.frame(termo = termos, polaridade = polaridades)

tabela_sentimentos_tmp <- anti_join(select(oplexicon_v3.0, termo = term, polaridade = polarity),tabela_sentimentos,by = join_by(termo))
tabela_sentimentos <- union(tabela_sentimentos, tabela_sentimentos_tmp )
rm(tabela_sentimentos_tmp)

# Função que recebe dataframe com termos e devolve um relatório 
# de sentimento líquido e uma nuvem de palavras com os termos e sentimentos
# colocar como parametro dataframe já com os termos tokenizados e agrupados
# coluna text (termo), n (contagem de termo)
nuvem_de_sentimentos <- function(dftemp) {
  
  dftemp <- rename(dftemp, termo = text)
  dftempsent <- inner_join(dftemp, tabela_sentimentos)
  dftempsent <- mutate(dftempsent, 
                   sentimento = ifelse(polaridade > 0, "positivo",
                                       ifelse(polaridade == 0,"neutro","negativo")),
                   notasentimento = polaridade * n
  )
  
  #palavras_avaliadas <- as.numeric(count(dftempsent))
  palavras_avaliadas <- nrow(dftempsent)
  polaridade_liquida <- sum(dftempsent$notasentimento)
  
  pulalinha()
  
  cat(" Foram avaliadas", 
      palavras_avaliadas, "palavras. A soma das polaridades foi de",
      polaridade_liquida, ", portanto o texto tem sentimento líquido",
      ifelse(polaridade_liquida > 0, "positivo",
             ifelse(polaridade_liquida == 0,"neutro","negativo"))
  )
  
  palavrasdesconhecidas <- anti_join(dftemp, tabela_sentimentos, by = join_by(termo))
  palavrasdesconhecidas <- filter(palavrasdesconhecidas, n > 1)
  
  pulalinha()
  
  print("As palavras a seguir não aparecem no léxico")
  print(palavrasdesconhecidas$termo)
  
  pulalinha()
  
  dftempsent %>%
    acast(termo ~ sentimento, value.var = "n", fill = 0, fun.aggregate = sum) %>%
    comparison.cloud(colors = c("red", "grey", "green"),
                     scale=c(1,1),
                     max.words = 100,
                     use.r.layout = FALSE)
}

# Função para transformar todas as palavras em um único vetor
# o data frame tem que ter uma coluna "palavras" com as palavras separadas por ponto e virgula
# usar de base o df Respostas que não tem nenhum tratamento
unir_palavras <- function(df) {
  vtTodasPalavras <- unlist(strsplit(paste(df$palavras, collapse = ";"), ";"))
  return(vtTodasPalavras)
}


# Função que monta o historigrama mostrando a frequencia das respostas
# tentei com barplot, ggplot2 e plot_ly, só este último funcionou razoavelmente
# Função que pega um vetor com uma lista de palavras e monta um gráfico de frequência
histograma_frequencia <- function(vtTodasPalavras) {
  
  # Criar um data frame com a contagem de frequência das palavras do vetor recebido
  df_frequencia <- data.frame(palavra = names(table(vtTodasPalavras)),
                              frequencia = as.numeric(table(vtTodasPalavras)))
  
  # Apagar registros em branco e truncar o texto em 20 caracteres
  df_frequencia <- filter(df_frequencia, palavra != "")
  df_frequencia <- df_frequencia %>% mutate(palavra = paste(substr(palavra, 1, 20),"..."))
  
  fig <- plot_ly(df_frequencia, x = ~palavra, y = ~frequencia, type = 'bar',
                 text = ~frequencia, textposition = 'auto',
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)', width = 1.5)),
                 legendgroup = ~frequencia)
  
  fig <- fig %>% layout(title = "Frequência de respostas",
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
                        
return(fig)
}  

# Recebe dataframe com 2 colunas e transforma em mapa de calor
mapa_de_calor <- function(df) {
  
  # Cria dfMatrizComp com base nos dados originais
  dfMatrizComp <- select(df, Coluna1 = 1, Coluna2 = 2)
  strNomeCol1 <- colnames(df[1])
  strNomeCol2 <- colnames(df[2])
  
  # cria df com agrupamento de setor e processos
  dfMatrizComp <- dfMatrizComp %>%
    separate_rows(Coluna2, sep = ";") %>%
    group_by(Coluna1, Coluna2) %>%
    summarise(n = n())
  
  # Tirar valores não informados e com apenas 1 ocorrência, para deixar o plot 
  # mais limpo, apenas com dados mais relevantes
  dfMatrizComp <- filter(dfMatrizComp, 
                         !is.na(Coluna1), 
                         !is.na(Coluna2),
                         Coluna2 != "",
                         n > 1
  )
  
  # Calcula e insere uma linha de total
  dfMatrizComp2 <- dfMatrizComp %>%
    group_by(Coluna2) %>%
    summarise(n = sum(n))
  
  dfMatrizComp2 <- mutate(dfMatrizComp2, Coluna1 = "* TOTAL *")
  dfMatrizCompAll <- union_all(dfMatrizComp, dfMatrizComp2)
  
  # Monta gráfico de matriz de comparação
  # consultar paletas de cores válidas com comando RColorBrewer::brewer.pal.info 
  fig <- plot_ly(dfMatrizCompAll,
                 x = ~Coluna2, y = ~Coluna1,
                 z = ~n, 
                 type = "heatmap",
                 colors = "Spectral"
  ) %>%
    add_annotations(
      data = dfMatrizCompAll,
      x = ~Coluna2,
      y = ~Coluna1,
      text = ~n,
      xref = 'x', 
      yref = 'y', 
      showarrow = FALSE, 
      font=list(color='black')
    ) %>%
    layout(title = paste(strNomeCol1,"X",strNomeCol2),
           xaxis = list(title = strNomeCol2),
           yaxis = list(title = strNomeCol1))
  
  return(fig)
}


# Função pra deixar a saída dos prints mais bonitos e legíveis no console
pulalinha <- function() {
    writeLines("")
    writeLines("--------------------------------------------------------------------------------------")
    writeLines("")
    
}