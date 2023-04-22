# Análise de respostas de pesquisa para o trabalho de conclusão de curso de Data Science
# e Analytics da USP/Esalq, turma 211
# Autor: Paulo Henrique Real Leite
# Data: 4/4/2023

##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("tidytext","ggplot2","dplyr","tibble","wordcloud","stringr",
             "SnowballC","readxl","tm","stopwords","textdata","data.table",
             "reshape2","plotly","lexiconPT","tidyverse")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)

# Carrega biblioteca de funções personalizadas
source("Funcoes.R")

# Carrega arquivo Excel e renomeia colunas para nomes menores
Respostas <- read_excel("Respostas.xlsx")

Respostas <- rename(Respostas,
        "Start" = "Start time",                                                   
        "Duracao" = "Completion time",                                              
        "Setor1" = "Setor econômico da empresa",                                   
        "Depto1" = "Departamento",                                                 
        "Posicao1" = "Grau hierárquico",                                             
        "Velocidade" = "Qual foi a velocidade de resposta da empresa perante a crise?",
        "ImpNeg" = "Quais foram os impactos negativos imediatos?" ,                
        "Oportunidades" = "Quais foram as oportunidades encontradas?" ,                   
        "ConsPassado" = "Considerações sobre o passado"   ,                             
        "Setor2" = "Setor econômico da empresa2" ,                                 
        "Depto2" = "Departamento2"     ,                                           
        "Posicao2" = "Grau hierárquico2"    ,                                        
        "Processos" = "Processos de negócio"  ,                                       
        "ConsPresente" = "Considerações sobre o presente")

# Monta Dataframe limpo para Text Mining
# Retira colunas Name e Email que não são usadas
# Remove acentos ASCII
# Troca [, ; e \] por espaço para as palavras não ficarem grudadas
dfRespostas_limpo <- select(Respostas, everything(), -Name, -Email)

dfRespostas_limpo <- as.data.frame(apply(dfRespostas_limpo,c(1,2), subs_sinais))
                   
# Análise 1: As pessoas mudaram de empresa, área ou posição?
# Faz uma comparação simples para avaliar se as pessoas tiveram mudanças de 
# carreira no período. Gera um gráfico para demonstrar o percentual de mudanças
# em setor, departamento e posição e outro para mostrar o impacto mais pessoal
# relativo a mudança de grau hierárquico

# Monta o relatório em texto
a <- nrow(dfRespostas_limpo)
b <- nrow(filter(dfRespostas_limpo, Setor1 != Setor2, !is.na(Setor1), !is.na(Setor2)))
c <- nrow(filter(dfRespostas_limpo, Depto1 != Depto2))
d <- nrow(filter(dfRespostas_limpo, Posicao1 != Posicao2))

cat("Dos ",
  a, "entrevistados,",
  b, "(", 
  round(b/a*100,2), "%) mudaram de setor econômico,",
  c, "(",
  round(c/a*100), "%) mudaram de departamento e",
  d, "(",
  round(d/a*100), "%) mudaram de posição hierárquica.") 



# Faz o gráfico geral das mudanças
dfMudancas <- data.frame(Mudanças = c("Setor","Departamento","Posição"),
                    Valor = c(b,c,d),
                    Perc = c(b/a*100,c/a*100,d/a*100))

plotMudancas <- ggplot(dfMudancas, aes(x=Mudanças, y=Valor)) + geom_bar(stat='identity') + geom_text(aes(label = paste(round(Perc), '%')), vjust=-0.5)

plotMudancas

# Faz o gráfico com as mudanças agrupadas
tst1 <- select(Respostas, posic = "Posicao1")
tst1 <- mutate(tst1, momento = "Início da pandemia")
tst2 <- select(Respostas, posic = "Posicao2")
tst2 <- mutate(tst2, momento = "Momento atual")
tst3 <- data.frame(table(union_all(tst1,tst2)))

tst3 %>% 
  plot_ly(
    x = ~posic,
    y = ~Freq,
    type = "bar",
    name = ~momento
  ) %>%
 layout(title = "Mudanças de hierarquia",
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))



# Faz o gráfico de subiu ou desceu
tst4 <- select(Respostas, posic1 = "Posicao1", posic2 = "Posicao2") 
tst4 <- tst4 %>%
  mutate(posic1=case_when(
    posic1 == "Presidência / Direção" ~ 3,
    posic1 == "Gerência / Coordenação / Supervisão" ~ 2,
    posic1 == "Contribuidor individual" ~ 1,
    posic1 == NA ~ 0
  )) %>%
  mutate(posic2=case_when(
    posic2 == "Presidência / Direção" ~ 3,
    posic2 == "Gerência / Coordenação / Supervisão" ~ 2,
    posic2 == "Contribuidor individual" ~ 1,
    posic2 == NA ~ 0
  )) %>%
  mutate(mudança=case_when(
    posic1 == posic2 ~ "manteve",
    posic1 > posic2 ~ "desceu",
    posic2 > posic1 ~ "subiu"
  ))%>%
  mutate(mudança=if_else(is.na(mudança),"não informado",mudança
  ))

tst4 <- filter(tst4, mudança != "manteve")

tst4 %>%
  plot_ly(x = ~mudança, showlegend = F) %>% 
  add_histogram() %>%
  group_by(mudança) %>%
  summarise(n = n()) %>%
  add_text(
    text = ~scales::comma(n), y = ~n, 
    textposition = "top middle", 
    cliponaxis = FALSE
  )



# As empresas sofreram os mesmos desafios e viram as mesmas oportunidades?

#------------
# Preciso rodar as rotinas de limpeza para várias colunas
# então criei a função tratadataframe que já faz os tratamentos necessários
# e retorna a tabela com os termos contados
# Falta fazer stemming (radicais das palavras) e topic modeling (junção de termos como "Power BI") 
#------------
# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# Avaliando coluna Impactos Negativos
dfRespImpactNeg <- select(dfRespostas_limpo,doc_id=ID,text=ImpNeg)
dfRespImpactNegTermos <- tratadataframe(dfRespImpactNeg)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespImpactNegTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                         colors=pal, min.freq = 2, 
                                         scale=c(1.5,0.9), use.r.layout = F))

# Avaliando coluna Oportunidades Encontradas
dfRespOport <- select(dfRespostas_limpo,doc_id=ID,text=Oportunidades)
dfRespOportTermos <- tratadataframe(dfRespOport)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespOportTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                     colors=pal, min.freq = 2, 
                                     scale=c(1.5,0.9), use.r.layout = F))

# Avaliando coluna Considerações do passado
dfRespConsPassado <- select(dfRespostas_limpo,doc_id=ID,text=ConsPassado)
dfRespConsPassadoTermos <- tratadataframe(dfRespConsPassado)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespConsPassadoTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                           colors=pal, min.freq = 2, 
                                           scale=c(1.5,0.9), use.r.layout = F))

# Avaliando coluna Processos
dfRespProcessos <- select(dfRespostas_limpo,doc_id=ID,text=Processos)
dfRespProcessosTermos <- tratadataframe(dfRespProcessos)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespProcessosTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                         colors=pal, min.freq = 2, 
                                         scale=c(1.5,0.9), use.r.layout = F))

# Avaliando coluna Tecnologias
dfRespTecnologias <- select(dfRespostas_limpo,doc_id=ID,text=Tecnologias)
dfRespTecnologiasTermos <- tratadataframe(dfRespTecnologias)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespTecnologiasTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                           colors=pal, min.freq = 2, 
                                           scale=c(1.5,0.9), use.r.layout = F))

# Avaliando coluna Considerações do presente
dfRespConsPresente <- select(dfRespostas_limpo,doc_id=ID,text=ConsPresente)
dfRespConsPresenteTermos <- tratadataframe(dfRespConsPresente)
dev.off() # pra limpar o plot, evita problemas de exibição comuns
dfRespConsPresenteTermos %>% with(wordcloud(text, n, random.order = FALSE, max.words = 200, 
                                            colors=pal, min.freq = 2, 
                                            scale=c(1.5,0.9), use.r.layout = F))

# Quais processos de negócio e tecnologias permitiram uma reação rápida e assertiva perante a crise?
# preparar um df com apenas a coluna de impactos negativos, depois fazer isso pras outras colunas

# Aplicar a função no data frame para obter todas as palavras em um vetor
vtImpNegativos <- unir_palavras(select(Respostas, palavras = ImpNeg))
vtProcessos <- unir_palavras(select(Respostas, palavras = Processos))
vtTecnologias <- unir_palavras(select(Respostas, palavras = Tecnologias))

# Historigramas para visualização, mas não entram no relatório final porque
# vou fazer a matriz de avaliação
histograma_frequencia(vtImpNegativos)
histograma_frequencia(vtProcessos)
histograma_frequencia(vtTecnologias)

# Matriz de avaliação, ou mapa de calor, com os principais processos e tecnologias
# identificados por setor econômico
mapa_de_calor(select(Respostas, Setor = Setor2, Processos))
mapa_de_calor(select(Respostas, Setor = Setor2, Tecnologias))

# Análise de sentimentos nas considerações do passado e do presente
# Usei LexiconPT, mas algumas palavras não existem ou estão com polaridade "errada"
# Então criei meu próprio: tabela_sentimentos (termo, polaridade)
# Dessa forma uso o LexiconPT como base mas posso editar as polaridades

dev.off() # pra limpar o plot, evita problemas de exibição comuns
nuvem_de_sentimentos(dfRespConsPassadoTermos)
dev.off()
nuvem_de_sentimentos(dfRespConsPresenteTermos)


# Fazer análise TF-IDF comparando as considerações do passado e do presente
# Faz um union das considerações do passado e do presente
dfConsPass_x_Pres <- 
    rbind(
      mutate(dfRespConsPassadoTermos,origem = "passado"),
      mutate(dfRespConsPresenteTermos,origem = "presente")
    )

# Faz o TF-IDF
consPass_x_Pres_tf_idf <- dfConsPass_x_Pres %>% bind_tf_idf(text, origem, n)

#Realiza gráfico com palavras mais importantes
tf_idf_graph <- consPass_x_Pres_tf_idf %>%
  group_by(origem) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(text = reorder(text, tf_idf)) %>%
  filter(n>1) # coloquei esse filtro pq tava gerando muitas palavras aparecendo apenas 1 vez, comum em dataset pequeno

tf_idf_graph %>% ggplot(aes(tf_idf, text, fill = origem)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~origem, ncol = 2, scales = "free")







