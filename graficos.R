#---------------------------------- Gráficos -----------------------------------

# Carregando pacotes
library (devtools)    # Facilidade em desenvolvimento e instalação de pacotes
library (dplyr)       # Manipulação e transformação de Dados
library (lubridate)   # Manipulação e conversão de data e hora
library (ggplot2)     # Criação de Gráficos
library (plotly)      # Gráficos Interativos
library (stringr)     # Manipulação de strings
library(readr)        # Leitura e escrita de arquivos .csv
library(tidyr)        # Transformação de dados
library(scales)       # Formatação de eixos e rótulos em gráficos
library(RColorBrewer) # Paleta de cores para Gráficos

# Limpar ambiente
rm (list = ls ())

# Coleta de lixo em caso de memória não utilizada
gc ()

# Seleciona o diretório de trabalho
setwd ("C:/caminho/para/sua/pasta/com/a/serie/temporal")
getwd ()

# Verifica todos os arquivos com extensão ".csv"
arquivos <- list.files (path = ".", pattern = "\\.csv$", full.names = TRUE,
                        ignore.case = TRUE)

#----------------------- Leitura dos Estados individuais -----------------------

# Seleção do arquivo desejado
arquivo <- arquivos[1]

# Leitura dos dados
serie_temporal <- read_csv (arquivo)

#--------------------------- Tratamento dos Dados ------------------------------

# Exibe todos os nomes de colunas
print (colnames (serie_temporal))

# Verifica se há alguma coluna com nome NA
any (colnames (serie_temporal) == "NA")

# Substitui o nome "NA" por "Outras doenças"
colnames (serie_temporal)[colnames (serie_temporal) == "NA"] <- "Outros"

# Manipulação fica a critério do usuário, de acordo com os dados

#--------------------------- Criação dos Gráficos ------------------------------

# NOMES QUE ESTÃO APENAS COMO EXEMPLO --> 'serie_temporal, 'nome1', 'nome2', 'legenda'
#                                         'coluna1'

# Definindo uma paleta personalizada a partir de uma paleta existente
cores <- colorRampPalette(brewer.pal(12, "Paired"))(23)

# Gráfico de Barras
grafico_barras <-  serie_temporal %>%
  # MUDAR OS NOMES (nome1, nome2, legenda) de acordo com os dados que estão sendo
  # trabalhados
  ggplot (aes (x = nome1, y = nome2, fill = legenda)) +
  geom_bar (stat = "identity") +
  
  # Adiciona os valores em cima das barras
  # Mude o nome 'capitulo' para o que deseja
  geom_text (aes (label = capitulo), 
             vjust = -0.5,
             size = 4) +

  # Adiciona títulos e rótulos aos eixos do gráfico
  # Altere para os nome que deseja
  labs (title = "titulo",
        x = "nome1",
        y = "nome2",
        fill = NULL) +
  scale_fill_manual (values = cores) +
  theme_minimal () +
  
  # Centraliza o título, Remove as legendas inclinadas e move para baixo, e
  # remove as grades do fundo
  theme (
    plot.title = element_text (hjust = 0.5), # Centraliza o título
    axis.text.x = element_blank (),          # Remove os rótulos do eixo X
    legend.position = "bottom",              # Posiciona a legenda abaixo do gráfico
    panel.grid = element_blank ()) +         # Remove as grades do fundo
  
  # Formatar os números no eixo Y tirando o formato científico
  scale_y_continuous (labels = scales::comma,
                      
                      # Preencha os números conforme seus dados 
                      breaks = seq (0, max (serie_temporal$nome2), by = 100))

print (grafico_barras)

# Salvar o gráfico com dimensões adequadas
ggsave ("grafico_barras.png", plot = grafico_barras, width = 10, height = 6, dpi = 150, bg = "white")

#-------------------------------------------------------------------------------

# Gráfico de Pizza
grafico_pizza <- plot_ly (
  serie_temporal,
  labels = ~legenda, # Substitua 'legenda' de acordo com os dados
  values = ~coluna1, # Substitua 'coluna1' de acordo com os dados
  type = 'pie',
  textposition = 'inside', # Valores ficam dentro das fatias da Pizza
  marker = list (colors = cores) # Define as cores da Pizza
) %>%
  # Remove as grades, as linhas de zero e os rótulos dos eixos X e Y
  layout (xaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )

print (grafico_pizza)

#-------------------------------------------------------------------------------

# Gráfico de Linha
grafico_linha <- serie_temporal %>%
  ggplot (aes (x = nome1, y = nome2)) +
  geom_line (color = "#005f4f") +
  scale_x_continuous (breaks = seq (min (serie_temporal$nome1), max (serie_temporal$nome1), 1)) +
  scale_y_continuous (labels = scales::percent_format (scale = 1)) +
  
  # Marca uma linha em cada parâmetro de porcentagem
  geom_hline (yintercept = seq (10, 30, by = 10), linetype = "dashed", color = "#5CA4D5") +
  geom_point (color = "#005f4f") +
  theme_minimal () +
  theme (panel.grid = element_blank(),
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         axis.title.x = element_blank(),
         axis.title.y = element_blank()) + 
  
  # Exibe o percentual em cima de cada ponto de marcação
  geom_text(aes(label = scales::percent (PERCENTUAL / 100, accuracy = 0.1)), vjust = -0.5)

print (grafico_linha)

# Salvar o gráfico com dimensões adequadas
ggsave ("grafico_linha.png", plot = grafico_linha, width = 10, height = 6, dpi = 150, bg = "white")
