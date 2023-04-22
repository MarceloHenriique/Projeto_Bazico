setwd("C:/Users/Marcelo/Desktop/Bazico")
library(readr)
library(magrittr)
library(dplyr)
dados <- read_csv("vendas_de_produtos.csv")
clientes <- read_csv("clientes.csv")

#Ordenando os dados
dados_ordenados <- dados %>%
  arrange(ID_Cliente, Data)

# criando uma coluna com a proxima visita do cliente
dados_com_proxima_visita <- dados_ordenados %>%
  group_by(ID_Cliente) %>%
  mutate(Proxima_Visita = lead(Data)) %>%
  ungroup()

# criando uma coluna se o cliente retornou em 14 dias
# 0 - Não; 1-Sim
dados_com_resultado <- dados_com_proxima_visita %>%
  mutate(Retorno = ifelse(Proxima_Visita == Data, 0,ifelse(
    Proxima_Visita <= (Data + 14), 1, 0)))

#Juntando com as informacoes sobre o logradouro dos clientes e exlucindo os outros objetos criados

dados <- merge(dados_com_resultado, clientes, by = "ID_Cliente", all = FALSE, suffixes = c("_bd1", "_bd2"))
rm(clientes,dados_com_proxima_visita,dados_com_resultado,dados_ordenados)

#Substituindo NA's por 0
dados$Retorno <- ifelse(is.na(dados$Retorno), 0, dados$Retorno)

# Dividir os dados em conjunto de treinamento e teste
library(caTools)
set.seed(123)
divisao <- sample.split(dados$Retorno, SplitRatio = 0.8)
treinamento <- subset(dados, divisao == TRUE)
teste <- subset(dados, divisao == FALSE)

# Ajustar o modelo de Regressão Logística
modelo <- glm(Retorno ~ ID_Cliente+Preço_Unitário+
                Desconto+Frete+Total_do_Pedido, data = treinamento, family = binomial)

# Avaliar o desempenho do modelo no conjunto de teste
library(pROC)
previsoes <- predict(modelo, newdata = teste, type = "response")
auc <- roc(teste$Retorno, previsoes)
print(auc)

##############

# Fazer previsões para todos os clientes da base de dados
probabilidades_retorno <- predict(modelo, newdata = dados, type = "response")

# Adicionar as probabilidades ao conjunto de dados
dados_com_probabilidades <- cbind(dados, probabilidades_retorno)

# Classificar os clientes em ordem decrescente de probabilidade de recompra
dados_com_probabilidades <- dados_com_probabilidades[order(-dados_com_probabilidades$probabilidades_retorno), ]

# Ajustar a porcentagem de probabiliade de retorno em 14 dias
dados_com_probabilidades$probabilidades_retorno <- round(dados_com_probabilidades$probabilidades_retorno,4)*100

# Retirando clientes duplicados
dados_unicos <- distinct(dados_com_probabilidades, dados_com_probabilidades$ID_Cliente, .keep_all = TRUE)
dados_unicos <- dados_unicos[,-17]
View(dados_unicos)

# listando os 100 primeiros e depois os 280 primeiros
dados_unicos[1:100,c(1,16)]
dados_unicos[1:280,c(1,16)]


### ANALISE DESCRITIVA
summary(dados$Total_do_Pedido)
sort(table(dados_unicos$Cidade),decreasing = T)
sort(prop.table(table(dados_unicos$Cidade))*100,decreasing = T)
sort(prop.table(table(dados_unicos$Estado))*100,decreasing = T)




### Grafico para curva ROC
# Obter os dados da curva ROC e transformar os dados em dataframe
roc_data <- roc(dados$Retorno, probabilidades_retorno)

roc_df <- data.frame(coords(roc_data))

# Plotar a curva ROC utilizando o ggplot2
library(ggplot2)
ggplot(data = roc_df) +
  aes(x = 1 - specificity, y = sensitivity) +
  geom_path() + geom_line(col="red") +
  scale_x_reverse() +
  labs(x = "1 - Especificidade", y = "Sensibilidade", 
       title = "Curva ROC", subtitle = paste0("AUC = ", round(roc_data$auc, 2)))


### Grafico dos clientes por mes
# Criando uma nova coluna com o ano e mês de cada registro
dados$ano_mes <- format(dados$Data, "%Y-%m")

# Agregando os dados por ano_mes e contando o número de registros
dados_por_mes <- aggregate(dados$Data, by = list(ano_mes = dados$ano_mes), length)

# Separando o ano e o mês em colunas diferentes
dados_por_mes$ano <- substr(dados_por_mes$ano_mes, 1, 4)
dados_por_mes$mes <- substr(dados_por_mes$ano_mes, 6, 7)

# Ordenando os dados por ano e mês
dados_por_mes <- dados_por_mes[order(dados_por_mes$ano, dados_por_mes$mes),]

# Adicionando os rotulos para o eixo X do grafico
meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
dados_por_mes$mes <- factor(dados_por_mes$mes, levels = sprintf("%02d", 1:12), labels = meses)

# Visualizando o resultado
ggplot(dados_por_mes, aes(x = mes, y = x, fill = ano)) + 
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Mês") + ylab("Número de clientes") + 
  ggtitle("Número de compras por mês e ano") + labs(fill="Ano")

