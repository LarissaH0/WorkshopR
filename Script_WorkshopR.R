# ----------------------------------------------------------------------- #
#                               Workshop R                                #
#                        H0 Consultoria + DevGirls                        #
#                         Larissa Bueno Fernandes                         #
#                               26/01/2019                                #
# ----------------------------------------------------------------------- #

options(OutDec = ",", scipen = 100, digits = 3) # Evitar formato científico

# Instalar pacotes --------------------------------------------------------

install.packages("data.table") # Leitura e manipulação de grandes BDs
install.packages("dplyr")      # Manipulação
install.packages("tidyr")      # Manipulação
install.packages("magrittr")   # Manipulação
install.packages("ggplot2")    # Gráficos
install.packages("lubridate")  # Manipulação de datas
install.packages("MASS")       # Regressão logística ordinal (não carregar)

# Carregar pacotes --------------------------------------------------------

library(data.table) # Leitura e manipulação de grandes BDs
library(dplyr)      # Manipulação
library(tidyr)      # Manipulação
library(magrittr)   # Manipulação
library(ggplot2)    # Gráficos
library(lubridate)  # Manipulação de datas

# Definir diretório -------------------------------------------------------

getwd()                   # Verifica o diretório corrente
setwd(".../WorkshopR/BD") # Define o diretório
list.files()              # Lista todos os arquivos do diretório corrente

arquivos <- list.files(pattern = ".csv") # Seleciona apenas os que terminam com .csv
arquivos

# Ler dados ---------------------------------------------------------------

# Pedidos
df_ped <- fread("olist_orders_dataset.csv",encoding = "UTF-8")
glimpse(df_ped)

# Itens
df_itens <- fread("olist_order_items_dataset.csv",encoding = "UTF-8")
glimpse(df_itens)

# Avaliação
df_aval <- fread("olist_order_reviews_dataset.csv",encoding= "UTF-8")
glimpse(df_aval)


# Estruturação ------------------------------------------------------------

# Juntar datasets

df_itens %>%
  group_by(order_id) %>%
  summarise(order_product_value = sum(price),
            order_freight_value = sum(freight_value),
            order_itens_qty = length(product_id),
            order_sellers_qty = length(unique(seller_id))) %>%
  right_join(df_ped) %>%
  left_join(df_aval) -> df

# Análise exploratória ----------------------------------------------------

glimpse(df)

# 1) Como se comporta a avaliação dos consumidores? -----------------------

df %>%
  ggplot(aes(x = review_score)) +
  geom_bar(tat= "count") +
  labs(x = "Escore de satisfação", y = "Frequência absoluta")

df %>%
  group_by(review_score) %>%
  summarise(freq = n(),
            porc = 100 * freq / nrow(df))

# 2) Quantos itens são comprados online diariamente? ------------

df %>%
  mutate(order_approved_at = as.Date(substr(order_approved_at,1,10))) %>%
  ggplot(aes(x = order_approved_at)) +
  geom_line(stat = "count") +
  labs(x = "Data da aprovação da compra", y = "Número de itens comprados")

# 3) Quanto é gasto em compras online mensalmente? --------------

df %>%
  mutate(mes_compra = substr(order_approved_at,1,10) %>%
           as.Date() %>%
           format("%m-%y") %>%
           parse_date_time("my")) %>%
  group_by(mes_compra)%>%
  summarise(gasto = sum(order_product_value, na.rm = T)) %>%
  ggplot(aes(x = mes_compra, y = gasto))+
  geom_line()+
  labs(x = "Mês da compra", y = "Valor gasto")

# 4) Como se distribui o tempo de entrega dos produtos? -------------------

df %>%
  mutate(order_delivery_time = as.Date(substr(order_delivered_customer_date,1,10)) - as.Date(substr(order_approved_at,1,10))) %>%
  ggplot(aes(x = order_delivery_time))+
  geom_histogram()+
  labs(x = "Tempo de entrega", y = "Frequência absoluta")

# Novas variáveis ---------------------------------------------------------

df %<>%
  mutate(order_estimated_delivery_time = as.Date(substr(order_estimated_delivery_date,1,10)) - as.Date(substr(order_approved_at,1,10)),
         order_delivery_time = as.Date(substr(order_delivered_customer_date,1,10)) - as.Date(substr(order_approved_at,1,10)),
         order_late = order_estimated_delivery_time < order_delivery_time,
         order_average_price = order_product_value / order_itens_qty,
         order_ratio_freight = order_freight_value / order_product_value)

# Selecionar variáveis

df %<>%
  select(-order_id, 
         -customer_id, 
         -order_status, 
         -order_purchase_timestamp, 
         -order_approved_at, 
         -order_delivered_carrier_date, 
         -order_delivered_customer_date,
         -order_estimated_delivery_date,
         -order_estimated_delivery_time,
         -review_id,
         -review_comment_title,
         -review_comment_message,
         -review_creation_date,
         -review_answer_timestamp) %>%
  na.omit()


# Relação entre as variáveis  e a avaliação -------------------------------------

# Correlação

df %>%
  gather(variavel, value, -review_score) %>%
  group_by(variavel) %>%
  summarise(corr = cor(value, review_score, method = "spearman")) %>%
  arrange(corr)

# Resumo do tempo de entrega

df %>%
  group_by(review_score) %>%
  summarise(Média = mean(order_delivery_time), 
            DP = sd(order_delivery_time),
            Mediana = median(order_delivery_time))

# Médias de todas as variáveis

df %>%
  gather(variavel, value, -review_score) %>%
  group_by(variavel, review_score) %>%
  summarise(Média = mean(value)) %>%
  spread(review_score, Média)

# Modelo de regressão linear

fit_lm <- lm(review_score ~ ., data = df)
summary(fit_lm)

# Modelo de regressão logística ordinal

fit_log <- MASS::polr(factor(review_score) ~ ., data = df, Hess = T)
summary(fit_log)
exp(coef(fit_log))

# Análise preditiva -------------------------------------------------------

# Dividir banco de dados

set.seed(123)
test_pos <- sample(1:nrow(df), nrow(df) * 0.5, replace = F)
test_df <- df[test_pos,]
train_df <- df[-test_pos,]

# Regressão

fit_lm <- lm(review_score ~ ., data = train_df)
summary(fit_lm)

pred_lm <- predict(fit_lm, test_df)

cbind(test_df$review_score, pred_lm)

RSME <- sqrt(mean((test_df$review_score - pred_lm)^2))
RSME