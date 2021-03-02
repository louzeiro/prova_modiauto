library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
setwd('/media/louzeiro/Dados/estagio/mobiauto/')

## carregamento da base de dados
dados <- read.csv2("car_data_intern.csv", sep = ",", header = T)

## Exploração dos dados
dim(dados)
head(dados)
glimpse(dados)
dados$price <- as.numeric(as.character(dados$price))

## Remoção dos NAs e dos casos onde o preço ficou abaixo de 0 
dados <- dados %>% drop_na()
dim(dados)

# boxplot dos precos
boxplot(dados$price)
min(dados$price)
max(dados$price)

# Pelo boxplot suspeita-se de outlier
# retirando os registros onde o preço é abaixo de 0
dados <- dados %>% filter(as.numeric(price)>0)
min(dados$price)

# Analisando os possíveis outliers
outliers <- boxplot((dados$price), plot=FALSE)$out
Q <- quantile(dados$price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(dados$price)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

dados<- subset(dados, dados$price > (Q[1] - 1.5*iqr) & dados$price < (Q[2]+1.5*iqr))
boxplot(dados$price)

min(dados$price)
max(dados$price)

dim(dados)

br_boxplot <-
  dados %>%
  select(price, color, region) %>%
  #filter(region == "RS") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, as.numeric(price))) +
  #  geom_density(mapping = aes(group = color)) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA,
    coef = 1000,
    #position = position_nudge(.2), 
    color = "black"
  ) +
  stat_summary(fun = mean, geom="point", shape=20, size=3, 
               color="red", fill="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(
    x = "Cores dos veículos",
    y = "Preço",
    title = "BR"
  )

br_boxplot

# Distribuição das marcas 
dados %>%
  group_by(make) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(feq.rel = freq/sum(freq))
  #tally(sort = TRUE) 

# Distribuição dos modelos
dados %>%
  group_by(model) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(feq.rel = freq/sum(freq))



##  ESTADOS SELECIONADOS
dados_work <- 
  dados %>%
  filter(region %in% c("SP", "DF", "BA", "RS"))

head(dados_work)

rm(dados)

attach(dados_work)

# *Item (a)* Boxplots por região dos preços dos veículos:
sp_boxplot <-
  dados_work %>%
  select(price, color, region) %>%
  filter(region == "SP") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, y = price)) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA,
    coef = 1000,
    #position = position_nudge(.2), 
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun = mean, geom="point", shape=20, size=2, 
               color="red", fill="red") +
  labs(
    x = "Cores dos veículos",
    y = "Preço",
    title = "SP"
  )

df_boxplot <-
  dados_work %>%
  select(price, color, region) %>%
  filter(region == "DF") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, as.numeric(price))) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA,
    coef = 1000,
    #position = position_nudge(.2), 
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun = mean, geom="point", shape=20, size=3, 
               color="red", fill="red") +
  labs(
    x = "Cores dos veículos",
    y = "Preço",
    title = "DF"
  )

ba_boxplot <-
  dados_work %>%
  select(price, color, region) %>%
  filter(region == "BA") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, as.numeric(price))) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA,
    coef = 1000,
    #position = position_nudge(.2), 
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun = mean, geom="point", shape=20, size=3, 
               color="red", fill="red") +
    labs(
    x = "Cores dos veículos",
    y = "Preço",
    title = "BA"
  )


rs_boxplot <-
  dados_work %>%
  select(price, color, region) %>%
  filter(region == "RS") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, as.numeric(price))) +
#  geom_density(mapping = aes(group = color)) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA,
    coef = 1000,
    #position = position_nudge(.2), 
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun = mean, geom="point", shape=20, size=3, 
               color="red", fill="red") +
    labs(
    x = "Cores dos veículos",
    y = "Preço",
    title = "RS"
  )


rs_boxplot

grid.arrange(sp_boxplot, df_boxplot,
             df_boxplot, rs_boxplot,
             ncol=2, nrow=2)

# *Item (b)* Distribuição das marcas por região:
dados_work %>%
  select(make, region) %>%
  group_by(region) %>%
  table()

# Distribuição dos modelos por região:
dados_work %>%
  select(model, region) %>%
  group_by(region) %>%
  table()



# **Exercício 2**
veiculos_mais_vendidos <-
  dados_work %>%
  select(model, region) %>%
  filter(region == "SP") %>%
  table()

sort(veiculos_mais_vendidos)

veiculos_mais_vendidos <- dados_work$model[veiculos_mais_vendidos > 1000]
sort(veiculos_mais_vendidos)

valores_mais_vendidos <- 
  dados_work %>%
  select(model, region, km, modelyear, price) %>%
  filter(region == "SP") %>%
  filter(model %in% veiculos_mais_vendidos) %>%
  group_by(model) %>%
  mutate(preco_tempo = cor(as.numeric(price), as.numeric(modelyear)))

head(valores_mais_vendidos)

# Vemos que há modelos com correlação positiva indicando que há valorização ao
# longo do tempo. Por outro lado, veiculo com correlção negativa apresenta
# desvalorização ao longo do tempo.


km_mais_vendidos <- 
  dados_work %>%
  select(model, region, km, modelyear, price) %>%
  filter(region == "SP") %>%
  filter(model %in% veiculos_mais_vendidos) %>%
  group_by(model) %>%
  mutate(km_tempo = cor(as.numeric(km), as.numeric(modelyear)))

head(km_mais_vendidos)

# Como a correlação é negativa podemos concluir que, em geral,
# quanto maior a quilometragem menor o valor do veículo.