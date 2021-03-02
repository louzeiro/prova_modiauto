#### Instaladores
#install.packages("ggpubr")


### Carregamento das bibliotecas
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
theme_set(theme_pubr())

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
# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
outliers <- boxplot((dados$price), plot=FALSE)$out
print(outliers)
Q <- quantile(dados$price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(dados$price)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

dados<- subset(dados, dados$price > (Q[1] - 1.5*iqr) & dados$price < (Q[2]+1.5*iqr))
boxplot(dados$price)

min(dados$price)
max(dados$price)

dim(dados)

###### Brasil ########
br_boxplot <-
  dados %>%
  select(price, color, region) %>%
  #filter(region == "RS") %>%
  #group_by(color) %>%
  ggplot(mapping = aes(x = color, price)) +
  #  geom_density(mapping = aes(group = color)) +
  geom_boxplot( width = .2, outlier.colour = NA,coef = 1000,color = "black") +
  stat_summary(fun = mean, geom="point", shape=20, size=3, color="red", fill="red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Cores dos veículos",y = "Preço",title = "BR")

br_boxplot

# Distribuição das marcas 
  df <- dados %>%
    select(make)%>%
    group_by(make) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    mutate(freq.rel = freq/sum(freq)*100)
  
  df <- df[1:10,]
  
  ggplot(df, aes(x = reorder(make, -freq.rel), y = freq.rel, fill())) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes( label = paste(round(freq.rel,2),'%'), vjust = -.5,
                   y= freq.rel )) +
    labs(x = "",y = "Frequencia Relativa (%)",
         title = paste("Distribuição de frequência relativa das 10 marcas mais presentes - Brasil \n representando ", 
                       round(sum(df$freq.rel),2), "% da região"))+
    theme_pubclean()
  
  ggplot(tips, aes(x= day,  group=sex)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="day") +
    facet_grid(~sex) +
    scale_y_continuous(labels = scales::percent)

  # Distribuição dos modelos
  df <- dados %>%
    select(model) %>%
    group_by(model) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    mutate(freq.rel = freq/sum(freq)*100)
  
  df <- df[1:10,]
  
  ggplot(df, aes(x = reorder(model, -freq.rel), y = freq.rel, fill())) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes( label = paste(round(freq.rel,2),'%'), vjust = -.5, y= freq.rel )) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x = "",y = "Frequencia Relativa (%)",
      title = paste("Distribuição de frequência relativa dos 10 modelos mais presentes - Brasil \n representando ", 
                    round(sum(df$freq.rel),2), "% da região"))+
    theme_pubclean()
  


##  ESTADOS SELECIONADOS
dados_work <- 
  dados %>%
  filter(region %in% c("SP", "DF", "BA", "RS"))

head(dados_work)

#rm(dados)

attach(dados_work)

# *Item (a)* Boxplots por região dos preços dos veículos:
boxplot.estado <- function(estado){
  estado_boxplot <-
    dados_work %>%
    select(price, color, region) %>%
    filter(region == estado) %>%
    ggplot(mapping = aes(x = color, y = price)) +
    geom_boxplot(width = .2, outlier.colour = NA,coef = 1000,color = "black") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    stat_summary(fun = mean, geom="point", shape=20, size=2, 
                 color="red", fill="red") +
    labs(x = "",y = "Preço",title = estado)
  return(estado_boxplot)
}

sp_boxplot = boxplot.estado("SP")
df_boxplot = boxplot.estado("DF")
ba_boxplot = boxplot.estado("BA")
rs_boxplot = boxplot.estado("RS")




grid.arrange(sp_boxplot, df_boxplot,
             df_boxplot, rs_boxplot,
             ncol=2, nrow=2)

# *Item (b)* Distribuição das marcas por região:

############ Marcas ######################
dist.marcas <- function(estado){
  df <- dados_work %>%
    select(make,region) %>%
    filter(region==estado)%>%
    group_by(make) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    mutate(freq.rel = freq/sum(freq)*100)
  
  df <- df[1:10,]
  
  ggplot(df, aes(x = reorder(make, -freq.rel), y = freq.rel, fill())) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes( label = paste(round(freq.rel,2),'%'), vjust = -.5,
                   y= freq.rel )) +
labs( x = "", y = "Frequencia Relativa (%)",
      title = paste("Distribuição de frequência relativa das 10 marcas mais presentes - ", estado, 
                    "\n representando ", round(sum(df$freq.rel),2), "% da região")
    )+
    theme_pubclean()
  
}

dist.marcas('BA')
dist.marcas('SP')
dist.marcas('RS')
dist.marcas('DF')


dist.modelo <- function(estado){
  df <- dados_work %>%
    select(model,region) %>%
    filter(region==estado)%>%
    group_by(model) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    mutate(freq.rel = freq/sum(freq)*100)
  
  df <- df[1:10,]
  
  ggplot(df, aes(x = reorder(model, -freq.rel), y = freq.rel, fill())) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes( label = paste(round(freq.rel,2),'%'), vjust = -.5,
                   y= freq.rel )) +
    labs(x = "", y = "Frequencia Relativa (%)",
      title = paste("Distribuição de frequência relativa dos 10 modelos mais presentes - ", estado, 
                    "\n representando ", round(sum(df$freq.rel),2), "% da região"))+
    theme_pubclean()
}

dist.modelo('BA')
dist.modelo('SP')
dist.modelo('RS')
dist.modelo('DF')


# **Exercício 2**

#veiculos_mais_vendidos <-modelos.mais_vendidos("SP")

modelos.mais_vendidos <- function(estado){
  veiculos_mais_vendidos <- dados_work %>%
    select(model, region) %>%
    filter(region == estado)
  
  veiculos_mais_vendidos <- dados_work$model[veiculos_mais_vendidos > 1000]
  return(veiculos_mais_vendidos)
}

cor.preco_ano <- function(estado){
   veiculos_mais_vendidos <- modelos.mais_vendidos(estado)
   df <- dados_work %>%
     select(model, region, km, modelyear, price) %>%
       filter(region == estado) %>%
     filter(model %in% veiculos_mais_vendidos) %>%
     group_by(model) %>%
     mutate(preco_tempo = cor(as.numeric(price), as.numeric(modelyear)))
   return(df)
}

 modelos.mais_vendidos("DF")
 
 df <- dados_work %>%
   select(model, region) %>%
   filter(region == "DF") 
   
 
 df <- df
 
 View(dados_work)
 
 veiculos_mais_vendidos <- dados_work$model[veiculos_mais_vendidos > 1000]
 print(veiculos_mais_vendidos)
 
 veiculos_mais_vendidos <- modelos.mais_vendidos("BA")
 
 print(veiculos_mais_vendidos)
 corrSP <- cor.preco_ano("SP")  
 print(corrSP)
 corrDF <- cor.preco_ano("DF")
 print(corrDF)
 corrBA<- cor.preco_ano("BA")
 corrRS <- cor.preco_ano("RS")

 corrSP
 corrBA
 head(teste)

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



#### olds commands


# dados %>%
#   group_by(make) %>%
#   summarise(freq = n()) %>%
#   arrange(desc(freq)) %>%
#   mutate(feq.rel = freq/sum(freq))


# # Distribuição dos modelos
# dados %>%
#   group_by(model) %>%
#   summarise(freq = n()) %>%
#   arrange(desc(freq)) %>%
#   mutate(feq.rel = freq/sum(freq))

# dados_work %>%
#   select(make, region)%>%
#   filter(region=='SP')%>%
#   group_by(make) %>%
#   summarise(freq = n()) %>%
#   arrange(desc(freq)) %>%
#   mutate(feq.rel = freq/sum(freq))

# # Distribuição de marca por regiao
# dados_work %>%
#   select(make, region) %>%
#   group_by(region) %>%
#   table()
# 
# # Distribuição dos modelos por região:
# dados_work %>%
#   select(model, region) %>%
#   group_by(region) %>%
#   table()
