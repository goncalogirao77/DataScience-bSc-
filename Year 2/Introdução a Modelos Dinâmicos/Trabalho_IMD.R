# Conhecimento da Base de Dados

library(corrplot)
library(olsrr)
library(dplyr)
library(stringr)
library(car)
library(caTools)
library(base)
library(lmtest)
library(tseries)
library(Metrics)
library(ggplot2)

dados <- read.csv('listings.csv')
View(dados)

summary(dados) # Estatística descritiva básica das variáveis numéricas
str(dados) #Tipos de todas as variáveis/ estrutura da base de dados

mediana <- median(dados$price,na.rm=TRUE)  #Substituir NAs pelo valor da mediana
dados$price[which(is.na(dados$price))] <- mediana

mediana1 <- median(dados$reviews_per_month,na.rm=TRUE)
dados$reviews_per_month[which(is.na(dados$reviews_per_month))] <- mediana1

# Seperação da 1ºcoluna em 5 colunas

dados_name <- dados[c(2)] # Coluna name tem 4 elementos
dados_name 
dados_name <- cbind(dados_name,  # Adicionar as 5 novas colunas
                    Name = NA,
                    Stars = NA,
                    nbedrooms = NA,
                    nbeds = NA,
                    nbaths = NA)

dados_name$Name <- sub("·.*", "", dados_name[,1]) #Extrair o 1ºelemento dos valores da coluna para coluna "Name"

elemento_estrela <- gsub(".*★([^·]*).*", "\\1", dados_name[,1])
elemento_estrela <- ifelse(grepl("★", dados_name[,1]), elemento_estrela, NA)
dados_name$Stars <- elemento_estrela

elemento_quarto <- gsub(".*\\b(\\d+\\s*(?:bedroom|bedrooms))\\b.*|.*\\b(Studio)\\b.*", "\\1\\2", dados_name[,1])
elemento_quarto <- ifelse(grepl("\\b(\\d+\\s*(?:bedroom|Studio|bedrooms))\\b|\\b(Studio)\\b", dados_name[,1]), elemento_quarto, NA)
dados_name$nbedrooms <- elemento_quarto

elemento_cama <- gsub(".*\\b(\\d+\\s*beds?\\b).*", "\\1", dados_name[,1])
elemento_cama <- ifelse(grepl("\\b(\\d+\\s*beds?\\b)", dados_name[,1]), elemento_cama, NA)
dados_name$nbeds<- elemento_cama

ultima_parte <- sub(".* · ([^·]+)$", "\\1", dados_name[,1])
nbaths <- ifelse(grepl("\\b(bath|baths)\\b", ultima_parte), ultima_parte, NA)
dados_name$nbaths <- nbaths

View(dados_name)

dados2 <- cbind(dados_name,dados,by="name") # Juntar o dataframe inicial com o df mais limpo
dados2 <- subset(dados2, select = -c(1, 8)) # Apagar as colunas chamadas name

View(dados2)
summary(dados2)
nrow(dados2)

dados_n2 <- na.omit(dados2) #Sem valores omissos
nrow(dados_n2)
nrow(dados2) - nrow(dados_n2)

# Remover linhas onde a coluna "Stars" é "new"
dados_n2 <- dados_n2[dados_n2$Stars != "New", ]
dados_n2 <- subset(dados_n2, Stars != "")

# Agora converter a coluna "Stars" para numérica
dados_n2$Stars <- as.numeric(dados_n2$Stars)

dados_n2$nbeds <- as.integer(gsub("[^0-9]", "", dados_n2$nbeds)) #transformar variável "nbeds" em int
dados_n2$nbedrooms <- ifelse(dados_n2$nbedrooms == "Studio", 0, dados_n2$nbedrooms) #Substituir Studio por "0"
dados_n2$nbedrooms <- as.integer(gsub("[^0-9]", "", dados_n2$nbedrooms)) #Transformar em INT

table(dados_n2$room_type) #Tabela de frequência de acordo com o tipo de quarto

# Criação de variável númerica para room_type
dados_n2$room_type_cod = ifelse(dados_n2$room_type == 'Entire home/apt', 1, 
                                ifelse(dados_n2$room_type == 'Hotel room', 2,
                                       ifelse(dados_n2$room_type == 'Private room', 3, 4)))

dados_n2$nbaths <- gsub("0 shared baths|0 baths", "0", dados_n2$nbaths) #substitui os valores "0 shared baths" por "0 baths"
dados_n2$baths_int <- dados_n2$nbaths
dados_n2$baths_int <- gsub("Half-bath|Private half-bath|Shared half-bath|1 private bath|1 shared bath|1 bath", "1", dados_n2$baths_int)
dados_n2$baths_int <- gsub("1.5 baths|1.5 shared baths|2 shared baths|2 baths", "2", dados_n2$baths_int)
dados_n2$baths_int <- gsub("2.5 baths|2.5 shared baths|3 shared baths|3 baths", "3", dados_n2$baths_int)
dados_n2$baths_int <- gsub("3.5 baths|4 shared baths|4 baths", "4", dados_n2$baths_int)
dados_n2$baths_int <- gsub("4.5 baths|5 baths", "5", dados_n2$baths_int)
dados_n2$baths_int <- gsub("5.5 baths|6 baths|5.5", "6", dados_n2$baths_int)
dados_n2$baths_int <- gsub("7 baths", "7", dados_n2$baths_int)
dados_n2$baths_int <- gsub("7.5 baths|8 baths|7.5", "8", dados_n2$baths_int)
dados_n2$baths_int <- as.integer(dados_n2$baths_int)

# Extrair parte da string até "in" e substituir na coluna "name"
extrair_nome <- function(string) {
  if (grepl("in", string)) {
    return(str_trim(str_extract(string, ".*?\\bin")))
  } else {
    return(string)
  }
}

dados_n2$Name <- sapply(dados_n2$Name, extrair_nome)
extrair_nome2 <- function(string) {
  novo_nome <- gsub("\\sin", "", string)
  return(trimws(novo_nome))
}

# Aplicar a função à coluna "name"
dados_n2$Name <- sapply(dados_n2$Name, extrair_nome2)
str(dados_n2)

# Criar o histograma usando ggplot2
dados_n2$log_price <- log(dados_n2$price)
ggplot(dados_n2, aes(x = log_price)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribuição Logaritmizada de Preços", x = "Log(Preço)", y = 
         "Frequência") + theme_minimal()

#Comparar a variável nbeds e nbedrooms 
dados_agrupados <- aggregate(nbedrooms ~ nbeds, data = dados_n2, FUN = mean) 
ggplot(dados_agrupados, aes(x = nbedrooms, y = nbeds)) + 
  geom_bar(stat = "identity", fill = "skyblue", color = "black") + 
  labs(title = "Número de quartos por número de camas", x = "Número de camas", 
       y = "Número de quartos") + theme_minimal() 


amostra_final <- dados_n2[,-c(1,5,6,8,9,10,13,17,22,23)] #DF com variáveis numéricas
amostra_final1 <- na.omit(amostra_final)

(cor(amostra_final1))
corrplot(cor(amostra_final1), method = "circle")
corrplot(cor(amostra_final1), method = "number")
pairs(amostra_final1) 

#Retirar variaveis - Multicolineariedade 

amostra_final1 <- select(amostra_final1, -"number_of_reviews_ltm") 
amostra_final1 <- select(amostra_final1, -"nbeds") 
corrplot(cor(amostra_final1), method = "number")

modelo_inicial <- lm(price ~ ., data = amostra_final1)
(melhor_modelo_pelo_p <- ols_step_both_p(modelo_inicial))
(melhor_modelo_pelo_aic <- ols_step_both_aic(modelo_inicial))

modelo1 <- lm(log(price) ~ nbedrooms + baths_int + room_type_cod + host_id
              + reviews_per_month + availability_365 + latitude, data = 
                amostra_final1)

summary(modelo1)
summary(modelo1)$coefficient #Variáveis significativas

#Verificação dos pressupostos
mean(modelo1$residuals) 
bptest(modelo1)  
bgtest(modelo1)
jarque.bera.test(modelo1$residuals)

#Representação gráfica dos resíduos
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(modelo1)

vif(modelo1) #vifs todos menor que 5

#Modelo 2 sem outliers do preço
(outliers_price <- boxplot(amostra_final1$price, plot = FALSE)$out)
amostra_final2 <- amostra_final1[!amostra_final1$price %in% outliers_price, ]
boxplot(amostra_final2$price)

modelo2 <- lm(log(price) ~ nbedrooms + baths_int + room_type_cod + host_id
   + reviews_per_month + availability_365 + latitude, data = 
     amostra_final2)

summary(modelo2)
summary(modelo2)$coefficient #Variáveis significativas

#Verificação dos pressupostos
mean(modelo2$residuals) 
bptest(modelo2)  
bgtest(modelo2)
jarque.bera.test(modelo2$residuals)

#Representação gráfica dos resíduos
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(modelo2)

vif(modelo2) #vifs todos menor que 5
AIC(modelo1,modelo2)

crPlots (modelo2) # ver se existem não-linearidades nas variáveis/resíduos

modelo3 <- lm(log(price) ~ poly(nbedrooms,degree=3) + poly(baths_int,degree = 3) + poly(room_type_cod,degree=2)
              + host_id
              + poly(reviews_per_month,degree=3) + availability_365 + poly(latitude,degree=2), data = 
                amostra_final2)

summary(modelo3)
summary(modelo3)$coefficient #Variáveis significativas

#Verificação dos pressupostos
mean(modelo3$residuals) 
bptest(modelo3)  
bgtest(modelo3)
jarque.bera.test(modelo3$residuals)

#Representação gráfica dos resíduos
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(modelo3)

vif(modelo3) #vifs todos menor que 5
AIC(modelo1,modelo2,modelo3)

#Agora com subamostra
nrow(amostra_final2)
nrow(dados)
amostra_final3<-amostra_final2[1:300,]

modelo4 <- lm(log(price) ~ poly(nbedrooms,degree=3) + poly(baths_int,degree = 3) + poly(room_type_cod,degree=2)
              + host_id
              + poly(reviews_per_month,degree=3) + availability_365 + poly(latitude,degree=2), data = 
                amostra_final3)

summary(modelo4)
summary(modelo4)$coefficient #Variáveis significativas

#Verificação dos pressupostos - Todos verificados
mean(modelo4$residuals) #Media nula
bptest(modelo4) #variância constante
bgtest(modelo4) #ausência de correlação
jarque.bera.test(modelo4$residuals) #resíduos normalmente distribuídos

#Representação gráfica dos resíduos
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(modelo4)

vif(modelo4) #vifs todos menor que 5

AIC(modelo1,modelo2,modelo3,modelo4) #Modelo3 é o melhor

#Previsão e avaliação da performance
#Previsao in sample modelo 1
pr1 <-predict(modelo1,amostra_final1)
plot(exp(pr1), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Número do registo da base de dados",
     ylab = "Valor",main="Previsão in-sample do modelo1",xlim=c(5400,6100),ylim = c(0,600))
lines(amostra_final1$price, pch = 18, col = "blue", type = "b", lty = 2, xlim=c(5400,6100),ylim = c(0,600))

#Valor do MAPE
actual1 <- amostra_final1$price
MAPE1 <- mean(abs((actual1 - exp(pr1)) / actual1)) * 100
round(MAPE1, 3)  

#Previsao in sample modelo2
pr <-predict(modelo2,amostra_final2)
plot(exp(pr), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Número do registo da base de dados",
     ylab = "Valor",main="Previsão in-sample do modelo2",xlim=c(5400,5500), ylim = c(0,250))
lines(amostra_final2$price, pch = 18, col = "blue", type = "b", lty = 2, xlim=c(5400,5500),ylim = c(0,250))

#Valor do MAPE
actual2 <- amostra_final2$price
MAPE2 <- mean(abs((actual2 - exp(pr)) / actual2)) * 100
round(MAPE2, 3)  

#Resumo dos 2 modelos
n_modelo <- c(1,2)
erros <- c(MAPE1,MAPE2)
AICs <- c(AIC(modelo1),AIC(modelo2))
R_quadrado <- c(summary(modelo1)$r.squared,summary(modelo2)$r.squared)
cbind(n_modelo, erros, AICs,R_quadrado)


#Previsao in sample modelo 3
pr3 <-predict(modelo3,amostra_final2)
plot(exp(pr3), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Número do registo da base de dados",
     ylab = "Valor",main="Previsão in-sample do modelo3",xlim=c(5400,5500),ylim = c(0,250))
lines(amostra_final2$price, pch = 18, col = "blue", type = "b", lty = 2, xlim=c(5400,5500),ylim = c(0,250))

#Valor do MAPE
actual3 <- amostra_final2$price
MAPE3 <- mean(abs((actual3 - exp(pr3)) / actual3)) * 100
round(MAPE3, 3)  

#Resumo dos 3 modelos
n_modelo1 <- c(1,2,3)
erros1 <- c(MAPE1,MAPE2,MAPE3)
AICs1 <- c(AIC(modelo1),AIC(modelo2),AIC(modelo3))
R_quadrado1 <- c(summary(modelo1)$r.squared,summary(modelo2)$r.squared,summary(modelo3)$r.squared)
cbind(n_modelo1, erros1, AICs1,R_quadrado1)


#Previsao in sample modelo 4
pr4 <-predict(modelo4,amostra_final3)
plot(exp(pr4), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Número do registo da base de dados",
     ylab = "Valor",main="Previsão in-sample do modelo4")
lines(amostra_final3$price, pch = 18, col = "blue", type = "b", lty = 2)

#Valor do MAPE
actual4 <- amostra_final3$price
MAPE4 <- mean(abs((actual4 - exp(pr4)) / actual4)) * 100
round(MAPE4, 3)  


#Resumo dos 4 modelos - Resumo Total dos resultados
n_modelo <- c(1,2,3,4)
erros <- c(MAPE1,MAPE2,MAPE3,MAPE4)
AICs <- c(AIC(modelo1),AIC(modelo2),AIC(modelo3),AIC(modelo4))
R_quadrado <- c(summary(modelo1)$r.squared,summary(modelo2)$r.squared,summary(modelo3)$r.squared,summary(modelo4)$r.squared)
cbind(n_modelo, erros, AICs,R_quadrado)


#Conjunto de treino e teste 

set.seed(127)
separar = sample.split(amostra_final2, SplitRatio = 0.90)
train = amostra_final2[separar,]
test = amostra_final2[!(separar),]
nrow(train)
nrow(test)

#correr o modelo3 sobre o conjunto de treino

modelo3_n <- lm(log(price) ~ poly(nbedrooms,degree=3) + poly(baths_int,degree = 3) + poly(room_type_cod,degree=2)
              + host_id
              + poly(reviews_per_month,degree=3) + availability_365 + 
                poly(latitude,degree=2), data = train)

pr3_n <-predict(modelo3_n,test)
plot(exp(pr3_n), type = "b", frame = FALSE, pch = 19, col = "red", xlab = 
       "Número do registo na base de dados", ylab = "Valor", 
     main="Previsão out-sample do modelo3_n")

lines(test$price, pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), 
       lty = 1:2, cex=0.8)

# erro de previsão

actual3_n<-test$price
prediction3_n <- exp(pr3_n)
n<-length(test$price)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE3_n <- (1/n) * sum(abs((actual3_n - prediction3_n)/actual3_n))
MAPE3_n


#Subamostra conjunto de treino e teste

set.seed(2)
separar = sample.split(amostra_final3, SplitRatio = 0.90)
train1 = amostra_final3[separar,]
test1 = amostra_final3[!(separar),]
nrow(train1)
nrow(test1)

modelo4_n <- lm(log(price) ~ poly(nbedrooms,degree=3) + poly(baths_int,degree = 3)
                + poly(room_type_cod,degree=2) + host_id + 
                  poly(reviews_per_month,degree=3) + availability_365 + 
                poly(latitude,degree=2), data = train1)

pr4_n <-predict(modelo4_n, test1)
plot(exp(pr4_n), type = "b", frame = FALSE, pch = 19, col = "red", 
     xlab = "Número do registo na base de dados", ylab = "Valor", 
     main="Previsão out-sample do modelo4_n")
lines(test1$price, pch = 18, col = "blue", type = "b", lty = 2)
legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual4_n<-test1$price
prediction4_n <- exp(pr4_n)
n<-length(test1$price)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE4_n <- (1/n) * sum(abs((actual4_n - prediction4_n)/actual4_n))
MAPE4_n


