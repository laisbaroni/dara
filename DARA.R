library(readxl)

load("~/preprocessamento/DataPrep.RData")
filtrarvalores <- read_excel("filtrarvalores.xlsx")
data_old <- data

  #vivax
dataset_Vivax <- read_csv("Regras/dataset_Vivax.csv")
vivax <- data[data$resultado.exame == "Vivax",]
vivax <- data.frame(sapply(vivax, as.factor))
data <- vivax
regra <- dataset_Vivax

  #negativo
dataset_Negativo <- read_csv("Regras/dataset_Negativo.csv")
negativo <- data[data$resultado.exame == "Negativo",]
negativo <- data.frame(sapply(negativo, as.factor))
data <- negativo
regra <- dataset_Negativo

  #falciparum
dataset_Falciparum <- read_csv("Regras/dataset_Falciparum.csv")
falciparum <- data[data$resultado.exame == "Falciparum",]
falciparum <- data.frame(sapply(falciparum, as.factor))
data <- falciparum
regra <- dataset_Falciparum

  #nao falciparum
dataset_NaoF <- read_csv("Regras/dataset_NaoF.csv")
naoF <- data[data$resultado.exame == "Nao F",]
naoF <- data.frame(sapply(naoF, as.factor))
data <- naoF
regra <- dataset_NaoF


regra$mes.notificacao <- as.factor(regra$mes.notificacao)
levels(regra$mes.notificacao) <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12")

resultados <- list() 
data$resultado.exame <- NULL


for (i in 1:length(data)) {
         atributos <- colnames(data)
         atributo <- atributos[i]
         print(atributo)
         tabela_dataset <- data[,atributo]
         tabela_dataset <- table(tabela_dataset)
         tabela_regras <- regra[,atributo]
         tabela_regras <- table(tabela_regras)
         
         if (length(tabela_regras)==0)
           next
         
         df_dataset <- as.data.frame(tabela_dataset)
         filtro <- filtrarvalores$ignorar[atributo==filtrarvalores$atributo]
         df_dataset <- df_dataset[!(df_dataset$tabela_dataset %in% filtro),]
         
         df_regras <- as.data.frame(tabela_regras)
         
         dados <- merge(x = df_dataset, y = df_regras, by.x = 'tabela_dataset', by.y = 'tabela_regras',  all.x = TRUE)
         colnames(dados) <- c("valor", "tabela", "regra")
         dados$regra[is.na(dados$regra)] <- 0
         dados$c <- 0
         if (nrow(dados) > 1) {
           for (j in 1:(nrow(dados)-1)) {
             for (k in (j+1):(nrow(dados))) {
               if ((dados$tabela[j] > dados$tabela[k]) && (dados$regra[j] < dados$regra[k])) {
                 dados$c[j] <- dados$c[j] - 1
                 dados$c[k] <- dados$c[k] + 1
               } 
               else if ((dados$tabela[j] < dados$tabela[k]) && (dados$regra[j] > dados$regra[k])) {
                 dados$c[j] <- dados$c[j] + 1
                 dados$c[k] <- dados$c[k] - 1
               } 
             }
           }
         }
         den <- nrow(dados)-1
#         dados$rc <- as.integer(100* dados$c / den)
         ordem <- order(dados$c)
         dados <- dados[ordem,]
         resultados[[i]] <- dados 
         print(dados)
}

valores = sprintf("data$%s", atributo)
Y <- table(valores)

n <- rep(0, length(A))
