source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")

loadlibrary("readr")
loadlibrary("readxl")
loadlibrary("writexl")
loadlibrary("dplyr")
loadlibrary("arules")
loadlibrary("stringr")
loadlibrary("arulesViz")

#---------------------#----------------------#----------------------#----------------------#----------------------#----------------------#----------------------#

# Passando todos os valores para o tipo "FATOR"

MyData <- data.frame(sapply(data, as.factor))

# Importar biblioteca de padr?es frequentes
library(arules)

#table(MyData$resultado.exame)

#dataset <- MyData

#ocupacao, tempo.exame, tempo.tratamento, tempo.notificacao, mes.notificacao, ano.notificacao, tipo.exame, tratamento.anterior,hemoparasita, escolaridade, raca, idade, sexo, sintomas, tipo.deteccao, migracao, caso.autoctone, local.residencia
compute_support <- function(dataset, resultado) {
  itemsets <- dataset %>% select(mes.notificacao, ano.notificacao, local.residencia, sexo, idade) %>%
    group_by(mes.notificacao, ano.notificacao, local.residencia, sexo, idade) %>% 
    summarize(x = n()) %>% 
    group_by(x) %>% filter (x >= 1) %>%
    summarize(count = n())  %>%
    arrange(x) 
  
  #itemsets$x <- log(itemsets$x)
  curvature_count <- curvature.max(itemsets$x, itemsets$count)
  return (curvature_count$x)
}

#resultado <- "Negativo"  #1452 regras
#resultado <- "Malariae"  #0 regras
#resultado <- "Falciparum"  #44 regras
#resultado <- "Vivax"  #64104 regras
resultado <- "Nao F"  #regras #407 regras

dataset <- MyData %>% filter(resultado.exame==resultado)
sup <- compute_support(dataset, resultado)

rhs_constraint = sprintf("resultado.exame=%s", resultado) 

sup_r <- sup / nrow(MyData)
#general_rules_a <- apriori(dataset, parameter=list(supp = sup_r, conf = 0.8, minlen=3, maxlen=4, target = "rules"), 
#                           appearance = list(rhs=rhs_constraint), control = list(verbose=FALSE))  

general_rules_a <- apriori(MyData, parameter=list(supp = sup_r, conf = 0.8, minlen=3, maxlen=4, target = "rules"), 
                                                      appearance = list(rhs=rhs_constraint), control = list(verbose=FALSE))  
                           
print(paste('Number of rules: ', length(general_rules_a), sep=''))
general_rules_a <- subset(general_rules_a, (subset = lhs %pin% "local.residencia=") & 
                            (subset = !(lhs %pin% "migracao=Nao")) & (subset = !(lhs %pin% "caso.autoctone=Sim")) & 
                            (subset = !(lhs %pin% "tempo.tratamento=no mesmo dia")) & (subset = !(lhs %pin% "tempo.exame=no mesmo dia")) & 
                            (subset = !(lhs %pin% "caso.autoctone=Sim")) & (subset = !(lhs %pin% "ocupacao=Outros")) & 
                            (subset = !(lhs %pin% "tratamento.anterior=Nao")) & (subset = !(lhs %pin% "escolaridade=Nao se aplica")) &
                            (subset = !(lhs %pin% "ocupacao=Ignorado")))

print(paste('Number of rules (after filtering): ', length(general_rules_a), sep=''))
imrules <- interestMeasure(general_rules_a, transactions = dataset)
#general_rules_a <- general_rules_a[imrules$kulczynski > 0.5 & imrules$lift >= 1 & imrules$count > 1]
#print(paste('Number of rules (filtering lift & kulc): ', length(general_rules_a), sep=''))
general_rules_a <- general_rules_a[imrules$lift >= 1 & imrules$count > 1]
print(paste('Number of rules (filtering lift & kulc): ', length(general_rules_a), sep=''))
general_rules_a <- (general_rules_a[!is.redundant(general_rules_a)])
print(paste('Number of rules (filtering redundance): ', length(general_rules_a), sep=''))

#---------------------#----------------------#----------------------#----------------------#----------------------#----------------------#----------------------#
## estudando regras no R

#plots
plot(general_rules_a, measure=c("support", "lift"), shading="confidence")
plot(general_rules_a)
plot(general_rules_a, shading="order", control=list(main = "Two-key plot"))

head(quality(general_rules_a))

#filtrar regras
subrules <- general_rules_a[quality(general_rules_a)$kulc > 0.5]
plot(subrules)

# Usando ferramenta de visualizualização de regras
inspectDT(general_rules_a)

# Vendo resumo das Regras
summary(general_rules_a)

#---------------------#----------------------#----------------------#----------------------#----------------------#----------------------#----------------------#
##Roda padroes frequentes e gera do dataframe com as regras

#rulesdf <- dataset[order(rulesdf$support, decreasing = TRUE),]

imrules <- interestMeasure(general_rules_a, transactions = data)
dataset <- as(general_rules_a, "data.frame")

if (length(general_rules_a) > 0) {
  splitted_rules <- str_split(dataset$rules, " => ", simplify=TRUE) #separa o lado direto da regra
  dataset <- data.frame(lhs = splitted_rules[,1], rhs = gsub("_", "=", gsub("resultado.exame=", "", splitted_rules[,2])), 
                        support = dataset$support, confidence = dataset$confidence, 
                        lift = imrules$lift, count = imrules$count, kulc=imrules$kulczynski, ir=imrules$imbalance)  #tira o 'resultado.exame' e coloca as medidas de interesse em outras colunas
  }

#---------------------#----------------------#----------------------#----------------------#----------------------#----------------------#----------------------#
## Enriquecimento das Regras
  dataset$rhs <- gsub("\\{", "", dataset$rhs)  #tira {
  dataset$rhs <- gsub("\\}", "", dataset$rhs)  #tira }
  splitted_rules <- str_split(dataset$rhs, "=", simplify=TRUE)
  dataset$rhs <- splitted_rules[,1]
  dataset$rhs.i <- as.numeric(splitted_rules[,2])
  
  dataset$lhs <- gsub("\\{", "", dataset$lhs)
  dataset$lhs <- gsub("\\}", "", dataset$lhs)

  dataset$ocupacao=NA
  dataset$tempo.exame=NA
  dataset$tempo.tratamento=NA
  dataset$tempo.notificacao=NA
  dataset$mes.notificacao=NA
  dataset$ano.notificacao=NA
  dataset$tipo.exame=NA
  dataset$tratamento.anterior=NA
  dataset$hemoparasita=NA
  dataset$escolaridade=NA
  dataset$raca=NA
#  dataset$resultado.exame=NA
  dataset$idade=NA
  dataset$sexo=NA
  dataset$sintomas=NA
  dataset$tipo.deteccao=NA
  dataset$migracao=NA
  dataset$caso.autoctone=NA
  dataset$local.residencia=NA
  
  x <- str_split(dataset$lhs, ",")
  for (i in 1:length(x)) {
    k <- x[[i]]
    for (kk in k) {
      nv <- str_split(kk, "=", simplify = TRUE) 
      if (length(nv)==2) {
        dataset[i,trimws(nv[1,1])] <- trimws(nv[1,2])
      }
    }
  }
  
  dataset$mes.notificacao=as.numeric(dataset$mes.notificacao)
  dataset$ano.notificacao=as.numeric(dataset$ano.notificacao)
  dataset$lift=as.numeric(dataset$lift)
  dataset$count=as.numeric(dataset$count)
  dataset$confidence=as.numeric(dataset$confidence)
  dataset$support=as.numeric(dataset$support)
  dataset$kulc=as.numeric(dataset$kulc)
  dataset$ir=as.numeric(dataset$ir)
  
  ##salvando csv
write.csv(dataset, file = "~/Regras/dataset_NaoF.csv")
  