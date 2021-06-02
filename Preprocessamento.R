load("malaria/malariaFiocruz.RData")

SoPos <- malaria_concat3

# Variveis a serem removidas
# Transformao: Removidas por nao serem necessrias
SoPos$X1 <- NULL # so indices
SoPos$COD_OCUP <- NULL # outro campo j feito com os nomes das ocupaes (feitos por FC)
SoPos$ano_mes <- NULL # so juntou ano e mes
SoPos$SEM_NOTI <- NULL
SoPos$LOC_INFE <- NULL #a nivel de rs
SoPos$LOC_RESI <- NULL #a nivel de rs
SoPos$UF_INFEC <- NULL #a nivel de rs
SoPos$UF_NOTIF <- NULL #a nivel de rs
SoPos$PAIS_INF <- NULL #a nivel de rs
SoPos$DT_NASCI <- NULL #já tem idade

# Transformao: Removidas porque eu refiz
SoPos$pais_infec <- NULL
SoPos$tripanosoma <- NULL
SoPos$pos_vivax <- NULL
SoPos$pos_falci <- NULL
SoPos$exam_tripanosoma <- NULL
SoPos$pos_malaria <- NULL
SoPos$ano <- NULL
SoPos$mes <- NULL

#Apagando campos inconsistentes de data (que desobedeam as regras logicas)
#data do sintoma igual ou anterior  data de notificao
y <- SoPos$DT_NOTIF >= SoPos$DT_SINTO
SoPos$DT_SINTO[y==FALSE]<- NA

#data do exame igual ou posterior  data de notificao
y <- SoPos$DT_NOTIF <= SoPos$DT_EXAME
SoPos$DT_EXAME[y==FALSE]<- NA

#quando sintoma=Nao ou 2, no pode ter nada no atributo "DT_SINTO"
SoPos$DT_SINTO[SoPos$SINTOMAS==2&SoPos$DT_SINTO>="2000-01-01"] <- NA # 1933 correes

#data de sintoma tem que aparecer at antes de 2000
SoPos$DT_SINTO[SoPos$DT_SINTO<"2000-01-01"] <- NA # 117 correes

#criação de atributos de diferença de datas
SoPos$tempo.exame1 <- SoPos$DT_EXAME-SoPos$DT_NOTIF
SoPos$tempo.tratamento1 <- SoPos$DT_TRATA-SoPos$DT_EXAME
SoPos$tempo.notificacao1 <- SoPos$DT_NOTIF-SoPos$DT_SINTO

SoPos$DT_SINTO <- NULL
SoPos$DT_EXAME <- NULL

#categorizando
SoPos$tempo.exame <-rep(NA,length(SoPos$tempo.exame1))
SoPos$tempo.exame[SoPos$tempo.exame1==0]<- "no mesmo dia"
SoPos$tempo.exame[SoPos$tempo.exame1>=1 & SoPos$tempo.exame1<=7]<- "de 1 a 7 dias"
SoPos$tempo.exame[SoPos$tempo.exame1>=8 & SoPos$tempo.exame1<30]<- "de 8 a 30 dias"
SoPos$tempo.exame[SoPos$tempo.exame1>=30]<- "mais de um mês"

SoPos$tempo.tratamento <-rep(NA,length(SoPos$tempo.tratamento1))
SoPos$tempo.tratamento[SoPos$tempo.tratamento1<0]<- "tratamento antes do exame"
SoPos$tempo.tratamento[SoPos$tempo.tratamento1==0]<- "no mesmo dia"
SoPos$tempo.tratamento[SoPos$tempo.tratamento1>=1 & SoPos$tempo.tratamento1<=7]<- "de 1 a 7 dias"
SoPos$tempo.tratamento[SoPos$tempo.tratamento1>=8 & SoPos$tempo.tratamento1<30]<- "de 8 a 30 dias"
SoPos$tempo.tratamento[SoPos$tempo.tratamento1>=30]<- "mais de um mês"

SoPos$tempo.notificacao <-rep(NA,length(SoPos$tempo.notificacao1))
SoPos$tempo.notificacao[SoPos$tempo.notificacao1==0]<- "no mesmo dia"
SoPos$tempo.notificacao[SoPos$tempo.notificacao1>=1 & SoPos$tempo.notificacao1<=7]<- "de 1 a 7 dias"
SoPos$tempo.notificacao[SoPos$tempo.notificacao1>=8 & SoPos$tempo.notificacao1<30]<- "de 8 a 30 dias"
SoPos$tempo.notificacao[SoPos$tempo.notificacao1>=30]<- "mais de um mês"

SoPos$tempo.exame1 <- NULL
SoPos$tempo.tratamento1 <- NULL
SoPos$tempo.notificacao1 <- NULL

# Varivel: DT_NOTIF -> mes.notificacao, ano.notificacao
# Transformao: Decomposio temporal em ms e ano
SoPos$mes.notificacao <- substr(SoPos$DT_NOTIF, 6, 7)
SoPos$ano.notificacao <- substr(SoPos$DT_NOTIF, 1, 4)
SoPos$DT_NOTIF <- NULL

# Varivel: DT_TRATA -> mes.tratamento, ano.tratamento
# Transformao: Decomposio temporal em ms e ano
SoPos$mes.tratamento <- substr(SoPos$DT_TRATA, 6, 7)
SoPos$ano.tratamento <- substr(SoPos$DT_TRATA, 1, 4)
SoPos$DT_TRATA <- NULL

# Transformao: de municpio para regio de sade
load("malaria/tb.rs.RData")
#
SoPos <- merge(x = SoPos, y = tb.rs, by.x=c("MUN_NOTI"), by.y=c("cod_mun"), all.x=TRUE, stringsAsFactors = FALSE)
names(SoPos)[names(SoPos)=="nome_rs"] <- "rs.notificacao"
SoPos$MUN_NOTI <- NULL
#
SoPos <- merge(x = SoPos, y = tb.rs, by.x=c("MUN_RESI"), by.y=c("cod_mun"), all.x=TRUE)
names(SoPos)[names(SoPos)=="nome_rs"] <- "rs.residencia"
#
SoPos <- merge(x = SoPos, y = tb.rs, by.x=c("MUN_INFE"), by.y=c("cod_mun"), all.x=TRUE)
names(SoPos)[names(SoPos)=="nome_rs"] <- "rs.infeccao"

SoPos$MUN_RESI <- NULL
SoPos$MUN_INFE <- NULL

rm(tb.rs)

# Varivel: EXAME. -> tipo.exame
# Transformao: De cdigos para descries
SoPos$tipo.exame <- ordered(SoPos$EXAME, levels = c(1,2),
                            labels = c("Gota espessa/Esfregaco", "Teste rapido"))
SoPos$EXAME. <- NULL

# Varivel: FALCIPARUM e VIVAX -> tratamento.anterior
# Transformao: De cdigos para descries
SoPos$tratamento.anterior<-rep(NA,length(SoPos$VIVAX))
SoPos$tratamento.anterior[SoPos$VIVAX==1&SoPos$FALCIPARUM==1]<- "Vivax e Falciparum"
SoPos$tratamento.anterior[SoPos$VIVAX==1&SoPos$FALCIPARUM==2]<- "Vivax"
SoPos$tratamento.anterior[SoPos$VIVAX==2&SoPos$FALCIPARUM==1]<- "Falciparum"
SoPos$tratamento.anterior[SoPos$VIVAX>=2&SoPos$FALCIPARUM==2]<- "Nao"

SoPos$FALCIPARUM <- NULL
SoPos$VIVAX <- NULL

# Varivel: HEMOPARASI -> outros hemoparasitas pesquisados
# Transformao: De cdigos para descries
#tirei "negativo" e tirei "não pesquisados"
SoPos$hemoparasita <- ordered(SoPos$HEMOPARASI, levels = c(2,3,4),
                            labels = c("Trypanosoma", "Microfilaria", "Trypanosoma e Microfilaria"))
SoPos$HEMOPARASI <- NULL

# Varivel: NIV_ESCO_1 e NIV_ESCO -> escolaridade
#Passar de anos de estudo para nvel de escolaridade
SoPos$escolaridade <-rep(NA,length(SoPos$NIV_ESCO))
SoPos$escolaridade[SoPos$NIV_ESCO_1==1]<- 0
SoPos$escolaridade[SoPos$NIV_ESCO_1==2]<- 1
SoPos$escolaridade[SoPos$NIV_ESCO_1==3]<- 3
SoPos$escolaridade[SoPos$NIV_ESCO_1==4]<- 4 
SoPos$escolaridade[SoPos$NIV_ESCO_1==5]<- 7
SoPos$escolaridade[SoPos$NIV_ESCO_1==6|SoPos$NIV_ESCO_1==9]<- 10
SoPos$NIV_ESCO_1 <- NULL

# Transformao: De cdigos para descries
SoPos$escolaridade[SoPos$NIV_ESCO==0]<- "Analfabeto"
SoPos$escolaridade[SoPos$NIV_ESCO==1]<- "1a a 4a serie incompleta do EF"
SoPos$escolaridade[SoPos$NIV_ESCO==2]<- "4a serie completa do EF"
SoPos$escolaridade[SoPos$NIV_ESCO==3]<- "5a a 8a serie incompleta do EF"
SoPos$escolaridade[SoPos$NIV_ESCO==4]<- "EF completo"
SoPos$escolaridade[SoPos$NIV_ESCO==5]<- "EM incompleto"
SoPos$escolaridade[SoPos$NIV_ESCO==6]<- "EM completo"
SoPos$escolaridade[SoPos$NIV_ESCO==7]<- "ES incompleto"
SoPos$escolaridade[SoPos$NIV_ESCO==8]<- "ES completo"
SoPos$escolaridade[SoPos$NIV_ESCO==10]<- "Nao se aplica"
SoPos$NIV_ESCO <- NULL

# Transformao: De cdigos para descries (dos que se vieram de "NIV_ESCO_1" -- assim s ficam aqueles onde tinha dado para NIV_ESCO_1 e no tinha para NIV_ESCO)
SoPos$escolaridade[SoPos$escolaridade==0]<- "Analfabeto"
SoPos$escolaridade[SoPos$escolaridade==1]<- "1a a 4a serie incompleta do EF"
SoPos$escolaridade[SoPos$escolaridade==3]<- "5a a 8a serie incompleta do EF"
SoPos$escolaridade[SoPos$escolaridade==4]<- "EF completo"
SoPos$escolaridade[SoPos$escolaridade==7]<- "ES incompleto"
SoPos$escolaridade[SoPos$escolaridade==10]<- "Nao se aplica"

# Varivel: RACA -> raca
# Transformao: De cdigos para descries
SoPos$raca <- ordered(SoPos$RACA, levels = c(1,2,3,4,5),
                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"))
SoPos$RACA <- NULL

# Varivel: RES_EXAM -> resultado.exame
# Transformao: De cdigos para descries
# Tirei "negativo"
SoPos$resultado.exame <- ordered(SoPos$RES_EXAM, levels = c(1,2,3,4,5,6,7,8,9,10,11),
                                 labels = c("Negativo","Falciparum", "F+FG", "Vivax", "F+V", "V+FG", "FG", "Malariae", "F+M", "Ovale", "Nao F"))
SoPos$RES_EXAM <- NULL

# Varivel: ID_PACIE -> idade
# Transformao: Converso para anos e discretizao em faixa etria
# Diviso em classes de acordo a Organizao Pan-Americana de Sade (OPS)
# http://tabnet.saude.es.gov.br/cgi/tabnet/sim/sim96/obtdescr.htm
SoPos$idade<-rep(NA,length(SoPos$ID_PACIE))
SoPos$idade[SoPos$ID_PACIE<=1]<- "menor 01 ano" 
SoPos$idade[1<=SoPos$ID_PACIE&SoPos$ID_PACIE<=4]<-"01 a 04 anos"
SoPos$idade[5<=SoPos$ID_PACIE&SoPos$ID_PACIE<=14]<-"05 a 14 anos"
SoPos$idade[15<=SoPos$ID_PACIE&SoPos$ID_PACIE<=24]<-"15 a 24 anos"
SoPos$idade[25<=SoPos$ID_PACIE&SoPos$ID_PACIE<=34]<-"25 a 34 anos"
SoPos$idade[35<=SoPos$ID_PACIE&SoPos$ID_PACIE<=44]<-"35 a 44 anos"
SoPos$idade[45<=SoPos$ID_PACIE&SoPos$ID_PACIE<=54]<-"45 a 54 anos"
SoPos$idade[55<=SoPos$ID_PACIE&SoPos$ID_PACIE<=64]<-"55 a 64 anos"
SoPos$idade[65<=SoPos$ID_PACIE&SoPos$ID_PACIE<=74]<-"65 a 74 anos"
SoPos$idade[SoPos$ID_PACIE>=75]<-"maior 75 anos"
SoPos$ID_PACIE <- NULL
SoPos$idade[SoPos$ID_DIMEA=='M'|SoPos$ID_DIMEA=='D']<-"menor 01 ano" 
SoPos$ID_DIMEA <- NULL

# Varivel: SEXO -> sexo
# Transformao: De cdigos para descries
#tirei 'ignorado"
SoPos$sexo <- ordered(SoPos$SEXO, levels = c("M","F"),
                    labels = c("Masculino", "Feminino"))
SoPos$SEXO <- NULL

# Varivel: SINTOMAS -> sintomas
# Transformao: De cdigos para descries
SoPos$sintomas <- ordered(SoPos$SINTOMAS, levels = c(1,2), labels = c("Sim", "Nao"))
SoPos$SINTOMAS <- NULL

# Varivel: TIPO_LAM -> tipo.deteccao
# Transformao: De cdigos para descries
#tirei LVC pq nao tem nenhum caso
SoPos$tipo.deteccao <- ordered(SoPos$TIPO_LAM, levels = c(1,2),
                             labels = c("Passiva", "Ativa"))
SoPos$TIPO_LAM <- NULL

#Criao de nova varivel - migração
#quando regio de notificao eh igual da regiao de residencia nao, senao sim.
#mesma regiao de saude
x <- SoPos$rs.notificacao == SoPos$rs.residencia

SoPos$migracao <-rep(NA,length(SoPos$rs.residencia))
#SoPos$migracao[x==T] <- "Nao"
SoPos$migracao[x==F] <- "Sim"

#Criao de nova varivel - caso.autoctone
  #Significado de caso autóctone: O nome que se dá para uma doença que adquirida na zona da residência do enfermo.
#quando regio de residencia eh igual da regio de infeccao sim, senao nao.
#mesma regiao de saude
x <- SoPos$rs.infeccao == SoPos$rs.residencia

SoPos$caso.autoctone <-rep(NA,length(SoPos$rs.residencia))
#SoPos$caso.autoctone[x==T] <- "Sim"
SoPos$caso.autoctone[x==F] <- "Nao"

SoPos$rs.infeccao <- NULL
SoPos$rs.notificacao <- NULL

#Completar a regiao de residencia com os estados fora da regiao da Amazonia Legal e com paises fora do Brasil

# Varivel: PAIS_RES -> pais.residencia
 # a correspondncia do codigo pro pas est no documento que o Marcel mandou
 # usar "PAIS_RES" para trs classes: Brasil, Fronteira com estados da Amaznia Legal (Bolvia(25), Peru(165), Colmbia (48), Venezuela(216), Guiana(84), Guiana Francesa(85), Suriname(198)) e Outros
SoPos$pais.residencia[SoPos$PAIS_RES==25|SoPos$PAIS_RES==48|SoPos$PAIS_RES==84|SoPos$PAIS_RES==85|SoPos$PAIS_RES==165|SoPos$PAIS_RES==198|SoPos$PAIS_RES==216]<- "País Fronteira" 
SoPos$pais.residencia[2<=SoPos$PAIS_RES&SoPos$PAIS_RES<=24|26<=SoPos$PAIS_RES&SoPos$PAIS_RES<=47|49<=SoPos$PAIS_RES&SoPos$PAIS_RES<=83|86<=SoPos$PAIS_RES&SoPos$PAIS_RES<=164|166<=SoPos$PAIS_RES&SoPos$PAIS_RES<=197|199<=SoPos$PAIS_RES&SoPos$PAIS_RES<=215|217<=SoPos$PAIS_RES]<- "Outros Países" 

# Varivel: UF_RESID -> uf.residencia
SoPos$uf.residencia[SoPos$UF_RESID==50|SoPos$UF_RESID==52|SoPos$UF_RESID==22|SoPos$UF_RESID==29]<- "Estado Fronteira"
SoPos$uf.residencia[SoPos$UF_RESID>=23&SoPos$UF_RESID<=27|SoPos$UF_RESID>=31&SoPos$UF_RESID<=35|SoPos$UF_RESID>=41&SoPos$UF_RESID<=43|SoPos$UF_RESID==53]<- "Outros Estados" 

SoPos$local.residencia <- as.character(SoPos$rs.residencia)
SoPos$local.residencia[is.na(SoPos$local.residencia)] <- as.character(SoPos$uf.residencia[is.na(SoPos$local)])
SoPos$local.residencia[is.na(SoPos$local.residencia)] <- as.character(SoPos$pais.residencia[is.na(SoPos$local)])
table(SoPos$local.residencia)

SoPos$PAIS_RES <- NULL
SoPos$UF_RESID <- NULL
SoPos$uf.residencia <- NULL
SoPos$rs.residencia <- NULL
SoPos$pais.residencia <- NULL

# Transformacao: Mundando Nome da Coluna
names(SoPos)[names(SoPos)=="cod_ocup"] <- "ocupacao"

colnames(SoPos)

data <- SoPos

## salvando
save(data, file = "preprocessamento/DataPrep.RData")
