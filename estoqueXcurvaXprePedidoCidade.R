setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
library(plyr)

# ID PEDIDO DROGARIA CIDADE
cobertura_min_semanas <- 4  
data_mascara <- "20160614"
id_pedido    <- paste0(data_mascara,"")

# busca EANs na base
mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
sqlString1 <- paste0("select * from PRODUTOS_CLIENTES PC, PRODUTOS P, EANS_PRODUTOS E "
                    ,"where PC.COD_PRODUTO = P.COD_PRODUTO AND E.PRODUTOS_COD_PRODUTO=P.COD_PRODUTO"
                    ," and PC.COD_CLIENTE=","1") # cliente Drogaria Cidade
sqlString2 <- paste0("select * from PRODUTOS_CLIENTES PC, PRODUTOS P, EANS_PRODUTOS E "
                     ,"where PC.COD_PRODUTO = P.COD_PRODUTO AND E.PRODUTOS_COD_PRODUTO=P.COD_PRODUTO"
                     ," and PC.COD_CLIENTE=","2") # cliente Drogaria Desconto Popular
EANScidade   <- dbGetQuery(mydb,sqlString1)
EANSdesconto <- dbGetQuery(mydb,sqlString2)

# le datasets inventario, Produtos e pre-pedido Cidade
curv       <- read.csv(paste0("datasetCurva",data_mascara,".csv"),sep=";",stringsAsFactors = F,dec=",")
stok       <- read.csv(paste0("datasetEstoque",data_mascara,".csv"),sep=";",stringsAsFactors = F,dec=",")
cidade     <- read.csv(paste0("datasetPrePedido",id_pedido,".csv"),sep=";",stringsAsFactors = F,dec=",")
tipo_pr    <- read.csv("datasetTipo.csv"         ,sep=";",stringsAsFactors = F,dec=",")
produto    <- read.csv("datasetProdutos.csv"     ,sep=";",stringsAsFactors = F,dec=",")
pedFilial3 <- read.csv("pedido Fl03.csv"         ,sep=";",stringsAsFactors = F,dec=",")
# anula pedido filial 3
pedFilial3$qtd<-0
# funde datasets
curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))
curvaEstoque <- merge(curvaEstoque,tipoProduto,by.x = c("COD_INTERNO","COD_CLIENTE"),by.y=c("codigo","COD_CLIENTE"))

# filtra linhas com estoque maior que zero ou que já tiveram vendas
curvaEstoque <- curvaEstoque[curvaEstoque$valor_total>0 | !is.na(curvaEstoque$descricao.y),]
# preenche valores e calcula novas colunas
curvaEstoque$qtd_venda[is.na(curvaEstoque$qtd_venda)]<-0
curvaEstoque$total[is.na(curvaEstoque$total)]<-0
# grava cobertura equivalente ao quantil 97.5 (2 dp) para NA e infinito (itens sem venda mas com estoque/coberturas muito altas)
maxRef <- quantile(curvaEstoque$cobertura_semanas[!is.infinite(curvaEstoque$cobertura_semanas)],0.975,na.rm = TRUE)
curvaEstoque$cobertura_semanas[is.na(curvaEstoque$cobertura_semanas) | is.infinite(curvaEstoque$cobertura_semanas)]<- maxRef[[1]]
curvaEstoque$cobertura_ponderada <- curvaEstoque$valor_total*curvaEstoque$cobertura_semanas
curvaEstoque$sem_venda   <- curvaEstoque$qtd_venda==0                                                              
curvaEstoque$sem_estoque <- curvaEstoque$qtd==0

# quebra curva por filial, gera arquivo geral e inclui colunas referentes as filiais
curvaEstoque1 <- curvaEstoque[curvaEstoque$COD_FILIAL_CLIENTE==1,]
curvaEstoque2 <- curvaEstoque[curvaEstoque$COD_FILIAL_CLIENTE==2,]
curvaGeral    <- ddply(curvaEstoque
                      ,.(COD_INTERNO, COD_CLIENTE, descricao.x, laboratorio.x)
                      ,summarize
                      ,estoque_geral=sum(qtd)
                      ,venda_geral=sum(qtd_venda))
curvaFusao <- merge(curvaGeral,curvaEstoque1[c("COD_INTERNO","unitario","cobertura_semanas","qtd","qtd_venda")],all.x=TRUE)
names(curvaFusao)[3]  <- "DESCRICAO"
names(curvaFusao)[4]  <- "LABORATORIO"
names(curvaFusao)[5]  <- "ESTOQUE_GERAL"
names(curvaFusao)[6]  <- "VENDA_GERAL"
names(curvaFusao)[7]  <- "CUSTO_FL_1"
names(curvaFusao)[8]  <- "COBERTURA_FL_1"
names(curvaFusao)[9]  <- "QTD_FL1"
names(curvaFusao)[10] <- "VENDA_FL1"
curvaFusao <- merge(curvaFusao,curvaEstoque2[c("COD_INTERNO","unitario","cobertura_semanas","qtd","qtd_venda")],all.x=TRUE)
names(curvaFusao)[11] <- "CUSTO_FL_2"
names(curvaFusao)[12] <- "COBERTURA_FL_2"
names(curvaFusao)[13] <- "QTD_FL2"
names(curvaFusao)[14] <- "VENDA_FL2"

# funde pedido da filial 3
curvaFusao <- merge(curvaFusao,pedFilial3,by.x = "COD_INTERNO",by.y = "codigo",all.x=TRUE)
names(curvaFusao)[15] <- "RESERVA_FL_3"

# joga zero para os campos de quantidade NA
curvaFusao[is.na(curvaFusao$QTD_FL1),"QTD_FL1"] <- 1
curvaFusao[is.na(curvaFusao$QTD_FL2),"QTD_FL2"] <- 1
curvaFusao[is.na(curvaFusao$RESERVA_FL3),"RESERVA_FL3"] <- 0
curvaEstoque$sem_estoque <- curvaEstoque$qtd==0

# cruza prePedido com EANs
prePedido <- merge(EANScidade,cidade,by.x=c("CODIGO_EAN"),by.y=c("EAN"))
# funde pedido da Cidade
pedido    <- merge(curvaFusao,prePedido[c("COD_INTERNO","compra")],by.x=c("COD_INTERNO"),by.y = ("COD_INTERNO"),all.y=TRUE)

# preenche itens sem cobertura
max_cobertura <- max(rbind(max(curvaFusao$COBERTURA_FL_1[!is.na(curvaFusao$COBERTURA_FL_1)])
                          ,max(curvaFusao$COBERTURA_FL_2[!is.na(curvaFusao$COBERTURA_FL_2)])))
curvaFusao$COBERTURA_FL_1[is.na(curvaFusao$COBERTURA_FL_1)] <- max_cobertura
curvaFusao$COBERTURA_FL_1[is.na(curvaFusao$COBERTURA_FL_1)] <- max_cobertura

# calcula cobertura mínima para filiais 

curvaFusao$RESERVA_FL1 <- round((curvaFusao$QTD_FL1/curvaFusao$COBERTURA_FL_1)*cobertura_min_semanas)
curvaFusao$RESERVA_FL1[curvaFusao$RESERVA_FL1==0]<-1
curvaFusao$RESERVA_FL2 <- round((curvaFusao$QTD_FL2/curvaFusao$COBERTURA_FL_2)*cobertura_min_semanas)
curvaFusao$RESERVA_FL2[curvaFusao$RESERVA_FL2==0]<-1

                                
                          
        
curvaFusao$COB_MIN_QTD_FL1 <- c 


######################################
# cruza prePedido com estoque Desconto
######################################
transf1 <- merge(prePedido,curvaEstoque1,by.x=("COD_INTERNO"),by.y=("COD_INTERNO"),all.x = TRUE)
transf2 <- merge(prePedido,curvaEstoque2,by.x=("COD_INTERNO"),by.y=("COD_INTERNO"),all.x = TRUE)

write.csv2(file="curvaEstoque3.csv",curvaEstoque,row.names = F)
write.csv2(file="prePedidoCidade-F01.csv",transf1,row.names=TRUE)
write.csv2(file="prePedidoCidade-F02.csv",transf2,row.names=TRUE)


