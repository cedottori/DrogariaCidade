setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
source("carregaPrePedido.R")
library(plyr)

# ID PEDIDO DROGARIA CIDADE
#id_pedido             <- "REFERENCIA"
#id_pedido             <- "EMS"
#id_pedido             <- "MEDLEY"
#id_pedido             <- "SANDOZ"
#id_pedido             <- "NEO QUIMICA"
#id_pedido             <- "EUROFARMA"
id_pedido <-"20160615"
data_mascara_cidade <- "20160615" # DATA DO INVENTÁRIO CIDADE
data_mascara <-id_pedido
cod_cliente <- 1

source("XL7.R")
cobertura_min_semanas <- 3

# busca EANs na base
xl7_fechaconexoesdb()
mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
sqlString1 <- paste0("select * from PRODUTOS_CLIENTES PC, PRODUTOS P, EANS_PRODUTOS E "
                     ,"where PC.COD_PRODUTO = P.COD_PRODUTO AND E.PRODUTOS_COD_PRODUTO=P.COD_PRODUTO"
                     ," and PC.COD_CLIENTE=","1") # cliente Drogaria Cidade
sqlString2 <- paste0("select * from PRODUTOS_CLIENTES PC, PRODUTOS P, EANS_PRODUTOS E "
                     ,"where PC.COD_PRODUTO = P.COD_PRODUTO AND E.PRODUTOS_COD_PRODUTO=P.COD_PRODUTO"
                     ," and PC.COD_CLIENTE=","2") # cliente Desconto Popular
EANScidade   <- dbGetQuery(mydb,sqlString1)
EANSdesconto <- dbGetQuery(mydb,sqlString2)

######################################################
# le datasets inventario, Produtos e pre-pedido Cidade
carga         <- carregaPrePedido(id_pedido)
curv          <- read.csv(paste0("datasetCurva",data_mascara,".csv"),sep=";",stringsAsFactors = F,dec=",")
stok          <- read.csv(paste0("datasetEstoque",data_mascara,".csv"),sep=";",stringsAsFactors = F,dec=",")
estoqueCidade <- read.csv(paste0("datasetEstoque",data_mascara_cidade,"-CIDADE.csv"),sep=";",stringsAsFactors = F,dec=",")
tipo_pr       <- read.csv("datasetTipo.csv"         ,sep=";",stringsAsFactors = F,dec=",")
produto       <- read.csv("datasetProdutos.csv"     ,sep=";",stringsAsFactors = F,dec=",")
pedFilial3    <- read.csv("pedido Fl03.csv"         ,sep=";",stringsAsFactors = F,dec=",")
cidade        <- read.csv(paste0("datasetPrePedido",id_pedido,".csv"),sep=";",stringsAsFactors = F,dec=",")
pedFilial3$qtd<-0

# funde datasets
curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))
curvaEstoque <- merge(curvaEstoque,tipoProduto,by.x = c("COD_INTERNO","COD_CLIENTE.x"),by.y=c("codigo","COD_CLIENTE"))

# trata estoque Cidade
custoCidade <- estoqueCidade[estoqueCidade$COD_FILIAL_CLIENTE==1,c(3,6)]

# filtra linhas com estoque maior que zero ou que já tiveram vendas
curvaEstoque <- curvaEstoque[curvaEstoque$valor_total>0 | !is.na(curvaEstoque$descricao.y),]

##########################################
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
curvaEstoque  <- rbind(curvaEstoque1, curvaEstoque2)
curvaGeral    <- ddply(curvaEstoque
                       ,.(COD_INTERNO, COD_CLIENTE.x, descricao.x, laboratorio.x)
                       ,summarize
                       ,estoque_geral=sum(qtd)
                       ,venda_geral=sum(qtd_venda))
curvaFusao <- merge(curvaGeral,curvaEstoque1[c("COD_INTERNO","unitario","cobertura_semanas","qtd","qtd_venda")],all.x=TRUE)
names(curvaFusao)[3]  <- "DESCRICAO"
names(curvaFusao)[4]  <- "LABORATORIO"
names(curvaFusao)[5]  <- "ESTOQUE_GERAL"
names(curvaFusao)[6]  <- "VENDA_GERAL"
names(curvaFusao)[7]  <- "CUSTO_FL1"
names(curvaFusao)[8]  <- "COBERTURA_FL1"
names(curvaFusao)[9]  <- "QTD_FL1"
names(curvaFusao)[10] <- "VENDA_FL1"
curvaFusao <- merge(curvaFusao,curvaEstoque2[c("COD_INTERNO","unitario","cobertura_semanas","qtd","qtd_venda")],all.x=TRUE)
names(curvaFusao)[11] <- "CUSTO_FL2"
names(curvaFusao)[12] <- "COBERTURA_FL2"
names(curvaFusao)[13] <- "QTD_FL2"
names(curvaFusao)[14] <- "VENDA_FL2"

# funde pedido da filial 3
curvaFusao <- merge(curvaFusao,pedFilial3,by.x = "COD_INTERNO",by.y = "codigo",all.x=TRUE)
names(curvaFusao)[15] <- "RESERVA_FL3"

# joga zero para os campos de quantidade NA e negativos
curvaFusao[is.na(curvaFusao$QTD_FL1),"QTD_FL1"]         <- 0
curvaFusao[is.na(curvaFusao$QTD_FL2),"QTD_FL2"]         <- 0
curvaFusao[curvaFusao$QTD_FL1<0,"QTD_FL1"]              <- 0
curvaFusao[curvaFusao$QTD_FL2<0,"QTD_FL2"]              <- 0
curvaFusao[is.na(curvaFusao$VENDA_FL1),"VENDA_FL1"]     <- 0
curvaFusao[is.na(curvaFusao$VENDA_FL2),"VENDA_FL2"]     <- 0
curvaFusao[is.na(curvaFusao$RESERVA_FL3),"RESERVA_FL3"] <- 0

# preenche itens sem cobertura com cobertura máxima da série
max_cobertura <- max(rbind(max(curvaFusao$COBERTURA_FL1[!is.na(curvaFusao$COBERTURA_FL1)])
                           ,max(curvaFusao$COBERTURA_FL2[!is.na(curvaFusao$COBERTURA_FL2)])))
curvaFusao$COBERTURA_FL1[is.na(curvaFusao$COBERTURA_FL1)] <- max_cobertura
curvaFusao$COBERTURA_FL2[is.na(curvaFusao$COBERTURA_FL2)] <- max_cobertura

# calcula reserva de quantidades para filiais, sem arredondamento
curvaFusao$RESERVA_FL1 <- (curvaFusao$QTD_FL1/curvaFusao$COBERTURA_FL1)*cobertura_min_semanas
curvaFusao$RESERVA_FL2 <- (curvaFusao$QTD_FL2/curvaFusao$COBERTURA_FL2)*cobertura_min_semanas
curvaFusao$RESERVA_FL1[is.nan(curvaFusao$RESERVA_FL1)]<- 0
curvaFusao$RESERVA_FL2[is.nan(curvaFusao$RESERVA_FL2)]<- 0


# valores entre 0 e 1 reservam 1 unidade
curvaFusao$RESERVA_FL1[sign(1-curvaFusao$RESERVA_FL1)>0]<-1
curvaFusao$RESERVA_FL2[sign(1-curvaFusao$RESERVA_FL2)>0]<-1

# valores maiores que 1 arredondam para baixo a reserva
curvaFusao$RESERVA_FL1[sign(1-curvaFusao$RESERVA_FL1)<0]<-floor(curvaFusao$RESERVA_FL1[sign(1-curvaFusao$RESERVA_FL1)<0])
curvaFusao$RESERVA_FL2[sign(1-curvaFusao$RESERVA_FL2)<0]<-floor(curvaFusao$RESERVA_FL2[sign(1-curvaFusao$RESERVA_FL2)<0])

# se venda foi zero nas duas lojas, reserva 1 unidade em alguma
temEstoque_FL1    <- curvaFusao$QTD_FL1>0
temEstoque_FL2    <- curvaFusao$QTD_FL2>0
temVenda_FL1      <- curvaFusao$VENDA_FL1>0
temVenda_FL2      <- curvaFusao$VENDA_FL2>0

curvaFusao$RESERVA_FL1[!temVenda_FL1] <- 0
curvaFusao$RESERVA_FL2[!temVenda_FL2] <- 0
curvaFusao$RESERVA_FL3<-0  # lógica para filial 3 ainda não OK

curvaFusao$RESERVA_FL1[!temVenda_FL1&!temVenda_FL2&temEstoque_FL1] <- 1
curvaFusao$RESERVA_FL2[!temVenda_FL1&!temVenda_FL2&!temEstoque_FL1&temEstoque_FL2] <- 1

#########################################
# calcula quantidade atendida para Cidade
# cruza prePedido com EANs
prePedido               <- merge(EANScidade,cidade,by.x=c("CODIGO_EAN"),by.y=c("EAN"))
# funde pedido da Cidade
pedido                  <- merge(prePedido[c("COD_INTERNO","compra")],curvaFusao,by.y=c("COD_INTERNO"),by.x = ("COD_INTERNO"),all.x=TRUE)
names(pedido)[2]        <- "QTD_PED_CIDADE"
#names(pedido)[3]     <- "VLR_PED_CIDADE"
#names(pedido)[4]     <- "PRECO_COMPRA"
#pedido$PRECO_COMPRA  <- pedido$VLR_PED_CIDADE/pedido$QTD_PED_CIDADE
pedido$ESTOQUE_GERAL[is.na(pedido$ESTOQUE_GERAL)]<-0

# calcula quantidade a vender para cidade
for (i in 1:nrow(pedido)){
      
      estoque_disponivel <- pedido$ESTOQUE_GERAL[i] - pedido$RESERVA_FL1[i] - pedido$RESERVA_FL2[i] - pedido$RESERVA_FL3[i]
      
      if (is.na(estoque_disponivel)){estoque_disponivel<-0}
      
      if (pedido$QTD_PED_CIDADE[i] >= estoque_disponivel){
            qtd_pedido <- estoque_disponivel
      } else {
            qtd_pedido <- pedido$QTD_PED_CIDADE[i]
      }
      
      pedido$VENDA_CIDADE[i]     <- qtd_pedido

      # debita pedido do estoque das filiais preservando quantidades reservadas
      pedido$VENDA_CIDADE_FL1[i] <- ifelse(pedido$QTD_FL1[i]-pedido$RESERVA_FL1[i]>=qtd_pedido 
                                           ,saida <- qtd_pedido
                                           ,saida <- pedido$QTD_FL1[i]-pedido$RESERVA_FL1[i])

      qtd_pedido <- qtd_pedido - pedido$VENDA_CIDADE_FL1[i]
      
      pedido$VENDA_CIDADE_FL2[i] <- ifelse(pedido$QTD_FL2[i]-pedido$RESERVA_FL2[i]>=qtd_pedido 
                                          ,saida <- qtd_pedido
                                          ,saida <- pedido$QTD_FL2[i]-pedido$RESERVA_FL2[i])
      
      
      
      
}

# calcula valor venda e joga zero nos campos NA de venda e custo 
pedido$VENDA_CIDADE[(pedido$VENDA_CIDADE<0)]            <- 0
pedido$VENDA_CIDADE[is.na(pedido$VENDA_CIDADE)]         <- 0
pedido$VENDA_CIDADE_FL1[(pedido$VENDA_CIDADE_FL1<0)]    <- 0 
pedido$VENDA_CIDADE_FL1[is.na(pedido$VENDA_CIDADE_FL1)] <- 0 
pedido$VENDA_CIDADE_FL2[(pedido$VENDA_CIDADE_FL2<0)]    <- 0 
pedido$VENDA_CIDADE_FL2[is.na(pedido$VENDA_CIDADE_FL2)] <- 0 
pedido$VALOR_VENDA_FL1   <- pedido$VENDA_CIDADE_FL1*pedido$CUSTO_FL1
pedido$VALOR_VENDA_FL2   <- pedido$VENDA_CIDADE_FL2*pedido$CUSTO_FL2
pedido$VALOR_VENDA_GERAL <- pedido$VALOR_VENDA_FL1+pedido$VALOR_VENDA_FL2
pedido$VALOR_VENDA_FL1[is.na(pedido$VALOR_VENDA_FL1)]     <- 0
pedido$VALOR_VENDA_FL2[is.na(pedido$VALOR_VENDA_FL2)]     <- 0
pedido$VALOR_VENDA_GERAL[is.na(pedido$VALOR_VENDA_GERAL)] <- 0
pedido$CUSTO_FL1[is.na(pedido$CUSTO_FL1)]                 <- 0
pedido$CUSTO_FL2[is.na(pedido$CUSTO_FL2)]                 <- 0

# cola o custo Cidade no final do dataset
pedido <- merge(pedido,custoCidade,all.x=TRUE)
names(pedido)[25]   <- "CUSTO_CIDADE"
pedido$VALOR_PEDIDO <- pedido$CUSTO_CIDADE*pedido$VENDA_CIDADE*0.95
pedido$CUSTO_PEDIDO <- (pedido$CUSTO_FL1*pedido$VENDA_CIDADE_FL1)+(pedido$CUSTO_FL2*pedido$VENDA_CIDADE_FL2)


pedidoF1 <- pedido[(pedido$VENDA_CIDADE_FL1>0), c(1:5,8:10 ,17,20,22)]
pedidoF2 <- pedido[(pedido$VENDA_CIDADE_FL2>0), c(1:5,12:14,18,21,23)]

write.csv2(file=paste0("PedidoCidade",id_pedido,"-COMPLETO.csv"),pedido,row.names=FALSE)
write.csv2(file=paste0("PedidoCidade",id_pedido,"-F01.csv"),pedidoF1,row.names=FALSE)
write.csv2(file=paste0("PedidoCidade",id_pedido,"-F02.csv"),pedidoF2,row.names=FALSE)
