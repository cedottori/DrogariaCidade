# returns string w/o leading or trailing whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
cod_cliente <- 1 # CIDADE
mascara_periodo <- "201606"
tipo_fatura <- "CUSTO_DP" # "CUSTO_CIDADE"
library(plyr)
library(gdata)
source("carregaMovimentoCompra.R")

# ler EANs
# busca SKUs e PRODUTOS da base
mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
PRODUTOS_EANS     <- dbGetQuery(mydb,paste0("select * from EANS_PRODUTOS E, PRODUTOS P WHERE E.PRODUTOS_COD_PRODUTO = P.COD_PRODUTO "))
PRODUTOS_CLIENTES <- dbGetQuery(mydb,paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cod_cliente))
tipo_pr <- read.csv("datasetTipo.csv",sep=";",stringsAsFactors = F,dec=",")
produto <- read.csv("datasetProdutos.csv",sep=";",stringsAsFactors = F,dec=",")
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))

# ler dataset onerosos EMS
ems          <- read.csv("C:/Users/cedot/OneDrive for Business/CONTROLADORIA/Créditos fornecedores/Lista de preços EMS.csv",sep=";",stringsAsFactors = FALSE)
ems_oneroso  <- ems[ems$DESC.FIN=="0%",c(1,2,3,7)]
ems_oner_ean <- merge(ems_oneroso,PRODUTOS_EANS    ,by.x ="EAN"        ,by.y="CODIGO_EAN")
ems_oner_ean <- merge(ems_oner_ean,PRODUTOS_CLIENTES,by.x ="COD_PRODUTO",by.y="COD_PRODUTO")

# tratamento compra Drogaria Cidade
#carregaMovCompra("compra_cidade/MOVIMENTACAO-CIDADE-GERAL-MAIO.txt","201605",puser="cedot")
#cidade5 <- read.csv("datasetVendaCliente-201605.csv",sep=";",stringsAsFactors = F,dec=",")
carregaMovCompra("compra_cidade/MOVIMENTACAO-CIDADE-GERAL-JUNHO2.txt",mascara_periodo,puser="cedot")
cidade5 <- read.csv("datasetVendaCliente-201606.csv",sep=";",stringsAsFactors = F,dec=",")

# FATURA DIFERENTES FORMA DEPENDENDO DA BASE
if (tipo_fatura=="CUSTO_CIDADE"){
      # carrega inventário base e filtra filial 1
      estoqueCidade <- read.csv("datasetEstoque20160524-CIDADE.csv",sep=";",stringsAsFactors = F,dec=",")
      estoqueCidade <- estoqueCidade[estoqueCidade$COD_FILIAL_CLIENTE==1,]
      
      # funde datasets e calcula valor da venda (95% do custo Cidade)
      fatura  <- merge(cidade5, estoqueCidade, by.x="COD_INTERNO",by.y="COD_INTERNO")
      fatura  <- merge(fatura, ems_oner_ean, by.x="COD_INTERNO",by.y="COD_INTERNO",all.x=TRUE)
      fatura  <- merge(fatura, tipoProduto[,c(4:9)], by.x = "COD_INTERNO",by.y="codigo",all.x=TRUE)
      fatura$oneroso <- !is.na(fatura$COD_PRODUTO)

      } else { if (tipo_fatura=="CUSTO_DP"){

      # carrega inventário base e filtra filial 1
      estoqueDP <- read.csv("datasetEstoque20160628.csv",sep=";",stringsAsFactors = F,dec=",")
      fatura    <- merge(cidade5, estoqueDP[estoqueDP$COD_FILIAL_CLIENTE==1, ], by.x="COD_INTERNO",by.y="COD_INTERNO")
      fatura    <- merge(fatura, ems_oner_ean, by.x="COD_INTERNO",by.y="COD_INTERNO",all.x=TRUE)
      fatura    <- merge(fatura, tipoProduto[,c(4:9)], by.x = "COD_INTERNO",by.y="codigo",all.x=TRUE)
      fatura$oneroso <- !is.na(fatura$COD_PRODUTO)
      
            } # if     
      } # else

fatura$valor_fatura <- fatura$VALOR_COMPRA*0.95

# separa vendas
fatura$laboratorio <- trim(fatura$laboratorio)
labGenericos <- c("MEDLEY","SANDOZ","EUROFARMA","EMS","MERCK SA","NEO QUIMICA")

faturaGenericos <- fatura[fatura$laboratorio %in% labGenericos,]
faturaEticos    <- fatura[!(fatura$laboratorio %in% labGenericos),]

# calcula totais por laboratórios / genéricos não
totalGenericos <- ddply(faturaGenericos
                       ,.(laboratorio
                          ,ifelse(oneroso==TRUE
                                  ,saida<-"sim"
                                  ,saida<-"não")
                          ,ifelse(is.na(GRUPO_PRINCIPAL)
                                  ,saida<-"Genéricos   "
                                  ,saida<-GRUPO_PRINCIPAL))
                       ,summarize
                       ,quantidade=sum(qtd)
                       ,custo=sum(VALOR_COMPRA)
                       ,fatura_95=sum(valor_fatura))

names(totalGenericos)[2] <- "oneroso"
names(totalGenericos)[3] <- "Grupo_principal"

totalEticos        <- data.frame("DIVERSOS", "não", "Éticos      ", sum(faturaEticos$QTD_COMPRA), sum(faturaEticos$VALOR_COMPRA),sum(faturaEticos$valor_fatura) )
names(totalEticos) <- names(totalGenericos)
totalGeral         <- rbind(totalGenericos,totalEticos)
write.csv2(file=paste0("fatura_cidade",mascara_periodo,".csv"),totalGeral,row.names = FALSE,quote = FALSE)

totalGeral

