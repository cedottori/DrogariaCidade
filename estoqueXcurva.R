library(plyr)
library(reshape)
library(data.table)
library(fst)
source("XL7.R")

# PARAMETROS GERAIS
DATA                <- "20180128"
#USER                <- "cedot"
USER                <- "cedot"
#USER                <- "cdottori"
DATA_INICIAL        <- "20170828"  #in?cio das opera??es para c?lculo espec?fico
COD_CLIENTE         <- 2
SEMANAS_VENDA       <- 4
data_minimo_demanda <- "" # data do arquivo de bloqueios - deixado sem data para ser sempre fixo

# seta diret?rio
working_dir <- ifelse (USER=="cedot"
                      ,saida<- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/"
                      ,ifelse(USER=="lgiar"
                             ,saida <- "C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/"
                             ,ifelse(USER=="cdottori"
                             ,saida <- "C:/Users/cdottori/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")))
               
setwd(working_dir)

# processa compras (diret?rio diferente)
#__________revisado
#source("processaCompras.R")
#dadosCompra <- XL7_Processa_compra(cliente=COD_CLIENTE,puser=USER,dia=DATA)
#histCompra  <- read.csv2(paste0("C:/Users/",USER,"/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_historica/datasetCompra.csv"),sep=";",stringsAsFactors = FALSE)

#__________revisado
# seta diret?rio
if (USER=="cedot"){
      setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
} else {
      setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")      
}

# chama scripts para tratamento dos arquivos
source("insereProdutosSKUs.R")
source("importaTipoProduto.R")
#__________revisado
source("carregaCurva4.R")
demanda2 <- carregaCurva(data = DATA,puser=USER, data_inicial=DATA_INICIAL,pNumeroSemanasVenda=SEMANAS_VENDA)

#__________revisado
source("importaEstoque2.R")
carregaEstoque(cod_cliente=COD_CLIENTE, pmascara_arquivo=DATA)
#__________revisado

# ______revisado
# cruza dataset inventario, curva ABC e classe Site PV
curv     <- read.csv(paste0("datasetCurva",DATA,".csv"),sep=";",stringsAsFactors = F,dec=",")
stok     <- read.csv(paste0("datasetEstoque",DATA,".csv"),sep=";",stringsAsFactors = F,dec=",")
tipo_pr  <- read.csv("datasetTipo.csv",sep=";",stringsAsFactors = F,dec=",")
produto  <- read.csv("datasetProdutos.csv",sep=";",stringsAsFactors = F,dec=",")
#fracoes  <- read.csv(paste0("PRODUTOS fracionados.csv"),sep=";",stringsAsFactors = FALSE)
#names(fracoes)[11]<-"FRACAO"

# ______revisado
# busca SKUs e PRODUTOS da base
#sqlString   <-           paste0("select P.*,PC.COD_CLIENTE,PA.PRINCIPIO_ATIVO from PRINCIPIOS_ATIVOS PA, ASSOCIACOES A, PRINC_ATIV_ASSOCS PAA, PRODUTOS P, PRODUTOS_CLIENTES PC ")
#sqlString   <- paste0(sqlString,"where PAA.COD_ASSOCIACAO = A.COD_ASSOCIACAO and PAA.COD_PRINCIPIO_ATIVO=PA.COD_PRINCIPIO_ATIVO ")
#sqlString   <- paste0(sqlString,"  and P.COD_ASSOCIACAO = A.COD_ASSOCIACAO and PC.COD_PRODUTO = P.COD_PRODUTO")
#mydb        <- dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
#PRINCIPIOS  <- dbGetQuery(mydb,sqlString)
#dbDisconnect(mydb)

# ______revisado
# funde datasets
curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))
curvaEstoque <- merge(curvaEstoque,tipoProduto,by.x = c("COD_INTERNO","COD_CLIENTE"),by.y=c("codigo","COD_CLIENTE"))

# ______revisado
# filtra linhas com estoque maior que zero ou que j? tiveram vendas
curvaEstoque <- curvaEstoque[curvaEstoque$valor_total!=0 | !is.na(curvaEstoque$descricao.x),]

# ______revisado
# preenche valores e calcula novas colunas
curvaEstoque$qtd_venda[is.na(curvaEstoque$qtd_venda)]<-0
curvaEstoque$total[is.na(curvaEstoque$total)]<-0

# ______revisado
# grava cobertura equivalente ao quantil 97.5 (2 dp) para NA e infinito (itens sem venda mas com estoque/coberturas muito altas)
maxRef <- quantile(curvaEstoque$cobertura_semanas[!is.infinite(curvaEstoque$cobertura_semanas)],0.975,na.rm = TRUE)
curvaEstoque$cobertura_semanas[is.na(curvaEstoque$cobertura_semanas) | is.infinite(curvaEstoque$cobertura_semanas)]<- maxRef[[1]]
curvaEstoque$cobertura_ponderada[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)] <- curvaEstoque$valor_total[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)]*curvaEstoque$cobertura_semanas[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)]

# ______revisado
# faixa de custo
curvaEstoque$faixa_custo <- ifelse(curvaEstoque$unitario<50,saida<-"R$ 0 -30",
                                   ifelse(curvaEstoque$unitario<100,saida<-"R$ 30 - 80"
                                                        ,saida<-"R$ 80 +"))
curvaEstoque$cond_custo_medio <- 1-(curvaEstoque$unitario / curvaEstoque$pr_custo)


# ______revisado
# faixa de condi??o (espec?fica para ?ticos)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="?ticos      "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="?ticos      "]       <0.1,saida<-"10% - "
            ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="?ticos      "]<0.2,saida<-"10% a 20%"
                                                                                                   ,saida<-"20% +"))

# ______revisado
# faixa de condi??o (espec?fica para gen?ricos)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="Gen?ricos   "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Gen?ricos   "]       <0.25,saida<-"25% - "
             ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Gen?ricos   "]<0.5,saida<-"25% a 50%"
                     ,saida<-"50% +"))
# ______revisado
# faixa de condi??o (espec?fica para similares)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "]       <0.25,saida<-"25% - "
             ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "]<0.5,saida<-"25% a 50%"
                     ,saida<-"50% +"))

curvaEstoque$sem_venda   <- curvaEstoque$qtd_venda==0                                                              
curvaEstoque$sem_estoque <- curvaEstoque$qtd==0
write.csv2(file="curvaEstoqueFilial.csv",curvaEstoque,row.names = F)

# verifica itens que est?o no estoque e n?o apareceram no curva estoque, grava arquivo com a diferen?a
cv2   <- curvaEstoque[,c("COD_FILIAL_CLIENTE","COD_INTERNO")]
stok2 <- stok[,c("COD_FILIAL_CLIENTE","COD_INTERNO")]
diferenca <- setdiff(stok2,cv2)
write.csv2(file="diferenca.csv",diferenca)

#########################################

#####  RODAR AT? AQUI ###################

#########################################


#__________revisar
# cria campos de estoque, venda e cobertura para as demais filiais
source("minimoDemanda.R")
carregaMinimoDemanda(pdata=data_minimo_demanda,puser=USER,pcliente=COD_CLIENTE)

bloqueio <- read.csv(paste0("datasetBloqueios",data_minimo_demanda,".csv"),sep=";",stringsAsFactors = F,dec=",")

curvaQuebra     <- curvaEstoque[,c("COD_INTERNO","COD_FILIAL_CLIENTE","qtd","qtd_venda","cobertura_semanas")]
curvaSplitGeral <- split(curvaEstoque[,c("COD_INTERNO","qtd","qtd_venda","cobertura_semanas")],curvaEstoque$COD_FILIAL_CLIENTE)

curvaSplit1 <- curvaEstoque[curvaEstoque$COD_FILIAL_CLIENTE==1,c("COD_INTERNO","qtd","qtd_venda","cobertura_semanas")]
curvaSplit2 <- curvaEstoque[curvaEstoque$COD_FILIAL_CLIENTE==2,c("COD_INTERNO","qtd","qtd_venda","cobertura_semanas")]
curvaSplit3 <- curvaEstoque[curvaEstoque$COD_FILIAL_CLIENTE==3,c("COD_INTERNO","qtd","qtd_venda","cobertura_semanas")]
names(curvaSplit1)[c(2,3,4)]<-c("est_fl1","venda_fl1","cobert_sem_fl1")
names(curvaSplit2)[c(2,3,4)]<-c("est_fl2","venda_fl2","cobert_sem_fl2")
names(curvaSplit3)[c(2,3,4)]<-c("est_fl3","venda_fl3","cobert_sem_fl3")

novasColunas <- data.frame(unique(curvaEstoque$COD_INTERNO))
names(novasColunas) <- c("COD_INTERNO")
novasColunas <- merge(novasColunas,curvaSplit1,all.x=TRUE)
novasColunas <- merge(novasColunas,curvaSplit2,all.x=TRUE)
novasColunas <- merge(novasColunas,curvaSplit3,all.x=TRUE)

## inclui colunas de demandas nas filiais e itens bloqueados
curvaEstoqueFinal <- merge(curvaEstoque,novasColunas,all.x = TRUE)
curvaEstoqueFinal <- merge(curvaEstoqueFinal,bloqueio[c(2,3,5,6,7)],by.x=c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("COD_FILIAL_CLIENTE","COD_INTERNO"),all.x=TRUE)

## inclui colunas da melhor e ?ltima compra, fra??o e demanda para 3 e 6 semanas
curvaEstoqueFinal <- merge(curvaEstoqueFinal,dadosCompra,by.x="COD_INTERNO",by.y="codigo",all.x=TRUE)
curvaEstoqueFinal <- merge(curvaEstoqueFinal,fracoes[c(1,11)],by.x="COD_INTERNO",by.y="CODIGO",all.x=TRUE)
curvaEstoqueFinal <- merge(curvaEstoqueFinal,demanda2,by.x=c("COD_INTERNO","COD_FILIAL_CLIENTE"),by.y=c("codigo","filial"),all.x=TRUE)

## gera arquivo de sa?da
write.csv2(file="curvaEstoque.csv",curvaEstoqueFinal,row.names = F)

######################################################
######################################################
## CALCULA ?NDICE DE FALTAS E GERA ARQUIVO
######################################################
######################################################

# crit?rios: venda hist?rica >0, demanda >0, n?o est? bloqueado
skus_totais <- curvaEstoqueFinal[curvaEstoqueFinal$qtd_venda>0&curvaEstoqueFinal$dem>0&is.na(curvaEstoqueFinal$tipo_compra),]
skus_palha  <- curvaEstoqueFinal[curvaEstoqueFinal$qtd_venda==0|curvaEstoqueFinal$dem==0|!is.na(curvaEstoqueFinal$tipo_compra),]

totaisSKUs        <- ddply(skus_totais
                           ,.(GRUPO_PRINCIPAL,COD_FILIAL_CLIENTE)
                           ,summarize  
                           ,no_skus=length(qtd)
                           ,valor_estoque=sum(valor_total))

totaisSKUsZeradas <- ddply(skus_totais[skus_totais$qtd<=0,]
                           ,.(GRUPO_PRINCIPAL,COD_FILIAL_CLIENTE)
                           ,summarize  
                           ,no_sku_faltas=length(qtd)
                           ,valor_estoque=sum(valor_total))

indicador             <- merge(totaisSKUs,totaisSKUsZeradas,by.x=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE"),by.y=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE"))
indicador$perc_faltas <- round(indicador$no_sku_faltas/indicador$no_skus*100,1)

# por laborat?rio
totaisSKUs_lab        <- ddply(skus_totais
                           ,.(laboratorio.x)
                           ,summarize  
                           ,no_skus=length(qtd)
                           ,valor_estoque=sum(valor_total))

totaisSKUsZeradas_lab <- ddply(skus_totais[skus_totais$qtd<=0,]
                           ,.(laboratorio.x)
                           ,summarize  
                           ,no_sku_faltas=length(qtd)
                           ,valor_estoque=sum(valor_total))

indicador_lab             <- merge(totaisSKUs_lab,totaisSKUsZeradas_lab,by.x=c("laboratorio.x"),by.y=c("laboratorio.x"))
indicador_lab$perc_faltas_lab <- round(indicador_lab$no_sku_faltas/indicador_lab$no_skus*100,1)
indicador_lab <- arrange(indicador_lab,desc(no_sku_faltas))

# skus zeradas ordenadas por venda hist?rica
skus_zeradas <- arrange(skus_totais[skus_totais$qtd<=0,],desc(qtd_venda))

# indicadores de compras - retira compras de imobilizado e devolu??es drogaria cidade
histCompra <- histCompra[!(histCompra$descr_forn %in% c("- DELL COMPUTADORES DO BRASIL LTDA"
                                                        ,"- DROGARIA CIDADE LTDA"
                                                        ,"DROGARIA CIDADE LTDA"
                                                        ,"AUTOMATECH SISTEMAS DE AUTOMACAO LTDA")),]
histCompra$mes       <- as.numeric(substr(histCompra$dia,6,7))
histCompra$customedio[is.na(histCompra$customedio)] <-0
histCompra$total[is.na(histCompra$total)]           <-0

indicador_compraForn      <- cast(histCompra[c("mes","descr_forn","total")], descr_forn~mes, sum)
indicador_compraMes1_cm   <- cast(histCompra[c("mes","customedio")]        , ~mes          , sum)

# gera arquivo de sa?da
saida <- list(NULL)
saida[[3]] <- c(" "," HIST?RICO DE COMPRAS MENSAL E POR DISTRIBUIDOR "," ")
#saida[[4]] <- indicador_compraForn
saida[[5]] <- indicador_compraMes1_cm
saida[[1]] <- c(" RELAT?RIO DE COMPRAS E SKUS ATIVAS - demanda>0, n?o bloqueadas, venda hist?rica>0 "," "," FALTAS POR FILIAL "," ")
saida[[2]] <- indicador
saida[[6]] <- c(" "," FALTAS POR LABORAT?RIO - ordenado por skus totais em falta"," ")
saida[[7]] <- indicador_lab
saida[[8]] <- c(" "," SKUS EM FALTAS - ordenadas por maior venda hist?rica "," ")
saida[[9]] <- skus_zeradas

file.remove("indicadorEstoque.csv")

lapply(saida
      ,function(x) write.table( data.frame(x)
                               ,'indicadorEstoque.csv'  
                               ,append= TRUE
                               ,sep=';'
                               ,dec=","
                               ,row.names = FALSE))
