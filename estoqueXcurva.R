library(data.table)
library(fst)

# PARAMETROS GERAIS
DATA                <- "20180302"
#USER               <- "cedot"
USER                <- "cedot"
#USER               <- "cdottori"
DATA_INICIAL        <- "20170828"  #in?cio das opera??es para c?lculo espec?fico
COD_CLIENTE         <- 2
SEMANAS_VENDA       <- 26 ## quantidade de semanas no histórico de vendas
data_minimo_demanda <- "" # data do arquivo de bloqueios - deixado sem data para ser sempre fixo

# seta diretorio
working_dir <- ifelse (USER=="cedot"
                      ,saida<- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/"
                      ,ifelse(USER=="lgiar"
                             ,saida <- "C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/"
                             ,ifelse(USER=="cdottori"
                             ,saida <- "C:/Users/cdottori/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")))
               
setwd(working_dir)
source("XL7.R")

# seta diretorio
if (USER=="cedot"){
      setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
} else {
      setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")      
}

# chama scripts para tratamento dos arquivos
source("C:/Users/cedot/documents/DOTTORI/DrogariaCidade/insereProdutosSKUs.R")
source("C:/Users/cedot/documents/DOTTORI/DrogariaCidade/importaTipoProduto.R")

source("C:/Users/cedot/documents/DOTTORI/DrogariaCidade/carregaCurva4.R")
demanda2 <- carregaCurva(data = DATA,puser=USER, data_inicial=DATA_INICIAL,pNumeroSemanasVenda=SEMANAS_VENDA)

source("C:/Users/cedot/documents/DOTTORI/DrogariaCidade/importaEstoque2.R")
carregaEstoque(cod_cliente=COD_CLIENTE, pmascara_arquivo=DATA)

# cria campos de estoque, venda e cobertura para as demais filiais
source("C:/Users/cedot/documents/DOTTORI/DrogariaCidade/minimoDemanda.R")
carregaMinimoDemanda(pdata=data_minimo_demanda,puser=USER,pcliente=COD_CLIENTE)

# cruza dataset inventario, curva ABC e classe Site PV
curv     <- setDT(read.csv(paste0("datasetCurva",DATA,".csv"),sep=";",stringsAsFactors = F,dec=","))
stok     <- setDT(read.csv(paste0("datasetEstoque",DATA,".csv"),sep=";",stringsAsFactors = F,dec=","))
tipo_pr  <- setDT(read.csv("datasetTipo.csv",sep=";",stringsAsFactors = F,dec=","))
produto  <- setDT(read_fst("datasetProdutos.csv"))

tipo_pr[,COD_TIPO_PRODUTO:=as.numeric(COD_TIPO_PRODUTO)]

# funde datasets
curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))
curvaEstoque <- merge(curvaEstoque,tipoProduto,by.x = c("COD_INTERNO","COD_CLIENTE"),by.y=c("codigo","COD_CLIENTE"))

# filtra linhas com estoque maior que zero ou que j? tiveram vendas
curvaEstoque <- curvaEstoque[curvaEstoque$valor_total!=0 | !is.na(curvaEstoque$descricao.x),]

# preenche valores e calcula novas colunas
curvaEstoque$qtd_venda[is.na(curvaEstoque$qtd_venda)]<-0
curvaEstoque$total[is.na(curvaEstoque$total)]<-0

# grava cobertura equivalente ao quantil 97.5 (2 dp) para NA e infinito (itens sem venda mas com estoque/coberturas muito altas)
maxRef <- quantile(curvaEstoque$cobertura_semanas[!is.infinite(curvaEstoque$cobertura_semanas)],0.975,na.rm = TRUE)
curvaEstoque$cobertura_semanas[is.na(curvaEstoque$cobertura_semanas) | is.infinite(curvaEstoque$cobertura_semanas)]<- maxRef[[1]]
curvaEstoque$cobertura_ponderada[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)] <- curvaEstoque$valor_total[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)]*curvaEstoque$cobertura_semanas[is.na(curvaEstoque$cobertura_ponderada) | is.infinite(curvaEstoque$cobertura_ponderada)]
curvaEstoque[qtd_venda==0&qtd==0,cobertura_semanas:=0]

# faixa de custo
curvaEstoque$faixa_custo      <- ifelse(curvaEstoque$unitario<50,saida<-"R$ 0-30",
                                        ifelse(curvaEstoque$unitario<100,saida<-"R$ 30-80"
                                                                        ,saida<-"R$ 80+"))
curvaEstoque$cond_custo_medio <- 1-(curvaEstoque$unitario / curvaEstoque$pr_custo)

# faixa de condi??o (especifica para ?ticos)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="Éticos      "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Éticos      "]       <0.1,saida<-"< 10%"
            ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Éticos      "]<0.2,saida<-"10% a 20%"
                                                                                                   ,saida<-"> 20%"))

# faixa de condicao (especifica para genericos)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="Genéricos   "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Genéricos   "]       <0.25,saida<-"< 25%"
             ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Genéricos   "]<0.5,saida<-"25% a 50%"
                     ,saida<-"> 50%"))

# faixa de condicao (especifica para similares)
curvaEstoque$faixa_cond_entrada[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "] <- 
      ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "]       <0.25,saida<-"25% - "
             ,ifelse(curvaEstoque$cond_custo_medio[curvaEstoque$GRUPO_PRINCIPAL=="Similares   "]<0.5,saida<-"25% a 50%"
                     ,saida<-"50% +"))

curvaEstoque$sem_venda   <- curvaEstoque$qtd_venda==0                                                              
curvaEstoque$sem_estoque <- curvaEstoque$qtd==0

# verifica itens que estao no estoque e nao apareceram no curva estoque, grava arquivo com a diferente
cv2   <- curvaEstoque[,c("COD_FILIAL_CLIENTE","COD_INTERNO")]
stok2 <- stok[,c("COD_FILIAL_CLIENTE","COD_INTERNO")]
diferenca <- setdiff(stok2,cv2)
write.csv2(file="diferenca.csv",diferenca)

bloqueio <- setDT(read.csv(paste0("datasetBloqueios",data_minimo_demanda,".csv"),sep=";",stringsAsFactors = F,dec=","))

curvaSplitGeral <- split(curvaEstoque[,c("COD_INTERNO","qtd","qtd_venda","cobertura_semanas")],
                         curvaEstoque$COD_FILIAL_CLIENTE)

curvaEstoque <- merge(curvaEstoque,bloqueio,by=c("COD_INTERNO", "COD_CLIENTE", "COD_FILIAL_CLIENTE"),all.x=T)

# grava arquivo geral
write.csv2(curvaEstoque,file="curvaEstoqueFilial.csv",row.names = F)

# criterios: venda historica >0, demanda >0, nao esta bloqueado
skus_totais <- curvaEstoque[qtd_venda>0&dem>0&is.na(tipo_compra),]
skus_palha  <- curvaEstoque[qtd_venda==0|dem==0|!is.na(tipo_compra),]

totaisSKUs        <- skus_totais[,{numero_skus=length(pr_custo)
                                   vlr_estoque=sum(valor_total)
                                   list(no_skus=numero_skus,valor_estoque=vlr_estoque)
                                   },by=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE")]

totaisSKUsZeradas <- skus_totais[qtd<=0,
                                 {numero_skus=length(pr_custo)
                                  vlr_estoque=sum(valor_total)
                                  list(no_skus_faltas=numero_skus,valor_estoque_faltas=vlr_estoque)
                                  },by=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE")]

indicador             <- setDT(merge(totaisSKUs,totaisSKUsZeradas,
                               by.x=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE"),
                               by.y=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE")))

indicador[,perc_faltas:=round((no_skus_faltas/no_skus)*100,2)]

# por laboratorio
totaisSKUs_lab    <- skus_totais[,{numero_skus=length(pr_custo)
                                 vlr_estoque=sum(valor_total)
                                 list(no_skus=numero_skus,valor_estoque=vlr_estoque)
                                 },by=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE","laboratorio.x")]

totaisSKUsZeradas_lab <- skus_totais[qtd<=0,
                                 {numero_skus=length(pr_custo)
                                 vlr_estoque=sum(valor_total)
                                 list(no_skus_faltas=numero_skus,valor_estoque_faltas=vlr_estoque)
                                 },by=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE","laboratorio.x")]

indicador_lab         <- setDT(merge(totaisSKUs_lab,totaisSKUsZeradas_lab,
                                     by.x=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE","laboratorio.x"),
                                     by.y=c("GRUPO_PRINCIPAL","COD_FILIAL_CLIENTE","laboratorio.x")))

indicador_lab[,perc_faltas_lab:=round((no_skus_faltas/no_skus)*100,2)]

#                  __________________________________________
# REVISADO ATÉ AQUI__________________________________________
#                  __________________________________________

# skus zeradas ordenadas por venda historica
skus_zeradas <- skus_totais[skus_totais$qtd<=0,][order(-total),]

# # indicadores de compras - retira compras de imobilizado e devolucoes drogaria cidade
# histCompra <- histCompra[!(histCompra$descr_forn %in% c("- DELL COMPUTADORES DO BRASIL LTDA"
#                                                         ,"- DROGARIA CIDADE LTDA"
#                                                         ,"DROGARIA CIDADE LTDA"
#                                                         ,"AUTOMATECH SISTEMAS DE AUTOMACAO LTDA")),]
# histCompra$mes       <- as.numeric(substr(histCompra$dia,6,7))
# histCompra$customedio[is.na(histCompra$customedio)] <-0
# histCompra$total[is.na(histCompra$total)]           <-0
# 
# indicador_compraForn      <- cast(histCompra[c("mes","descr_forn","total")], descr_forn~mes, sum)
# indicador_compraMes1_cm   <- cast(histCompra[c("mes","customedio")]        , ~mes          , sum)

# gera arquivo de sa?da
saida <- list(NULL)
# saida[[3]] <- c(" "," HIST?RICO DE COMPRAS MENSAL E POR DISTRIBUIDOR "," ")
#saida[[4]] <- indicador_compraForn
# saida[[5]] <- indicador_compraMes1_cm
saida[[1]] <- c(" RELATORIO DE COMPRAS E SKUS ATIVAS - demanda>0, nao bloqueadas, venda historica>0 "," "," FALTAS POR FILIAL "," ")
saida[[2]] <- indicador
saida[[6]] <- c(" "," FALTAS POR LABORATORIO - ordenado por skus totais em falta"," ")
saida[[7]] <- indicador_lab
saida[[8]] <- c(" "," SKUS EM FALTAS - ordenadas por maior venda historica "," ")
saida[[9]] <- skus_zeradas

lapply(saida
      ,function(x) write.table( data.frame(x)
                               ,paste0("indicadorEstoque",DATA,".csv")
                               ,append= TRUE
                               ,sep=';'
                               ,dec=","
                               ,row.names = FALSE))
