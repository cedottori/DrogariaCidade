carregaCurva <- function(data='20160607',data_inicial='20160323',puser="lgiar"){
library(data.table)
####################################################################
##
##  TRATA ARQUIVO CURVA ABC DE PRODUTOS SOFTPHARMA
##
##  CRIA UM DATASET LIMPO
##
##  CHAMA PROCEDIMENTO PARA CRIAÇÃO DE RELATÓRIO GERENCIAL/ANALÍTICO (EM CRIAÇÃO)
##
####################################################################
if (puser=="cedot"){
      setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
} else {
      setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")      
}
source("carregaMovimentoCompra.R")
      
# PARAMETRO DE ENTRADA
numero_de_meses   <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial,"%Y%m%d"))/30
numero_de_semanas <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial,"%Y%m%d"))/7

# fator de ajuste futuro: crescimento de 30% / 
fatorAjusteFuturo <- 1.25^((numero_de_meses/2)+0.5)

# tratamento compra Drogaria Cidade
carregaMovCompra("compra_cidade/MOVIMENTACAO-CIDADE-GERAL-MAIO.txt","201605")
cidade5 <- read.csv("datasetVendaCliente-201605.csv",sep=";",stringsAsFactors = F,dec=",")

# ARQUIVOS DE SAIDA E ENTRADA
arq_entrada <- paste0("CURVA ABC-",data,".txt")
arq_saida   <- paste0("datasetCurva",data,".csv")

# ler arquivo
curva <- read.table(arq_entrada
                    ,header = FALSE
                    ,sep = ";"
                    ,encoding = "UTF-16"
                    ,stringsAsFactors = FALSE)

curvaItens <- data.frame(1,2,"descricao","lab",1,1,1,"class_giro",1,1,1,1,1,1,stringsAsFactors = FALSE)

names(curvaItens)<-c("filial","codigo","descricao","laboratorio","qtd_venda","custo_medio"
                     ,"total","class_giro","repr_perc","min","dem","est","calculo_minimo","estoque_demanda")

curvaTemp <- curvaItens
filial <- 1

j <- 1 

## i <- 20; filial <- 1

first_i <- NULL

## separa registros por tipo
############################
for (i in 1:nrow(curva)){
      
      
      ## identifica linha dataset
      if (!is.na(as.numeric(substr(curva[i,],1,6)))){
            ## não é nulo, portanto produto válido
            curvaTemp$filial[j]          <- filial
            curvaTemp$codigo[j]          <- as.numeric(substr(curva[i,],1 ,6))
            curvaTemp$descricao[j]       <- substr(curva[i,],8 ,38)
            curvaTemp$laboratorio[j]     <- substr(curva[i,],40,57) 
            curvaTemp$qtd_venda[j]       <- as.numeric(substr(curva[i,],59,63))
            curvaTemp$custo_medio[j]     <- as.numeric(substr(curva[i,],66,71))
            curvaTemp$total[j]           <- as.numeric(substr(curva[i,],73,81))
            curvaTemp$class_giro[j]      <- substr(curva[i,],83,88)
            curvaTemp$repr_perc[j]       <- as.numeric(substr(curva[i,],92,97))
            curvaTemp$min[j]             <- as.numeric(substr(curva[i,],100,102))
            curvaTemp$dem[j]             <- as.numeric(substr(curva[i,],104,107))
            curvaTemp$est[j]             <- as.numeric(substr(curva[i,],111,116))
            curvaTemp$calculo_minimo[j]  <- as.numeric(substr(curva[i,],120,124))
            curvaTemp$estoque_demanda[j] <- as.numeric(substr(curva[i,],127,132))
            
            
            if (!is.null(first_i)){
                  #print("segundo")
                  curvaItens <- rbind(curvaItens,curvaTemp[j,])
            } 
            
            if (is.null(first_i)){
                  #print("primeiro")
                  first_i <- 1
                  curvaItens[j,] <- curvaTemp[j,]
            } 
            
            
            ## identifica linha filial
      } else if (length(grep("<<",curva[i,]))!=0) {
            
            filial <-substr(curva[i,],12,12)
      } 
      
} ## for

# insere movimento Cidade
curvaItens <- merge(curvaItens,cidade5,by.x = c("filial","codigo"),by.y=c("FILIAL_DP","COD_INTERNO"),all.x=TRUE)
curvaItens$QTD_COMPRA[is.na(curvaItens$QTD_COMPRA)] <- 0
curvaItens$VALOR_COMPRA[is.na(curvaItens$VALOR_COMPRA)] <- 0

# subtrai movimento Cidade da venda geral
curvaItens$qtd_venda <- curvaItens$qtd_venda - curvaItens$QTD_COMPRA
curvaItens$total     <- curvaItens$total     - curvaItens$VALOR_COMPRA

## converte data table e cria coluna para cobertura com fator de ajuste de crescimento
curvaItensDT         <- data.table(curvaItens)
curvaItensDT[,cobertura_semanas:=est/((qtd_venda/numero_de_semanas)*fatorAjusteFuturo)]

# grava cobertura equivalente ao quantil 97.5 (2 dp) para NA e infinito (itens sem venda mas com estoque/coberturas muito altas)
maxRef <- quantile(curvaItensDT$cobertura_semanas[!is.infinite(curvaItensDT$cobertura_semanas)],0.975,na.rm = TRUE)
curvaItensDT$cobertura_semanas[is.na(curvaItensDT$cobertura_semanas) | is.infinite(curvaItensDT$cobertura_semanas)]<- maxRef[[1]]
curvaItensDT[,cobertura_ponderada:=custo_medio*cobertura_semanas]

# grava arquivo de saída
write.table(file=arq_saida,curvaItensDT,row.names = FALSE,dec=",",sep=";")
}