carregaMovCompra <- function(nome_arquivo="",mascara_periodo="201705",cod_cliente=1,puser='lgiar'){

source("XL7.R")
library(dplyr)
#################################################################################
##  TRATA ARQUIVO DE MOVIMENTO DE COMPRA DE PRODUTOS SOFTPHARMA
##  CRIA UM DATASET LIMPO
#################################################################################


#########################################
# ler arquivo como um vetor de caracteres
print("lendo arquivo de movimento de compra")
mascara_filial <- ""
arquivo <- readLines(nome_arquivo)
print(paste0("lidas ",length(arquivo)," linhas de pedido"))

movCompra           <- data.frame(COD_CLIENTE        =rep(0,length(arquivo))
                                 ,FILIAL_DP          =rep(0,length(arquivo))
                                 ,COD_INTERNO        =rep(0,length(arquivo))
                                 ,DESCRICAO          =rep("DUMMY",length(arquivo))             
                                 ,QTD_COMPRA         =rep(0,length(arquivo))
                                 ,VALOR_COMPRA       =rep(0,length(arquivo))
                                 ,CUSTO_COMPRA       =rep(0,length(arquivo))
                                 ,stringsAsFactors   =FALSE)             

filial  <- 1
first_i <- NULL
j       <- 1

print(paste("quebrando arquivo de venda",nome_arquivo))
######################################
## interpreta arquivo e gera dataframe
for (i in 1:length(arquivo)){
      
      ## identifica linha dataset
      
      if (!is.na(as.numeric(substr(arquivo[i],1,6)))&&!is.na(as.numeric(substr(arquivo[i],72,80)))){
            ## não é nulo, portanto produto válido
            movCompra$COD_CLIENTE[j]        <- cod_cliente
            movCompra$FILIAL_DP[j]          <- filial
            movCompra$COD_INTERNO[j]        <- as.numeric(substr(arquivo[i],1,6))
            movCompra$DESCRICAO[j]          <- substr(arquivo[i],8,37) 
            movCompra$QTD_COMPRA[j]         <- as.numeric(substr(arquivo[i],95,99))
            movCompra$VALOR_COMPRA[j]       <- as.numeric(substr(arquivo[i],56,62))
            movCompra$CUSTO_COMPRA[j]       <- as.numeric(substr(arquivo[i],105,111))

            j <- j + 1

            ## identifica linha filial
      } else if (length(grep("FILIAL...",arquivo[i]))!=0) {
            
            filial <- as.numeric(substr(arquivo[i],12,13))
            
      } 
      # mensagem de processamento
      if(i%%100==0){
        print(paste0("processadas ",i," linhas"))
      }
      
} ## for

movCompra <- movCompra[movCompra$DESCRICAO!="DUMMY",]  

########################
# grava arquivo de saída
print("gerando arquivo de saída")
write.csv2(file=paste0("datasetVendaCliente-",mascara_periodo,".csv"),data.frame(movCompra),row.names = FALSE)
print("OK")
}
