carregaPrePedido <- function(mascara_periodo="20160614"){
setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
#setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
source("XL7.R")
library(dplyr)
#################################################################################
##  TRATA ARQUIVO DE PRE-PEDIDO DE PRODUTOS SOFTPHARMA
##  CRIA UM DATASET LIMPO
##  CHAMA PROCEDIMENTO PARA CRIAÇÃO DE RELATÓRIO GERENCIAL/ANALÍTICO (EM CRIAÇÃO)
#################################################################################

# PARÂMETROS DE ENTRADA
cod_cliente     <- 2 # Desconto Popular

#########################################
# ler arquivo como um vetor de caracteres
print("lendo arquivo de pré-pedido")
arquivo  <- readLines(paste0("PEDIDO DE COMPRA-",mascara_periodo,".txt"))
print(paste0("lidas ",length(arquivo)," linhas de pedido"))

prePedido           <- data.frame(COD_CLIENTE        =rep(0,length(arquivo))
                                 ,COD_FILIAL_CLIENTE =rep(0,length(arquivo))
                                 ,EAN                =rep(0,length(arquivo))
                                 ,descricao          =rep("DUMMY",length(arquivo))             
                                 ,laboratorio        =rep("DUMMY",length(arquivo))             
                                 ,cl                 =rep(0,length(arquivo))
                                 ,min                =rep(0,length(arquivo))
                                 ,dem                =rep(0,length(arquivo))
                                 ,est                =rep(0,length(arquivo))
                                 ,compra             =rep(0,length(arquivo))
                                 ,valor_compra       =rep(0,length(arquivo))
                                 ,stringsAsFactors   =FALSE)             

filial  <- 1
first_i <- NULL
j       <- 1

print(paste("quebrando arquivo de pre-pedido",paste0("PEDIDO DE COMPRA-",mascara_periodo,".txt")))
######################################
## interpreta arquivo e gera dataframe
for (i in 1:length(arquivo)){
      
      ## identifica linha dataset
      
      if (!is.na(as.numeric(substr(arquivo[i],9,21)))&&!is.na(as.numeric(substr(arquivo[i],90,95)))){
            ## não é nulo, portanto produto válido
            prePedido$COD_CLIENTE[j]        <- cod_cliente
            prePedido$COD_FILIAL_CLIENTE[j] <- filial
            prePedido$EAN[j]                <- as.numeric(substr(arquivo[i],9 ,21))
            prePedido$descricao[j]          <- substr(arquivo[i],24,55) 
            prePedido$laboratorio[j]        <- substr(arquivo[i],57,63)
            prePedido$cl[j]                 <- substr(arquivo[i],65,65)
            prePedido$min[j]                <- as.numeric(substr(arquivo[i],68,70))
            prePedido$dem[j]                <- as.numeric(substr(arquivo[i],72,74))
            prePedido$est[j]                <- as.numeric(substr(arquivo[i],76,78))
            prePedido$compra[j]             <- as.numeric(substr(arquivo[i],83,85))
            prePedido$valor_compra[j]       <- as.numeric(substr(arquivo[i],88,95))

            j <- j + 1
            ## identifica linha filial
      } else if (length(grep("<<",arquivo[i]))!=0) {
            
            filial <- as.numeric(substr(arquivo[i],15,15))

      } else { 
            ## imprime cabecalhos
             #print(arquivo[i])
            
      }
      # mensagem de processamento
      if(i%%1000==0){
        print(paste0("processadas ",i," linhas"))
      }
      
} ## for

prePedido <- prePedido[prePedido$descricao!="DUMMY",]  

########################
# grava arquivo de saída
print("gerando arquivo de saída")
write.csv2(file=paste0("datasetPrePedido",mascara_periodo,".csv"),data.frame(prePedido),row.names = FALSE)
print("OK")
}
