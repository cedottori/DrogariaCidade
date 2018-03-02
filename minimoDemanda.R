carregaMinimoDemanda <- function(pdata='',puser="cedot",pcliente=2){
####################################################################
##
##  TRATA ARQUIVO MINIMO DEMANDA DE PRODUTOS SOFTPHARMA
##
##  CRIA UM DATASET LIMPO
##
####################################################################
#print(puser)
library(dplyr)
#########################################
# ler arquivo como um vetor de caracteres
print("lendo arquivo de mínimo demanda")
estoque  <- readLines(paste0("MINIMO DEMANDA",pdata,".txt"))
print(paste0("lidas ",length(estoque)," linhas de minimo demanda"))

minimoDemanda        <- data.frame(COD_CLIENTE        =rep(0,length(estoque))
                                  ,COD_FILIAL_CLIENTE =rep(0,length(estoque))
                                  ,COD_INTERNO        =rep(0,length(estoque))
                                  ,descricao          =rep("DUMMY",length(estoque))             
                                  ,ultima_compra      =rep(as.Date("20160601","%Y%m%d"),length(estoque))
                                  ,ultima_venda       =rep(as.Date("20160601","%Y%m%d"),length(estoque))
                                  ,tipo_compra        =rep(0,length(estoque))
                                  ,stringsAsFactors   =FALSE)             

filial  <- 1
first_i <- NULL
j       <- 1

print("quebrando arquivo de mínimo demanda")
######################################
## interpreta arquivo e gera dataframe
for (i in 1:length(estoque)){
      
      ## identifica linha dataset
      criterioLinha <- !is.na(as.numeric(substr(estoque[i],1,6)))&&!is.na(as.numeric(substr(estoque[i],71,75)))
      
      if (criterioLinha){
            ## não é nulo, portanto produto válido
            minimoDemanda$COD_CLIENTE[j]        <- pcliente
            minimoDemanda$COD_FILIAL_CLIENTE[j] <- filial
            minimoDemanda$COD_INTERNO[j]        <- as.numeric(substr(estoque[i],1,6))          
            minimoDemanda$descricao[j]          <- substr(estoque[i],8,37)
            minimoDemanda$ultima_compra[j]      <- as.Date(substr(estoque[i],93 ,100),"%d/%m/%y")
            minimoDemanda$ultima_venda[j]       <- as.Date(substr(estoque[i],103,110),"%d/%m/%y")
            minimoDemanda$tipo_compra[j]        <- substr(estoque[i],118,120)

            j <- j + 1
            ## identifica linha filial
      } else if (length(grep("FILIAL...:",estoque[i]))!=0) {
            
            filial <- as.numeric(substr(estoque[i],12,13))
            print(filial)
            
      } 
      # mensagem de processamento
      if(i%%1000==0){
            print(paste0("processadas ",i," linhas"))
      }
      
} ## for

minimoDemanda <- minimoDemanda[minimoDemanda$descricao!="DUMMY",]  

########################
# grava arquivo de saída
print("gerando arquivo de saída")
write.csv2(file=paste0("datasetBloqueios",pdata,".csv"),data.frame(minimoDemanda),row.names = FALSE)
}