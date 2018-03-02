#setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
#################################################################################
##  TRATA ARQUIVO INVENTARIO DE PRODUTOS SOFTPHARMA
##  CRIA UM DATASET LIMPO
##  CHAMA PROCEDIMENTO PARA CRIA??O DE RELAT?RIO GERENCIAL/ANAL?TICO (EM CRIA??O)
#################################################################################

# PAR?METROS DE ENTRADA
#########################################
# ler arquivo como um vetor de caracteres
print("lendo arquivo de tipos de produtos")
tipoProduto  <- readLines("LISTAGEM DE TP.txt")
cod_cliente <- COD_CLIENTE

print(paste0("lidas ",length(tipoProduto)," linhas de estoque"))

tipo        <- data.frame(COD_CLIENTE        =rep(0,length(tipoProduto))
                         ,COD_TIPO_PRODUTO   =rep("DUMMY",length(tipoProduto))             
                         ,DESCRICAO          =rep("DUMMY",length(tipoProduto))             
                         ,GRUPO_PRINCIPAL    =rep("DUMMY",length(tipoProduto))             
                         ,stringsAsFactors   =FALSE)             

first_i <- NULL
j       <- 1

print("quebrando arquivo de invent?rio")
######################################
## interpreta arquivo e gera dataframe
for (i in 1:length(tipoProduto)){
      
      ## identifica linha dataset
      
      if (!is.na(as.numeric(substr(tipoProduto[i],1,5)))){
            ## n?o ? nulo, portanto produto v?lido
            tipo$COD_CLIENTE[j]        <- cod_cliente
            tipo$COD_TIPO_PRODUTO[j]   <- as.numeric(substr(tipoProduto[i],1 ,5))
            tipo$DESCRICAO[j]          <- substr(tipoProduto[i],7,37) 
            tipo$GRUPO_PRINCIPAL[j]    <- substr(tipoProduto[i],105,116)

            j <- j + 1
      }
      # mensagem de processamento
      if(i%%1000==0){
        print(paste0("processadas ",i," linhas"))
      }
      
} ## for

tipo <- tipo[tipo$DESCRICAO!="DUMMY",]  

########################
# grava arquivo de sa?da
print("gerando arquivo de saida")
write.csv2(file="datasetTipo.csv",data.frame(tipo),row.names = FALSE)
