carregaEstoque <- function(cod_cliente=2, pmascara_arquivo=DATA){
#setwd("C:/RAWDATA")
#setwd("C:/Users/LGiaretta/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
source("XL7.R")
library(dplyr)
#################################################################################
##  TRATA ARQUIVO INVENTARIO DE PRODUTOS SOFTPHARMA
##  CRIA UM DATASET LIMPO
##  CHAMA PROCEDIMENTO PARA CRIAÇÃO DE RELATÓRIO GERENCIAL/ANALÍTICO (EM CRIAÇÃO)
#################################################################################

# PARÂMETROS DE ENTRADA
#mascara_periodo <- "20160619-CD-CIDADE" 
mascara_periodo <- pmascara_arquivo

#########################################
# ler arquivo como um vetor de caracteres
print("lendo arquivo de inventário")
estoque  <- readLines(paste0("INVENTARIO-",mascara_periodo,".txt"))
print(paste0("lidas ",length(estoque)," linhas de estoque"))

estoqueItens        <- data.frame(COD_CLIENTE        =rep(0,length(estoque))
                                 ,COD_FILIAL_CLIENTE =rep(0,length(estoque))
                                 ,COD_INTERNO        =rep(0,length(estoque))
                                 ,descricao          =rep("DUMMY",length(estoque))             
                                 ,laboratorio        =rep("DUMMY",length(estoque))             
                                 ,unitario           =rep(0,length(estoque))
                                 ,qtd                =rep(0,length(estoque))
                                 ,valor_total        =rep(0,length(estoque))
                                 ,tributacao         =rep("DUMMY",length(estoque))
                                 ,stringsAsFactors   =FALSE)             

filial  <- 1
first_i <- NULL
j       <- 1

print("quebrando arquivo de inventário")
######################################
## interpreta arquivo e gera dataframe
for (i in 1:length(estoque)){
      ## identifica linha dataset
      linhaItem   <- !is.na(as.numeric(substr(estoque[i],1,6)))&&!is.na(as.numeric(substr(estoque[i],98,106)))
      
      if (linhaItem){
            ## não é nulo, portanto produto válido
            estoqueItens$COD_CLIENTE[j]        <- cod_cliente
            estoqueItens$COD_FILIAL_CLIENTE[j] <- filial
            estoqueItens$COD_INTERNO[j]        <- as.numeric(substr(estoque[i],1 ,6))
            estoqueItens$descricao[j]          <- substr(estoque[i],8,65) 
            estoqueItens$laboratorio[j]        <- substr(estoque[i],67,77)
            estoqueItens$unitario[j]           <- as.numeric(substr(estoque[i],81,88))
            estoqueItens$qtd[j]                <- as.numeric(substr(estoque[i],92,95))
            estoqueItens$valor_total[j]        <- as.numeric(substr(estoque[i],98,106))
            estoqueItens$tributacao[j]         <- substr(estoque[i],109,132)

            j <- j + 1
            ## identifica linha filial
      } else if (length(grep("<<",estoque[i]))!=0) {
            
            filial <- as.numeric(gsub(">","",substr(estoque[i],15,16)))
            print(filial)

      } #else { 
            ## imprime cabecalhos
             #print(estoque[i])
      #}
      # mensagem de processamento
      if(i%%1000==0){
        print(paste0("processadas ",i," linhas ",Sys.time()))
      }
      
} ## for

estoqueItens <- estoqueItens[estoqueItens$descricao!="DUMMY",]  

########################
# grava arquivo de saída
print("gerando arquivo de saída")
write.csv2(file=paste0("datasetEstoque",mascara_periodo,".csv"),data.frame(estoqueItens),row.names = FALSE)

########################################################################################
# insere linhas na base XL7
########################################################################################

# zera registros para o dia
#mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')

#seriesDados<- dbGetQuery(mydb,paste0("select * from SERIES_DADOS where COD_CLIENTE=",cod_cliente
 #                                    ," and MASCARA_PERIODO="   ,mascara_periodo))

#SERIES_DADOS_INSERIR   <- anti_join(estoqueItens,seriesDados)
#SERIES_DADOS_ATUALIZAR <- merge(estoqueItens,seriesDados)

#####################
# INSERE SERIES_DADOS
#####################
#mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
#print("inserindo linhas na base XL7")

#i <- nrow(SERIES_DADOS_INSERIR)
#while (i > 0){

 #     sqlString <- paste0("insert into SERIES_DADOS (COD_CLIENTE,COD_FILIAL_CLIENTE,COD_INTERNO,MASCARA_PERIODO,POSICAO_ESTOQUE_QTD,POSICAO_ESTOQUE_VALOR)"
  #                        ," values (",SERIES_DADOS_INSERIR$COD_CLIENTE[i]
   #                       ,",",SERIES_DADOS_INSERIR$COD_FILIAL_CLIENTE[i]
    #                      ,",",SERIES_DADOS_INSERIR$COD_INTERNO[i]
     #                     ,",'",mascara_periodo,"'"
      #                    ,",",SERIES_DADOS_INSERIR$qtd[i]
       #                   ,",",SERIES_DADOS_INSERIR$valor_total[i]
        #                  ,")")
      
#      dbGetQuery(mydb, sqlString)
      # mensagem de processamento
 #     if(i%%500==0){
  #          print(paste0("processadas ",i," linhas"))
   #   }
    #  i <- i -1
#}

#mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
#print("atualizando linhas na base XL7")

#i <- nrow(SERIES_DADOS_ATUALIZAR)
#while (i > 0){
      
#      sqlString <- paste0("update  SERIES_DADOS "
 #                         ," set POSICAO_ESTOQUE_QTD="  ,SERIES_DADOS_ATUALIZAR$qtd[i]
  #                        ,"   , POSICAO_ESTOQUE_VALOR=",SERIES_DADOS_ATUALIZAR$valor_total[i]
   #                       ," where COD_CLIENTE="        ,SERIES_DADOS_INSERIR$COD_CLIENTE[i]
    #                      ,"   and COD_FILIAL_CLIENTE=" ,SERIES_DADOS_ATUALIZAR$COD_FILIAL_CLIENTE[i]
     #                     ,"   and COD_INTERNO="        ,SERIES_DADOS_ATUALIZAR$COD_INTERNO[i]
      #                    ,"   and MASCARA_PERIODO='"   ,mascara_periodo,"'")
      
#      dbGetQuery(mydb, sqlString)
      # mensagem de processamento
#      if(i%%500==0){
#            print(paste0("processadas ",i," linhas"))
#      }
#      i <- i -1
#}
#dbDisconnect(mydb)
}