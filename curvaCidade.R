####################################################################
##
##  TRATA ARQUIVO CURVA ABC DE PRODUTOS SOFTPHARMA
##
##  CRIA UM DATASET LIMPO
##
##  CHAMA PROCEDIMENTO PARA CRIAÇÃO DE RELATÓRIO GERENCIAL/ANALÍTICO (EM CRIAÇÃO)
##
####################################################################
#print(puser)
setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_cidade")
      
# ARQUIVOS DE SAIDA E ENTRADA
arq_entrada <- paste0("CURVA ABC-CIDADE-PERFUMARIA.txt")
arq_saida   <- paste0("datasetCurvaCidade.csv")

# ler arquivo
curva      <- readLines (arq_entrada)
curvaItens <- data.frame(1,2,"descricao","lab",1,1,1,"class_giro",1,1,1,1,1,1,"fix",stringsAsFactors = FALSE)

names(curvaItens)<-c("filial","codigo","descricao","laboratorio","qtd_venda","custo_medio"
                     ,"total","class_giro","repr_perc","min","dem","est","calculo_minimo","estoque_demanda","demanda_fixada")

curvaTemp <- curvaItens
filial    <- 1
j         <- 1 
first_i   <- NULL

## separa registros por tipo
############################
for (i in 1:length(curva)){
      
      
      ## identifica linha dataset
      if (!is.na(as.numeric(substr(curva[i],1,6)))){
            ## não é nulo, portanto produto válido
            curvaTemp$filial[j]          <- filial
            curvaTemp$codigo[j]          <- as.numeric(substr(curva[i],1 ,6))
            curvaTemp$descricao[j]       <- substr(curva[i],8 ,38)
            curvaTemp$laboratorio[j]     <- substr(curva[i],40,57) 
            curvaTemp$qtd_venda[j]       <- as.numeric(substr(curva[i],59,63))
            curvaTemp$custo_medio[j]     <- as.numeric(substr(curva[i],66,71))
            curvaTemp$total[j]           <- as.numeric(substr(curva[i],73,81))
            curvaTemp$class_giro[j]      <- substr(curva[i],83,88)
            curvaTemp$repr_perc[j]       <- as.numeric(substr(curva[i],92,97))
            curvaTemp$min[j]             <- as.numeric(substr(curva[i],104,106))
            curvaTemp$dem[j]             <- as.numeric(substr(curva[i],109,111))
            curvaTemp$est[j]             <- as.numeric(substr(curva[i],114,118))
            curvaTemp$calculo_minimo[j]  <- as.numeric(substr(curva[i],120,124))
            curvaTemp$estoque_demanda[j] <- as.numeric(substr(curva[i],127,132))
            curvaTemp$demanda_fixada[j]  <- substr(curva[i],91,95)
            
            
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
      } else if (length(grep("<<",curva[i]))!=0) {
            
            filial <-substr(curva[i],12,12)
      } 
      
      if(i%%1000==0){
            print(paste0("processadas ",i," linhas"))
      }

} ## for

curvaItens <- curvaItens[curvaItens$descricao!="descricao",]

# grava arquivo de saída
write.csv2(file=arq_saida,curvaItens,row.names = FALSE)
