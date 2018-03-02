carregaCurva <- function(data='20170615',data_inicial='20161215',data_inicial_filial3="20161227",puser="lgiar"){
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
if (puser=="cedot"){
      setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
} else {
      setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")      
}
#source("carregaMovimentoCompra.R")
      
# PARAMETRO DE ENTRADA
numero_de_meses      <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial,"%Y%m%d"))/30
numero_de_semanas    <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial,"%Y%m%d"))/7
#numero_de_mesesF03   <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial_filial3,"%Y%m%d"))/30
#numero_de_semanasF03 <- as.numeric(as.Date(data,"%Y%m%d") - as.Date(data_inicial_filial3,"%Y%m%d"))/7

# fator de ajuste futuro: crescimento de 0%% / em relação ao período médio (vida da loja / 2)
fatorAjusteFuturo    <- 1.0^((numero_de_meses/2)+0.5)
#fatorAjusteFuturoF03 <- 1.0^((numero_de_mesesF03/2)+0.5)

# tratamento compra Drogaria Cidade
#carregaMovCompra("compra_cidade/MOVIMENTACAO-CIDADE-GERAL.txt","201600",1,puser)
#cidade5 <- read.csv("datasetVendaCliente-201600.csv",sep=";",stringsAsFactors = F,dec=",")

# ARQUIVOS DE SAIDA E ENTRADA
arq_entrada <- paste0("CURVA ABC-",data,".txt")
arq_saida   <- paste0("datasetCurva",data,".csv")

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
            
            filial <-gsub(">","",substr(curva[i],12,13))
      } 
      
      if(i%%1000==0){
            print(paste0("processadas ",i," linhas"))
      }

} ## for

curvaItens <- curvaItens[curvaItens$descricao!="descricao",]

# insere movimento Cidade
#curvaItens <- merge(curvaItens,cidade5,by.x = c("filial","codigo"),by.y=c("FILIAL_DP","COD_INTERNO"),all.x=TRUE)
#curvaItens$QTD_COMPRA[is.na(curvaItens$QTD_COMPRA)] <- 0
#curvaItens$VALOR_COMPRA[is.na(curvaItens$VALOR_COMPRA)] <- 0

# subtrai movimento Cidade da venda geral
#curvaItens$qtd_venda <- curvaItens$qtd_venda - curvaItens$QTD_COMPRA
#curvaItens$total     <- curvaItens$total     - curvaItens$VALOR_COMPRA

# cria coluna para cobertura com fator de ajuste de crescimento
curvaItens$cobertura_semanas   <- NA
curvaItens$cobertura_ponderada <- NA

curvaItens$cobertura_semanas <- curvaItens$est/((curvaItens$qtd_venda/numero_de_semanas)*fatorAjusteFuturo)
#curvaItens$cobertura_semanas[curvaItens$filial==3]          <- curvaItens$est[curvaItens$filial==3]/((curvaItens$qtd_venda[curvaItens$filial==3]/numero_de_semanasF03)*fatorAjusteFuturoF03)

curvaItens$cobertura_semanas[curvaItens$est==0]       <- 0
curvaItens$cobertura_ponderada[curvaItens$est==0]     <- 0
curvaItens$cobertura_semanas[curvaItens$est<0]        <- 0
curvaItens$cobertura_ponderada[curvaItens$est<0]      <- 0
curvaItens$cobertura_semanas[is.na(curvaItens$est)]   <- 0
curvaItens$cobertura_ponderada[is.na(curvaItens$est)] <- 0

# calcula cobertura equivalente ao quantil 97.5 (2 dp) 
quebra <- split(curvaItens,curvaItens$filial)
quant <- function(x){
      quantile(x$cobertura_semanas[!is.infinite(x$cobertura_semanas)],.975,na.rm=TRUE)
}
maxRef <- sapply(quebra,quant)

# preenche maxRef[1] (97.5 quantile filial 1) para NA e infinito (itens sem venda mas com estoque/coberturas muito altas)
curvaItens$cobertura_semanas[is.na(curvaItens$cobertura_semanas) | is.infinite(curvaItens$cobertura_semanas)]<- maxRef[1]
curvaItens$cobertura_ponderada<-curvaItens$custo_medio*curvaItens$est*curvaItens$cobertura_semanas

# calcula demanda equivalente a 3 e 6 semanas
demanda <- curvaItens

demanda$dem3sem <- ((demanda$qtd_venda/numero_de_semanas)*fatorAjusteFuturo*3)-(demanda$est)
#demanda$dem3sem[demanda$filial %in% c(3)]   <- ((demanda$qtd_venda[demanda$filial %in% c(3)]/numero_de_semanas)*fatorAjusteFuturo*3)-(demanda$est[demanda$filial %in% c(3)])

demanda$dem6sem <- ((demanda$qtd_venda/numero_de_semanas)*fatorAjusteFuturo*6)-(demanda$est)
#demanda$dem6sem[demanda$filial %in% c(3)]   <- ((demanda$qtd_venda[demanda$filial %in% c(3)]/numero_de_semanas)*fatorAjusteFuturo*6)-(demanda$est[demanda$filial %in% c(3)])

demanda <- demanda[c("filial","codigo","dem3sem","dem6sem")]
demanda$dem3sem[is.na(demanda$dem3sem)] <- 0
demanda$dem6sem[is.na(demanda$dem6sem)] <- 0
demanda$dem3sem[demanda$dem3sem<1] <- ceiling(demanda$dem3sem[demanda$dem3sem<1])
demanda$dem3sem[demanda$dem3sem>1] <- floor(demanda$dem3sem[demanda$dem3sem>1])
demanda$dem6sem[demanda$dem6sem<1] <- ceiling(demanda$dem6sem[demanda$dem6sem<1])
demanda$dem6sem[demanda$dem6sem>1] <- floor(demanda$dem6sem[demanda$dem6sem>1])

# grava arquivo de saída
write.csv2(file=arq_saida,curvaItens,row.names = FALSE)
write.csv2(file="demanda",demanda,row.names = FALSE)

demanda

}
