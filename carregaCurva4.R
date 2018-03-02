carregaCurva <- function(data=DATA,
                         data_inicial=DATA_INICIAL,
                         data_inicial_filial3="20161227",
                         puser=USER,
                         pNumeroSemanasVenda=4){
####################################################################
##
##  TRATA ARQUIVO CURVA ABC DE PRODUTOS SOFTPHARMA
##
##  CRIA UM DATASET LIMPO
##
##  CHAMA PROCEDIMENTO PARA CRIACAO DE RELATORIO GERENCIAL/ANALITICO 
##
####################################################################
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

# fator de ajuste futuro: crescimento de 0%% / em rela??o ao per?odo m?dio (vida da loja / 2)
fatorAjusteFuturo    <- 1#1.0^((numero_de_meses/2)+0.5)
#fatorAjusteFuturoF03 <- 1.0^((numero_de_mesesF03/2)+0.5)

# tratamento compra Drogaria Cidade
#carregaMovCompra("compra_cidade/MOVIMENTACAO-CIDADE-GERAL.txt","201600",1,puser)
#cidade5 <- read.csv("datasetVendaCliente-201600.csv",sep=";",stringsAsFactors = F,dec=",")

# ARQUIVOS DE SAIDA E ENTRADA
arq_entrada <- paste0("CURVA ABC-",data,".txt")
arq_saida   <- paste0("datasetCurva",data,".csv")

# ler arquivo
curva      <- readLines (arq_entrada)
linhas     <- length(curva)
curvaItens <- data.table(rep(1,linhas),rep(2,linhas),rep("descricao",linhas),
                         rep("lab",linhas),rep(1,linhas),rep(1,linhas),rep(1,linhas),
                         rep("class_giro",linhas),rep(1,linhas),rep(1,linhas),rep(1,linhas),
                         rep(1,linhas),rep(1,linhas),rep(1,linhas),rep("fix",linhas),
                         stringsAsFactors = FALSE)

names(curvaItens)<-c("filial","codigo","descricao","laboratorio","qtd_venda","custo_medio"
                     ,"total","class_giro","repr_perc","min","dem","est","calculo_minimo","estoque_demanda","demanda_fixada")

curvaTemp <- curvaItens
filial    <- 1
j         <- 0 
first_i   <- NULL
i         <- 0

## separa registros por tipo
############################
inicial <- Sys.time()
for (i in 1:length(curva)){# 
      ## identifica linha dataset
      if (!is.na(as.numeric(substr(curva[i],1,6)))){
            ## nao e nulo, portanto produto valido
            # j <- j+1
            curvaItens[i] <- data.table(filial,as.numeric(substr(curva[i],1 ,6)),
                                   substr(curva[i],8 ,38),substr(curva[i],40,57),
                                   as.numeric(substr(curva[i],59,63)),as.numeric(substr(curva[i],66,71)),
                                   as.numeric(substr(curva[i],73,81)),substr(curva[i],83,88),
                                   as.numeric(substr(curva[i],92,97)),as.numeric(substr(curva[i],104,106)),
                                   as.numeric(substr(curva[i],109,111)),as.numeric(substr(curva[i],114,118)),
                                   as.numeric(substr(curva[i],120,124)),as.numeric(substr(curva[i],127,132)),
                                   substr(curva[i],91,95))
            # curvaItens[j,filial:= filial]
            # curvaItens[j,codigo:= as.numeric(substr(curva[i],1 ,6))]
            # curvaItens[j,descricao:= substr(curva[i],8 ,38)]
            # curvaItens[j,laboratorio:= substr(curva[i],40,57) ]
            # curvaItens[j,qtd_venda:= as.numeric(substr(curva[i],59,63))]
            # curvaItens[j,custo_medio:= as.numeric(substr(curva[i],66,71))]
            # curvaItens[j,total:= as.numeric(substr(curva[i],73,81))]
            # curvaItens[j,class_giro:=substr(curva[i],83,88)]
            # curvaItens[j,repr_perc:=as.numeric(substr(curva[i],92,97))]
            # curvaItens[j,min:= as.numeric(substr(curva[i],104,106))]
            # curvaItens[j,dem:= as.numeric(substr(curva[i],109,111))]
            # curvaItens[j,est:=  as.numeric(substr(curva[i],114,118))]
            # curvaItens[j,calculo_minimo:=  as.numeric(substr(curva[i],120,124))]
            # curvaItens[j,estoque_demanda:=  as.numeric(substr(curva[i],127,132))]
            # curvaItens[j,demanda_fixada:=  substr(curva[i],91,95)]

            # if (!is.null(first_i)){
            #       #print("segundo")
            #       curvaItens <- rbind(curvaItens,curvaTemp[j,])
            # } 
            # 
            # if (is.null(first_i)){
            #       #print("primeiro")
            #       first_i <- 1
            #       curvaItens[j,] <- curvaTemp[j,]
            # } 
            
            
            ## identifica linha filial
      } else if (length(grep("<<",curva[i]))!=0) {
            
            filial <-gsub(">","",substr(curva[i],12,13))
      } 
      
      if(i%%1000==0){
            print(paste0("processadas ",i," linhas"))
      }

} ## for
final <- Sys.time()
print(final-inicial)

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

curvaItens$cobertura_semanas <- curvaItens$est/((curvaItens$qtd_venda/pNumeroSemanasVenda)*fatorAjusteFuturo)
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

demanda <- demanda[,c("filial","codigo","dem3sem","dem6sem")]
demanda$dem3sem[is.na(demanda$dem3sem)] <- 0
demanda$dem6sem[is.na(demanda$dem6sem)] <- 0
demanda$dem3sem[demanda$dem3sem<1] <- ceiling(demanda$dem3sem[demanda$dem3sem<1])
demanda$dem3sem[demanda$dem3sem>1] <- floor(demanda$dem3sem[demanda$dem3sem>1])
demanda$dem6sem[demanda$dem6sem<1] <- ceiling(demanda$dem6sem[demanda$dem6sem<1])
demanda$dem6sem[demanda$dem6sem>1] <- floor(demanda$dem6sem[demanda$dem6sem>1])

# grava arquivo de saida
write.csv2(file=arq_saida,curvaItens)
write.csv2(file="demanda.csv",demanda)

demanda

}
