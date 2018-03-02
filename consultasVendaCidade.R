ddply(movimentacaoItens
      ,c("dia","filial"),summarise,venda=sum(total))

ddply(movimentacaoItens
      ,c("filial"),summarise,venda=sum(total))

dia25 <- movimentacaoItens[movimentacaoItens$dia=="25/05/2016",c(2:7,10)]
dia24 <- movimentacaoItens[movimentacaoItens$dia=="24/05/2016",c(2:7,10)]

sum(dia25$total[dia25$total<0])
sum(dia24$total[dia24$total<0])

################################

library(stringr)

path <- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_cidade/"

print("Carregando arquivos para memória...")

# pega todos os arquivos no padrao MOVIMENTACAO*.txt do diretorio path
filenames <- list.files(path, glob2rx("QMOVIMENTACAO*.txt"), full.names=TRUE)

#movimentacao <- lapply(filenames, read.csv,sep=";"
#                       ,header = FALSE
#                       ,encoding = "UTF-16"   # pois tem acento em maiuculas...
##                       ,stringsAsFactors = FALSE)

movimentacao <- lapply(filenames, readLines
                       ,encoding = "UTF-16"   # pois tem acento em maiuculas...)
)

movimentacaoTemp <- movimentacao[[1]] 

i <- 2

while (length(movimentacao)>=i){
      
      movimentacaoTemp <- c(movimentacaoTemp,movimentacao[[i]])
      i <- i + 1
      
}

movimentacao <- movimentacaoTemp

#movimentacao<-do.call("rbind",movimentacao)


## cria data frame com estrutura definitiva

movimentacaoItens <- data.frame(cliente=rep(0,length(movimentacao))
                                ,filial=rep(0,length(movimentacao))
                                ,codigo=rep(0,length(movimentacao))
                                ,descricao=rep("DUMMY",length(movimentacao))
                                ,tipoproduto=rep("DUMMY",length(movimentacao))
                                ,total=rep(0,length(movimentacao))
                                ,unitario=rep(0,length(movimentacao))
                                ,qtd_av=rep(0,length(movimentacao))
                                ,qtd_apz=rep(0,length(movimentacao))
                                ,qtd_total=rep(0,length(movimentacao))
                                ,customedio=rep(0,length(movimentacao))
                                ,lab=rep("DUMMY",length(movimentacao))
                                ,laboratorio=rep("DUMMY",length(movimentacao))
                                ,estoque=rep(0,length(movimentacao))
                                ,dia=rep("DUMMY",length(movimentacao))
                                ,stringsAsFactors = FALSE)

print("Iniciando processamento dos dados...")

j <- 0 

for (i in 1:length(movimentacao)){
      
      if (!is.na(as.numeric(substr(movimentacao[i],1,6)))&&!is.na(as.numeric(substr(movimentacao[i],56,62)))){
            ## não é nulo, portanto produto válido
            j <- j + 1
            
            movimentacaoItens$cliente[j]     <- cliente
            movimentacaoItens$filial[j]      <- filial
            movimentacaoItens$codigo[j]      <- as.numeric(substr(movimentacao[i],1 ,6))
            movimentacaoItens$descricao[j]   <- substr(movimentacao[i],8,38)
            movimentacaoItens$tipoproduto[j] <- substr(movimentacao[i],39,50)
            movimentacaoItens$total[j]       <- as.numeric(substr(movimentacao[i],52,62))
            movimentacaoItens$unitario[j]    <- as.numeric(substr(movimentacao[i],64,71))
            movimentacaoItens$qtd_av[j]      <- as.numeric(substr(movimentacao[i],73,79))
            movimentacaoItens$qtd_apz[j]     <- as.numeric(substr(movimentacao[i],81,88))
            movimentacaoItens$qtd_total[j]   <- as.numeric(substr(movimentacao[i],90,99))
            movimentacaoItens$customedio[j]  <- as.numeric(substr(movimentacao[i],100,111))
            movimentacaoItens$lab[j]         <- substr(movimentacao[i],113,116)
            movimentacaoItens$laboratorio[j] <- substr(movimentacao[i],117,125)
            movimentacaoItens$estoque[j]     <- as.numeric(substr(movimentacao[i],127,132))
            movimentacaoItens$dia[j]         <- dia
            
            if (j %% 5000==0){
                  print(paste0("Registros importados: ",j))
            }
            
            
            #movimentacaoItens <- rbind(movimentacaoItens,movimentacaoTemp)
            
            
      } else if (length(grep("FILIAL...:",movimentacao[i]))!=0) {
            
            filial <-substr(movimentacao[i],11,14)
            
      } else if (length(grep("\\| Data: ",movimentacao[i]))!=0) {
            
            dia<-substr(strsplit(movimentacao[i],"Data: ")[[1]][2],1,10)
            
      }
      
} ## for

# retira as linhas que sobraram 
movimentacaoItens<-movimentacaoItens[movimentacaoItens$descricao!="DUMMY",]
