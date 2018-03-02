################################################################################################################################
# Função que processa os arquivos gerados pelo SoftPharma com a movimentação das lojas. 
# Os arquivos est?o sendo gerados agrupando as vendas quinzenalmente.
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
#
# 18/02/2016    fpaim   agrupando registros em data frames de 5000 linhas e concatenando no data frame final para
#                       aumentar a performance.
#
##################################################################################################################################
XL7_Processa_compra <- function(cliente=2,puser="cedot",dia){

      # returns string w/o leading whitespace
      trim.leading <- function (x)  sub("^\\s+", "", x)      
      library(stringr)
      library(plyr)
      
      source(paste0("C:/Users/",puser,"/OneDrive/XL7 COMPARTILHADA/Desconto Popular/XL7.R"))
      source(paste0("C:/Users/",puser,"/OneDrive/XL7 COMPARTILHADA/Desconto Popular/XL7-PHARMA.R"))
      
      if (puser=="cedot"){
            setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_historica")
            path <- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_historica"
      } else {
            setwd("C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_historica")      
            path <- "C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_historica"
      }

#      if (!hasArg(puser) | !hasArg(cliente))
#            stop("Sintaxe: XL7_ImportaMovimentacao_SoftPharma ('cliente','user')\nA funcao importa todos os arquivos em 'user' nomeados no seguinte padrao MOVIMENTACAO*.txt.")
      
      print("COMPRA - carregando arquivos para memória...")
      
      # pega todos os arquivos no padrao MOVIMENTACAO*.txt do diretorio path
      filenames <- list.files(path, glob2rx("MOVIMENTACAO*.txt"), full.names=TRUE)
      
      #movimentacao <- lapply(filenames, read.csv,sep=";"
      #                       ,header = FALSE
      #                       ,encoding = "UTF-16"   # pois tem acento em maiuculas...
      #                       ,stringsAsFactors = FALSE)
      movimentacao <- lapply(filenames, readLines)

      stringao     <- " "
      for (i in 1:length(movimentacao)){
            stringao <- c(stringao,movimentacao[[i]])
            
      }
      #movimentacao<-do.call("rbind",movimentacao)
      movimentacao<-stringao
      
      ## cria data frame com estrutura definitiva
      movimentacaoItens <- data.frame(cliente=rep(0,length(movimentacao))
                                      ,filial=rep(0,length(movimentacao))
                                      ,codigo=rep(0,length(movimentacao))
                                      ,descricao=rep("DUMMY",length(movimentacao)) 
                                      ,tipoproduto=rep(0,length(movimentacao))
                                      ,total=rep(0,length(movimentacao))
                                      ,unitario=rep(0,length(movimentacao))
                                      ,qtd_av=rep(0,length(movimentacao))
                                      ,qtd_apz=rep(0,length(movimentacao))
                                      ,qtd_total=rep(0,length(movimentacao))
                                      ,customedio=rep(0,length(movimentacao))
                                      ,lab=rep("DUMMY",length(movimentacao)) 
                                      ,laboratorio=rep("DUMMY",length(movimentacao)) 
                                      ,estoque=rep(0,length(movimentacao))
                                      ,dia=rep(as.Date("01-01-0001","%d-%m-%Y"),length(movimentacao))
                                      ,cod_forn=rep(0,length(movimentacao))
                                      ,descr_forn=rep("DUMMY",length(movimentacao))
                                      ,stringsAsFactors = FALSE)
      
      print("Iniciando processamento dos dados...")
      
      j <- 0 
      
      for (i in 1:length(movimentacao)){
            print(i)
            if (!is.na(as.numeric(substr(movimentacao[i],1,6)))&!is.na(as.numeric(substr(movimentacao[i],65,71)))){
                  ## n?o ? nulo, portanto produto v?lido
                  j <- j + 1
                  
                  movimentacaoItens$cliente[j]     <- cliente
                  movimentacaoItens$filial[j]      <- filial
                  movimentacaoItens$codigo[j]      <- as.numeric(substr(movimentacao[i],1 ,6))
                  movimentacaoItens$descricao[j]   <- substr(movimentacao[i],8,38)
                  movimentacaoItens$tipoproduto[j] <- as.numeric(substr(movimentacao[i],39,50))
                  movimentacaoItens$total[j]       <- as.numeric(substr(movimentacao[i],52,62))
                  movimentacaoItens$unitario[j]    <- as.numeric(substr(movimentacao[i],64,71))
                  movimentacaoItens$qtd_av[j]      <- as.numeric(substr(movimentacao[i],73,79))
                  movimentacaoItens$qtd_apz[j]     <- as.numeric(substr(movimentacao[i],81,88))
                  movimentacaoItens$qtd_total[j]   <- as.numeric(substr(movimentacao[i],90,99))
                  movimentacaoItens$customedio[j]  <- as.numeric(substr(movimentacao[i],100,111))
                  movimentacaoItens$lab[j]         <- as.numeric(substr(movimentacao[i],113,116))
                  movimentacaoItens$laboratorio[j] <- substr(movimentacao[i],117,125)
                  movimentacaoItens$estoque[j]     <- as.numeric(substr(movimentacao[i],127,132))
                  movimentacaoItens$dia[j]         <- as.Date(dia,"%d/%m/%Y")
                  movimentacaoItens$cod_forn[j]    <- as.numeric(cod_forn)
                  movimentacaoItens$descr_forn[j]  <- trim.leading(descr_forn)
                  
                  
                  if (j %% 5000==0){
                        print(paste0("Registros importados: ",j))
                  }
                  
            } else if (length(grep("FILIAL...:",movimentacao[i]))!=0) {
                  # pega codigo da filial
                  filial <-substr(movimentacao[i],11,14)
                  
            } else if (length(grep("\\| Data: ",movimentacao[i]))!=0) {
                  # pega data atual
                  dia<-substr(strsplit(movimentacao[i],"Data: ")[[1]][2],1,10)
                  
            } else if (!is.na(as.numeric(substr(movimentacao[i],1,1)))) {
                  # pega fonecedor
                  cod_forn   <- gsub("-","",substr(movimentacao[i],1,3))
                  descr_forn <- substr(movimentacao[i],5,nchar(movimentacao[i]))
            }

        if (i %% 5000==0){
          print(paste0("Registros varridos: ",i))
        }
        
      } ## for
      
      # retira linhas em excesso
      movimentacaoItens<-movimentacaoItens[movimentacaoItens$descricao!="DUMMY",]

      # remove espaços em branco
      movimentacaoItens$descricao<-trimws(movimentacaoItens$descricao,which='both')
      movimentacaoItens$laboratorio<-trimws(movimentacaoItens$laboratorio,which='both')
      movimentacaoItens$codigo<-as.numeric(movimentacaoItens$codigo)
      
      ####################
      # busca melhor custo
      ####################
      movCompra              <- movimentacaoItens
      movCompra$custo_compra <- movCompra$customedio/movCompra$qtd_total
      # busca melhor custo por item
      melhorCompra <- ddply(movCompra[movCompra$dia!=as.Date("16-03-2016","%d-%m-%Y")&movCompra$dia!=as.Date("01-04-2016","%d-%m-%Y"),]
                            ,.(codigo)
                            ,summarize
                            ,custo_compra=min(custo_compra))

      ## faz join entre melhor custo e demais campos
      melhorCompra2 <- merge(melhorCompra,unique(movCompra[c("codigo","custo_compra","dia","cod_forn","descr_forn")]))
      
      ## pega a maior data do melhor custo
      melhorCompra3 <- ddply(melhorCompra2
                             ,.(codigo,custo_compra)
                             ,summarize
                             ,dia=max(dia))
      ## faz join entre melhor custo, maior data e demais campos
      melhorCompra4 <- merge(melhorCompra3,unique(movCompra[c("codigo","custo_compra","dia","cod_forn","descr_forn")]))

      ## pega a menor distribuidora do menor custo e maior data
      melhorCompra5 <- ddply(melhorCompra4
                             ,.(codigo,custo_compra,dia)
                             ,summarize
                             ,cod_forn=min(cod_forn))
      melhorCompra6 <- merge(melhorCompra5,unique(movCompra[c("codigo","custo_compra","dia","cod_forn","descr_forn")]))
      
      names(melhorCompra6)[c(2,3,4,5)]<-c("melhor_custo","melhor_dia","melhor_cod_forn","melhor_desc_forn")

      write.csv2(file=paste0("datasetMelhor.csv"),data.frame(melhorCompra6),row.names = FALSE)

      #####################
      # busca ultima compra 
      #####################
      ultimaCompra  <- ddply(movCompra[c("codigo","descricao","dia")]
                             ,.(codigo)
                             ,summarize
                             ,dia=max(dia))
      ultimaCompra2 <- merge(ultimaCompra,unique(movCompra[c("codigo","descricao","custo_compra","dia","cod_forn","descr_forn")]))
      ## busca menor custo do maior dia
      ultimaCompra3 <- ddply(ultimaCompra2
                             ,.(codigo,dia)
                             ,summarize
                             ,custo_compra=min(custo_compra))
      ultimaCompra4 <- merge(ultimaCompra3,unique(movCompra[c("codigo","descricao","custo_compra","dia","cod_forn","descr_forn")]))

      ## pega a menor distribuidora do menor custo e maior data
      ultimaCompra5 <- ddply(ultimaCompra4
                             ,.(codigo,custo_compra,dia)
                             ,summarize
                             ,cod_forn=min(cod_forn))
      ultimaCompra6 <- merge(ultimaCompra5,unique(movCompra[c("codigo","custo_compra","dia","cod_forn","descr_forn")]))

      names(ultimaCompra6)[c(2,3,4,5)]<-c("ultimo_custo","ultimo_dia","ultimo_cod_forn","ultimo_desc_forn")

      write.csv2(file=paste0("datasetUltimo.csv"),data.frame(ultimaCompra2),row.names = FALSE)

      ################################
      # monta data set final e retorna
      ################################
      movimentacaoCompras <- merge(ultimaCompra6,melhorCompra6,all.x=T)
      print("gerando arquivo de compra")
      write.csv2(file=paste0("datasetCompra.csv"),data.frame(movCompra),row.names = FALSE)

      # retorna data frame criado
      movimentacaoCompras
}
