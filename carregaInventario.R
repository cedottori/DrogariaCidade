################################################################################################################################
# Função que processa os arquivos gerados pelo SoftPharma com o estoque das lojas. 
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
#
# 18/02/2016    fpaim   Agrupando registros em data frames de 5000 linhas e concatenando no data frame final para
#                       aumentar a performance.
# 26/02/2016    fpaim   Processamento de todos os arquivos com padrão INVENTARIO*.txt no path
#
##################################################################################################################################
XL7_ImportaEstoque_SoftPharma <- function(cliente,path){
      
      library(data.table)
      library(dplyr)
      library(reshape2)
      
      if (!hasArg(path) | !hasArg(cliente))
            stop("Sintaxe: XL7_ImportaEstoque_SoftPharma ('cliente','path')\nA funcao importa todos os arquivos em 'path' nomeados no seguinte padrao INVENTARIO*.txt.")
      
      print("Carregando arquivos para memória...")
      
      # pega todos os arquivos no padrao INVENTARIO*.txt do diretorio path
      filenames <- list.files(path, glob2rx("INVENTARIO*.txt"), full.names=TRUE)
      
      estoque <- lapply(filenames, read.csv,sep=";"
                        ,header = FALSE
                        ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                        ,stringsAsFactors = FALSE)
      estoque<-do.call("rbind",estoque)
      
      
      ## cria data frame com estrutura definitiva
      estoqueItens <- data.frame(cliente=character()
                                 ,filial=character()
                                 ,codigo=character()
                                 ,descricao=character()
                                 ,laboratorio=character()
                                 ,unitario=character()
                                 ,qtd=character()
                                 ,valor_total=character()
                                 ,tributacao=character()
                                 ,mesano=character()
                                 ,stringsAsFactors = FALSE)
      
      estoqueTemp <-data.frame(matrix("",ncol=10,nrow=5000),stringsAsFactors = F)
      names(estoqueTemp)<-c("cliente","filial","codigo","descricao","laboratorio","unitario","qtd","valor_total","tributacao","mesano")
      
      #estoqueTemp<-NULL
      
      #estoqueItens  <- NULL
      #estoqueTemp  <- estoqueItens
      
      
      j <- 0 
      
      ## respondentes <- tendencias[0,]
      
      ## separa registros por tipo
      ############################
      for (i in 1:nrow(estoque)){
            
            #estoque[i,] <- gsub("\\*","",estoque[i,])
            
            #teste_logico <- !is.na(as.numeric(substr(estoque[i,],2,14)))
            ## indentifica filial
            if (!is.na(as.numeric(substr(estoque[i,],2,14)))){
                  ## não é nulo, portanto produto válido
                  j <- j + 1
                  
                  
                  
                  estoqueTemp$cliente[j]     <- cliente
                  estoqueTemp$filial[j]      <- filial
                  estoqueTemp$codigo[j]      <- substr(estoque[i,],2 ,14)
                  estoqueTemp$descricao[j]   <- substr(estoque[i,],16,65)
                  estoqueTemp$laboratorio[j] <- substr(estoque[i,],67,77)
                  estoqueTemp$unitario[j]    <- substr(estoque[i,],81,88)
                  estoqueTemp$qtd[j]         <- substr(estoque[i,],92,96)
                  estoqueTemp$valor_total[j] <- substr(estoque[i,],98,106)
                  estoqueTemp$tributacao[j]  <- substr(estoque[i,],109,132)
                  estoqueTemp$mesano[j]      <- mesano
                  
                  if (j %% 5000==0){
                        estoqueItens<-rbind(estoqueItens,estoqueTemp)
                        print(paste0("Registros importados: ",nrow(estoqueItens)))
                        j<-0
                  }
                  
                  
                  #estoqueItens <- rbind(estoqueItens,estoqueTemp)
                  
                  
            } else if (length(grep("<<",estoque[i,]))!=0) {
                  
                  filial <-substr(estoque[i,],15,16)
                  
            } else if (length(grep("Data Proc.:",estoque[i,]))!=0) {
                  
                  mesano<-substr(estoque[i,],16,22)
                  
            }
            
      } ## for
      
      # concatena as linhas restantes do estoqueTemp
      estoqueItens<-rbind(estoqueItens,estoqueTemp[1:j,])
      
      estoqueItens
      #write.csv(estoqueItens,file="estoque.csv",row.names = F)
      
}





