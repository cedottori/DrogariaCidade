source("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/XL7.R")

library(plyr)
library(dplyr)

################################################################################################################################
# Função que importa o arquivo de produtos do SoftPharma. 
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
##################################################################################################################################
XL7_Importaproduto_SoftPharma <- function(cliente,path){
    
    if (!hasArg(path) | !hasArg(cliente))
        stop("Sintaxe: XL7_Importaproduto_SoftPharma ('cliente','path')\nA funcao importa todos os 
             arquivos em 'path' nomeados no seguinte padrao PRODUTOS*.txt.")
    
    print("Carregando arquivos para memória...")
    
    # pega todos os arquivos no padrao PRODUTOS*.txt do diretorio path
    filenames <- list.files(path, glob2rx("PRODUTOS*.txt"), full.names=TRUE)
    
    produto <- lapply(filenames, read.csv,sep=";"
                      ,header = FALSE
                      ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                      ,stringsAsFactors = FALSE)
    produto<-do.call("rbind",produto)
    
    
    ## cria data frame com estrutura definitiva
    produtoItens <- data.frame(cliente=character()
                               ,codigo=character()
                               ,EAN=character()
                               ,descricao=character()
                               ,cl=character()
                               ,c.medio=character()
                               ,p.custo=character()
                               ,lucro=character()
                               ,p.des=character()
                               ,p.venda=character()
                               ,c.pdr=character()
                               ,c.nv2=character()
                               ,c.nv3=character()
                               ,c.nv4=character()
                               ,trib=character()
                               ,tp=character()
                               ,cmv=character()
                               ,stringsAsFactors = FALSE)
    
    produtoTemp <-data.frame(matrix("",ncol=17,nrow=5000),stringsAsFactors = F)
    names(produtoTemp)<-c("cliente","codigo","EAN","descricao","cl","c.medio","p.custo",
                          "lucro","p.des","p.venda","c.pdr","c.nv2","c.nv3","c.nv4","trib","tp","cmv")
    
    print("Iniciando processamento dos dados...")
    
    j <- 0 
    
    for (i in 1:nrow(produto)){
        
        if (!is.na(as.numeric(substr(produto[i,],1,6)))){
            ## não é nulo, portanto produto válido
            j <- j + 1
            
            produtoTemp$cliente[j]     <- cliente
            produtoTemp$codigo[j]      <- substr(produto[i,],1 ,6)
            produtoTemp$EAN[j]   <- substr(produto[i,],8,21)
            produtoTemp$descricao[j] <- substr(produto[i,],23,49)
            produtoTemp$cl[j]       <- substr(produto[i,],51,52)
            produtoTemp$c.medio[j]    <- substr(produto[i,],54,60)
            produtoTemp$p.custo[j]      <- substr(produto[i,],62,69)
            produtoTemp$lucro[j]     <- substr(produto[i,],71,76)
            produtoTemp$p.des[j]   <- substr(produto[i,],78,82)
            produtoTemp$p.venda[j]  <- substr(produto[i,],84,90)
            produtoTemp$c.pdr[j]         <- substr(produto[i,],92,96)
            produtoTemp$c.nv2[j] <- substr(produto[i,],98,102)
            produtoTemp$c.nv3[j]     <- substr(produto[i,],104,108)
            produtoTemp$c.nv4[j]     <- substr(produto[i,],110,114)
            produtoTemp$trib[j]     <- substr(produto[i,],116,119)
            produtoTemp$tp[j]     <- substr(produto[i,],121,125)
            produtoTemp$cmv[j]     <- substr(produto[i,],127,130)
            
            # testa se EAN é numérico. Se não, o arquivo PRODUTO.txt está no formato errado (SEM CODIGO DE BARRAS!)
            if (is.na(as.numeric(produtoTemp$EAN[j]))){
                print("Arquivo no formato errado, sem codigo de barras.")
                break
            }
            
            if (j %% 5000==0){
                produtoItens<-rbind(produtoItens,produtoTemp)
                print(paste0("Registros importados: ",nrow(produtoItens)))
                j<-0
            }
            
            
            #produtoItens <- rbind(produtoItens,produtoTemp)
            
            
        }
        
    } ## for
    
    # concatena as linhas restantes do produtoTemp
    produtoItens<-rbind(produtoItens,produtoTemp[1:j,])
    
    # corrige tipo das colunas numericas
    #for (coluna in c("total","unitario","qtd_av","qtd_apz","qtd_total","customedio","estoque")){
    #    produtoItens[,coluna]<-as.numeric(produtoItens[,coluna])
    
    #write.csv2(produtoItens,"produto.csv",row.names = F)
    
    #remove espaços no inicio e fim da descricao
    produtoItens$descricao<-trimws(produtoItens$descricao,which='both')
    
    
    #remove duplicados
    produtoItens<-unique(produtoItens)
    
    # retorna data frame criado
    produtoItens

    
    
    
    
}
#produtos<-XL7_Importaproduto_SoftPharma("1","C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA")
#write.csv2(produtos,"C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/produtos.csv",row.names = F)





################################################################################################################################
# Função que importa os Grupos Principais dos Tipos de Produtos gerados pelo ShoftPharma
################################################################################################################################
XL7_ImportaTipoProduto_SoftPharma <- function(cliente,path){
    
    library(stringr)
    
    
    if (!hasArg(path) | !hasArg(cliente))
        stop("Sintaxe: XL7_ImportaTipoProduto_SoftPharma ('cliente','path')\nA funcao importa todos os arquivos em 'path' nomeados no seguinte padrao 'LISTAGEM DE TP.txt'.")
    
    print("Carregando arquivos para memória...")
    
    # pega todos os arquivos no padrao MOVIMENTACAO*.txt do diretorio path
    filenames <- list.files(path, glob2rx("LISTAGEM DE TP.txt"), full.names=TRUE)
    
    tipos <- lapply(filenames, read.csv,sep=";"
                           ,header = FALSE
                           ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                           ,stringsAsFactors = FALSE)
    tipos<-do.call("rbind",tipos)
    
    
    ## cria data frame com estrutura definitiva
    tiposItens <- data.frame(cliente=character()
                                    ,cod=character()
                                    ,descricao=character()
                                    ,marg_posit=character()
                                    ,marg_negat=character()
                                    ,marg_neutro=character()
                                    ,com_padrao=character()
                                    ,com_niv2=character()
                                    ,com_niv3=character()
                                    ,com_niv4=character()
                                    ,desc=character()
                                    ,grupoprincipal=character()
                                    ,sit=character()
                                    ,stringsAsFactors = FALSE)
    
    tiposTemp <-data.frame(matrix("",ncol=15,nrow=5000),stringsAsFactors = F)
    names(tiposTemp)<-c("cliente", "cod","descricao","marg_posit","marg_negat","marg_neutro","com_padrao","com_niv2","com_niv3","com_niv4",
                        "desc","grupoprincipal","sit")
    
    print("Iniciando processamento dos dados...")
    
    j <- 0 
    
    for (i in 1:nrow(tipos)){
        
        if (!is.na(as.numeric(substr(tipos[i,],1,5)))){
            ## não é nulo, portanto produto válido
            j <- j + 1
            
            tiposTemp$cliente[j]     <- cliente
            tiposTemp$cod[j]         <- substr(tipos[i,],1 ,5)
            tiposTemp$descricao[j]   <- substr(tipos[i,],7,39)
            tiposTemp$marg_posit[j] <- substr(tipos[i,],41,46)
            tiposTemp$marg_negat[j]       <- substr(tipos[i,],50,55)
            tiposTemp$marg_neutro[j]    <- substr(tipos[i,],59,64)
            tiposTemp$com_padrao[j]      <- substr(tipos[i,],67,72)
            tiposTemp$com_niv2[j]     <- substr(tipos[i,],76,80)
            tiposTemp$com_niv3[j]   <- substr(tipos[i,],84,88)
            tiposTemp$com_niv4[j]  <- substr(tipos[i,],92,96)
            tiposTemp$desc[j]         <- substr(tipos[i,],99,102)
            tiposTemp$grupoprincipal[j] <- substr(tipos[i,],105,119)
            tiposTemp$sit[j]     <- substr(tipos[i,],122,124)

            if (j %% 5000==0){
                tiposItens<-rbind(tiposItens,tiposTemp)
                print(paste0("Registros importados: ",nrow(tiposItens)))
                j<-0
            }
            
            
            #tiposItens <- rbind(tiposItens,tiposTemp)
            
            
        }
    } ## for
    
    # concatena as linhas restantes do tiposTemp
    tiposItens<-rbind(tiposItens,tiposTemp[1:j,])
    
    
    # corrige campos
    tiposItens$cod<-trimws(tiposItens$cod)
    tiposItens$descricao<-trimws(tiposItens$descricao)
    tiposItens$grupoprincipal<-trimws(tiposItens$grupoprincipal)
    
    tiposItens[!grepl("\\.",tiposItens$cod),"cod"]<-paste0( tiposItens[!grepl("\\.",tiposItens$cod),"cod"],".00")
    tiposItens$cod<-format(as.numeric(tiposItens$cod), digits=2, nsmall=2)
    
    tiposItens$cod_pai<-paste0(str_split_fixed(tiposItens$cod,"\\.",2)[,1],".00")
    
    return(tiposItens)
    
    
    df<-data.frame(COD_CLIENTE=tiposItens$cliente,COD_CLASSE_COMERCIAL=tiposItens$cod,COD_CLASSE_COMERCIAL_PAI=tiposItens$cod_pai,
                   DESCR_CLASSE_COMERCIAL=tiposItens$descricao, GRUPO_PRINCIPAL=toupper(tiposItens$grupoprincipal))
    
    
    ##########################################################
    # *** AGURADANDO NOVA MODELAGEM - 12/04/2016 ***
    ##########################################################
    
    df_banco<-consultadb("select * from CLASSE_COMERCIAL")
    
    faltam<-df[! df$COD_CLASSE_COMERCIAL %in% df_banco$COD_CLASSE_COMERCIAL,]
    
    for (n in 1:nrow(faltam)){
        sql<-paste0("insert into CLASSE_COMERCIAL (COD_CLASSE_COMERCIAL,COD_CLASSE_COMERCIAL_PAI,DESCR_CLASSE_COMERCIAL) ")
    }
}






################################################################################################################################
# Função que processa os arquivos gerados pelo SoftPharma com a movimentação das lojas. 
# Os arquivos estão sendo gerados agrupando as vendas quinzenalmente.
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
#
# 18/02/2016    fpaim   agrupando registros em data frames de 5000 linhas e concatenando no data frame final para
#                       aumentar a performance.
#
##################################################################################################################################
XL7_ImportaMovimentacao_SoftPharma <- function(cliente,path){
    
    
    library(stringr)
    
    
    if (!hasArg(path) | !hasArg(cliente))
        stop("Sintaxe: XL7_ImportaMovimentacao_SoftPharma ('cliente','path')\nA funcao importa todos os arquivos em 'path' nomeados no seguinte padrao MOVIMENTACAO*.txt.")
    
    print("Carregando arquivos para memória...")
    
    # pega todos os arquivos no padrao MOVIMENTACAO*.txt do diretorio path
    filenames <- list.files(path, glob2rx("MOVIMENTACAO*.txt"), full.names=TRUE)
    
    movimentacao <- lapply(filenames, read.csv,sep=";"
                           ,header = FALSE
                           ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                           ,stringsAsFactors = FALSE)
    movimentacao<-do.call("rbind",movimentacao)
    
    
    ## cria data frame com estrutura definitiva
    movimentacaoItens <- data.frame(cliente=character()
                                    ,filial=character()
                                    ,codigo=character()
                                    ,descricao=character()
                                    ,tipoproduto=character()
                                    ,total=character()
                                    ,unitario=character()
                                    ,qtd_av=character()
                                    ,qtd_apz=character()
                                    ,qtd_total=character()
                                    ,customedio=character()
                                    ,lab=character()
                                    ,laboratorio=character()
                                    ,estoque=character()
                                    ,dia=character()
                                    ,stringsAsFactors = FALSE)
    
    movimentacaoTemp <-data.frame(matrix("",ncol=15,nrow=5000),stringsAsFactors = F)
    names(movimentacaoTemp)<-c("cliente","filial","codigo","descricao","tipoproduto","total","unitario","qtd_av","qtd_apz","qtd_total","customedio","lab","laboratorio","estoque","dia")
    
    print("Iniciando processamento dos dados...")
    
    j <- 0 
    
    for (i in 1:nrow(movimentacao)){
        
        if (!is.na(as.numeric(substr(movimentacao[i,],1,6)))){
            ## não é nulo, portanto produto válido
            j <- j + 1
            
            movimentacaoTemp$cliente[j]     <- cliente
            movimentacaoTemp$filial[j]      <- filial
            movimentacaoTemp$codigo[j]      <- substr(movimentacao[i,],1 ,6)
            movimentacaoTemp$descricao[j]   <- substr(movimentacao[i,],8,38)
            movimentacaoTemp$tipoproduto[j] <- substr(movimentacao[i,],39,50)
            movimentacaoTemp$total[j]       <- substr(movimentacao[i,],52,62)
            movimentacaoTemp$unitario[j]    <- substr(movimentacao[i,],64,71)
            movimentacaoTemp$qtd_av[j]      <- substr(movimentacao[i,],73,79)
            movimentacaoTemp$qtd_apz[j]     <- substr(movimentacao[i,],81,88)
            movimentacaoTemp$qtd_total[j]   <- substr(movimentacao[i,],90,99)
            movimentacaoTemp$customedio[j]  <- substr(movimentacao[i,],100,111)
            movimentacaoTemp$lab[j]         <- substr(movimentacao[i,],113,116)
            movimentacaoTemp$laboratorio[j] <- substr(movimentacao[i,],117,125)
            movimentacaoTemp$estoque[j]     <- substr(movimentacao[i,],127,132)
            movimentacaoTemp$dia[j]      <- dia
            
            if (j %% 5000==0){
                movimentacaoItens<-rbind(movimentacaoItens,movimentacaoTemp)
                print(paste0("Registros importados: ",nrow(movimentacaoItens)))
                j<-0
            }
            
            
            #movimentacaoItens <- rbind(movimentacaoItens,movimentacaoTemp)
            
            
        } else if (length(grep("FILIAL...:",movimentacao[i,]))!=0) {
            
            filial <-substr(movimentacao[i,],11,14)
            
        } else if (length(grep("\\| Data: ",movimentacao[i,]))!=0) {
            
            dia<-substr(strsplit(movimentacao[i,],"Data: ")[[1]][2],1,10)
            
        }
        
    } ## for
    
    # concatena as linhas restantes do movimentacaoTemp
    movimentacaoItens<-rbind(movimentacaoItens,movimentacaoTemp[1:j,])
    
    
    # corrige tipo das colunas numericas
    for (coluna in c("total","unitario","qtd_av","qtd_apz","qtd_total","customedio","estoque")){
        movimentacaoItens[,coluna]<-as.numeric(movimentacaoItens[,coluna])
    }
    
    movimentacaoItens$codigo<-as.character(movimentacaoItens$codigo)
    movimentacaoItens$descricao<-trimws(movimentacaoItens$descricao,which='both')
    movimentacaoItens$laboratorio<-trimws(movimentacaoItens$laboratorio,which='both')
    movimentacaoItens$codigo<-as.numeric(movimentacaoItens$codigo)
    
    
    print("Buscando EAN para os produtos atraves do arquivo produtos.csv")
    # busca arquivo de produtos para buscar o EAN atraves do codigo interno do produto
    
    produto<-read.csv2(paste0(path,"/produtos.csv"))
    produto$EAN<-as.character(produto$EAN)
    produto$descricao<-trimws(produto$descricao,which='both')
    produto<-unique(produto[,c("codigo","descricao","EAN","tp")])

    movimentacaoItens<-merge(x=movimentacaoItens,y=produto[,c("codigo","EAN","tp")],by="codigo",all.x=T)
    movimentacaoItens$tp_pai<-str_split_fixed(movimentacaoItens$tp,"\\.",2)[,1]
    movimentacaoItens$tp_pai<-paste0(movimentacaoItens$tp_pai,".0")
    movimentacaoItens$tp=trimws(movimentacaoItens$tp)
    
    # pega os tipos de produtos do arquivo
    # TROCAR PARA PEGAR DA BASE DE DADOS (ASSIM QUE TIVER SIDO IMPORTADO PARA LA!)
    tipoproduto<-XL7_ImportaTipoProduto_SoftPharma(cliente,path)
    tipoproduto$cod<-trimws(tipoproduto$cod)
    
    # adiciona grupo principal
    movimentacaoItens<-merge(x=movimentacaoItens,y=tipoproduto[,c("cod","grupoprincipal")],by.x="tp",by.y="cod",all.x=T)
    
    movimentacaoItens$ano<-substr(movimentacaoItens$dia,7,10)
    movimentacaoItens$mes<-substr(movimentacaoItens$dia,4,5)
 
    # calcula rentabilidade (dividir pelo total para dar a porcentagem)
    movimentacaoItens$rentabilidade<-movimentacaoItens$total-movimentacaoItens$customedio
    
    
    # retorna data frame criado
    movimentacaoItens
}
#movimentacao<-XL7_ImportaMovimentacao_SoftPharma("1","C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA")
#write.csv2(movimentacao,"C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/movimentacao.csv",row.names = F)

#movimentacao<-XL7_ImportaMovimentacao_SoftPharma("2","C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/Desconto Popular")
#write.csv2(movimentacao,"C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/Desconto Popular/movimentacaoDescontoPopular.csv",row.names = F)




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
    
    prin("Carregando arquivos para memória...")
    
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







################################################################################################################################
# Função que processa os arquivos gerados pelo SoftPharma com a DEVOLUÇÂO das lojas. 
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
# 
# Funcao adaptada da XL7_ImportaMovimentacao_SoftPharma
#
##################################################################################################################################
XL7_ImportaDevolucao_SoftPharma <- function(cliente,path){
    
    library(stringr)
    
    
    if (!hasArg(path) | !hasArg(cliente))
        stop("Sintaxe: XL7_ImportaDevolucao_SoftPharma ('cliente','path')\nA funcao importa todos os arquivos em 'path' nomeados no seguinte padrao DEVOLUCOES*.txt.")
    
    print("Carregando arquivos para memória...")
    
    # pega todos os arquivos no padrao DEVOLUCOES*.txt do diretorio path
    filenames <- list.files(path, glob2rx("DEVOLUCOES*.txt"), full.names=TRUE)
    
    devolucao <- lapply(filenames, read.csv,sep=";"
                           ,header = FALSE
                           ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                           ,stringsAsFactors = FALSE)
    devolucao<-do.call("rbind",devolucao)
    
    
    ## cria data frame com estrutura definitiva
    devolucaoItens <- data.frame(cliente=character()
                                    ,filial=character()
                                    ,codigo=character()
                                    ,descricao=character()
                                    ,tipoproduto=character()
                                    ,total=character()
                                    ,unitario=character()
                                    ,qtd_av=character()
                                    ,qtd_apz=character()
                                    ,qtd_total=character()
                                    ,customedio=character()
                                    ,lab=character()
                                    ,laboratorio=character()
                                    ,estoque=character()
                                    ,dia=character()
                                    ,stringsAsFactors = FALSE)
    
    devolucaoTemp <-data.frame(matrix("",ncol=15,nrow=5000),stringsAsFactors = F)
    names(devolucaoTemp)<-c("cliente","filial","codigo","descricao","tipoproduto","total","unitario","qtd_av","qtd_apz","qtd_total","customedio","lab","laboratorio","estoque","dia")
    
    print("Iniciando processamento dos dados...")
    
    j <- 0 
    
    for (i in 1:nrow(devolucao)){
        
        if (!is.na(as.numeric(substr(devolucao[i,],1,6)))){
            ## não é nulo, portanto produto válido
            j <- j + 1
            
            devolucaoTemp$cliente[j]     <- cliente
            devolucaoTemp$filial[j]      <- filial
            devolucaoTemp$codigo[j]      <- substr(devolucao[i,],1 ,6)
            devolucaoTemp$descricao[j]   <- substr(devolucao[i,],8,38)
            devolucaoTemp$tipoproduto[j] <- substr(devolucao[i,],39,50)
            devolucaoTemp$total[j]       <- substr(devolucao[i,],52,62)
            devolucaoTemp$unitario[j]    <- substr(devolucao[i,],64,71)
            devolucaoTemp$qtd_av[j]      <- substr(devolucao[i,],73,79)
            devolucaoTemp$qtd_apz[j]     <- substr(devolucao[i,],81,88)
            devolucaoTemp$qtd_total[j]   <- substr(devolucao[i,],90,99)
            devolucaoTemp$customedio[j]  <- substr(devolucao[i,],100,111)
            devolucaoTemp$lab[j]         <- substr(devolucao[i,],113,116)
            devolucaoTemp$laboratorio[j] <- substr(devolucao[i,],117,125)
            devolucaoTemp$estoque[j]     <- substr(devolucao[i,],127,132)
            devolucaoTemp$dia[j]      <- dia
            
            if (j %% 5000==0){
                devolucaoItens<-rbind(devolucaoItens,devolucaoTemp)
                print(paste0("Registros importados: ",nrow(devolucaoItens)))
                j<-0
            }
            
            
            #devolucaoItens <- rbind(devolucaoItens,devolucaoTemp)
            
            
        } else if (length(grep("FILIAL...:",devolucao[i,]))!=0) {
            
            filial <-substr(devolucao[i,],11,14)
            
        } else if (length(grep("\\| Data: ",devolucao[i,]))!=0) {
            
            dia<-substr(strsplit(devolucao[i,],"Data: ")[[1]][2],1,10)
            
        }
        
    } ## for
    
    # concatena as linhas restantes do devolucaoTemp
    devolucaoItens<-rbind(devolucaoItens,devolucaoTemp[1:j,])
    
    
    # corrige tipo das colunas numericas
    for (coluna in c("total","unitario","qtd_av","qtd_apz","qtd_total","customedio","estoque")){
        devolucaoItens[,coluna]<-as.numeric(devolucaoItens[,coluna])
    }
    
    devolucaoItens$codigo<-as.character(devolucaoItens$codigo)
    devolucaoItens$descricao<-trimws(devolucaoItens$descricao,which='both')
    devolucaoItens$laboratorio<-trimws(devolucaoItens$laboratorio,which='both')
    devolucaoItens$codigo<-as.numeric(devolucaoItens$codigo)
    
    #print("Buscando EAN para os produtos atraves do arquivo estoque.csv")
    # busca arquivo de estoque para buscar o EAN atraves da descricao.
    # o certo seria buscar atraves do codigo do produto que vem no arquivo de devolucao comparando com algum
    # relatorio do SoftPharma que tenha o cadastro dos produtos com codigo e EAN
    # inclusive, no estoque aparecem varios EAN para algumas descricoes: ex: 'ALPRAZOLAM 1 MG C/20 CPR'
    #estoque<-read.csv2(paste0(path,"/estoque.csv"))
    #estoque$descricao<-trimws(estoque$descricao,which='both')
    #estoque<-unique(estoque[order(-estoque$qtd),c("codigo","descricao","qtd")])
    #colnames(estoque)[1]<-"EAN"
    
    # como pegando o EAN atraves do estoque aparece mais de 1 EAN para cada produto, 
    # vou deixar apenas o primeiro EAN (ordenado por qtd no estoque acima)!
    #estoque<-estoque[!duplicated(estoque[,c('descricao')]),]
    
    #devolucaoItens<-merge(x=devolucaoItens,y=estoque[,c("EAN","descricao")],by="descricao",all.x=T)
    
    
    print("Buscando EAN para os produtos atraves do arquivo produtos.csv")
    # busca arquivo de produtos para buscar o EAN atraves do codigo interno do produto
    
    produto<-read.csv2(paste0(path,"/produtos.csv"))
    produto$EAN<-as.character(produto$EAN)
    produto$descricao<-trimws(produto$descricao,which='both')
    produto<-unique(produto[,c("codigo","descricao","EAN","tp")])
    
    devolucaoItens<-merge(x=devolucaoItens,y=produto[,c("codigo","EAN","tp")],by="codigo",all.x=T)
    devolucaoItens$tp_pai<-str_split_fixed(devolucaoItens$tp,"\\.",2)[,1]
    devolucaoItens$tp_pai<-paste0(devolucaoItens$tp_pai,".0")
    devolucaoItens$tp=trimws(devolucaoItens$tp)
    
    # pega os tipos de produtos do arquivo
    # TROCAR PARA PEGAR DA BASE DE DADOS (ASSIM QUE TIVER SIDO IMPORTADO PARA LA!)
    tipoproduto<-XL7_ImportaTipoProduto_SoftPharma(cliente,path)
    tipoproduto$cod<-trimws(tipoproduto$cod)
    
    # adiciona grupo principal
    devolucaoItens<-merge(x=devolucaoItens,y=tipoproduto[,c("cod","grupoprincipal")],by.x="tp",by.y="cod",all.x=T)
    
    devolucaoItens$ano<-substr(devolucaoItens$dia,7,10)
    devolucaoItens$mes<-substr(devolucaoItens$dia,4,5)
    
    # calcula rentabilidade
    #devolucaoItens$rentabilidade<-devolucaoItens$total-devolucaoItens$customedio
    
    
    # retorna data frame criado
    devolucaoItens
}





##################################################################################################################################
# Função busca conformidade de preços no site da Anvisa
# A função retorna um data frame com todos os registros e por isso deve ser direcionada para uma variavel na chamada
#
# 26/02/2016    fpaim   Nao consegui importar direto pra memoria, tive que fazer download primeiro para depois importar...
##################################################################################################################################
XL7_ImportaPrecoMaximoAnvisa<-function(){
    
    require(XML)
    require(xlsx)
    
    # PEGAR O LINK CORRETO A PARTIR DESSE LINK: http://portal.anvisa.gov.br/wps/portal/anvisa/anvisa/regulado
    
    
    # importa xls da anvisa com preços: http://portal.anvisa.gov.br/wps/portal/anvisa/anvisa/regulado/!ut/p/c4/04_SB8K8xLLM9MSSzPy8xBz9CP0os3hnd0cPE3MfAwMDMydnA093Uz8z00B_A3cXQ_2CbEdFAMyIKzk!/?urile=wcm%3Apath%3A/anvisa+portal/anvisa/pos+-+comercializacao+-+pos+-+uso/regulacao+de+marcado/publicacao+regulacao+economica/listas+de+precos+de+medicamentos+03
    #anvisa<-read.xlsx2("C:/Users/Felipe/OneDrive/Documentos/Negocios/XL7/Farmacias/Dados/xls_conformidade_2016_01_29.xls",1,startRow = 4,header = T,colClasses = c("character","character","character","character","character","character","character","character","character","double","double","double","double","double","double","double","double","double","double","double","double","double","double","character","character","character","character","character"))
    #http://portal.anvisa.gov.br/wps/wcm/connect/baf114004c270d30ae1dbe31039a6daa/xls_conformidade_2016_03_24.XLS?MOD=AJPERES
    
    #busca html
    #html<- htmlTreeParse(paste0('http://portal.anvisa.gov.br/wps/portal/anvisa/anvisa/regulado/!ut/p/c4/04_SB8K8xLLM9MSSzPy8xBz9CP0os3hnd0cPE3MfAwMDMydnA093Uz8z00B_A3cXQ_2CbEdFAMyIKzk!/?urile=wcm%3Apath%3A/anvisa+portal/anvisa/pos+-+comercializacao+-+pos+-+uso/regulacao+de+marcado/publicacao+regulacao+economica/listas+de+precos+de+medicamentos+03'),useInternal=TRUE)
    html<-htmlTreeParse(paste0('http://portal.anvisa.gov.br/wps/portal/anvisa/anvisa/regulado/!ut/p/c5/04_SB8K8xLLM9MSSzPy8xBz9CP0os3hnd0cPE3MfAwMDMydnA093Uz8z00B_A3cXQ6B8JE55A38jYnQb4ACOBgR0e-lHZSbl6pUn5-oZ6JkZGxkbG1hYmJgam5maGRnph4P8gt9tIHk8tvt55Oem6hfkhkZUBgekAwD2wtpI/?1dmy&urile=wcm%3apath%3a/anvisa+portal/anvisa/pos+-+comercializacao+-+pos+-+uso/regulacao+de+marcado/assunto+de+interesse/mercado+de+medicamentos/listas+de+precos+de+medicamentos+03'),useInternal=TRUE)
    
    # pega todos os links, pois o html nao tem estrutura 
    link<-xpathSApply(html,"//a",xmlGetAttr,'href')
    
    # pega o link correto baseado no padrao xls_conformidade_ano (para evira pegar o xls_conformidade_gov)
    link<-link[grep('xls_conformidade_[0-9]',link)][[1]]
    link<-paste0("http://portal.anvisa.gov.br",link)
    
    # como nao consegui importar o xls direto para memoria, primeiro tenho que baixar o arquivo depois importar
    arq<-strsplit(link,'/')
    arq<-arq[[1]][length(arq[[1]])]
    arq<-strsplit(arq,"\\?")[[1]][1]
    arq<-paste0("c:/Temp/",arq)
    
    #OLDLINK: "http://portal.anvisa.gov.br/wps/wcm/connect/d3ef73004bc7ab768c9efdd8b204cf25/xls_conformidade_2016_02_22.XLS?MOD=AJPERES"
    download.file(link, arq, mode="wb")
    
    # importa data frame
    anvisa<-read.xlsx2(arq,1,startRow = 10,header = T, 
                       colClasses = c("character","character","character","character","character","character",
                                    "character","character","character","double","double","double","double",
                                    "double","double","double","double","double","double","double","double",
                                    "double","double","character","character","character","character","character"))
    
    names(anvisa)<-c("PrincipioAtivo","CNPJ","Laboratorio","CodigoGGREM","Registro","EAN","Produto","Apresentacao","ClasseTerapeutica","PF.0","PF.12",
                     "PF.17","PF.18","PF.19","PF.17.ZFManaus","PF.18.ZFManaus","PMC.0","PMC.12","PMC.17","PMC.18","PMC.19","PMC.17.ZFManaus",
                     "PMC.18.ZFManaus","RestricaoHospitalar","CAP","CONFAZ-87","AnaliseRecursal","UltimaAlteracao")
    
    
    # correcoes
    anvisa$Apresentacao<-as.character(anvisa$Apresentacao)
    anvisa$Apresentacao<-trimws(anvisa$Apresentacao,which = 'both')
    # tira a sujeira que fica a esquerda de algumas apresentacoes, que não é um espaço em branco no Excel da ANVISA!
    anvisa$Apresentacao<-gsub(" ","",anvisa$Apresentacao)
    #remove caracteres que geram problemas
    anvisa$Apresentacao<-gsub("\\","",anvisa$Apresentacao,fixed=TRUE)
    anvisa$Apresentacao<-gsub("'","",anvisa$Apresentacao,fixed=TRUE)

    #corrige charset
    anvisa$PrincipioAtivo<- iconv(anvisa$PrincipioAtivo,'UTF-8','latin1')
    anvisa$Laboratorio<- iconv(anvisa$Laboratorio,'UTF-8','latin1')
    anvisa$Produto<- iconv(anvisa$Produto,'UTF-8','latin1')
    anvisa$Apresentacao<- iconv(anvisa$Apresentacao,'UTF-8','latin1')
    anvisa$ClasseTerapeutica<- iconv(anvisa$ClasseTerapeutica,'UTF-8','latin1')
    
    # retorna data frame
    anvisa
}





##################################################################################################################################
# Função que cadastra na base: Medicamentos, Classes Terapeuticas, etc.
# Os dados são obtidos diretamente do site da ANVISA através da função XL7_ImportaPrecoMaximoAnvisa
##################################################################################################################################
XL7_CadastraDadosANVISA<-function(){
    
    library(stringr)
    # Busca dados através da tabela de preço Maximo ao Consumidor no site da Anvisa
    anvisa<-XL7_ImportaPrecoMaximoAnvisa()
    
    
    print("Atualizando Fabricantes")
    # Busca fabricantes ja cadastradas
    lista<-consultadb("select CNPJ,DESCR_FABRICANTE from FABRICANTE")
    
    lista_anvisa<-unique(anvisa[,c("CNPJ","Laboratorio")])
    # deixa apenas numeros do CNPJ
    #lista_anvisa$CNPJ<-gsub("[^0-9]", "", unlist(lista_anvisa$CNPJ))
    
    
    faltam<-lista_anvisa[! lista_anvisa$CNPJ %in% lista$CNPJ,c("CNPJ","Laboratorio")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        
        for (CNPJ in faltam$CNPJ){
            Laboratorio<-faltam[faltam$CNPJ==CNPJ,"Laboratorio"]
            if(!is.na(CNPJ)){
                #converte charset
                #Laboratorio<-iconv(Laboratorio,'UTF-8','latin1')
                
                # consulta se ja existe
                linha<-dbGetQuery(mydb,paste0("select * from FABRICANTE where CNPJ='",CNPJ,"'"))
                
                if (nrow(linha)==0){
                    # cadastra fabricante
                    dbGetQuery(mydb,paste0("insert into FABRICANTE (CNPJ,DESCR_FABRICANTE) values ('",CNPJ,"','",Laboratorio,"')"))
                } else if (linha$DESCR_FABRICANTE!=Laboratorio){ # verifica se trocou o nome do laboratorio com base no CNPJ
                    # altera descricao pois esta diferente na base
                    dbGetQuery(mydb,paste0("update FABRICANTE set DESCR_FABRICANTE='",Laboratorio,"' where CNPJ='",CNPJ,"'"))
                    
                }
            }
            
        }
        dbDisconnect(mydb)
    }
    
    
    print("Atualizando Classes Terapeuticas")
    # Cadastra Classes Terapeuticas Novas

    
    classes<-data.frame(DESCR_ANVISA=anvisa$ClasseTerapeutica)

    classes$COD_ANVISA<-str_split_fixed(classes$DESCR_ANVISA," - ",2)[,1]
    classes$DESCR_CLASSE_TERAP<-str_split_fixed(classes$DESCR_ANVISA," - ",2)[,2]
    classes<-classes[complete.cases(classes),]
    
    # mesmo trocando o charset na funcao que busca os dados da ANVISA, para classe_terapeutica tem que trocar de novo
    classes$DESCR_CLASSE_TERAP<-iconv(classes$DESCR_CLASSE_TERAP,'UTF-8','latin1')
    
    
    classes_base<-consultadb("select * from CLASSE_TERAPEUTICA")
    
    classes<-unique(classes)
    
    faltam<-classes[(! classes$COD_ANVISA %in%  classes_base$COD_ANVISA)
                              ,c("COD_ANVISA","DESCR_CLASSE_TERAP")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            # cadastra associacao nova (codigo auto-incremental)
            dbGetQuery(mydb,paste0("insert into CLASSE_TERAPEUTICA (COD_ANVISA,DESCR_CLASSE_TERAP) 
                                   values ('",faltam[n,"COD_ANVISA"],"','",faltam[n,"DESCR_CLASSE_TERAP"],"')"))
            
            
        }
        dbDisconnect(mydb)
    }
    
    
    
    print("Atualizando Apresentações")
    # Busca Apresentações ja cadastradas
    lista<-consultadb("select DESCR_APRESENTACAO from APRESENTACOES")
    anvisa_apresentacao<-data.frame(apresentacao=unique(anvisa$Apresentacao))
    anvisa_apresentacao$apresentacao<-as.character(anvisa_apresentacao$apresentacao)
    
    
    faltam<-anvisa_apresentacao[! anvisa_apresentacao$apresentacao %in% lista$DESCR_APRESENTACAO,"apresentacao"]
    
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    
    for (apresentacao in faltam){
      
        if(!is.na(apresentacao)){
            # cadastra apresentacao nova (codigo auto-incremental)
            dbGetQuery(mydb,paste0("insert into APRESENTACOES (DESCR_APRESENTACAO) values ('",apresentacao,"')"))
        }

    }
    dbDisconnect(mydb)
    
    
    ############
    # cria df que servira para popular as tabelas de principios ativos 
    ############
    
    print("cria lista de principios ativos...")
    #lista_anvisa<-data.frame(apresentacao=anvisa$Apresentacao,principio=anvisa$PrincipioAtivo)
    
    #lista_anvisa$apresentacao<-as.character(lista_anvisa$apresentacao)
    #lista_anvisa$principio<-as.character(lista_anvisa$principio)
    
    # quebra principios ativos (separados por ;) para deixar itens únicos
    #associacao<-data.frame(apresentacao=character(),principio=character())
    principios<-data.frame(PRINCIPIO_ATIVO=character())
    for (n in unique(anvisa$PrincipioAtivo)){
        for (x in strsplit(n,";")){
            #associacao<-rbind(associacao,data.frame(apresentacao=lista_anvisa[n,"apresentacao"],principio=trimws(x,which = 'both')) )          
            principios<-rbind(principios,data.frame(PRINCIPIO_ATIVO=trimws(x)))
        } 
    }
    # remove dupliacados
    principios<-unique(principios)
    
    print("Atualizando Principios Ativos")
    # Busca principios ativos ja cadastrados
    lista<-consultadb("select PRINCIPIO_ATIVO from PRINCIPIOS_ATIVOS")
    
    
    # gera lista com principios nao cadastrados na base
    faltam<-principios[! principios$PRINCIPIO_ATIVO %in% lista$PRINCIPIO_ATIVO,"PRINCIPIO_ATIVO"]
    
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    
    for (principio in faltam){
        #converte charset
        #principio<-iconv(principio,'UTF-8','latin1')
        
        if(!is.na(principio)){
            # cadastra principio novo (codigo auto-incremental)
            dbGetQuery(mydb,paste0("insert into PRINCIPIOS_ATIVOS (PRINCIPIO_ATIVO) values ('",principio,"')"))
        }
        
    }
    dbDisconnect(mydb)
    
    

    print("Atualizando Associacoes")
    # Busca associacoes ja cadastrados
    lista<-consultadb("select * from ASSOCIACOES")
    
    # cria associacao baseada em principios + apresentacao
    apresentacao<-consultadb("select * from APRESENTACOES")
    associacao<-anvisa[,c("PrincipioAtivo","Apresentacao")]
    
    #associacao$Apresentacao<-iconv(associacao$Apresentacao,'UTF-8','latin1')
    
    
    #remove duplicados
    associacao<-unique(associacao)
    
    # usa dataset criado anteriormente
    associacao<-merge(associacao,apresentacao,by.x='Apresentacao',by.y='DESCR_APRESENTACAO',all=T)
   
    associacao$DESCR_ASSOCIACAO<-paste(associacao$PrincipioAtivo,associacao$Apresentacao)

    # gera lista com principios nao cadastrados na base
    faltam<-associacao[! associacao$DESCR_ASSOCIACAO %in%  lista$DESCR_ASSOCIACAO ,c("DESCR_ASSOCIACAO","COD_APRESENTACAO")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            if(!is.na(faltam[n,"DESCR_ASSOCIACAO"])){
                # cadastra associacao nova (codigo auto-incremental)
                dbGetQuery(mydb,paste0("insert into ASSOCIACOES (DESCR_ASSOCIACAO,APRESENTACOES_COD_APRESENTACAO) 
                                   values ('",faltam[n,"DESCR_ASSOCIACAO"],"',",faltam[n,"COD_APRESENTACAO"],")"))
            }
            
        }
        dbDisconnect(mydb)
    }
    
    #############################
    # popula PRINC_ATIV_ASSOCS
    ############################
    
    # busca os codigos das associacoes
    
    assoc_base<-consultadb("select * from ASSOCIACOES")
    
    # merge para pegar cod_principio e cod_associacao para popular PRINC_ATIV_ASSOCS
    temp<-merge(associacao,assoc_base,by.x='COD_APRESENTACAO',by.y='APRESENTACOES_COD_APRESENTACAO',all.x=TRUE)
    colnames(temp)[3]<-"PRINCIPIO_ATIVO"
    temp$PRINCIPIO_ATIVO<-as.character(temp$PRINCIPIO_ATIVO)
    
    # quebra principios ativos (separados por ;) para deixar itens únicos
    princ_ativ_assocs<-data.frame(PRINCIPIO_ATIVO=character(),COD_PRINCIPIO_ATIVO=character(),COD_APRESENTACAO=character(),
                                  COD_ASSOCIACAO=character(),APRESENTACAO=character())
    #Sys.time()
    #for (n in 1:NROW(temp)){
    #    for (x in strsplit(temp[n,"PRINCIPIO_ATIVO"],";")){
    #       # if(nrow(princ_ativ_assocs[princ_ativ_assocs$PRINCIPIO_ATIVO==x & princ_ativ_assocs$COD_APRESENTACAO==temp[n,"COD_APRESENTACAO"],])==0){
    #            princ_ativ_assocs<-rbind(unique(princ_ativ_assocs),data.frame(PRINCIPIO_ATIVO=trimws(x),COD_PRINCIPIO_ATIVO=NA,
    #                                            COD_APRESENTACAO=temp[n,"COD_APRESENTACAO"],COD_ASSOCIACAO=temp[n,"COD_ASSOCIACAO"],
    #                                            APRESENTACAO=temp[n,"Apresentacao"]))
    #        #}
    #    } 
    #}
    #Sys.time()
    
    s <- strsplit(as.character(temp$PRINCIPIO_ATIVO), ';')
    princ_ativ_assocs<-data.frame(PRINCIPIO_ATIVO=trimws(unlist(s)), COD_APRESENTACAO=rep(temp$COD_APRESENTACAO, sapply(s, FUN=length)),
                  COD_PRINCIPIO_ATIVO=NA,
                  COD_ASSOCIACAO=rep(temp$COD_ASSOCIACAO, sapply(s, FUN=length)),
                  APRESENTACAO=rep(temp$Apresentacao, sapply(s, FUN=length)))
    princ_ativ_assocs<-unique(princ_ativ_assocs)
    
    
    # atualiza dataframe principios da base
    principios<-consultadb("select * from PRINCIPIOS_ATIVOS")
    
    # concatena o codigo do principio
    princ_ativ_assocs<-merge(princ_ativ_assocs,principios,by="PRINCIPIO_ATIVO",all.x=TRUE)
    princ_ativ_assocs$COD_PRINCIPIO_ATIVO.x<-NULL
    colnames(princ_ativ_assocs)[5]<-"COD_PRINCIPIO_ATIVO"
    
    
    
    princ_ativ_assocs_base<-consultadb("select * from PRINC_ATIV_ASSOCS")
    
    faltam<-princ_ativ_assocs[(! princ_ativ_assocs$COD_ASSOCIACAO %in%  princ_ativ_assocs_base$COD_ASSOCIACAO)
                              & (! princ_ativ_assocs$COD_PRINCIPIO_ATIVO %in%  princ_ativ_assocs_base$COD_PRINCIPIO_ATIVO)
                                  ,c("COD_PRINCIPIO_ATIVO","COD_ASSOCIACAO","APRESENTACAO")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            if(!is.na(faltam[n,"APRESENTACAO"])){
                # cadastra princ_ativ_assocs nova (codigo auto-incremental)
                dbGetQuery(mydb,paste0("insert into PRINC_ATIV_ASSOCS (COD_ASSOCIACAO,COD_PRINCIPIO_ATIVO,DESCRITIVO_DOSAGEM) 
                                   values (",faltam[n,"COD_ASSOCIACAO"],",",faltam[n,"COD_PRINCIPIO_ATIVO"],",'",
                                       faltam[n,"APRESENTACAO"], "')"))
            }
            
        }
        dbDisconnect(mydb)
    }
    
    
    
    
    
    ######################################
    # PRODUTOS
    ######################################
    produtos<-anvisa[,c("PrincipioAtivo","EAN","Produto","Apresentacao","ClasseTerapeutica","CNPJ")]
    #produtos$ClasseTerapeutica<-iconv(produtos$ClasseTerapeutica,'UTF-8','latin1')
    produtos$COD_ANVISA<-str_split_fixed(produtos$ClasseTerapeutica," - ",2)[,1]
    
    # busca novamente associacoes e apresentacoes atualizadas
    associacoes<-consultadb("select * from ASSOCIACOES")

    produtos$Associacao<-paste(produtos$PrincipioAtivo,produtos$Apresentacao)
    produtos<-merge(produtos,associacoes,by.x="Associacao",by.y="DESCR_ASSOCIACAO",all=T)
    
    
    # pega as classes terapeuticas
    classes<-consultadb("select * from CLASSE_TERAPEUTICA")
    # reconstroi a descricao original da anvisa, separando codigo e descricao por " - "
    classes$DESCR_ANVISA<-paste0(classes$COD_ANVISA," - ",classes$DESCR_CLASSE_TERAP)
    
    produtos<-merge(produtos,classes,by.x="ClasseTerapeutica",by.y="DESCR_ANVISA",all=F)
    
    
    # busca fabricante
    fabricante<-consultadb("select * from FABRICANTE")
    
    produtos<-merge(produtos,fabricante,by="CNPJ",all.x=T)
    
    # busca produtos ja cadastrados
    produtos_base<-consultadb("select * from PRODUTOS")
    

    faltam<-produtos[(! produtos$COD_FABRICANTE %in%  produtos_base$COD_FABRICANTE)
                              & (! produtos$Produto %in%  produtos_base$NOME_COMERCIAL)
                              ,c("COD_FABRICANTE","Produto","COD_CLASSE_TERAP","COD_ASSOCIACAO")]
    
    faltam<-unique(faltam) # pois tem produtos com mais de 1 EAN. Senao, cadastraria 2 produtos iguais (pois nao vai o EAN na tab. produto)
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            # cadastra produto novo (codigo auto-incremental)
            dbGetQuery(mydb,paste0("insert into PRODUTOS (NOME_COMERCIAL,COD_FABRICANTE,COD_CLASSE_TERAP,COD_ASSOCIACAO) 
                                   values ('",faltam[n,"Produto"],"',",faltam[n,"COD_FABRICANTE"],",'",
                                   faltam[n,"COD_CLASSE_TERAP"],"',",
                                   faltam[n,"COD_ASSOCIACAO"], ")"))
            
        }
        dbDisconnect(mydb)
    }
    
    
    
    
    
    ###############################
    # EAN PRODUTO
    ###############################
    
    # busca os codigos dos produtos e atualiza o data frame criado acima
    cod_prods<-consultadb("select * from PRODUTOS")
    
    produtos<-merge(produtos,cod_prods,by.x=c("Produto","COD_FABRICANTE","COD_CLASSE_TERAP","COD_ASSOCIACAO"),
                    by.y=c("NOME_COMERCIAL","COD_FABRICANTE","COD_CLASSE_TERAP","COD_ASSOCIACAO"),all.x=T)
    
    # busca eans_produtos ja cadastrados
    eans_produtos_base<-consultadb("select * from EANS_PRODUTOS")
    
    
    faltam<-produtos[(! produtos$COD_PRODUTO %in%  eans_produtos_base$PRODUTOS_COD_PRODUTO)
                     & (! produtos$EAN %in%  eans_produtos_base$CODIGO_EAN)
                     ,c("COD_PRODUTO","EAN")]

    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            # cadastra produto novo (codigo auto-incremental)
            dbGetQuery(mydb,paste0("insert into EANS_PRODUTOS (CODIGO_EAN,PRODUTOS_COD_PRODUTO) 
                                   values ('",faltam[n,"EAN"],"',",faltam[n,"COD_PRODUTO"],")"))
            
        }
        dbDisconnect(mydb)
    }
    
    
    
    
    
    
    
}



##################################################################################################
#
# Usa o data frame gerado na função XL7_Importaproduto_SoftPharma e cadastra na base XL7
#
##################################################################################################
XL7_CadastraProdutosClientes<-function(cliente,path){
    
    # para mostrar o EAN sem notificacao cientifica
    options("scipen"=100, "digits"=4)
    
    # busca produtos do cliente do arquivo exportado da SoftPharma
    prod_cli<-XL7_Importaproduto_SoftPharma(cliente,path)
    prod_cli$EAN<-as.numeric(prod_cli$EAN)
    names(prod_cli)[names(prod_cli) == 'cliente'] <- 'COD_CLIENTE'
    prod_cli$codigo<-as.numeric(prod_cli$codigo)
    
    # busca produtos com os códigos da XL7
    prod_XL7<-consultadb("select CODIGO_EAN,COD_PRODUTO from PRODUTOS, EANS_PRODUTOS where PRODUTOS.COD_PRODUTO=EANS_PRODUTOS.PRODUTOS_COD_PRODUTO")
    
    # relaciona codigo cliente com codigo XL7 atraves do EAN
    prod<-merge(prod_XL7,prod_cli,by.x="CODIGO_EAN",by.y="EAN")
    names(prod)[names(prod) == 'codigo'] <- 'COD_INTERNO'
    prod$COD_INTERNO<-as.numeric(prod$COD_INTERNO)

    
    # pega os produtos ja cadastrados do cliente
    prod_base<-consultadb(paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cliente))
    
    # verifica o que falta cadastrar e cadastra
    faltam<-prod[(! as.numeric(prod$COD_INTERNO) %in%  as.numeric(prod_base$COD_INTERNO)), c("COD_CLIENTE","COD_PRODUTO","COD_INTERNO")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into PRODUTOS_CLIENTES (COD_CLIENTE,COD_PRODUTO,COD_INTERNO) 
                                   values (",faltam[n,"COD_CLIENTE"],",",faltam[n,"COD_PRODUTO"],",",faltam[n,"COD_INTERNO"],")"))
            
        }
        dbDisconnect(mydb)
    }
    
 
}




##################################################################################################
#
# Funcao que cadastra os SKUs
# 20/04/2016 - Está cadastrando atualmente todos os produtos para todas as filiais do cliente
#
##################################################################################################
XL7_CadastraSKUs<-function(cliente){
    
    # para mostrar o EAN sem notificacao cientifica
    options("scipen"=100, "digits"=4)
    
    # busca as filiais do cliente
    filial<-consultadb(paste0("select * from FILIAL where COD_CLIENTE=",cliente))
    
    # busca PRODUTOS do cliente
    prod<-consultadb(paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cliente))
    
    # gera data frame das SKUs
    sku<-merge(filial,prod,by="COD_CLIENTE",all.x=T)
    
    # busca SKUs cadastradas na base
    sku_base<-consultadb(paste0("select * from SKU where COD_CLIENTE=",cliente))
    
    # verifica o que falta cadastrar e cadastra
    faltam<-sku[(! sku$COD_CLIENTE %in%  sku_base$COD_CLIENTE) &
                    (! sku$COD_PRODUTO %in% sku_base$COD_PRODUTO) &
                    (! sku$COD_INTERNO %in% sku_base$COD_INTERNO) & 
                    (! sku$COD_FILIAL_CLIENTE %in% sku_base$COD_FILIAL_CLIENTE)
                    , c("COD_CLIENTE","COD_PRODUTO","COD_INTERNO","COD_FILIAL_CLIENTE")]
    
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into SKU (COD_CLIENTE,COD_PRODUTO,COD_INTERNO,COD_FILIAL_CLIENTE) 
                                   values (",faltam[n,"COD_CLIENTE"],",",faltam[n,"COD_PRODUTO"],",",faltam[n,"COD_INTERNO"],",",faltam[n,"COD_FILIAL_CLIENTE"],")"))
            
        }
        dbDisconnect(mydb)
    }
    
    
}




XL7_Cadastra_Periodos<-function(anoinicial,anofinal){
    
    # pega quantidade de meses
    n<-(anofinal-anoinicial+1)*12
    
    if(n>0){
        #cria df com as datas
        primeirodia<-seq(as.Date(paste0(anoinicial,"-01-01")), length = n, by = "mon")
        ultimodia<-seq(as.Date(paste0(anoinicial,"-02-01")), length = n, by = "mon")-1
        
        p<-as.data.frame(cbind(primeirodia=as.character(primeirodia),ultimodia=as.character(ultimodia)))
        
        # mensal        TIPOS_DE_PERIODOS: 2
        # quinzenal     TIPOS_DE_PERIODOS: 1
        # diario        TIPOS_DE_PERIODOS: 3
        periodos<-data.frame(MASCARA_PERIODO=as.character(),COD_TIPO_PERIODO=as.numeric(),DATA_INICIAL=as.character(),DATA_FINAL=as.character())
        
        
        for (x in 1:nrow(p)){
            
            # periodo diario
            # mascara = proprio dia
            for (dia in as.character(seq(as.Date(p[x,"primeirodia"]),length=as.numeric(as.Date(p[x,"ultimodia"])-as.Date(p[x,"primeirodia"])+1),
                                         by="day"))){
                periodos<-rbind(periodos,data.frame(MASCARA_PERIODO=gsub("-","",dia),COD_TIPO_PERIODO=as.numeric(3),DATA_INICIAL=dia,DATA_FINAL=dia))
            }
     
            # periodo quinzenal
            periodos<-rbind(periodos,data.frame(MASCARA_PERIODO=paste0(substr(gsub("-","",dia),1,6),"Z1"),COD_TIPO_PERIODO=as.numeric(1),
                                                DATA_INICIAL=p[x,"primeirodia"],DATA_FINAL=paste0(substr(p[x,"primeirodia"],1,8),"15")))
            periodos<-rbind(periodos,data.frame(MASCARA_PERIODO=paste0(substr(gsub("-","",dia),1,6),"Z2"),COD_TIPO_PERIODO=as.numeric(1),
                                                DATA_INICIAL=paste0(substr(p[x,"primeirodia"],1,8),"16"),DATA_FINAL=p[x,"ultimodia"]))

            
            # periodo mensal
                periodos<-rbind(periodos,data.frame(MASCARA_PERIODO=paste0(substr(gsub("-","",dia),1,4),"M",substr(gsub("-","",dia),5,6)),
                                                COD_TIPO_PERIODO=as.numeric(2),DATA_INICIAL=p[x,"primeirodia"],DATA_FINAL=p[x,"ultimodia"]))

            
            
        }
        
        # busca pepriodos ja cadastrados
        periodos_base<-consultadb(paste0("select * from PERIODOS where DATA_INICIAL>='",primeirodia[1],"' and DATA_FINAL<='",ultimodia[length(ultimodia)],"'"))
  
        
        # pega os periodos que faltam        
        faltam<-periodos[(! periodos$MASCARA_PERIODO %in%  periodos_base$MASCARA_PERIODO),]
        
        # cadastra!
        if(nrow(faltam)>0){
            mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
            for (n in 1:nrow(faltam)){
                dbGetQuery(mydb,paste0("insert into PERIODOS (MASCARA_PERIODO,COD_TIPO_PERIODO,DATA_INICIAL,DATA_FINAL) 
                                       values ('",faltam[n,"MASCARA_PERIODO"],"',",faltam[n,"COD_TIPO_PERIODO"],",'",
                                       faltam[n,"DATA_INICIAL"],"','",faltam[n,"DATA_FINAL"],"')"))
                
            }
            dbDisconnect(mydb)
        }
        
  }
    
    
}



XL7_Importa_Fornecedores<-function(cliente,path){
    
    # busca fornecedores do arquivo gerado pelo SoftPharma para cadastrar fornecedores novos
    print("Carregando arquivos para memória...")
    
    # pega todos os arquivos no padrao DEVOLUCOES*.txt do diretorio path
    filenames <- list.files(path, glob2rx("FORNECEDORES*.txt"), full.names=TRUE)
    
    fornec <- lapply(filenames, read.csv,sep=";"
                     ,header = FALSE
                     ,encoding = "UTF-16"   # pois tem acento em maiuculas...
                     ,stringsAsFactors = FALSE)
    fornec<-do.call("rbind",fornec)

    ## cria data frame com estrutura definitiva
    fornecItens <- data.frame(codigo=character()
                                 ,descricao=character()
                                 ,CNPJ=character()
                                 ,stringsAsFactors = FALSE)
    
    fornecTemp <-data.frame(matrix("",ncol=3,nrow=5000),stringsAsFactors = F)
    names(fornecTemp)<-c("codigo","descricao","CNPJ")
    
    print("Iniciando processamento dos dados...")
    
    j <- 0 
    
    for (i in 1:nrow(fornec)){
        
        if (substr(fornec[i,],1,6)=="CODIGO"){
            j <- j + 1
            
            fornecTemp$codigo[j]         <- substr(fornec[i,],7 ,13)
            fornecTemp$descricao[j]      <- substr(fornec[i,],24 ,54)
            fornecTemp$CNPJ[j]           <- substr(fornec[i,],60 ,78)
            
            
            if (j %% 5000==0){
                fornecItens<-rbind(fornecItens,fornecTemp)
                print(paste0("Registros importados: ",nrow(fornecItens)))
                j<-0
            }
          
        }
        
    } ## for
    
    # concatena as linhas restantes do devolucaoTemp
    fornecItens<-rbind(fornecItens,fornecTemp[1:j,])
    
    fornecItens$codigo<-as.numeric(fornecItens$codigo)
    fornecItens$descricao<-trimws(fornecItens$descricao)
    fornecItens$CNPJ<-trimws(fornecItens$CNPJ)
    
    return(fornecItens)
    
    
}

#################################################################################################
# funcao que recebe o dataset criado pela XL7_Importa_Fornec e grava registros novos na base
#
# 23/04/2016    fpaim   Como no cadastro de fornecedores da base do SoftPharma existem diversos codigos para o mesmo CNPJ
#                       tive que deixar apenas 1 CNPJ para evitar redundancia na base
###################################################################################################

XL7_Cadastra_Fornecedores<-function(fornecItens){
    
    # busca fabricantes ja cadastrados
    fornec_base<-consultadb("select * from FABRICANTE")
    
    # deixa apenas a primeira ocorrencia de cada CNPJ
    fornecItens<-fornecItens[match(unique(fornecItens$CNPJ), fornecItens$CNPJ),]
    
    # verifica quais faltam cadastrar e cadatra
    faltam<-fornecItens[(! fornecItens$CNPJ %in%  fornec_base$CNPJ),]
    
    
    # adiciona na base series_dados
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into FABRICANTE (DESCR_FABRICANTE,CNPJ) 
                                   values ('",faltam[n,"descricao"],"','",faltam[n,"CNPJ"],"')"))
            
        }
        dbDisconnect(mydb)
    }
}



#############################################################################
#
# Funcao chamada da Cadastra_Seires_Dados para cadastrar eventuais produtos e fornecedores novos
#   que estejam no movimento mas não na base XL7
#
#############################################################################
XL7_Cadastra_Novos_ProdFornec<-function(cliente,path,mov){
    
    # para mostrar o EAN sem notificacao cientifica
    options("scipen"=100, "digits"=4)
    
   
    # RECEBE O MOV POR PARAMETRO
    # busca movimentacao
    #mov<-XL7_ImportaMovimentacao_SoftPharma(cliente,path)
    
    
    # adiciona codigo do produto XL7 no dataframe de acordo com o codigo interno do cliente
    cod_produto<-consultadb(paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cliente))
    
    # 
    movOK<-merge(mov,cod_produto,by.x="codigo",by.y="COD_INTERNO")
    
    
    # verifica produtos nao cadastrados na base (sem COD_PRODUTO xl7)
    novos<-mov[!(mov$codigo %in% movOK$codigo) & (mov$cliente %in% movOK$COD_CLIENTE),]
    novos<-novos[,c("codigo","tp","cliente","filial","descricao","lab","EAN","tp_pai")]
    novos<-unique(novos)
    novos$lab<-as.numeric(novos$lab)
    
    # busca fornecedores do arquivo gerado pelo SoftPharma
    fornec<-XL7_Importa_Fornecedores(cliente,path)
    
    # chama a funcao que vai cadastrar os fornecedores novos
    XL7_Cadastra_Fornecedores(fornec)
    
    # busca codigos fabricantes e adiciona ao data frame
    fornec_base<-consultadb("select * from FABRICANTE")
    
    fornec<-merge(fornec,fornec_base,by="CNPJ")
    colnames(fornec)[2]<-"lab"
    fornec$descricao<-NULL
    
    novos<-merge(novos,fornec,by="lab")
    
    faltam<-novos[,c("descricao","COD_FABRICANTE")]
    faltam<-unique(faltam)
    
    # cadastra produtos novos
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into PRODUTOS (NOME_COMERCIAL,COD_FABRICANTE) 
                                   values ('",faltam[n,"descricao"],"',",faltam[n,"COD_FABRICANTE"],")"))
            
        }
        dbDisconnect(mydb)
    }
  
    # busca codigos XL7 gerados dos produtos novos para cadastrar no EANS_PRODUTOS e PRODUTOS_CLIENTES
    base<-consultadb("select COD_PRODUTO,NOME_COMERCIAL,COD_FABRICANTE from PRODUTOS")  

    novos<-merge(novos,base,by.x=c("COD_FABRICANTE","descricao"),by.y=c("COD_FABRICANTE","NOME_COMERCIAL"))
    
    #############################
    
    # busca EANS_PRODUTOS ja cadastrados
    eans<-consultadb("select * from EANS_PRODUTOS")
    
    faltam<-novos[!(novos$EAN %in% eans$CODIGO_EAN) & !(novos$COD_PRODUTO %in% eans$PRODUTOS_COD_PRODUTO) , c("EAN","COD_PRODUTO")]
    faltam<-unique(faltam)
    
    # cadastra EANS_PRODUTOS novos
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into EANS_PRODUTOS (CODIGO_EAN,PRODUTOS_COD_PRODUTO) 
                                   values (",faltam[n,"EAN"],",",faltam[n,"COD_PRODUTO"],")"))
            
        }
        dbDisconnect(mydb)
    }
    
    ############################
    
    # busca PRODUTOS_CLIENTES ja cadastrados e cadastra os novos
    
    prods<-consultadb(paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cliente))
    faltam<-novos[ !(novos$COD_PRODUTO %in% prods$COD_PRODUTO) & !(novos$codigo %in% prods$COD_INTERNO),]
    faltam<-unique(faltam[,c("cliente","COD_PRODUTO","codigo")])
    
    # cadastra PRODUTOS_CLIENTES novos
    if(nrow(faltam)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(faltam)){
            dbGetQuery(mydb,paste0("insert into PRODUTOS_CLIENTES (COD_CLIENTE,COD_PRODUTO,COD_INTERNO) 
                                   values (",faltam[n,"cliente"],",",faltam[n,"COD_PRODUTO"],",",faltam[n,"codigo"],")"))
            
        }
        dbDisconnect(mydb)
    }
}







######################################################################
# Funcao que cadastra as series de dados
#
# caso não seja fornecido o COD_TIPO_PERIODO, a funcao detecta de acordo com os dados
# para isso, são necessários oelo menos 2 meses de dados...
######################################################################

XL7_Cadastra_Series_Dados<-function(cliente,path,COD_TIPO_PERIODO){
    
    
    # busca as SKUs do cliente
    sku<-consultadb(paste0("select * from SKU where COD_CLIENTE=",cliente))
    
    # busca movimentacao
    mov<-XL7_ImportaMovimentacao_SoftPharma(cliente,path)
    
    # CHAMA ROTINA QUE VALIDA SE NO MOVIMENTO TEM PRODUTOS/FORNECEDORES NAO CADASTRADOS NA BASE E CADASTRA
    XL7_Cadastra_Novos_ProdFornec(cliente,path,mov)
    
    # adiciona codigo do produto XL7 no dataframe de acordo com o codigo interno do cliente
    cod_produto<-consultadb(paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cliente))
    
    movOK<-merge(mov,cod_produto,by.x="codigo",by.y="COD_INTERNO")
    
    if (nrow(mov)!=nrow(movOK)){
        print("ALGO ESTA ERRADO!")
    } else {
        
        
        ##########################################################
        
        # cria campo diames
        mov$diames<-substr(mov$dia,1,2)
        
        if (is.null(COD_TIPO_PERIODO)){
            
            print("detectando COD_TIPO_PERIODO")
            ##################################
            # identifica o tipo de periodo
            ##################################
            
            # pega quantidade maxima de dias em um mes
            #
            # p=1 mensal        TIPOS_DE_PERIODOS: 2
            # p=2 quinzenal     TIPOS_DE_PERIODOS: 1
            # p>4 diario        TIPOS_DE_PERIODOS: 3
            #
            p<-max(ddply(mov,.(ano,mes),summarize,dias=length(unique(diames)))$dia)
            if (p==1){
                COD_TIPO_PERIODO<-2
            } else if (p==2){
                COD_TIPO_PERIODO<-1
            } else {
                COD_TIPO_PERIODO<-3    
            }
            print(paste0("COD_TIPO_PERIODO=",COD_TIPO_PERIODO))
            
        }
        
        
        if (COD_TIPO_PERIODO==2){
            mov$MASCARA_PERIODO<-paste0(mov$ano,"M",mov$mes)
        } else if (COD_TIPO_PERIODO==1){
            mov[mov$diames==01,]$MASCARA_PERIODO<-paste0(mov$ano,mov$mes,"Z1")
            mov[mov$diames==16,]$MASCARA_PERIODO<-paste0(mov$ano,mov$mes,"Z2")
        } else {
            mov$MASCARA_PERIODO<-paste0(mov$ano,mov$mes,mov$diames)
        }
        
        
        # busca series dados da base de acordo com o periodo importado dos movimentos
        series_base<-consultadb(paste0("select * from SERIES_DADOS where COD_CLIENTE=",cliente," and MASCARA_PERIODO>=",min(mov$MASCARA_PERIODO),
                                       " and MASCARA_PERIODO<=",max(mov$MASCARA_PERIODO)))
        
        # as series que nao tem adiciona
        faltam<-mov[(! mov$MASCARA_PERIODO %in%  series_base$MASCARA_PERIODO) & 
                        (! mov$COD_PRODUTO %in% series_base$COD_PRODUTO) & 
                        (! mov$codigo %in% series_base$COD_INTERNO) &
                        (! as.numeric(mov$cliente) %in% series_base$COD_CLIENTE) & 
                        (! as.numeric(mov$filial) %in% series_base$COD_FILIAL_CLIENTE),]
        
        
        # adiciona na base series_dados
        if(nrow(faltam)>0){
            mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
            for (n in 1:nrow(faltam)){
                dbGetQuery(mydb,paste0("insert into SERIES_DADOS (COD_CLIENTE,COD_FILIAL_CLIENTE,COD_PRODUTO,COD_INTERNO,MASCARA_PERIODO,
                                                                VENDAS_QTD,VENDAS_VALOR) 
                                   values (",faltam[n,"COD_CLIENTE"],",",faltam[n,"filial"],",",
                                       faltam[n,"COD_PRODUTO"],",",faltam[n,"codigo"],",'",
                                       faltam[n,"MASCARA_PERIODO"],"',",faltam[n,"qtd_total"],",",faltam[n,"total"],")"))
                
            }
            dbDisconnect(mydb)
        }
        
        
        # as series que ja mas possuem valor ou quantidade diferente, faz update
        faltam<-mov[( mov$MASCARA_PERIODO %in%  series_base$MASCARA_PERIODO) & 
                        ( mov$COD_PRODUTO %in% series_base$COD_PRODUTO) & 
                        ( mov$codigo %in% series_base$COD_INTERNO) &
                        ( as.numeric(mov$cliente) %in% series_base$COD_CLIENTE) & 
                        ( as.numeric(mov$filial) %in% series_base$COD_FILIAL_CLIENTE) & 
                        (! as.numeric(mov$qtd_total) %in% series_base$VENDAS_QTD) &
                        (! as.numeric(mov$total) %in% series_base$VENDAS_TOTAL) ,]
        
        
        # faz update na base series_dados
        if(nrow(faltam)>0){
            mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
            for (n in 1:nrow(faltam)){
                dbGetQuery(mydb,paste0("update SERIES_DADOS set VENDAS_QTD=",faltam[n,"qtd_total"],", VENDAS_VALOR=",faltam[n,"total"],
                                       " where COD_CLIENTE=",faltam[n,"COD_CLIENTE"]," and COD_FILIAL_CLIENTE=",faltam[n,"filial"],
                                       " and COD_PRODUTO=",faltam[n,"COD_PRODUTO"]," and COD_INTERNO=",faltam[n,"codigo"],
                                       " and MASCARA_PERIODO='",faltam[n,"MASCARA_PERIODO"],"'"))
                
            }
            dbDisconnect(mydb)
        }
        
    }
}


XL7_busca_vendas_series_dados<-function(cliente, datainicial,datafinal){
    
#    a<-consultadb(paste0("select SKU.*, coalesce(SERIES_DADOS.VENDAS_QTD,0) as VENDAS_QTD, coalesce(SERIES_DADOS.VENDAS_VALOR,0) as VENDAS_VALOR, 
#                  SERIES_DADOS.MASCARA_PERIODO from SKU LEFT JOIN SERIES_DADOS 
#                  on SKU.COD_CLIENTE=SERIES_DADOS.COD_CLIENTE and SKU.COD_FILIAL_CLIENTE=SERIES_DADOS.COD_FILIAL_CLIENTE
#                  where SKU.COD_CLIENTE=",cliente))
    
}



XL7_Cluster<-function()
{
    
    library(plyr)
    library(moments)
    
    cidade<-read.csv("C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/venda_ate2015.csv")
    
    cidade<-cidade[complete.cases(cidade),]
    
    medias<-ddply(cidade,.(cliente,filial,codigo),summarize,
                  sd=sd(qtd_total),mean=mean(qtd_total),kurtosis=kurtosis(qtd_total),skewness=skewness(qtd_total),
                  median=median(qtd_total),max=max(qtd_total),min=min(qtd_total))
    
    
    
}




XL7_Gera_Report_Vendas<-function(cliente){
   
    library(lubridate)
    
    # importa dados do clima do dia para os reports de venda
    XL7_importa_TEMPO()
    
    # importa movimentacoes da Desconto Popular
    
    movimentacao<-XL7_ImportaMovimentacao_SoftPharma("2","C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/Desconto Popular")
    write.csv2(movimentacao,"C:/Users/Felipe/OneDrive/XL7 COMPARTILHADA/Desconto Popular/movimentacaoDescontoPopular.csv",row.names = F)
    
    
    if (as.numeric(format(Sys.time(),"%H"))>19){
        data<-as.Date(format(Sys.Date(),"%Y-%m-%d"))
    } else
        data<-as.Date(format(Sys.Date()-days(1),"%Y-%m-%d"))
    
   # data<-Sys.Date()
    
    # pegar o Nome e lista de filiais do cliente no banco!
    nome<-"Desconto Popular"
    filial<-1:2
    
            
    
    for (f in c("TODAS",filial)){
        print(f)
        arq=paste0("Report_Vendas_",gsub(" ","_",nome),"_",f,"_",data,".html")
        rmarkdown::render("C:/Users/Felipe/OneDrive/Documentos/Negocios/XL7/Farmacias/DescontoPopular_Report.Rmd", 
                          params = list(filial = f, clima = 'N' ), output_file=arq)
    
        
    }

}

XL7_retorna_series_vendas <- function(cod_cliente, cod_filial, cod_tipo_periodo, data_inicial, data_final, identificador_arquivo){

      setwd("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/")
      
      if (is.na(cod_cliente)){
            # parâmetros para debug
            cod_cliente           <- 2
            cod_filial            <- NA # não implementado
            cod_tipo_periodo      <- 3  # diário
            data_inicial          <- "2016-03-01"
            data_final            <- "2016-03-31"
            identificador_arquivo <- "Teste"
            }
      
      # cria data frame com periodos x skus (por intervalo de data)
      sqlString <- paste0("select S.*, P.MASCARA_PERIODO from PERIODOS P, SKU S"
                          ," where P.COD_TIPO_PERIODO ="   ,cod_tipo_periodo
                          ,"   and P.DATA_INICIAL BETWEEN '",data_inicial,"' and '",data_final,"'"
                          ,"   and S.COD_CLIENTE      = "  ,cod_cliente)
      
      periodosSkuDF <- consultadb(sqlString)

      # busca dados da serie, filtrando por intervalo de data e tipo de período
      sqlString <- paste0("select  S.* from SERIES_DADOS S, PERIODOS P"
                         ," where  S.MASCARA_PERIODO = P.MASCARA_PERIODO"
                         ," and    P.DATA_INICIAL BETWEEN '",data_inicial,"' and '",data_final,"'"
                         ," and    P.COD_TIPO_PERIODO =",cod_tipo_periodo
                         ," and    S.COD_CLIENTE = ",cod_cliente)
      seriesDF <- consultadb(sqlString)
      
      # cria dataset final com zeros nas datas sem observações
      periodosSeriesDF <- merge(periodosSkuDF
                                ,seriesDF
                                ,by=c("COD_FILIAL_CLIENTE" 
                                      ,"COD_CLIENTE"        
                                      ,"COD_PRODUTO"        
                                      ,"COD_INTERNO"        
                                      ,"MASCARA_PERIODO")
                                ,all=TRUE) # outer-join

      # atribui zeros a valores NA
      periodosSeriesDF$VENDAS_VALOR[is.na(periodosSeriesDF$VENDAS_VALOR)] <- 0
      periodosSeriesDF$VENDAS_QTD[is.na(periodosSeriesDF$VENDAS_QTD)]     <- 0
      
      # grava arquivo de saída
      write.table(periodosSeriesDF,file=paste0("serie",identificador_arquivo,".csv"),row.names = FALSE)
      ## --------------------------------      
      ## EXEMPLOS consultas ao data frame
      
            # linhas com venda não zerada
            #periodosSeriesDF[periodosSeriesDF$VENDAS_VALOR>0,1:7]
            
            # produto específico
            #periodosSeriesDF[periodosSeriesDF$COD_INTERNO==57894,1:7]
            
            # total de vendas no período
            #sum(periodosSeriesDF[periodosSeriesDF$VENDAS_VALOR>0,7])

      # ---------------------------
      # devolve data frame de saída
      periodosSeriesDF
      
}

unique(c(c(1,2),c(1,2)))


