library(dplyr)
#################################################################################
##  TRATA ARQUIVO DE LISTAGEM DE PRODUTOS SOFTPHARMA COM CODIGO DE BARRAS
##  CRIA UM DATASET LIMPO
##  BUSCA PREÇO FÁBRICA E PMC DO ARQUIVO DO GUIA DA FARMÁCIA
##  INSERE NA BASE XL7 OS DADOS
#################################################################################
# PARÂMETROS DE ENTRADA
cod_cliente     <- 2 # Desconto Popular
mascara_periodo <- "20160610"
insere_base     <- FALSE

#########################################
# LER LISTAGEM DE PRODUTOS SOFTPHARMA
print("lendo arquivo de produtos")
produtos <- readLines("PRODUTOS.txt")
print(paste0("lidas ",length(produtos)," linhas de produtos"))

produtosItens        <- data.frame(codigo   =rep(0,length(produtos))
                                  ,EAN      =rep(0,length(produtos))
                                  ,descricao=rep("DUMMY",length(produtos))
                                  ,pr_custo =rep(0,length(produtos))
                                  ,tribut   =rep("DUMMY",length(produtos))
                                  ,tp       =rep(0,length(produtos))
                                  ,stringsAsFactors = FALSE)
                                  
filial  <- 1
first_i <- NULL
##############################
# processa arquivo de produtos
##############################
first_i <- NULL
j       <- 1
print("quebrando SKUs")
for (i in 1:length(produtos)){
      
      ## identifica linha dataset
      if (!is.na(as.numeric(substr(produtos[i],1,6)))&&!is.na(as.numeric(substr(produtos[i],54,60)))){
            # não é nulo, portanto produto válido
            produtosItens$codigo[j]     <- as.numeric(substr(produtos[i],1 ,6 ))
            produtosItens$EAN[j]        <- as.numeric(substr(produtos[i],8 ,21))
            produtosItens$descricao[j]  <- substr(produtos[i],23 ,48)
            produtosItens$pr_custo[j]   <- as.numeric(substr(produtos[i],63,69))
            produtosItens$tribut[j]     <- substr(produtos[i],116,119)
            produtosItens$tp[j]         <- as.numeric(substr(produtos[i],121,125))
            
            j <- j + 1

      } else if (length(grep("<<",produtos[i]))!=0) {
            filial <-substr(produtos[i],15,15)
      } else { 
            ## imprime cabecalhos
            #print(produtos[i])
      }
      
      # mensagem de processamento
      if(i%%1000==0){
            print(paste0("processadas ",i," linhas"))
      }
      
} # for

# retira dummies do dataset
produtosItens <- produtosItens[produtosItens$descricao!="DUMMY",]  

# grava arquivo de produtos
write.csv2(file="datasetProdutos.csv",data.frame(produtosItens),row.names = FALSE)

# ###########################################
# # LER ARQUIVO DE PREÇOS DO GUIA DA FARMÁCIA
# print("lendo arquivo de preços layout Guia da Farmácia")
# dadosfar <- readLines("Dadosfar.txt")
# print(paste0("lidas ",length(dadosfar)," linhas de produtos Guia da Farmácia"))
# 
# guia  <- data.frame(cod_guia        =rep(0,length(dadosfar))
#                    ,EAN             =rep(0,length(dadosfar))
#                    ,preco_fabrica   =rep(0,length(dadosfar))
#                    ,PMC             =rep(0,length(dadosfar))
#                    ,controle        =rep("DUMMY",length(dadosfar))
#                    ,stringsAsFactors = FALSE)             
# 
# ###################################
# # processa arquivo Guia da Farmácia
# j       <- 1
# print("quebrando Guia da Farmácia")
# for (i in 1:length(dadosfar)){
#       
#       # quebra linhas e interpreta campos
#       guia$cod_guia[j]      <- as.numeric(substr(dadosfar[i],1 ,6 ))
#       guia$EAN[j]           <- as.numeric(substr(dadosfar[i],13,25))
#       guia$preco_fabrica[j] <- as.numeric(substr(dadosfar[i],52,59))/100
#       guia$PMC[j]           <- as.numeric(substr(dadosfar[i],60,67))/100
#       guia$controle[j]      <- "OK"
#       
#       j <- j + 1
#             
#       # mensagem de processamento
#       if(i%%1000==0){
#             print(paste0("processadas ",i," linhas"))
#       }
#       
# } # for
# 
# # retira dummies do dataset
# guia <- guia[guia$descricao!="DUMMY",]  
# 
# # grava arquivo Guia da Farmácia
# write.csv2(file="datasetGuia.csv",data.frame(guia),row.names = FALSE)
# 
# if (insere_base){
# ########################################################################################
# # INSERE PRODUTOS NA BASE XL7
# ########################################################################################
# # busca SKUs e PRODUTOS da base
# mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
# PRODUTOS          <- dbGetQuery(mydb,paste0("select * from PRODUTOS "))
# PRODUTOS_EANS     <- dbGetQuery(mydb,paste0("select * from EANS_PRODUTOS E, PRODUTOS P WHERE E.PRODUTOS_COD_PRODUTO = P.COD_PRODUTO "))
# 
# PRODUTOS_CLIENTES <- dbGetQuery(mydb,paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cod_cliente))
# PRODUTOS_CLASSES  <- dbGetQuery(mydb,paste0("select P.* from PRODUTOS_CLASSES_COMERCIAIS C, PRODUTOS_CLIENTES P WHERE C.COD_INTERNO = P.COD_INTERNO "))
# 
# # retira aspas simples e brancos à direita
# produtosItens$descricao <- trimws(gsub("'","",produtosItens$descricao))
# PRODUTOS_INSERIR  <- anti_join(produtosItens, PRODUTOS_CLIENTES, by=c("codigo"="COD_INTERNO"))
# 
# # se não existe insere PRODUTO_CLIENTE na base 
# first_i <- NULL
# mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
# print("inserindo produtos do cliente")
# for (i in 1:nrow(PRODUTOS_INSERIR)){
# 
#       EAN_produto <- produtosItens$EAN[produtosItens$codigo==PRODUTOS_INSERIR$codigo[i]]
#       
#       if (length(EAN_produto)>=1){
#             cod_produto <- PRODUTOS_EANS$COD_PRODUTO[PRODUTOS_EANS$CODIGO_EAN==EAN_produto[1]][1]
#       } else {
#             cod_produto <- "NULL"
#       }
#       
#       if (is.na(cod_produto)){
#             cod_produto <- "NULL"
#       }
#       
#       sqlString <- paste0("insert into PRODUTOS_CLIENTES (cod_cliente, cod_interno, cod_produto, cod_fabric_cliente, descr_prod_cliente) values ("
#                           ,cod_cliente
#                           ,",",PRODUTOS_INSERIR$codigo[i]
#                           ,",",cod_produto
#                           ,",NULL"
#                           ,",'",PRODUTOS_INSERIR$descricao[i],"')")
#       
#       
#       result <- dbGetQuery(mydb,sqlString)
#       
#       # mensagem de processamento
#       if(i%%1000==0){
#             print(paste0("processadas ",i," linhas"))
#       }
# 
# } ## for
# 
# dbDisconnect(mydb)
# 
# ########################################################################################
# # INSERE SKUS
# ########################################################################################
# 
# # busca SKUs e PRODUTOS
# mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
# PRODUTOS_CLIENTES <- dbGetQuery(mydb,paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cod_cliente))
# SKU               <- dbGetQuery(mydb,paste0("select * from SKU where COD_CLIENTE=",cod_cliente))
# FILIAIS           <- dbGetQuery(mydb,paste0("select * from FILIAL where COD_CLIENTE=",cod_cliente))
# 
# # verifica quais SKUs não existem na base
# SKUS_INSERIR      <- merge(PRODUTOS_CLIENTES,FILIAIS,all=TRUE)
# SKUS_INSERIR      <- anti_join(SKUS_INSERIR,SKU)
# 
# print(paste0(nrow(SKUS_INSERIR)," linhas a inserir"))
# 
# # insere SKUs novas na base 
# mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
# print("inserindo SKUs do cliente")
# for (i in 1:nrow(SKUS_INSERIR)){
#       
#       sqlString <- paste0("insert into SKU (cod_cliente, cod_filial_cliente, cod_interno ) values ("
#                           ,SKUS_INSERIR$COD_CLIENTE[i]
#                           ,",",SKUS_INSERIR$COD_FILIAL_CLIENTE[i]
#                           ,",",SKUS_INSERIR$COD_INTERNO[i],")")
#       
#       result <- dbGetQuery(mydb,sqlString)
#       
#       # mensagem de processamento
#       if(i%%500==0){
#             print(paste0("processadas ",i," linhas"))
#       }
#       
# } ## for

#dbDisconnect(mydb)
#}
