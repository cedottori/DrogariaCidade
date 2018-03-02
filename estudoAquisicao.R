ddply(stok,.(stok$COD_CLIENTE,stok$COD_FILIAL_CLIENTE),summarize,skus=length(stok$COD_INTERNO))

curv     <- read.csv(paste0("datasetCurva",DATA,".csv"),sep=";",stringsAsFactors = F,dec=",")
stok     <- read.csv(paste0("datasetEstoque",DATA,".csv"),sep=";",stringsAsFactors = F,dec=",")
tipo_pr  <- read.csv("datasetTipo.csv",sep=";",stringsAsFactors = F,dec=",")
produto  <- read.csv("datasetProdutos.csv",sep=";",stringsAsFactors = F,dec=",")
bloqueio <- read.csv(paste0("datasetBloqueios",data_minimo_demanda,".csv"),sep=";",stringsAsFactors = F,dec=",")
fracoes  <- read.csv(paste0("PRODUTOS fracionados.csv"),sep=";",stringsAsFactors = FALSE)
names(fracoes)[11]<-"FRACAO"

nrow(produto)
nrow(curv)
nrow(stok)


curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)
tipoProduto  <- merge(tipo_pr,produto,by.x = c("COD_TIPO_PRODUTO"),by.y = c("tp"))
curvaEstoque2 <- merge(curvaEstoque,tipoProduto,by.x = c("COD_INTERNO","COD_CLIENTE.x"),by.y=c("codigo","COD_CLIENTE"))

nrow(curvaEstoque)
nrow(tipoProduto)
nrow(curvaEstoque2)

head(curvaEstoque2)

head(tipoProduto)

