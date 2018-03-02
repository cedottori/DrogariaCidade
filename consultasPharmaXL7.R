library(plyr)
mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')

# busca SKUs e PRODUTOS
PRODUTOS          <-dbGetQuery(mydb,paste0("select * from SERIES_DADOS where COD_CLIENTE=",cod_cliente))
PRODUTOS_CLIENTES <-dbGetQuery(mydb,paste0("select * from PRODUTOS_CLIENTES where COD_CLIENTE=",cod_cliente))
SKU               <-dbGetQuery(mydb,paste0("select * from SKU where COD_CLIENTE=",cod_cliente))

dbGetQuery(mydb, sqlString)

# cruza dataset inventario, curva ABC e classe Site PV
curv <- read.csv("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/datasetCurva20160514.csv",sep=" ",stringsAsFactors = F,dec=",")
stok <- read.csv("C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/datasetEstoque.csv",sep=";",stringsAsFactors = F,dec=",")

curv$codigo <- as.numeric(curv$codigo)
curv$filial <- as.numeric(curv$filial)
stok$COD_INTERNO <- as.numeric(stok$COD_INTERNO)
stok$COD_FILIAL_CLIENTE <- as.numeric(stok$COD_FILIAL_CLIENTE)

# funde datasets
curvaEstoque <- merge(stok,curv,by.x = c("COD_FILIAL_CLIENTE","COD_INTERNO"),by.y=c("filial","codigo"),all.x=TRUE)

# filtra linhas com estoque maior que zero ou que já tiveram vendas
curvaEstoque <- curvaEstoque[curvaEstoque$valor_total>0 | !is.na(curvaEstoque$descricao.y),]

curvaEstoque$cobertura_semanas[curvaEstoque$qtd_venda]
curvaEstoque$cobertura_ponderada <- curvaEstoque$cobertura_semanas*curvaEstoque$valor_total
write.csv2(file="curvaEstoque.csv",curvaEstoque,row.names = F)

summary(curv$filial)
summary(stok$COD_FILIAL_CLIENTE)
summary(curv$codigo)
summary(stok$COD_INTERNO)


nrow(curvaEstoque)
nrow(curva)
nrow(estoqueItens)

## exemplo

df1 = data.frame(CustomerId=c(1:10),
                 Hobby = c(rep("sing", 4), rep("pingpong", 3), rep("hiking", 3)),
                 Product=c(rep("Toaster",3),rep("Phone", 2), rep("Radio",3), rep("Stereo", 2)))

df2 = data.frame(CustomerId=c(2,4,6, 8, 10),State=c(rep("Alabama",2),rep("Ohio",1),   rep("Cal", 2)),
                 like=c("sing", 'hiking', "pingpong", 'hiking', "sing"))

df3 = merge(df1, df2, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))