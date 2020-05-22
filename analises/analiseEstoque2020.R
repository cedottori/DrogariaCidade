library(data.table)
pFontes <- "c:/users/cedot/documents/dottori/drogariaCidade"
pProc   <- "c:/RAWDATA/drogariaCidade/"

jan19 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20190131.csv")))
fev19 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20190228.csv")))
mar19 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20190331.csv")))
abr19 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20190430.csv")))
mai19 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20190521.csv")))

jan20 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20200131.csv")))
fev20 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20200229.csv")))
mar20 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20200331.csv")))
abr20 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20200430.csv")))
mai20 <- setDT(read.csv2(paste0(pProc,"/curvaEstoqueFilial 20200521.csv")))

jan19[,Anomes := 201901]
fev19[,Anomes := 201902]
mar19[,Anomes := 201903]
abr19[,Anomes := 201904]
mai19[,Anomes := 201905]
jan20[,Anomes := 202001]
fev20[,Anomes := 202002]
mar20[,Anomes := 202003]
abr20[,Anomes := 202004]
mai20[,Anomes := 202005]

arqFull <- rbind(jan19  ,fev19)
arqFull <- rbind(arqFull,mar19)
arqFull <- rbind(arqFull,abr19)
arqFull <- rbind(arqFull,mai19)
arqFull <- rbind(arqFull,jan20)
arqFull <- rbind(arqFull,fev20)
arqFull <- rbind(arqFull,mar20)
arqFull <- rbind(arqFull,abr20)
arqFull <- rbind(arqFull,mai20)

loopAnomes <- unique(arqFull$Anomes)

arqFull[ qtd>0,.N,by=.(COD_FILIAL_CLIENTE,Anomes)]

arqFull[ qtd>0&COD_INTERNO==60560,.N,by=.(COD_FILIAL_CLIENTE,Anomes)]
arqFull[ COD_INTERNO==60560,]

saida <- NULL
for (anomes in loopAnomes){
      print(anomes)
      estoqueMesAnt <- arqFull[Anomes==anomes-1,.(COD_FILIAL_CLIENTE,COD_INTERNO,qtd,anomes+1)]
      saida         <- rbind(saida,estoqueMesAnt,fill=T)
}
names(saida)[3:4]<- c("qtd_inicio","Anomes")

arqFullMerge <- merge(arqFull,saida,
                      by.x=c("COD_FILIAL_CLIENTE", "COD_INTERNO","Anomes"),
                      by.y=c("COD_FILIAL_CLIENTE", "COD_INTERNO","Anomes"),
                      all.x=T
                      )
arqFullMerge[is.na(qtd_inicio),qtd_inicio:= 0]

estoqueTotal <- arqFullMerge[,{estoque    = (sum(qtd_inicio+qtd)/2)
                               venda      = sum(valor_total)
                               venda_qtd  = sum(qtd_venda)
                               giro_perc  = venda_qtd/estoque
                               pr_cst_med = mean(pr_custo)
                               list(estoque=estoque,
                                    venda=venda,
                                    venda_qtd=venda_qtd,
                                    giro_perc=giro_perc,
                                    pr_cst_med=pr_cst_med)},
                               by=.(COD_INTERNO,
                                    "DESCRICAO_ITEM"=descricao.x,
                                    "LABORATORIO"=laboratorio.x,
                                    GRUPO_PRINCIPAL,
                                    "CATEGORIA"=DESCRICAO,
                                    Anomes)]

# corta janeiro por que não tem estoque inicial
estoqueTotal <- estoqueTotal[!Anomes%in%c(201901,202001)]

# atribui o giro máximo ao que ficou infinito ou indefinido
giroMaximo   <- estoqueTotal[!is.nan(giro_perc)&!is.infinite(giro_perc),.("giro_maximo"=max(giro_perc))]
estoqueTotal[is.infinite(giro_perc)|is.nan(giro_perc),giro_perc:=giroMaximo]

