library(mondate)
DATA                <- "2016-10-05"
#
#USER                <- "cedot"
#USER                <- "cdottori"
USER                <- "lgiaretta"
COD_CLIENTE         <- 2
# seta diretório
working_dir <- ifelse (USER=="cedot"
                       ,saida<- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular"
                       ,ifelse(USER=="lgiar"
                               ,saida <- "C:/Users/lgiar/OneDrive/XL7 COMPARTILHADA/Desconto Popular"
                               ,ifelse(USER=="cdottori"
                                       ,saida <- "C:/Users/cdottori/OneDrive/XL7 COMPARTILHADA/Desconto Popular"
                                       ,ifelse(USER=="lgiaretta"
                                               ,saida <- "C:/Users/lgiaretta/OneDrive/XL7 COMPARTILHADA/Desconto Popular"))))

setwd(working_dir)
source("XL7-PHARMA.R")
XL7_Gera_Report_Vendas(COD_CLIENTE,DATA)
