#########################################
# ler arquivo como um vetor de caracteres
print("arquivo de movimento")
nome    <- "C:/Users/cedot/OneDrive/XL7 COMPARTILHADA/Desconto Popular/compra_cidade/MOVIMENTACAO-20160624-CIDADE.txt"
arquivo <- readLines(nome)
arqnew  <- arquivo

for (i in 1:length(arquivo)){

      arq1    <- substr(arquivo[i],1,6)
      arq2    <- substr(arquivo[i],7,132)
      
      arq2 <- gsub("  1"," -1",arq2)
      arq2 <- gsub("  2"," -2",arq2)
      arq2 <- gsub("  3"," -3",arq2)
      arq2 <- gsub("  4"," -4",arq2)
      arq2 <- gsub("  5"," -5",arq2)
      arq2 <- gsub("  6"," -6",arq2)
      arq2 <- gsub("  7"," -7",arq2)
      arq2 <- gsub("  8"," -8",arq2)
      arq2 <- gsub("  9"," -9",arq2)

      arqnew[i] <- paste0(arq1,arq2)
      
}

arqnew <- data.frame(arqnew)

write.csv2(file=sub("MOVIMENTACAO","QMOVIMENTACAO",nome),arqnew,row.names = FALSE,quote=FALSE)
