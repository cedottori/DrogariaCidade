library(ggmap)
library(RMySQL)
library("sqldf")




#fatores para calculo metragem coordenadas
fatorLAT<<-0.0025
fatorLONG<<-0.0030




# funcao que arruma Bairros
arruma_bairro <- function(database="IMOVEL")
{
    
    # deixa apenas as primeiras letras maiusculas
    consultadb(paste0("update ",database," set Bairro=CAP_FIRST(Bairro)"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,' Da ',' da ') where Bairro like '% Da %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,' Das ',' das ') where Bairro like '% Das %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,' Do ',' do ') where Bairro like '% Do %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,' Dos ',' dos ') where Bairro like '% Dos %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,' De ',' de ') where Bairro like '% De %'"))
    
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Vl ','Vila ') where Bairro like 'Vl %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'N S ','Nossa Senhora ') where Bairro like 'N S %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Jd ','Jardim ') where Bairro like 'Jd %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'S ','Senhor ') where Bairro like 'S %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Pq. ','Parque ') where Bairro like 'Pq. %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Ch ','Chácara ') where Bairro like 'Ch %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'St ','Santa ') where Bairro like 'St %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Sta. ','Santa ') where Bairro like 'Sta. %'"))
    consultadb(paste0("update ",database," set Bairro=REPLACE(Bairro,'Cid ','Cidade ') where Bairro like 'Cid %'"))
    
    
    
    #popula coluna Fonema_Bairro
    consultadb(paste0("update ",database," set Fonema_Bairro=phonembr(Bairro)"))
    
    
}



# funcao para arrumar abreviacoes e maiusculas nos enderecos da base
arruma_endereco <- function(database="POI")
{
    #corrige idiotices
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Filhote','Forte') where Endereco like '%Filhote%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Os 18 do Forte','Os Dezoito do Forte') where Endereco like '%Os 18 do Forte%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Os 18 Forte','Os Dezoito do Forte') where Endereco like '%Os 18 Forte%'"))
    
    
    
    # deixa apenas as primeiras letras maiusculas
    consultadb(paste0("update ",database," set Endereco=CAP_FIRST(Endereco)")) #where Endereco=binary upper(Endereco)"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' Da ',' da ') where Endereco like '% Da %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' Das ',' das ') where Endereco like '% Das %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' Do ',' do ') where Endereco like '% Do %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' Dos ',' dos ') where Endereco like '% Dos %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' De ',' de ') where Endereco like '% De %'"))
    
    #corrige abreviacoes
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Fo.','Filho ') where Endereco like '%Fo.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sobr.','Sobrinho ') where Endereco like '%Sobr.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'&','e') where Endereco like '%&%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'nº',' ') where Endereco like '% nº%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Nº',' ') where Endereco like '% Nº%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'º','') where Endereco like '%º%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Cac.','Cacique ') where Endereco like '%Cac.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Pe.','Padre ') where Endereco like '% Pe.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Pe ','Padre ') where Endereco like '% Pe %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Dr.','Doutor ') where Endereco like '% Dr.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Dr','Doutor ') where Endereco like '% Dr%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Dr','Doutor ') where Endereco like 'Dr%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Srg.','Sargento ') where Endereco like '%Srg.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Mal.','Marechal ') where Endereco like '% Mal.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Mal ','Marechal ') where Endereco like '% Mal %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Gal.','General ') where Endereco like '% Gal.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Gal ','General ') where Endereco like '% Gal %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Gen.','General ') where Endereco like '%Gen.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Gen ','General ') where Endereco like '%Gen %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Dq','Duque ') where Endereco like '% Dq %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sto.','Santo ') where Endereco like '% Sto.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Cap.','Capitão ') where Endereco like '% Cap.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'D.','Don ') where Endereco like '% D.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Prof.','Professor ') where Endereco like '% Prof.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Prof ','Professor ') where Endereco like '% Prof %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Prf.','Professor ') where Endereco like '% Prf.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Prfa.','Professora ') where Endereco like '% Prfa.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Cel.','Coronel ') where Endereco like '%Cel.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Cel ','Coronel ') where Endereco like '%Cel %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Mq.','Marquês ') where Endereco like '% Mq.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Pres.','Presidente ') where Endereco like '% Pres.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Pres.','Presidente ') where Endereco like 'Pres.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Prca.','Praça ') where Endereco like '% Prca.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Pca','Praça ') where Endereco like 'Pca%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Ten.','Tenente ') where Endereco like '% Ten.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Vig.','Vigário ') where Endereco like '%Vig.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Eng.','Engenheiro ') where Endereco like '%Eng.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Eng ','Engenheiro ') where Endereco like '%Eng %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sen.','Senador ') where Endereco like '%Sen.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sen ','Senador ') where Endereco like '%Sen %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Maj.','Major ') where Endereco like '%Maj.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Maj ','Major ') where Endereco like '%Maj %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sra.','Senhora ') where Endereco like '% Sra. %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sra ','Senhora ') where Endereco like '% Sra %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sr.','Senhor ') where Endereco like '% Sr. %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Sr ','Senhor ') where Endereco like '% Sr %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Av ','Avenida ') where Endereco like 'Av %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Av.','Avenida ') where Endereco like 'Av.%'"))
    
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Rua Br.','Rua Barão ') where Endereco like 'Rua Br.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Avenida Br.','Avenida Barão ') where Endereco like 'Avenida Br.%'"))
    
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'R.','Rua ') where Endereco like 'R.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'R ','Rua ') where Endereco like 'R %'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Rod.','Rodovia ') where Endereco like 'Rod.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Est.','Estrada ') where Endereco like 'Est.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Estr.','Estrada ') where Endereco like 'Estr.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Br ','BR-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Br-','BR-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Br','BR-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Rs ','RS-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Rs-','RS-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Rs','RS-') where Endereco regexp '[BR][RS]-[0-9]*'"))
    
    
    
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'Furr.','Furriel ') where Endereco like '%Furr.%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,' - ',',') where Endereco like '%_-_%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'£',',') where Endereco like '%£%'"))
    consultadb(paste0("update ",database," set Endereco=REPLACE(Endereco,'©',',') where Endereco like '%©%'"))
    
    consultadb(paste0("update ",database," set Endereco=replace(Endereco,'  ',' ') where Endereco like '%  %'"))
    consultadb(paste0("update ",database," set Endereco=replace(Endereco,',,',',') where Endereco like '%,,%'"))
    
    
}


# funcao que corrige classificacoes incorretas de tipos de POI de acordo com o nome
arruma_tipo_POI<-function(database="POI_TEMP"){
    
    #move POIs classificados erroneamente para 'OUTROS'
    consultadb(paste0("update ",database," set Descricao = 'OUTROS' where  Nome like '%Equipam%'"))
    consultadb(paste0("update ",database,"  set Descricao = 'OUTROS' where  Nome like '%Consultoria%'"))
    consultadb(paste0("update ",database,"  set Descricao = 'OUTROS' where  Nome like '%material%'"))
    consultadb(paste0("update ",database,"  set Descricao = 'OUTROS' where  Nome like '%materiais%'"))
    consultadb(paste0("update ",database,"  set Descricao = 'OUTROS' where  Nome like '%Secretaria%Municipal%'"))
    #arruma caixas eletronicos como Bancos
    consultadb(paste0("update ",database,"  set Descricao = 'banco' where Nome like '%Caixa%Eletr_nico%' "))
    
    #arruma clinicas veterinarias
    consultadb(paste0("update ",database,"  set Descricao='clinica veterinaria' where Nome like 'clin%vet%'"))
    consultadb(paste0("update ",database,"  set Descricao='clinica veterinaria' where Nome like '%veterin%rio%'"))
    consultadb(paste0("update ",database,"  set Descricao='clinica veterinaria' where Nome like '%veterin%ria%'"))
    consultadb(paste0("update ",database,"  set Descricao='clinica veterinaria' where Nome like '%pet shop%' or Nome like '%dog%cat%' or Nome like '%cat%dog%' or Nome like '%C_o%gato%'       "))
    
    #arruma hospitais
    consultadb(paste0("update ",database,"  set Descricao='posto' where Descricao='hospital' and Nome like '%Posto_ %' and Nome not like '%Sa_de%' and Nome not like '%M_dico%'"))
    consultadb(paste0("update ",database,"  set Descricao='OUTROS' where Descricao='hospital' and Nome not like '%hospital %' and Nome not like '%Posto%Sa_de%' and Nome not like '%Posto%M_dico%' and Nome not like '%pronto%socorro%' and Nome not like '%pronto%atendimento%' and Nome not like '%ambulat_rio%' and Nome not like 'ubs %' and Nome not like '%unidade%sa_de%' and Nome not like 'Centro%Sa_de%'  "))
    
    #arruma estetica
    consultadb(paste0("update ",database,"  set Descricao='OUTROS' where Descricao='estetica' and Nome like '%industria%' or Nome like '%cosm_tico%' or Nome like '%perfum%' or Nome like '%magazine%'"))
    consultadb(paste0("update ",database,"  set Descricao='vestuario' where Descricao='estetica' and Nome like '%confec__es%' or Nome like '%confec__o%'"))
    
    
    
    consultadb(paste0("update ",database," set Status='?' where Descricao='OUTROS'"))
}


# funcao que identifica os municipior foneticamente
# exige intervenção manual
arruma_municpio_db<-function(database='POI_TEMP')
{
    #Coloca primeira letra em maiuscula
    # deixa apenas as primeiras letras maiusculas
    consultadb(paste0("update ",database," set Municipio=CAP_FIRST(Municipio)")) #where Endereco=binary upper(Endereco)"))
    consultadb(paste0("update ",database," set Municipio=REPLACE(Municipio,' Da ',' da ') where Municipio like '% Da %'"))
    consultadb(paste0("update ",database," set Municipio=REPLACE(Municipio,' Das ',' das ') where Municipio like '% Das %'"))
    consultadb(paste0("update ",database," set Municipio=REPLACE(Municipio,' Do ',' do ') where Municipio like '% Do %'"))
    consultadb(paste0("update ",database," set Municipio=REPLACE(Municipio,' Dos ',' dos ') where Municipio like '% Dos %'"))
    consultadb(paste0("update ",database," set Municipio=REPLACE(Municipio,' De ',' de ') where Municipio like '% De %'"))
    
    
    #popula coluna Fonema_Municipio
    consultadb(paste0("update ",database," set Fonema_Municipio=phonembr(Municipio)"))
    
    #importa dataset com municipios
    municipios_rs<-read.csv("c:/temp/municipios_rs.csv")
    
    total<-nrow(municipios_rs)
    for (n in 1:total)
    {
        municipio=municipios_rs[n,]
        cat(paste0("\n ",n," de ",total))
        print(paste0("Municipio: ",municipio))
        fonema<-consultadb(paste0("select phonembr('",municipio,"')"))
        fonema<-gsub("^\\s+", "",fonema)
        lista<-consultadb(paste0("select distinct Municipio from ",database," where Fonema_Municipio= '",fonema,"'"))
        if ( nrow(lista)>0){
            
            if (nrow(lista)==1 & lista[1,]==municipio)
            {
                cat("\nNada par ser alterado...\n")
            } 
            else
            {
                cat("Os seguintes Municipios serao alterados:\n")
                print(lista[,])
                cat("\nAtravés da seguinte query:")
                query=paste0(paste0('update ',database,' set Municipio="',municipio,'" where Fonema_Municipio="',fonema,'"'))
                print(query)
                x <- readline("\n\nDeseja fazer a alteração? [s/S]=Sim [x/X]=Sair [Qualquer outra tecla]=Não")
                if (x=='S' | x=='s')
                {
                    consultadb(query)
                } else if (x=='X' | x=='x')
                    break
            }
        }
        cat("\n\n--------------------------------------------------------------------------------------------------------------")
        
    }
}




xl7_aponta_erro_imoveis<-function(){
    
    print("Os seguintes imóveis podem estar com dados incorretos:")
    consultadb("select * from IMOVEL where Preco>50000 and (Tipo!='Galpão/Depósito/Armazém' and Tipo!='Prédio Inteiro' and Tipo!='Indústria' and Tipo!='Loja/Salão' and Tipo!='Terreno Padrão' and Tipo!='Casa Comercial' and Tipo!='Conjunto Comercial/Sala')")
}


# Calculate distance in kilometers between two points
xl7_distancia <- function (lat1,long1, lat2, long2)
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}

xl7_graus2dec <- function(dms, separators = c("º", "°", "\'", "\"","\'\'")) {
    # version 1.0 (25 Sep 3013)
    # dms: a vector (or column) of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
    # separators: the characters that are separating degrees, minutes and seconds in dms
    
    dms <- as.character(dms)
    dms <- gsub(pattern = " ", replacement = "", x = dms)
    for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
    
    splits <- strsplit(dms, split = "_splitHere_")
    n <- length(dms)
    deg <- min <- sec <- hem <- vector("character", n)
    
    for (i in 1:n) {
        deg[i] <- splits[[i]][1]
        min[i] <- splits[[i]][2]
        sec[i] <- splits[[i]][3]
        hem[i] <- splits[[i]][4]
    }
    
    dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
    sign <- ifelse (hem %in% c("N", "E"), 1, -1)
    dec <- sign * dec
    return(dec)
} 


loga<-function(x)
{
    arqLOG = file(paste("LOG.txt",sep=""),"a") 
    write(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ",x,sep=""),arqLOG,append=T)
    close(arqLOG) 
}



rm_acento <- function(str,pattern="all") {
    # Rotinas e funções úteis V 1.0
    # rm.accent - REMOVE ACENTOS DE PALAVRAS
    # Função que tira todos os acentos e pontuações de um vetor de strings.
    # Parâmetros:
    # str - vetor de strings que terão seus acentos retirados.
    # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
    #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
    #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
    #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
    if(!is.character(str))
        str <- as.character(str)
    
    str<-gsub("¿","",str)
    str<-gsub("§","c",str)
    str<-gsub("¡","",str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="Ç"))
        pattern[pattern=="Ç"] <- "ç"
    
    symbols <- c(
        acute = "áéíóúÁÉÍÓÚýÝ'",
        grave = "àèìòùÀÈÌÒÙ",
        circunflex = "âêîôûÂÊÎÔÛ",
        tilde = "ãõÃÕñÑ",
        umlaut = "äëïöüÄËÏÖÜÿ",
        cedil = "çÇ"
    )
    
    nudeSymbols <- c(
        acute = "aeiouAEIOUyY,",
        grave = "aeiouAEIOU",
        circunflex = "aeiouAEIOU",
        tilde = "aoAOnN",
        umlaut = "aeiouAEIOUy",
        cedil = "cC"
    )
    
    accentTypes <- c("´","`","^","~","¨","ç")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
        return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
        str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
}



####################################################
#funcao que executa query SQL no banco
####################################################
consultadb<-function(query){
    library(RMySQL)
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    result<-dbGetQuery(mydb, query)
    dbDisconnect(mydb)
    
    return(result)
}









#remove todas as conexoes ativas no MySQL
xl7_fechaconexoesdb<-function(){
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons)
        dbDisconnect(con)
    
}





#busca coordenadas
coordenadas<-function(endereco, source="google"){
    
    #remove dados temporarios para buscar sempre novamente as coordenadas do google
    #rm(.GeocodedInformation)
    
    if (!grepl("\\d",endereco)){
        print("Endereço sem número!")
        coord<-list(NA,NA)
        
        names(coord)<-c("lat","lon")
        
        return(c(coord,status="SEM_NUMERO"))
    }
    #coord<-geocode(endereco,messaging=F,source="google")
    retorno<-geocode(endereco,output='all', source=source, messaging=TRUE, override_limit=TRUE)
    if (retorno$status=="OVER_QUERY_LIMIT" || retorno$status=="ZERO_RESULTS" || retorno$status=="INVALID_REQUEST"){
        coord<-list(NA,NA)
        names(coord)<-c("lat","lon")
        
        return(c(coord,status=retorno$status))
    }
    
    if (retorno$status=="OK" & (retorno$results[[1]]$geometry$location_type=="ROOFTOP" | retorno$results[[1]]$geometry$location_type=="RANGE_INTERPOLATED")){
        coord<-retorno$results[[1]]$geometry$location
    } else
        coord<-list(NA,NA)
    
    names(coord)<-c("lat","lon")
    
    return(c(coord,status=retorno$status,precisao=retorno$results[[1]]$geometry$location_type))
}




# funcao que arruma o quadrante na base de acoordo com Latitude e Longitude
xl7_arruma_quadrante<-function(tabela='POI_TEMP'){
    
    query=paste0("update ",tabela," set Quadrante=CONCAT_WS(',',truncate(Latitude/",fatorLAT,",0) ,truncate(Longitude/",fatorLONG,",0)) where Quadrante<>CONCAT_WS(',',truncate(Latitude/",fatorLAT,",0) ,truncate(Longitude/",fatorLONG,",0)) and Status is NULL and Latitude is not null and Longitude is not null")
    consultadb(query)  
}




# funcao ajusta as coordenadas na base de acordo com Google
arruma_coord_db<-function(tabela='POI'){
    
    # garante que os quadrantes estão OK
    xl7_arruma_quadrante(tabela)
    
    # busca Fonema_Endereco com coordenadas validadas e altera na base as linhas com mesmo fonema para nao precisar buscar do Google
    print("Populando coordenadas buscando dados na propria base (tabela POI)...")
    
    a<-consultadb(paste0("select distinct Fonema_Endereco,Municipio,Latitude,Longitude,Quadrante from POI where Quadrante<>'NA,NA' and Status is NULL and Latitude is not NULL"))
    b<-consultadb(paste0("select distinct Fonema_Endereco,Municipio from ",tabela," where Status is NULL and (Quadrante='NA,NA' or Latitude is NULL)"))
    x<-merge(a,b,by=c('Fonema_Endereco','Municipio'))
    
    if(nrow(x)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(x)){
            query<-paste0("update ",tabela," set Latitude='",x$Latitude[n],"', Longitude='",x$Longitude[n],"', Quadrante='",x$Quadrante[n],"' where Municipio='",x$Municipio[n],"' and Fonema_Endereco='",x$Fonema_Endereco[n],"' and Latitude is NULL")
            print(query)
            dbGetQuery(mydb, query)
        }
        dbDisconnect(mydb)
    }
    
    print("Popula coordenadas buscando do Google...")
    print("========================================")
    
    ok=1
    n<-0
    while (ok==1){
        n<-n+1
        
        # não revalida coordenadas buscadas do proprio Google
        linha<-consultadb(paste0("select Endereco,Municipio,UF,Bairro,Fonema_Endereco,count(*) as total from ",tabela," where (Latitude is NULL or Longitude is NULL) and Status is NULL and Fonema_Endereco is not NULL and MotorBusca <> 'GOOGLE' and Descricao<>'OUTROS' order by Endereco LIMIT 1;"))
        
        # pega o Fonema_Endereco que é baseado apenas na rua,numero para atualizar todas as coordenadas desse local
        # nao tem porque buscar todas as coordenadas de todas as lojas de um shopping se rua e numero é o mesmo e muda apenas a loja
        FONEMA<-linha$Fonema_Endereco
        
        
        writeLines("\n\n-------------------------------------------------------------------------------------\n")
        print(linha)
        
        if(linha$total==0){
            ok<-0
            break
        }
        
        linha_semacento<-linha
        linha_semacento[1,]<-rm_acento(linha_semacento[1,])
        
        #05-11-15 tirei o bairro pois nos endereços da protasio alves, com bairro protasio alves o google devolvia sempre a mesma coordenada
        #coord<-coordenadas(paste(linha_semacento$Endereco,",",linha_semacento$Bairro,",",linha_semacento$Municipio,",",linha_semacento$UF,", Brasil"))
        
        
        if (linha_semacento$UF=='RS') linha_semacento$UF<-'Rio Grande do Sul'
        coord<-coordenadas(paste0(xl7_ruanumero(linha_semacento$Endereco),", ",linha_semacento$Municipio,", ",linha_semacento$UF,", Brasil"),"google")
        
        if (coord$status=="SEM_NUMERO"){
            query<-paste0("update ",tabela," set Latitude='NA', Longitude='NA', Quadrante='NA,NA', Status='X' where Endereco='",linha$Endereco,"' and Municipio='",linha$Municipio,"' and UF='",linha$UF,"' and Bairro='",linha$Bairro,"'")
            consultadb(query)
            print(paste0("query=",query))
            
        }else if (coord$status=="OVER_QUERY_LIMIT"){
            print("Limite diario do Google geocode atingido!")
            ok<-0
        } else {
            # se o resultado for APPROXIMATE a funcao coordenadas retorna como NA pois o google joga varios enderecos em uma mesma coordenada central da cidade
            if(is.na(coord$lat)){
                quadrante<-"NA,NA"
                query<-paste0("update ",tabela," set Latitude='",coord$lat,"', Longitude='",coord$lon,"', Quadrante='",quadrante,"', Status='?' where Fonema_Endereco='",FONEMA,"' and Municipio='",linha$Municipio,"' and UF='",linha$UF,"' and Bairro='",linha$Bairro,"'")
                
            } else{
                quadrante<-paste0(trunc(coord$lat/fatorLAT),",",trunc(coord$lon/fatorLONG))
                query<-paste0("update ",tabela," set Latitude='",coord$lat,"', Longitude='",coord$lon,"', Quadrante='",quadrante,"', Status = NULL where Fonema_Endereco='",FONEMA,"' and Municipio='",linha$Municipio,"' and UF='",linha$UF,"' and Bairro='",linha$Bairro,"'")
            }
            
            consultadb(query)
            print(paste0("query=",query))
            
            print(coord$status)
            
        }
        print(paste0("n=",n,"      ",coord$lat," , ",coord$lon))
    }
    
}






####################################################################################################################################
# descobre POIs com mesmo endereco e MotorBusca e limpa os que nao tenham Lat e Long evitando ficarem na lista do arruma_coordenadas

corrige_duplicados_db<-function (tabela='POI'){
    
    
    data<-format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # direct output to a file 
    sink(paste0("duplicados_",tabela,"_",gsub("[ :]","_",data),".txt"), append=FALSE, split=FALSE)
    
    writeLines(paste0("\nData: ",data))
    writeLines("\n\n")
    
    writeLines("PROVAVEIS POIs duplicados:\n")
    query<-paste0("select Descricao,Nome,Endereco,Bairro,Municipio,MotorBusca,count(*) as Duplicados from ",tabela," where Descricao <> 'antena' and Latitude is NULL and Status is NULL group by Descricao,Nome,Endereco,Bairro,Municipio,MotorBusca having Duplicados>1  order by Duplicados desc")
    writeLines(paste0("Query utilizada: ",query))
    duplicados<-consultadb(query)
    writeLines("\n\n")
    print(duplicados)
    writeLines(("\n\nMudando Status dos duplicados para X\n"))
    
    if(nrow(duplicados)>0){
        for (n in 1:nrow(duplicados)){
            query<-paste0("update ",tabela," set Status='X' where Status is NULL and Latitude is NULL and Nome='",duplicados[n,'Nome'],"' and Descricao='",duplicados[n,'Descricao'],"' and Endereco='",duplicados[n,'Endereco'],"' and Bairro='",duplicados[n,'Bairro'],"' and Municipio='",duplicados[n,'Municipio'],"' and MotorBusca='",duplicados[n,'MotorBusca'],"' limit ",duplicados[n,'Duplicados']-1)
            linha<-consultadb(query)
            print(query)
        }
    }
    
    
    writeLines("\n\n==================================================================================================================\n\n\n")
    
    writeLines("POSSIVEIS POIs duplicados:\n")
    
    query<-paste0("select Descricao,Endereco,Bairro,Municipio,MotorBusca,count(*) as Duplicados from ",tabela," where Descricao <> 'antena' and Status is NULL  group by Descricao,Endereco,Bairro,Municipio,MotorBusca having Duplicados>1  order by Duplicados desc")
    duplicados<-consultadb(query)
    
    
    writeLines(paste0("Query utilizada: ",query))
    writeLines("\n\n")
    
    for (n in 1:nrow(duplicados)){
        print(paste0("Duplicados: ",duplicados[n,"Duplicados"],"    Endereco: ",duplicados[n,'Endereco'],"  -  ",duplicados[n,'Bairro'],"  -  ",duplicados[n,'Municipio']))
        print("--------------------------------------------------------------------------------------------------------------------------------------")
        query<-paste0("select Descricao,Nome,Endereco,Bairro,Municipio,Latitude,Longitude,MotorBusca from",tabela," where Descricao='",duplicados[n,'Descricao'],"' and Endereco='",duplicados[n,'Endereco'],"' and Bairro='",duplicados[n,'Bairro'],"' and Municipio='",duplicados[n,'Municipio'],"' and MotorBusca='",duplicados[n,'MotorBusca'],"'  and Status is NULL")
        linha<-consultadb(query)
        print(linha)
        writeLines("\n\n")
    }
    
    
    
    # return output to the terminal 
    sink()
}




#funcao que classifica quadrantes de acordo com os nomes dos comercios. Quanto menor o valor, mais popular o quadrante
populometro<-function()
{
    #cria dataframe com os pesos de acordo com os nomes dos comercios
    tabela<-data.frame(nome=c("xis","careca","cavaco","mega","gordo","ki","delicia","inha","lavagem","ferragem","lanche","e cia","descont","econo","promo","atacad","rainha","drink","barat","popular","ofert","charm","R$","big","chiq","povo"))
    tabela$peso<- -1
    
    
    popds<-consultadb("select distinct Quadrante from POI where Status is NULL")
    popds$valor=0
    
    for (n in 1:nrow(tabela))
    {
        palavra<-tabela$nome[n]
        #temp<-consultadb(paste0("select Quadrante,count(Quadrante) as total from POI where Quadrante<>'NA,NA' and Nome like '%",palavra,"%' group by Quadrante"))
        temp<-consultadb(paste0("select Quadrante,count(Quadrante) as total from (select Nome,Descricao,Fonema_Endereco,Quadrante from POI where Status is NULL and Quadrante<>'NA,NA' and Descricao<>'OUTROS' and Nome like '%",palavra,"%' group by Descricao,Fonema_Endereco,Municipio) as x  group by Quadrante"))
        if(nrow(temp)>0){
            for(x in 1:nrow(temp))
            {
                #print(palavra)
                if(temp$Quadrante[x]=='-12011,-17076'){
                    print (palavra)
                    print(temp$total[x])
                }
                (popds$valor[popds$Quadrante==temp$Quadrante[[x]]]<-popds$valor[popds$Quadrante==temp$Quadrante[[x]]]+(temp$total[temp$Quadrante==temp$Quadrante[[x]]]*tabela$peso[tabela$nome==palavra]))
                
            }
        }
    }
    
    popds<-popds[popds$valor<0,]
    popds[order(popds$valor),]
}




#funcao que classifica Municipios de acordo com os nomes dos comercios. Quanto menor o valor, mais popular o Municipio
# tende a deixar mais popular os municipios com mais quantidade de POIS, deveria fazer alguma especie de media (% POI popular x normal)
populometro_municipio<-function()
{
    #cria dataframe com os pesos de acordo com os nomes dos comercios
    tabela<-data.frame(nome=c("xis","careca","cavaco","mega","gordo","ki","delicia","inha","lavagem","ferragem","lanche","e cia","descont","econo","promo","atacad","rainha","drink","barat","popular","ofert","charm","R$","big","chiq","povo"))
    tabela$peso<- -1
    
    popds<-consultadb("select distinct Municipio from POI where Status is NULL")
    popds$valor=0
    
    for (n in 1:nrow(tabela))
    {
        palavra<-tabela$nome[n]
        #temp<-consultadb(paste0("select Quadrante,count(Quadrante) as total from POI where Quadrante<>'NA,NA' and Nome like '%",palavra,"%' group by Quadrante"))
        temp<-consultadb(paste0("select Municipio,count(Municipio) as total from (select Nome,Descricao,Fonema_Endereco,Municipio from POI where Status is NULL and Quadrante<>'NA,NA' and Descricao<>'OUTROS' and Nome like '%",palavra,"%' group by Descricao,Fonema_Endereco,Municipio) as x  group by Municipio"))
        if(nrow(temp)>0){
            for(x in 1:nrow(temp))
            {
                #print(palavra)
                (popds$valor[popds$Municipio==temp$Municipio[[x]]]<-popds$valor[popds$Municipio==temp$Municipio[[x]]]+(temp$total[temp$Municipio==temp$Municipio[[x]]]*tabela$peso[tabela$nome==palavra]))
                
            }
        }
    }
    
    popds<-popds[popds$valor<0,]
    popds[order(popds$valor),]
}





# Funcao que retorna apenas nome da rua e numero (tirando complementos, etc)
# atencao especial para enderecos de rodovias, onde é necessario manter o km!!
xl7_ruanumero<-function(endereco){
    
    #verifica se o nome da rua é um numero...
    
    if(grepl("^Rodovia BR-\\d",endereco) || grepl ("^Rodovia RS-\\d",endereco) || grepl("^Estrada BR-\\d",endereco) || grepl ("^Estrada RS-\\d",endereco) || grepl("^Rua BR-\\d",endereco) || grepl ("^Rua RS-\\d",endereco)){
        temp<-strsplit(endereco, "(?<=(\\d\\D))",perl=T)[[1]]
        end<-paste0(temp[1],temp[2])
    } else if(grepl("Rua \\d",endereco) || grepl ("Avenida \\d",endereco) || grepl ("^BR-\\d",endereco) || grepl ("^RS-\\d",endereco)){
        temp<-strsplit(endereco, "(?<=(\\d\\D))",perl=T)[[1]]
        end<-paste0(temp[1],temp[2])
    } else {
        end<-strsplit(endereco, "(?<=(\\d))\\D",perl=T)[[1]][[1]]
    }
    
    end<-gsub("-","",end)
    return(end)
    
}  


# funcao que popula o campo Fonema_Endereco usando o campo RuaNumero (populado pela funcao que retorna apenas rua e numero)
xl7_popula_Fonema_Endereco<-function(tabela,force=FALSE){
    
    if (force==TRUE)
        lista<-consultadb(paste0("select distinct Endereco from ",tabela))
    else
        lista<-consultadb(paste0("select distinct Endereco from ",tabela, " where Fonema_Endereco is NULL"))
    
    tot<-nrow(lista)
    if(tot>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        
        for (n in 1:tot){
            print(paste0("Populando RuaNumero: ",n," de ",tot,"..."))
            #fonema_end<-consultadb(paste0("select phonembr('",xl7_ruanumero(lista$Endereco[n]),"')"))
            dbGetQuery(mydb, (paste0("update ",tabela," set RuaNumero='",xl7_ruanumero(lista$Endereco[n]),"' where Endereco='",lista$Endereco[n],"'")))
            
        }
        dbDisconnect(mydb)
    }
    
    print("Poupulando Fonema_Endereco...")
    consultadb(paste0("update ",tabela," set Fonema_Endereco=phonembr(RuaNumero) "))
    
}



# gera visao dos enderecos com coordenadas validadas
xl7_streetview<-function(tabela,descricao){
    
    DOC<-"<html><body><table><tr><td>Endereco</td><td>norte</td><td>sul</td><td>leste</td><td>oeste</td></tr>"
    lista<-consultadb(paste0("select Nome, Endereco, Municipio,UF,Latitude,Longitude from ",tabela," where Descricao='",descricao,"' and Status is NULL and Latitude is not NULL group by Fonema_Endereco,Municipio, Descricao order by Nome"))
    lista<-as.data.frame(lista)
    p<-1
    pag<-1
    for (n in 1:nrow(lista)){
        DOC<-paste0(DOC,"<tr><td>",lista$Nome[n],"<br>",lista$Endereco[n],"<br>",lista$Municipio[n]," - ",lista$UF[n],"</td>")
        DOC<-paste0(DOC,"<td><EMBED SRC='https://maps.googleapis.com/maps/api/streetview?size=300x150&location=",lista$Latitude[n],",",lista$Longitude[n],"&heading=0&pitch=-0.76&key=AIzaSyAOMKvKbPSKLtjqWKg6Ol6GK-uyIa-Qc9s'></EMBED></td>")
        DOC<-paste0(DOC,"<td><EMBED SRC='https://maps.googleapis.com/maps/api/streetview?size=300x150&location=",lista$Latitude[n],",",lista$Longitude[n],"&heading=180&pitch=-0.76&key=AIzaSyAOMKvKbPSKLtjqWKg6Ol6GK-uyIa-Qc9s'></EMBED></td>")
        DOC<-paste0(DOC,"<td><EMBED SRC='https://maps.googleapis.com/maps/api/streetview?size=300x150&location=",lista$Latitude[n],",",lista$Longitude[n],"&heading=90&pitch=-0.76&key=AIzaSyAOMKvKbPSKLtjqWKg6Ol6GK-uyIa-Qc9s'></EMBED></td>")
        DOC<-paste0(DOC,"<td><EMBED SRC='https://maps.googleapis.com/maps/api/streetview?size=300x150&location=",lista$Latitude[n],",",lista$Longitude[n],"&heading=270&pitch=-0.76&key=AIzaSyAOMKvKbPSKLtjqWKg6Ol6GK-uyIa-Qc9s'></EMBED></td></tr>")
        p<-p+1
        if(p==1001){
            p<-1
            DOC<-paste0(DOC,"</table></body></html>")
            writeLines(DOC,paste0('VISAO_360_',descricao,'_',lista$UF[n],'_pagina_',pag,'.html'))
            pag<-pag+1
            DOC<-"<html><body><table><tr><td>Endereco</td><td>norte</td><td>sul</td><td>leste</td><td>oeste</td></tr>"
        }
        
        
    }
    
    DOC<-paste0(DOC,"</table></body></html>")
    writeLines(DOC,paste0('VISAO_360_',descricao,'_',lista$UF[n],'_pagina_',pag,'.html'))
}



arruma_bairro_do_google<-function(){
    
    print("Corrigindo Bairro")
    ds<-consultadb("select * from POI where MotorBusca='GOOGLE' and (Bairro='NA' or Bairro='Na')")
    
    tot<-nrow(ds)
    
    for (n in 1:tot){
        end<-strsplit(ds$Endereco[n],',')
        bairro<-tail(end[[1]],2)
        
        bairro[1]<-gsub('^ ','',bairro[1])
        bairro[2]<-gsub('^ ','',bairro[2])
        if (!is.na(as.numeric(bairro[1])) | bairro[1]=='s/n')
        {  
            if(bairro[2]==ds$Municipio[n])
                ds$Bairro[n]<-'Indefinido'
            else
                ds$Bairro[n]<-bairro[2]
            
        }
        else
            ds$Bairro[n]<-bairro[1]
        
    }
    
    print("Alterando na base")
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    for (n in 1:tot){
        print(paste0("Alterando registro ",n," de ",tot,"..."))
        query<-paste0("update POI set Bairro='",ds$Bairro[n],"' where MotorBusca='GOOGLE' and id_no_MotorBusca='",ds$id_no_MotorBusca[n],"' and Endereco='",ds$Endereco[n],"'")
        dbGetQuery(mydb,query)
        
    }
    dbDisconnect(mydb)
    
    
}



xl7_backup_dados<-function(){
    
    data<-format(Sys.time(),"%d-%m-%Y")
    
    for (database in c("POI","IMOVEL","RENDA")){
        
        print(paste0("Salvando tabela ",tabela))
        bkp<-consultadb(paste0("select * from ",database))
        write.csv(bkp,file=paste0('BKP_',database,'_',data,'.csv'),row.names = F)
        
    }
    
}



# FUNCAO QUE LE UM .CSV GERADO PELO BUSCA_APONTADOR E INSERE NA BASE DE DADOS NA TABELA POI_TEMP
csv2banco<-function(arquivo,separador=",")
{
    ds<-read.csv(arquivo,stringsAsFactors=FALSE,sep=separador,header = T)
    
    #remove aspas simples
    ds<-as.data.frame(sapply(ds, function(x) gsub("'", "", x)))
    
    #remove & por e
    ds<-as.data.frame(sapply(ds, function(x) gsub("&", "e", x)))
    
    #remove nº 
    ds<-as.data.frame(sapply(ds, function(x) gsub("nº", "", x)))
    ds<-as.data.frame(sapply(ds, function(x) gsub("º", "", x)))
    ds<-as.data.frame(sapply(ds, function(x) gsub(' - ',',', x)))
    ds<-as.data.frame(sapply(ds, function(x) gsub('£',',', x)))
    ds<-as.data.frame(sapply(ds, function(x) gsub('©',',', x)))
    
    
    #deixa somente enderecos com numero
    OK<-grepl("\\d",ds$endereco)
    ds<-ds[OK,]
    
    #ds$local<-paste0(trunc(ds$latitude/fatorLAT),",",trunc(ds$longitude/fatorLONG))
    
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    
    
    nr<-nrow(ds)
    for (i in 1:nr)
    {
        print(paste0("inserindo ",i," de ",nr," ..."))
        query<-paste("insert into POI_TEMP (xLatitude,xLongitude,Nome,Endereco,Bairro,Municipio,UF,CEP,Telefone,Descricao,Data,Quadrante,MotorBusca,id_no_MotorBusca)  values ('",ds[i,]$latitude,"','",ds[i,]$longitude,"','",ds[i,]$nome,"','",ds[i,]$endereco,"','",ds[i,]$bairro,"','",ds[i,]$cidade,"','",  ds[i,]$uf,"','",ds[i,]$cep,"','",ds[i,]$fone,"','",ds[i,]$empresa,"','",as.Date(ds[i,]$data,'%d-%m-%Y %H:%M:%S'),"','",ds[i,]$local,"','",ds[i,]$motor,"','",ds[i,]$id,"')",sep="")
        #print(query)
        dbGetQuery(mydb, query)
        
    }
    
    dbDisconnect(mydb)
}





importa_novos_POI<-function(arrumamunicipio=FALSE){
    
    ###################################################################
    #                  ATENCAO
    # os arquivos devem ser importados para a tabela POI_TEMP através das funcao csv2banco
    ####################################################################
    
    
    
    
    # Arruma enderecos
    print("Arrumando enderecos...")
    arruma_endereco('POI_TEMP')
    
    if(arrumamunicipio==TRUE){
        # popula Fonema_Municipio e Fonema_Endereco
        print("Arrumando municipios...")
        arruma_municpio_db('POI_TEMP')
    }  
    
    print('==================')
    print("Populando Fonema Enderecos...")
    xl7_popula_Fonema_Endereco('POI_TEMP')
    
    # precisa criar uma stored procedure com o comando abaixo
    # verifica enderecos que ja tenham Latitude e Longitude validados na base POI e adiciona as coordenadas e quadrante
    print("Buscando coordenadas ja validadas e atualizando na POI_TEMP...")
    #consultadb("update POI_TEMP inner join POI on (POI_TEMP.Fonema_Endereco = POI.Fonema_Endereco and  POI_TEMP.Municipio=POI.Municipio and POI.Status is NULL and POI.Quadrante<>'NA,NA') set POI_TEMP.Quadrante=POI.Quadrante, POI_TEMP.Latitude=POI.Latitude, POI_TEMP.Longitude=POI.Longitude,POI_TEMP.xLatitude=POI.xLatitude, POI_TEMP.xLongitude=POI.xLongitude")
    
    # Busca do Google coordenadas para enderecos restantes caso não tenham sido buscadas do proprio google
    print("Arruma coordenadas no POI_TEMP...")
    arruma_coord_db('POI_TEMP')
    
    # para POIs buscados pelo Google, popula os campos Latitude e Longitude
    consultadb("update POI_TEMP set Latitude=xLatitude, Longitude=xLongitude where MotorBusca='GOOGLE'")
    
    # revalida Quadrantes
    xl7_arruma_quadrante(tabela='POI_TEMP')
    
    # insere na tabela POI os registros que tenham coordenadas validadas
    #consultadb("insert into POI select * from POI_TEMP where POI_TEMP.Latitude is not NULL and Latitude<>'NA' group by Fonema_Endereco,Municipio,Descricao")
    
    
}


importa_faixas_etarias_RS_2010<-function(){
    
    arquivo='C:/Users/Felipe/OneDrive/Documentos/Negocios/XL7/Arquivos CSV/IBGE RS CENSO 2010 Tabela 4.23.1.2 - faixas etarias.xls'
    
    library(xlsx)
    
    lista<-read.xlsx(arquivo,1,startRow = 53, endRow = 4049, header = F,colIndex = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17), encoding = 'UTF-8',stringsAsFactors=FALSE)
    x<-sqldf("select * from (select * from lista order by X2 ) as xx group by X1",drv="SQLite")
    x$X1<-iconv(x$X1,'utf-8','latin2')
    names(x)<-c("Municipio","Total","0-4anos","5-9anos","10-14anos","15-17anos","18-19anos","20-24anos","25-29anos","30-34anos","35-39anos","40-49anos","50-59anos","60-69anos","70+anos","CodigoUnidadeGeografica")
    x$AnoCenso<-2010
    x$UF<-'RS'
    for (n in 2:15){
        x[,n]<-as.numeric(x[,n])
        x[is.na(x[,n]),]<-0
        
    } 
    
    if(nrow(x)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(x)){
            if(is.na(x$Municipio[n])) break
            
            x$Municipio[n]<-gsub("'","",x$Municipio[n])
            x$Municipio[n]<-enc2native(x$Municipio[n])
            query<-paste0("insert into FAIXA_ETARIA (Municipio,Total,`0-4anos`,`5-9anos`,`10-14anos`,`15-17anos`,`18-19anos`,`20-24anos`,`25-29anos`,`30-34anos`,`35-39anos`,`40-49anos`,`50-59anos`,`60-69anos`,`70+anos`,CodigoUnidadeGeografica,AnoCenso,UF) values ('",x$Municipio[n],"', ",as.numeric(x$Total[n]),", ",as.numeric(x$`0-4anos`[n]),", ",as.numeric(x$`5-9anos`[n]),", ",as.numeric(x$`10-14anos`[n]),", ",as.numeric(x$`15-17anos`[n]),", ",as.numeric(x$`18-19anos`[n]),", ",as.numeric(x$`20-24anos`[n]),", ",as.numeric(x$`25-29anos`[n]),", ",as.numeric(x$`30-34anos`[n]),", ",as.numeric(x$`35-39anos`[n]),", ",as.numeric(x$`40-49anos`[n]),", ",as.numeric(x$`50-59anos`[n]),", ",as.numeric(x$`60-69anos`[n]),", ",as.numeric(x$`70+anos`[n]),", '",x$CodigoUnidadeGeografica[n],"', ",x$AnoCenso[n],",'",x$UF[n],"')")
            
            print(query)
            dbGetQuery(mydb, query)    
        }
        dbDisconnect(mydb)
    }
    
    
    
}


importa_Renda_Municipio<-function(arquivo,UF,ano){
    
    #arquivos buscados do IBGE:
    #http://www.ibge.gov.br/home/estatistica/populacao/censo2010/resultados_gerais_amostra/resultados_gerais_amostra_tab_uf_xls.shtm
    
    if(!hasArg(arquivo) | !hasArg(UF) | !hasArg(ano)){
        return("Sintae: importa_Renda_Censo(arquivo,UF,ano)")
    }
    
    library(xlsx)
    
    lista<-read.xlsx(arquivo,1,startRow = 9, header = F,colIndex = c(1,3,4,6,7,9,10,11), encoding = 'UTF-8',stringsAsFactors=FALSE)
    lista<-lista[complete.cases(lista),]
    
    names(lista)<-c("Municipio","QtdHomens","QtdMulheres","SalarioMedioHomens","SalarioMedioMulheres","SalarioMedianoHomens","SalarioMedianoMulheres","CodigoUnidadeGeografica")
    lista$UF<-UF
    lista$AnoCenso<-ano
    
    if(nrow(lista)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(lista)){
            if(is.na(lista$Municipio[n])) break
            
            lista$Municipio[n]<-gsub("'","",lista$Municipio[n])
            lista$Municipio[n]<-enc2native(lista$Municipio[n])
            query<-paste0("select Municipio from RENDA where UF='",UF,"' and Municipio='",lista$Municipio[n],"' and AnoCenso=",ano)
            ok<-dbGetQuery(mydb, query)
            if (nrow(ok)>0)
                query<-paste0("update RENDA set UF='",lista$UF[n],"', Municipio='",lista$Municipio[n],"', QtdHomens=",lista$QtdHomens[n],", QtdMulheres=",lista$QtdMulheres[n],", SalarioMedioHomens=",lista$SalarioMedioHomens[n],", SalarioMedioMulheres=",lista$SalarioMedioMulheres[n],", SalarioMedianoHomens=",lista$SalarioMedianoHomens[n],", SalarioMedianoMulheres=",lista$SalarioMedianoMulheres[n],", CodigoUnidadeGeografica='",lista$CodigoUnidadeGeografica[n],"', AnoCenso='",lista$AnoCenso[n],"' where UF='",lista$UF[n],"' and Municipio='",lista$Municipio[n],"' and AnoCenso=",lista$AnoCenso[n])
            else
                query<-paste0("insert into RENDA (UF,Municipio,QtdHomens,QtdMulheres,SalarioMedioHomens,SalarioMedioMulheres,SalarioMedianoHomens,SalarioMedianoMulheres,CodigoUnidadeGeografica,AnoCenso) values ('",lista$UF[n],"', '",lista$Municipio[n],"', ",lista$QtdHomens[n],", ",lista$QtdMulheres[n],", ",lista$SalarioMedioHomens[n],", ",lista$SalarioMedioMulheres[n],", ",lista$SalarioMedianoHomens[n],", ",lista$SalarioMedianoMulheres[n],", '",lista$CodigoUnidadeGeografica[n],"', '",lista$AnoCenso[n],"')")
            
            print(query)
            dbGetQuery(mydb, query)    
        }
        dbDisconnect(mydb)
    }
    
    
}



# importa xls com valores de imoveis para tabela IMOVEL 
importa_IMOVEL<-function(arquivo){
    
    
    if(!hasArg(arquivo)){
        return("Sintae: importa_IMOVEL(arquivo)")
    }
    
    
    print("Lendo arquivo")
    lista<-read.csv(arquivo)
    print("Arquivo lido")
    
    lista<-unique(lista)
    
    # REMOVER!
    #lista$MotorBusca<-'zapimoveis'
    #lista$cidade<-gsub(paste0(' ',lista$UF),'',lista$cidade)
    #lista$preco<-gsub('R\\$ ','',lista$preco)
    #lista$preco<-as.numeric(gsub('\\.','',lista$preco))
    #lista$tipo<-gsub('Ã£','ã',lista$tipo)
    #lista$tipo<-gsub('Ã³','ó',lista$tipo)
    #lista$tipo<-gsub('Ã','í',lista$tipo)
    #lista$tipo<-gsub('í©','é',lista$tipo)
    #lista$detalhes<-gsub('Â','',lista$detalhes)
    #lista$detalhes<-gsub('Ã','í',lista$detalhes)    
    #lista$id_no_MotorBusca<-0
    
    
    if(nrow(lista)>0){
        mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
        for (n in 1:nrow(lista)){
            
            # REMOVER!
            #lista$cidade[n]<-strsplit(lista$cidade,' -')[[n]][1]
            #lista$id_no_MotorBusca[n]<-strsplit(strsplit(as.character(lista$link[n]),'/ID-')[[1]][2],'/')[[1]][1]
            
            print(paste0("Analisando linha ",n))
            # caso ja exista um mesmo ID no mesmo Ano / Mes nao insere
            a<-dbGetQuery(mydb,paste0("select id_no_MotorBusca from IMOVEL where id_no_MotorBusca='",lista$id_no_MotorBusca[n],"' and date_format(Data,'%Y/%m')='",format(as.Date(lista$data[n],'%d-%m-%Y %H:%M:%S'),format='%Y/%m'),"'"))
            if(nrow(a)==0){
                query<-paste0("insert into IMOVEL (UF,Municipio,Bairro,Endereco,Preco,M2,Descricao,Tipo,Detalhes,Link,MotorBusca,Data,id_no_MotorBusca) values ('",lista$UF[n],"', '",lista$cidade[n],"', '",lista$bairro[n],"', '",lista$endereco[n],"', ",lista$preco[n],", ",lista$m2[n],", '",lista$descricao[n],"', '",lista$tipo[n],"', '",lista$detalhes[n],"', '",lista$link[n],"', '",lista$MotorBusca[n],"', '", as.Date(lista$data[n],'%d-%m-%Y %H:%M:%S'),"', '",lista$id_no_MotorBusca[n],"')")
                print(query)
                dbGetQuery(mydb, query)
            }
        }
        dbDisconnect(mydb)
    }
    
    arruma_bairro('IMOVEL')
    arruma_municpio_db('IMOVEL')
}






















# funcao que faz uma avaliacao no dataset em busca de NAs e demais inconsistencias
xl7_avalia_dataset<-function(ds){
    
    result<-data.frame(temNA=FALSE,ColunasNA=0,LinhasNA=0,NomeColunasDuplicadas=0)
    
    ColunasNA<-0
    LinhasNA<-0
    
    colunas<-names(ds)
    print("Buscando NAs...")
    for (n in 1:length(colunas)){
        x<-sum(is.na(ds[,colunas[n]]))
        if(x>0){
            print(paste0("Coluna ",colunas[n]," possui ",x," NAs!"))
            ColunasNA<-ColunasNA+1
        } 
    }
    if(ColunasNA>0) result$temNA<-TRUE
    result$ColunasNA<-ColunasNA
    if (result$temNA==TRUE){
        x<-sum(rowSums(is.na(ds)))
        print(paste0("Existem ",x," linhas contendo algum NA!"))
        result$LinhasNA<-x
    }
    
    result$NomeColunasDuplicadas<-length(grep("TRUE",duplicated(names(ds))))
    
    return(result)
    
}


XL7_importa_TEMPO<-function(arquivo){
    
    require(SAScii) 
    require(RCurl)
    require(XML)
    
    #http://www.cptec.inpe.br/cidades/tempo/237
    
    cidade<-237 # porto alegre
    
    html<- htmlTreeParse(paste('http://www.cptec.inpe.br/cidades/tempo/',cidade,sep=""),useInternal=TRUE,encoding = 'UTF-8')
    
    data<-xpathSApply(html,"//div[@class='tit']",xmlValue)[1]
    data<-gsub("\\.","/",strsplit(data," - ")[[1]][2])
    
    # verifica se ja tem a previsao do dia atual
    jatem<-consultadb(paste0("select count(*) from TEMPO2 where data='",as.Date(data,format("%d/%m/%Y")),"'"))
    if (jatem==0){
        
        prev<-xpathSApply(html,"//div[@class='prev']",xmlDoc)[[1]]
        img<-xpathSApply(prev,"//img",xmlGetAttr,'src')[1]
        tempmin<-xpathSApply(prev,"//div[@class='c2']",xmlValue)
        tempmax<-xpathSApply(prev,"//div[@class='c3']",xmlValue)
        probchuva<-xpathSApply(prev,"//div[@class='c4']",xmlValue)
        uv<-xpathSApply(prev,"//div[@class='c7']",xmlValue)
        obs<-xpathSApply(prev,"//div[@class='c8']",xmlValue)
        
        tempmin<-iconv(tempmin, "UTF-8","LATIN2")
        tempmax<-iconv(tempmax, "UTF-8","LATIN2")
        probchuva<-iconv(probchuva, "UTF-8","LATIN2")
        obs<-iconv(obs, "UTF-8","LATIN2")
        
        # pega apenas os numeros
        tempmin<-unique(na.omit(as.numeric(unlist(strsplit(unlist(tempmin), "[^0-9]+")))))
        tempmax<-unique(na.omit(as.numeric(unlist(strsplit(unlist(tempmax), "[^0-9]+")))))
        probchuva<-unique(na.omit(as.numeric(unlist(strsplit(unlist(probchuva), "[^0-9]+")))))
        
        if (length(tempmin)==0) tempmin<-"NULL"
        if (length(tempmax)==0) tempmin<-"NULL"
        if (length(probchuva)==0) tempmin<-"NULL"
        
        
        sql<-paste0("insert into TEMPO2 (data,img,tempmin,tempmax,probchuva,obs) values ('",as.Date(data,format("%d/%m/%Y")),"','",img,"',",
                    tempmin,",",tempmax,",",probchuva,",'",obs,"')")
        
        consultadb(sql)
    }
    
    
}







importa_TEMPO_INMET<-function(arquivo){
    
    # ORIGEM: http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=83942&btnProcesso=serie&mRelDtInicio=01/01/2011&mRelDtFim=31/01/2016&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,
    
    #arquivo="C:/Users/Felipe/OneDrive/Documentos/Negocios/XL7/Arquivos CSV/TEMPO/TEMPO_caxias.csv"
    txt<-readLines(arquivo,n=7)
    
    CodEstacao<-strsplit(txt[4],':')[[1]][3]
    CodEstacao<-gsub(" ","",CodEstacao)
    CodEstacao<-gsub(")","",CodEstacao)
    Cidade<-strsplit(txt[4],':')[[1]][2]
    Cidade<-strsplit(Cidade,' - ')[[1]][1]
    Cidade<-trimws(Cidade,"both")
    UF<-strsplit(txt[4],':')[[1]][2]
    UF<-strsplit(UF,' - ')[[1]][2]
    UF<-strsplit(UF,' ')[[1]][1]
    UF<-gsub(" ","",UF)
    Latitude<-strsplit(txt[5],":")[[1]][2]
    Latitude<-gsub(" ","",Latitude)
    Longitude<-strsplit(txt[6],":")[[1]][2]
    Longitude<-gsub(" ","",Longitude)
    Altitude<-strsplit(txt[7],":")[[1]][2]
    
    ds<-read.csv(arquivo,skip=15,sep=';',colClasses=c("character","character","character","character","character","character","character","character","character","character","character","character"))
    ds[is.na(ds)]<-"NULL"
    # remove coluna X
    ds<-ds[1:11]
    
    # arruma campo hora
    ds$Data<-paste0(ds$Data," ",substr(ds$Hora,1,2),":",substr(ds$Hora,3,4))
    ds<-ds[c(1,2,4,5,6,7,8,9,10,11)]
    
    names(ds)<-c("CodEstacao","Data","Precipitacao","TempMaxima","TempMinima","Insolacao","EvaporacaoPiche","Temperatura","UmidadeRelativa","VelocidadeVento")
    
    ds$Precipitacao<-as.numeric(ds$Precipitacao)
    ds$TempMaxima<-as.numeric(ds$TempMaxima)
    ds$TempMinima<-as.numeric(ds$TempMinima)
    ds$Insolacao<-as.numeric(ds$Insolacao)
    ds$EvaporacaoPiche<-as.numeric(ds$EvaporacaoPiche)
    ds$Temperatura<-as.numeric(ds$Temperatura)
    ds$UmidadeRelativa<-as.numeric(ds$UmidadeRelativa)
    ds$VelocidadeVento<-as.numeric(ds$VelocidadeVento)
    
    # troca NAs por NULL para inserir na base MySql
    ds[is.na(ds)]<-'NULL'
    
    # verificar se ja existe estacao cadastrada e cadastra
    
    a<-consultadb(paste0("select * from TEMPO_ESTACAO where CodEstacao='",CodEstacao,"'"))
    if(nrow(a)==0){
        consultadb(paste0("insert into TEMPO_ESTACAO (CodEstacao,Latitude,Longitude,UF,Municipio,Altitude) values ('",CodEstacao,"','",Latitude,"','",Longitude,"','",UF,"','",Cidade,"',",as.numeric(Altitude),")"))
    }
    
    mydb = dbConnect(MySQL(), user='xl71', password='cargawar1', dbname='xl71', host='mysql01.xl71.hospedagemdesites.ws')
    
    for (n in 1:nrow(ds)){
        a<-dbGetQuery(mydb,paste0("select * from TEMPO where CodEstacao='",ds$CodEstacao[n],"' and Data='",strptime(ds$Data[n],format='%d/%m/%Y %H:%M'),"'"))
        if(nrow(a)==0){
            query<-paste0("insert into TEMPO (CodEstacao,Data,Precipitacao,TempMaxima,TempMinima,Insolacao,EvaporacaoPiche,Temperatura,
                    UmidadeRelativa,VelocidadeVento) values ('",ds$CodEstacao[n],"','",strptime(ds$Data[n],format='%d/%m/%Y %H:%M'),"',",
                          ds$Precipitacao[n],",",ds$TempMaxima[n],",",ds$TempMinima[n],",",ds$Insolacao[n],",",ds$EvaporacaoPiche[n],",",
                          ds$Temperatura[n],",",ds$UmidadeRelativa[n],",",ds$VelocidadeVento[n],")")
            
            print(query)
            dbGetQuery(mydb, query)
        }
    }
    dbDisconnect(mydb)
    
    
    
}



xl7_gera_indicadores<-function(UF='RS',POI){
    
    require(xlsx)
    rend<-consultadb(paste0("select avg(Latitude) Latitude, avg(Longitude) Longitude, POI.Municipio,QtdHomens,QtdMulheres,SalarioMedioHomens,SalarioMedioMulheres from POI left join RENDA on POI.Municipio=RENDA.Municipio where POI.Status is NULL and Latitude is not NULL and POI.UF='",UF,"' group by POI.Municipio"))
    rend$RendaTotalHomens<-rend$QtdHomens*rend$SalarioMedioHomens
    rend$RendaTotalMulheres<-rend$QtdMulheres*rend$SalarioMedioMulheres
    rend$RendaTotal<-rend$RendaTotalHomens + rend$RendaTotalMulheres
    rend$TotalPopAtiva<-rend$QtdHomens+rend$QtdMulheres
    rend$SalarioMedio<-(rend$SalarioMedioHomens + rend$SalarioMedioMulheres)/2
    
    temp<-consultadb(paste0("select Municipio,Fonema_Endereco from POI where UF='",UF,"' and Descricao='",POI,"' and  Status is NULL and Quadrante<>'NA,NA'  group by Municipio,Fonema_endereco"))
    qtdPOI<-sqldf(paste0("select Municipio,count(*) as Qtd",POI," from temp group by Municipio"),drv="SQLite")
    
    idade<-consultadb(paste0("select * from FAIXA_ETARIA where UF='",UF,"'"))
    
    saida<-sqldf(paste0("select qtdPOI.Municipio as Municipio, Qtd",POI,", RendaTotal,TotalPopAtiva, RendaTotal/Qtd",POI,",RendaTotalHomens/Qtd",POI,", RendaTotalMulheres/Qtd",POI,",SalarioMedio/Qtd",POI,",   SalarioMedioHomens/Qtd",POI,", SalarioMedioMulheres/Qtd",POI,", TotalPopAtiva/Qtd",POI,", QtdHomens/Qtd",POI,", QtdMulheres/Qtd",POI,", `0-4anos`/Qtd",POI,", `5-9anos`/Qtd",POI,", `10-14anos`/Qtd",POI,", `15-17anos`/Qtd",POI,", `18-19anos`/Qtd",POI,", `20-24anos`/Qtd",POI,", `25-29anos`/Qtd",POI,", `30-34anos`/Qtd",POI,", `35-39anos`/Qtd",POI,", `40-49anos`/Qtd",POI,", `50-59anos`/Qtd",POI,", `60-69anos`/Qtd",POI,", `70+anos`/Qtd",POI,"  from qtdPOI inner join rend on qtdPOI.Municipio=rend.Municipio inner join idade on idade.Municipio=rend.Municipio"),drv="SQLite")
    saida<-saida[complete.cases(saida),]
    
    
    #saida[,10]<-as.numeric(format(round(saida[,10],digits=0),big.mark = '.', decimal.mark = ','))
    #saida[,11]<-as.numeric(format(round(saida[,9],digits=0),big.mark = '.', decimal.mark = ','))
    #saida[,12]<-as.numeric(format(round(saida[,8],digits=0),big.mark = '.', decimal.mark = ','))
    
    
    write.xlsx(saida,paste0(file="Indicadores ",POI,".xls"),row.names=T)
    
    
    # cria outro dataset com dados absolutos, sem a divisao por POI
    saida2<-sqldf(paste0("select qtdPOI.Municipio as Municipio, Qtd",POI,", RendaTotal,TotalPopAtiva, RendaTotalHomens, RendaTotalMulheres,SalarioMedio, SalarioMedioHomens, SalarioMedioMulheres, QtdHomens, QtdMulheres, `0-4anos`, `5-9anos`, `10-14anos`, `15-17anos`, `18-19anos`, `20-24anos`, `25-29anos`, `30-34anos`, `35-39anos`, `40-49anos`, `50-59anos`, `60-69anos`, `70+anos`  from qtdPOI inner join rend on qtdPOI.Municipio=rend.Municipio inner join idade on idade.Municipio=rend.Municipio"),drv="SQLite")
    saida2<-saida2[complete.cases(saida2),]
    
    # tirando porto alegre
    saida2<-subset(saida2,Municipio!="Porto Alegre")
    
    # dividindo rendas por 1.000.000
    saida2[,3:9]<-saida2[,3:9]/1000000
    
    
    options(scipen=5)
    
    pairs(saida2[2:7],main="Rendas em Milhões de R$")
    pairs(saida2[,c(2,8:11)],main="Rendas em Milhões de R$")
    
    cor(saida2[2:7])
    cor(saida2[,c(2,8:11)])
    
    
    pairs(saida2[,c(2,12:18)],main="Rendas em Milhões de R$")
    pairs(saida2[,c(2,19:24)],main="Rendas em Milhões de R$")
    
    cor(saida2[,c(2,12:18)])
    cor(saida2[,c(2,19:24)])
    
    
    
    
    # graficos
    #library(plotrix)  # para pie3D
    
    # farmacias por municipio
    s<-saida[saida$TotalPopAtiva>8000,]
    l<-s[order(-s$Qtdfarmacia),]
    fatias<-l$Qtdfarmacia
    txt<-l$Municipio
    perc<-round(fatias/sum(fatias)*100)
    fatias<-fatias[perc>0]
    txt<-txt[perc>0]
    txt<-paste0(txt," (",perc,"%)")
    pie(fatias,radius=1,cex=0.6,labels=txt,col=rainbow(length(fatias):1),main="Distribuicao Farmacias por Municipio")
    
    # renda por municipio
    s<-saida[saida$TotalPopAtiva>8000,]
    l<-s[order(-s$RendaTotal),]
    fatias<-l$RendaTotal
    txt<-l$Municipio
    perc<-round(fatias/sum(fatias)*100)
    fatias<-fatias[perc>0]
    txt<-txt[perc>0]
    txt<-paste0(txt," (",perc,"%)")
    pie(fatias,radius=1,cex=0.6,labels=txt,col=rainbow(length(fatias):1),main="Distribuicao Renda por Municipio")
    
    
    # renda total por farmacias
    s<-saida#[saida$TotalPopAtiva>8000,]
    l<-s[order(-s$`RendaTotal/Qtdfarmacia`),]
    fatias<-l$`RendaTotal/Qtdfarmacia`
    txt<-l$Municipio
    perc<-round(fatias/sum(fatias)*100)
    fatias<-fatias[perc>0]
    txt<-txt[perc>0]
    txt<-paste0(txt," (R$ ",round(fatias/1,2),")")
    #pie(fatias,radius=1,cex=0.6,labels=txt,col=rainbow(length(fatias):1),main="Renda por Farmacias")
    
    library(scales)
    
    qplot(data=l,RendaTotal,Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Renda Total vs Quantidade Farmacias") +
        theme(legend.position="none") + scale_x_continuous(labels = comma) 
    
    qplot(data=subset(l,Municipio!='Porto Alegre'),RendaTotal,Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Renda Total vs Quantidade Farmacias (sem POA)") + 
        theme(legend.position="none") + scale_x_continuous(labels = comma)
    
    qplot(data=l,RendaTotal,Qtdfarmacia,col=Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Renda Total vs Quantidade Farmacias") + 
        scale_x_continuous(labels = comma) + scale_fill_brewer(palette="Spectral")
    
    qplot(data=subset(l,Municipio!='Porto Alegre'),RendaTotal,Qtdfarmacia,col=Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Renda Total vs Quantidade Farmacias (sem POA") + 
        scale_x_continuous(labels = comma) + scale_fill_brewer(palette="Spectral")
    
    
    
    
    # idosos por farmacias (acima 60 anos)
    s<-saida[saida$TotalPopAtiva>8000,]
    s$idososporfarmacia<-s$'`60-69anos`/Qtdfarmacia'+s$'`70+anos`/Qtdfarmacia'
    l<-s[order(-s$idososporfarmacia),]
    
    l$Municipio<-factor(l$Municipio,levels=l$Municipio[order(l$idososporfarmacia)],ordered=TRUE)
    qplot(data=l,l$Municipio,geom='bar',fill=l$idososporfarmacia, weight=l$idososporfarmacia,main="Idosos por Farmacia") +
        theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_y_continuous(breaks = round(seq(min(l$idososporfarmacia), max(l$idososporfarmacia), by = 50),1)) +
        ylab("") + xlab("Municipio")
    
    
    qplot(data=l,idososporfarmacia*Qtdfarmacia,Qtdfarmacia,col=Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Quantidade de Farmacias x Número de Idosos") + 
        scale_x_continuous(labels = comma) + scale_fill_brewer(palette="Spectral") + ylab("Qtd. Farmacias") + xlab("Qtd. Idosos (>60 anos)")
    
    qplot(data=subset(l,Municipio!='Porto Alegre'),idososporfarmacia*Qtdfarmacia,Qtdfarmacia,col=Qtdfarmacia,group=Municipio,alpha=0.2,size=3,main="Quantidade de Farmacias x Número de Idosos (sem POA)") + 
        scale_x_continuous(labels = comma) + scale_fill_brewer(palette="Spectral") + ylab("Qtd. Farmacias") + xlab("Qtd. Idosos (>60 anos)")
    
    
    
    #barplot(l$idososporfarmacia,xlab=l$Municipio)
    
    
    
}



#xl7_analisa_Municipio<-function(){
#  
#  municipios<-consultadb("select ((QtdHomens*SalarioMedioHomens)+(QtdMulheres*SalarioMedioMulheres)) as RendaTotal,QtdHomens+QtdMulheres as PopulacaoAtiva, Municipio from RENDA group by RENDA.Municipio order by RendaTotal")
#  pois<-consultadb("select count(*) as Qtd, Descricao,Municipio from POI where Status is NULL and Quadrante<>'NA,NA' group by Descricao,Municipio,Fonema_Endereco")
#  pois<-as.data.frame(table(pois))
#  pois<-pois[pois$Freq>0,c(1:3)]
#}




teste<-function(){
    tab<-consultadb("select avg(Latitude) Latitude, avg(Longitude) Longitude, POI.Municipio,QtdHomens,QtdMulheres,SalarioMedioHomens,SalarioMedioMulheres from POI left join RENDA on POI.Municipio=RENDA.Municipio where POI.Status is NULL and Latitude is not NULL and POI.UF='RS' group by POI.Municipio")
    tab$RendaHomens<-tab$QtdHomens*tab$SalarioMedioHomens
    tab$RendaMulheres<-tab$QtdMulheres*tab$SalarioMedioMulheres
    tab$RendaTotal<-tab$RendaHomens + tab$RendaMulheres
    
    
    #desenho do municipio baseado nos pois
    municipio='Porto Alegre'
    
    #pega latitude mais ao sul (menor latitude)
    p1<-consultadb(paste0("select Latitude,Longitude,Municipio from POI  where Municipio='",municipio,"' and Status is NULL and Latitude is not NULL order by Latitude desc limit 1"))
    
    #vai pegando proximos POI aumentando longitude
    
    
}





analisa_top_nomes_populometro<-function(){
    
    # bairro popular
    bairro_popular<-"Sarandi"
    
    ds_popular<-consultadb(paste0("select distinct Nome from POI where Municipio='Porto Alegre' and Bairro='",bairro_popular,"'"))
    
    # pega a palavra de maior tamanho de cada medicamento, que nao contenha numero, assumindo que seria o nome do medicamento em si
    popular<-data.frame(palavra=character(),qty=character())
    for (n in toupper(ds_popular$Nome)){
        for(temp in strsplit(n," ")[[1]]){
            temp<-gsub(":","",temp)
            temp<-gsub("\\.","",temp)
            if (nchar(temp)>2){
                if(nrow(popular[popular$palavra==temp,])>0){
                    popular[popular$palavra==temp,"qty"]<-popular[popular$palavra==temp,"qty"]+1
                } else {
                    popular<-rbind(popular,data.frame(palavra=temp,qty=1))
                }
            }
            
        }
    }
    
    
    
    # bairro nobre
    bairro_nobre<-"Moinhos de Vento"
    
    ds_nobre<-consultadb(paste0("select distinct Nome from POI where Municipio='Porto Alegre' and Bairro='",bairro_nobre,"'"))
    
    # pega a palavra de maior tamanho de cada medicamento, que nao contenha numero, assumindo que seria o nome do medicamento em si
    nobre<-data.frame(palavra=character(),qty=character())
    for (n in toupper(ds_nobre$Nome)){
        for(temp in strsplit(n," ")[[1]]){
            temp<-gsub(":","",temp)
            temp<-gsub("\\.","",temp)
            if (nchar(temp)>2){
                if(nrow(nobre[nobre$palavra==temp,])>0){
                    nobre[nobre$palavra==temp,"qty"]<-nobre[nobre$palavra==temp,"qty"]+1
                } else {
                    nobre<-rbind(nobre,data.frame(palavra=temp,qty=1))
                }
            }
            
        }
    }
    
    so_nobre<-nobre[!(nobre$palavra %in% popular$palavra),]
    so_popular<-popular[!(popular$palavra %in% nobre$palavra),]
    
    #so_nobre[order(so_nobre$qty),]
    #so_popular[order(so_popular$qty),]
    
}
















# consultadb("select QUadrante,Latitude,Longitude,RuaNumero,count(*) as total from POI where Status is NULL group by Latitude,Longitude,RuaNumero order by total desc limit 20")


# Analise do custo / m2 por municipio, bairro, tipo e datacoleta
# a<-consultadb("select Municipio,Bairro,Tipo, count(*) as Quantidade,avg(Preco) as PrecoMedio, avg(M2) as TamanhoMedio, avg(Preco/M2) as ValorMedioM2, date_format(Data,'%Y/%m') as AnoMesColeta  from IMOVEL where M2>2 group by Municipio,Bairro,Tipo,AnoMesColeta having ValorMedioM2>0 order by avg(Preco/M2)")

# agrupa pot Residencial ou Comercial
# imovelcomercial<-consultadb("select Municipio,Bairro,Fonema_Municipio,Fonema_Bairro, 'Comercial' as Tipo, avg(Preco/M2) as ValorMedioM2, date_format(Data,'%Y/%m') as AnoMesColeta  from IMOVEL where M2>2 and (Tipo='Conjunto Comercial/Sala' or Tipo='Loja/Salão' or Tipo='Loja Shopping/ Ct Comercial' or Tipo='Casa Comercial') group by Fonema_Municipio,Fonema_Bairro,AnoMesColeta")
# imovelresidencial<-consultadb("select Municipio,Bairro,Fonema_Municipio,Fonema_Bairro, 'Residencial' as Tipo, avg(Preco/M2) as ValorMedioM2, date_format(Data,'%Y/%m') as AnoMesColeta  from IMOVEL where M2>2 and (Tipo='Casa' or Tipo='Casa de Condomínio' or Tipo='Apartamento' or Tipo='Quitinete' or Tipo='Flat' ) group by Fonema_Municipio,Fonema_Bairro,AnoMesColeta")

# quantidade de cada POI por Municipio e Bairro
# poibairro<-consultadb("select Municipio, Bairro, Descricao, count(Descricao) as Quantidade from (select Municipio, Bairro, Descricao, Fonema_Endereco from POI where Status is NULL and Latitude is not NULL and Bairro!='Na' and Bairro!='Indefinido' group by  Descricao,Municipio,Bairro,Fonema_Endereco) as a group by Municipio, Bairro, Descricao")
# poimunicipio<-consultadb("select Municipio, Descricao, count(Descricao) as Quantidade from (select Municipio,  Descricao, Fonema_Endereco from POI where Status is NULL and Latitude is not NULL  group by  Descricao,Municipio,Fonema_Endereco) as a group by Municipio, Descricao")

# imovelresidencial<-consultadb("select Municipio,Fonema_Municipio, 'Residencial' as Tipo, avg(Preco/M2) as ValorMedioM2, date_format(Data,'%Y/%m') as AnoMesColeta  from IMOVEL where M2>2 and (Tipo='Casa' or Tipo='Casa de Condomínio' or Tipo='Apartamento' or Tipo='Quitinete' or Tipo='Flat' ) group by Fonema_Municipio,AnoMesColeta")

