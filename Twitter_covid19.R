library(ROAuth)
library(streamR)
library(twitteR)
library(rtweet)
library(tidyverse)
library(tidytext)


# ACCESO A TWITTER --------------------------------------------------------


app_name <- "######################################"
consumer_key <- "######################################"
consumer_secret <- "######################################"
access_token <- "##########################################"
access_secret <- "#########################################"
twitter_token <- create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)
# COLOMBIA ----------------------------------------------------------------
t_Col<- search_tweets(q = "covid" , type ="recent" ,n = 2500, lang="es",include_rts = FALSE, geocode = "4.624335,-74.063644,50mi" )
t_Col<-as.data.frame(t_Col)
t_col<- filter(t_Col,verified == F)
Col<- select(t_col,screen_name,created_at,text,retweet_count)




Col$created_at <- as.character(Col$created_at)
col<-filter(Col, grepl('2020-05-23', created_at ))
view(col)

# ESPAÑA ------------------------------------------------------------------


t_Esp<- search_tweets(q = "covid" , type ="recent" ,n = 2500, lang="es",include_rts = FALSE, geocode = "40.416775,-3.703790,50mi" )
t_Esp<-as.data.frame(t_Esp)
t_Esp<- filter(t_Esp,verified == F)
Esp<- select(t_Esp,screen_name,created_at,text,retweet_count)


Esp$created_at <- as.character(Esp$created_at)
Esp<-filter(Esp, grepl('2020-05-23', created_at ))
view(Esp)
# ECUADOR -----------------------------------------------------------------
t_Eq<- search_tweets(q = "covid" , type ="recent" ,n = 2500, lang="es",include_rts = FALSE, geocode = "-0.180653,-78.467834,50mi" )
t_Eq<-as.data.frame(t_Eq)
t_Eq<- filter(t_Eq,verified == F)
Eq<- select(t_Eq,screen_name,created_at,text,retweet_count)





Eq$created_at <- as.character(Eq$created_at)

Eq<-filter(Eq, grepl('2020-05-23', created_at ))
view(Eq)
# MEXICO ------------------------------------------------------------------
t_Mx<- search_tweets(q = "covid" , type ="recent" ,n = 2500, lang="es",include_rts = FALSE, geocode = "19.432608,-99.133209,50mi" )
t_Mx<-as.data.frame(t_Mx)
t_Mx<- filter(t_Mx,verified == F)
Mx<- select(t_Mx,screen_name,created_at,text,retweet_count)





Mx$created_at <- as.character(Mx$created_at)
Mx<-filter(Mx, grepl('2020-05-23', created_at ))
view(Mx)

# PERU --------------------------------------------------------------------


t_Pe<- search_tweets(q = "covid" , type ="recent" ,n = 2500, lang="es",include_rts = FALSE, geocode = "-12.046374,-77.042793,50mi" )
t_Pe<-as.data.frame(t_Pe)
t_Pe<- filter(t_Pe,verified == F)
Pe<- select(t_Pe,screen_name,created_at,text,retweet_count)


Pe$created_at <- as.character(Pe$created_at)
Pe<-filter(Pe, grepl('2020-05-23', created_at ))
view(Pe)

# Paraguay ----------------------------------------------------------------


t_par<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = "-25.3006592,-57.63591,50mi" )
t_par<-as.data.frame(t_par)
t_par<- filter(t_par,verified == F)
par<- select(t_par,screen_name,created_at,text,retweet_count)
view(par)

par$created_at <- as.character(par$created_at)
par<-filter(par, grepl('2020-05-#DIA', created_at ))
view(par)

# Uruguay -----------------------------------------------------------------


t_Uru<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = "-34.9032784,-56.1881599,50mi" )
t_Uru<-as.data.frame(t_Uru)
t_Uru<- filter(t_Uru,verified == F)
Uru<- select(t_Uru,screen_name,created_at,text,retweet_count)
view(Uru)

Uru$created_at <- as.character(Uru$created_at)
Uru<-filter(Uru, grepl('2020-05-#DIA', created_at ))
view(Uru)

# Chile -------------------------------------------------------------------


t_Chi<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = " -33.4569400,-70.6482700,50mi" )
t_Chi<-as.data.frame(t_Chi)
t_Chi<- filter(t_Chi,verified == F)
Chi<- select(t_Chi,screen_name,created_at,text,retweet_count)
view(Eq)

Chi$created_at <- as.character(Chi$created_at)
Chi<-filter(Chi, grepl('2020-05-#DIA', created_at ))
view(Chi)

# Bolivia -----------------------------------------------------------------


t_Bol<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = " -16.5000000,-68.1500000,50mi" )
t_Bol<-as.data.frame(t_Bol)
t_Bol<- filter(t_Bol,verified == F)
Bol<- select(t_Bol,screen_name,created_at,text,retweet_count)
view(Bol)


Bol$created_at <- as.character(Bol$created_at)
Bol<-filter(Bol, grepl('2020-05-#DIA', created_at ))
view(Bol)

# Venezuela ---------------------------------------------------------------


t_Ven<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = "10.6222200,-66.5735300,50mi" )
t_Ven<-as.data.frame(t_Ven)
t_Ven<- filter(t_Ven,verified == F)
Ven<- select(t_Ven,screen_name,created_at,text,retweet_count)
view(Ven)


Ven$created_at <- as.character(Ven$created_at)
Ven<-filter(Ven, grepl('2020-05-#DIA', created_at ))
view(Ven)

# Argentina ---------------------------------------------------------------



t_Arg<- search_tweets(q = "covid 19" , type ="recent" ,n = 3000, lang="es",include_rts = FALSE, geocode = "-34.6131516,-58.3772316,50mi" )
t_Arg<-as.data.frame(t_Arg)
t_Arg<- filter(t_Arg,verified == F)
Arg<- select(t_Arg,screen_name,created_at,text,retweet_count)
view(Arg)


Arg$created_at <- as.character(Arg$created_at)
Arg<-filter(Arg, grepl('2020-05-#DIA', created_at ))
view(Arg)

# Exportar a excel --------------------------------------------------------
###CAMBIAR FECHAS EN LOS NOMBRES DEL ARCHIVO

write.table(par,file = "Par#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(Uru,file = "Uru#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(Chi,file = "Chi#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(Bol,file = "Bol#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(Ven,file = "Ven#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(Arg,file = "Arg#DIAMAY.csv" ,row.names = FALSE,sep = ";")
write.table(col,file = "Col23MAY.csv" ,row.names = FALSE,sep = ";")
write.table(Eq,file = "Eq23MAY.csv" ,row.names = FALSE,sep = ";")
write.table(Esp,file = "Esp23MAY.csv" ,row.names = FALSE,sep = ";")
write.table(Mx,file = "Mx23MAY.csv" ,row.names = FALSE,sep = ";")
write.table(Pe,file = "Pe23MAY.csv" ,row.names = FALSE,sep = ";")




######### DATOS COVID #########################


library(COVID19)

#Colombia
Col_cov <-covid19(country=170, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#Equador
Eq_cov <- covid19(country= 218, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#España
Esp_cov <- covid19(country = 724, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#Mexico
Mx_cov <- covid19(country = 484	, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#peru
Pe_cov <- covid19(country  = 604	, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#ARGENTINA
arg_cov <-covid19(country=32, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#BOLIVIA
bol_cov <- covid19( country= 68, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#CHILE
chi_cov <- covid19(country = 152, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#PARAGUAY
par_cov <- covid19(country = 600, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#URUGUAY
uru_cov <- covid19(country = 858, level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
#VENEZUELA
ven_cov <- covid19(country = 862 , level= 1, end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)




write.table(Col_cov,file = "Col_covid.csv" ,row.names = FALSE,sep = ";")
write.table(Eq_cov ,file = "Eq_covid.csv" ,row.names = FALSE,sep = ";")
write.table(Esp_cov ,file = "Esp_covid.csv" ,row.names = FALSE,sep = ";")
write.table(Mx_cov ,file = "Mx_covid.csv" ,row.names = FALSE,sep = ";")
write.table(Pe_cov ,file = "Pe_covid.csv" ,row.names = FALSE,sep = ";")
write.table(arg_cov,file = "arg_covid.csv" ,row.names = FALSE,sep = ";")
write.table(bol_cov ,file = "bol_covid.csv" ,row.names = FALSE,sep = ";")
write.table(chi_cov ,file = "chi_covid.csv" ,row.names = FALSE,sep = ";")
write.table(par_cov ,file = "par_covid.csv" ,row.names = FALSE,sep = ";")
write.table(uru_cov ,file = "uru_covid.csv" ,row.names = FALSE,sep = ";")
write.table(ven_cov ,file = "ven_covid.csv" ,row.names = FALSE,sep = ";")