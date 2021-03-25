source("Script/funciones.R")
library(openxlsx)
library(tidyverse)
library(magrittr)

data_banco <- read.xlsx(xlsxFile = "Base/Data_Banco.xlsx", sheet = "Data")
names(data_banco)[1]='ID_Sucursal'
data_sucursal <- read.xlsx(xlsxFile = "Base/Data_Banco.xlsx", sheet = "Data_Sucursal")
data_cajero = read.xlsx(xlsxFile = "Base/Data_Banco.xlsx", sheet = "Data_Cajero")
data_cajero$Cajero = as.numeric(data_cajero$Cajero)
data_cajero$Edad = as.numeric(data_cajero$Edad)

data_1 <- data_banco %>%
  left_join(data_sucursal,by="ID_Sucursal") %>%
  left_join(data_cajero,by="Cajero") %>%
  mutate(Anio.experiencia = 2019-Anio.Ingreso) %>% 
  mutate(Satisfaccion = factor(Satisfaccion,
                               levels = c('Muy Malo','Malo','Regular','Bueno','Muy Bueno'),
                               labels = c('Muy Malo','Malo','Regular','Bueno','Muy Bueno'),
                               ordered = T)) %>%
  mutate(Nivel_Formacion = factor(Nivel_Formacion,
                                  levels = c('Tecnologia','Bachiller','Univ Incom.','Tercer nivel'),
                                  labels = c('Tecnologia','Bachiller','Univ Incom.','Tercer nivel'),
                                  ordered = T)) %>% 
 
  mutate_if(is.character, as.factor) %>%
  mutate(Cajero=factor(Cajero)) %>% 
  mutate(Monto=as.numeric(Monto)) %>% 
  select(-Anio.Ingreso)
  names(data_1)
  str(data_1)
  summary(data_1)
  #######
  data_1%>% summarise_at(vars(Tiempo_Servicio_seg), 
                              funs(MEDIA= mean(., na.rm=TRUE), 
                                   VARIANZA= var(., na.rm = TRUE),
                                   RIQ=IQR(., na.rm = TRUE),
                                   RANGO=diff(range(., na.rm = TRUE)),
                                   CANTIDAD= n()))
  eda_tiempo<-MYEDA(data_1$Tiempo_Servicio_seg,plot=T)
  library(kableExtra)
  kable(eda_tiempo$TOT) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
#####                
  dt<-data_1 %>%  group_by(Sucursal) %>% summarise_at(vars(Tiempo_Servicio_seg), 
                                              funs(MEDIA= mean(., na.rm=TRUE),
                                                   VARIANZA= var(., na.rm = TRUE),
                                                  RIQ=IQR(., na.rm = TRUE),
                                                  RANGO=diff(range(., na.rm = TRUE)),
                                                  CANTIDAD= n()))
  kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  boxplot(data_1$Tiempo_Servicio_seg ~ data_1$Sucursal)
#####
  data_1 <- data_1 %>% mutate(TRANS=log(Tiempo_Servicio_seg))
  dt1<-data_1 %>%  group_by(Sucursal) %>% summarise_at(vars(TRANS), 
                                              funs(MEDIA= mean(., na.rm=TRUE),
                                                   VARIANZA= var(., na.rm = TRUE),
                                                   RIQ=IQR(., na.rm = TRUE),
                                                   RANGO=diff(range(., na.rm = TRUE)),
                                                   CANTIDAD= n()))
  kable(dt1) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  boxplot(data_1$LTS ~ data_1$Sucursal,xlab = "Sucursal",ylab = "Tiempo de Servicio Min")
######
  suc_Riocentro_sur <- data_1 %>% dplyr::filter(ID_Sucursal=="62")
  suc_Centro <- data_1 %>% dplyr::filter(ID_Sucursal=="85")
  suc_Alborada <- data_1 %>% dplyr::filter(ID_Sucursal=="267")
  suc_Mall_del_sol <- data_1 %>% dplyr::filter(ID_Sucursal=="443")
  suc_Via_Daule <- data_1 %>% dplyr::filter(ID_Sucursal=="586") 
  ##
  eda_Riocentro_sur<-MYEDA(suc_Riocentro_sur$TRANS, plot = F)
  kable(eda_Riocentro_sur[1]) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  ##
  eda_Centro<-MYEDA(suc_Centro$TRANS)
  kable(eda_Centro[1]) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  ##
  eda_alborada<-MYEDA(suc_Alborada$TRANS)
  kable(eda_alborada[1]) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  ##
  eda_mall<-MYEDA(suc_Mall_del_sol$TRANS)
  kable(eda_mall[1]) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  ##
  eda_via_daule<-MYEDA(suc_Via_Daule$TRANS)
  kable(eda_via_daule[1]) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  
  
  ###
  
  ###
  suc_Centro%>% group_by(Cajero,Sexo,Anio.experiencia,Edad) %>% 
    summarise(Tiempo_Servicio_min=mean(Tiempo_Servicio_seg)/60,
              NTransaccciones=n()) %>%
    kable()%>% 
    kable_styling(bootstrap_options = "striped", full_width = F)
  