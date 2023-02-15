############################## Clasificacion climatica de estaciones argentinas ##################################################################
rm(list=ls())

#Tengo dos archivos txt, uno con los datos y otro con el formato de los datos
require(here)
FILE1 <- here("otra parte del 87 en adelante.txt") #guardo en FILE1 el archivo con los datos

# Lectura del archivo, uso read.table:
tabla1 <- read.table(FILE1,skip=10,nrows=6315) #6315 obs. of 50 variables

#Selecciono V1 y le doy formato fecha:
tabla1$V1=as.Date(paste0(tabla1$V1, tabla1$V50), format='%Y%m%d')
#En la variable 50 estan guardados los dias, uso paste0 para tener la fecha completa
#y asi poder usar as.Date

#Busco cantidad de estaciones disponibes:
estaciones=tabla1$V2[!duplicated(tabla1$V2)] #Elimino duplicados para obtener estaciones
length(estaciones) #tengo 46 estaciones distintas

#Ahora busco para cada estacion la cantidad de datos
cant_datos_est=c()
for(i in 1:46){
  cant_datos_est[i]=length(tabla1$V2[which(tabla1$V2==estaciones[i])])
}

#Para buscar periodos que abarcan:
periodo=c()
for(i in 1:46){ #Selecciono las fechas correspondientes a cada estacion
  fecha_est=tabla1$V1[which(tabla1$V2==estaciones[i])] #vector con todas las fechas de estacion i
  periodo[i]=paste(fecha_est[1],"a",fecha_est[length(fecha_est)]) 
} #Guardo en periodo la fecha de inicio y la fecha de fin


#Para tener la informacion junta creo un data.frame:
reporte=data.frame(ESTACION=estaciones,DATOS=cant_datos_est,PERIODO=periodo)
reporte


#############################################################################################################################


#Ahora se desea seleccionar aquellas estaciones que cuenten con 20 anios de datos,
#graficar de las primeras 4 estaciones la temperatura media mensual climatologica junto con la maxima y 
#minima en lineas y superpuesto el grafico de la lluvia mensual climatologica. 

#Primero le asigno a los datos faltantes de dichas variables el valor NA 
#para que no influyan en los graficos
tabla1$V2[which(tabla1$V2==-99.9)]=NA 
tabla1$V3[which(tabla1$V3==-99.9)]=NA
tabla1$V4[which(tabla1$V4==-99.9)]=NA
tabla1$V5[which(tabla1$V5==-99.9)]=NA
tabla1$V13[which(tabla1$V13==-99.9)]=NA

#busco aquellas estaciones con 20 anios
cant_anios=c() #vector vacio para ir guardando
for(i in 1:46){ #para cada una de las estaciones
  d=tabla1$V1[which(tabla1$V2==estaciones[i])] #Selecciono las fechas de cada estacion
  inicio=as.numeric(format(d[1],'%Y')) #guardo el anio de inicio
  fin=as.numeric(format(d[length(d)],'%Y')) #guardo el anio de fin
  cant_anios[i]=fin-inicio+1 #resto los anios
}

est=estaciones[which(cant_anios==20)] #creo un vector con los numeros de estaciones que cumplen
length(est) #Se obtienen 24 estaciones con 20 anios de datos

#Creo un LOOP para calcular las medias de las variables de interes:

#1ro modifico la variable date tal que me muestre las fechas como mes numerico
tabla1$V1=as.numeric(format(tabla1$V1,'%m')) 

MEAN=array(NA,dim=c(24,12)) #creo arrays vacios para guardar datos
MIN=array(NA,dim=c(24,12))  #24 estaciones (filas) x 12 meses (columnas)
MAX=array(NA,dim=c(24,12))
RPCP=array(NA,dim=c(24,12))

for(i in 1:12){ #para los 12 meses
  for(j in 1:24){ #y las 24 estaciones
    #pido que calcule las medias:
    MEAN[j,i]=mean(tabla1$V3[which(tabla1$V2==est[j] & tabla1$V1==i)],na.rm=TRUE) 
    MIN[j,i]=mean(tabla1$V5[which(tabla1$V2==est[j] & tabla1$V1==i)],na.rm=TRUE)
    MAX[j,i]=mean(tabla1$V4[which(tabla1$V2==est[j] & tabla1$V1==i)],na.rm=TRUE)
    RPCP[j,i]=mean(tabla1$V13[which(tabla1$V2==est[j] & tabla1$V1==i)],na.rm=TRUE)
  }}

#Graficos en paneles 2x2:
require(ggplot2) #para graficar
library(tidyr) #para ordenar el data.frame

#Primeras 4 estaciones
df1=data.frame(mes=c(1:12),mean=MEAN[1,],min=MIN[1,],max=MAX[1,],rpcp=RPCP[1,]) #data.frame 1er estacion
df1 <- pivot_longer(df1, cols = c(2:5), names_to = "Variable") #reordeno el data.frame
df_1=data.frame(mes=c(1:12),rpcp=round(RPCP[1,],1)) #Creo otro data.frame para mostrar barras de ppt
est1=ggplot()+#guardo el grafico en la variable est1
  geom_bar(data=df_1,mapping = aes(x = mes, y =rpcp), stat = "identity", fill = "grey")+ #grafico de barras
  geom_line(data=df1,aes(mes,y=value,color = Variable), #grafico de lineas
            linetype = 1, #tipo linea
            lwd = 1.5)+ #tamanio linea
  labs(x="Mes",y="Valor",title='Estacion 87623')+ #agrego titulo para identificar estacion
  scale_color_discrete(labels = c("Tmax (ºc)","Tmean (ºc)","Tmin (ºc)","Ppt (mm)"))+ #modifico leyendas
  scale_x_continuous(breaks = c(1:12))+ #ordeno eje x
  geom_text(data=df_1, aes(x = mes, y =rpcp, label=paste0(rpcp,"mm"), vjust=1.6))+ #agrego valores a las barras
  theme(plot.title = element_text(size = 18, face = "bold"), #modifico los tamaños de las leyendas
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))

df2=data.frame(mes=c(1:12),mean=MEAN[2,],min=MIN[2,],max=MAX[2,],rpcp=RPCP[2,]) 
df2 <- pivot_longer(df2, cols = c(2:5), names_to = "Variable") 
df_2=data.frame(mes=c(1:12),rpcp=round(RPCP[2,],1)) 
est2=ggplot()+
  geom_bar(data=df_2,mapping = aes(x = mes, y =rpcp), stat = "identity", fill = "grey")+
  geom_line(data=df2,aes(mes,y=value,color = Variable),
            linetype = 1, 
            lwd = 1.5)+ 
  labs(x="Mes",y="Valor",title='Estacion 87645')+ 
  scale_color_discrete(labels = c("Tmax (ºc)","Tmean (ºc)","Tmin (ºc)","Ppt (mm)"))+
  scale_x_continuous(breaks = c(1:12))+
  geom_text(data=df_2, aes(x = mes, y =rpcp, label=paste0(rpcp,"mm"), vjust=1.6))+
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))

df3=data.frame(mes=c(1:12),mean=MEAN[3,],min=MIN[3,],max=MAX[3,],rpcp=RPCP[3,]) 
df3 <- pivot_longer(df3, cols = c(2:5), names_to = "Variable")
df_3=data.frame(mes=c(1:12),rpcp=round(RPCP[3,],1)) 
est3=ggplot()+
  geom_bar(data=df_3,mapping = aes(x = mes, y =rpcp), stat = "identity", fill = "grey")+
  geom_line(data=df3,aes(mes,y=value,color = Variable),
            linetype = 1, 
            lwd = 1.5)+ 
  labs(x="Mes",y="Valor",title='Estacion 87648')+ 
  scale_color_discrete(labels = c("Tmax (ºc)","Tmean (ºc)","Tmin (ºc)","Ppt (mm)"))+
  scale_x_continuous(breaks = c(1:12))+
  geom_text(data=df_3, aes(x = mes, y =rpcp, label=paste0(rpcp,"mm"), vjust=1.6))+
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))

df4=data.frame(mes=c(1:12),mean=MEAN[4,],min=MIN[4,],max=MAX[4,],rpcp=RPCP[4,]) 
df4 <- pivot_longer(df4, cols = c(2:5), names_to = "Variable") 
df_4=data.frame(mes=c(1:12),rpcp=round(RPCP[4,],1)) 
est4=ggplot()+
  geom_bar(data=df_4,mapping = aes(x = mes, y =rpcp), stat = "identity", fill = "grey")+
  geom_line(data=df4,aes(mes,y=value,color = Variable),
            linetype = 1, 
            lwd = 1.5)+ 
  labs(x="Mes",y="Valor",title='Estacion 87649')+ 
  scale_color_discrete(labels = c("Tmax (ºc)","Tmean (ºc)","Tmin (ºc)","Ppt (mm)"))+
  scale_x_continuous(breaks = c(1:12))+
  geom_text(data=df_4, aes(x = mes, y =rpcp, label=paste0(rpcp,"mm"), vjust=1.6))+
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))


#Agrupo en 4 paneles:
require(gridExtra) #para obtener una unica figura con varios paneles
require(grid) #para modificar los titulos

EST_1=grid.arrange(est1,est2,est3,est4,nrow=2,
                   top=textGrob("Datos mensuales por estacion", #agrego titulo
                                hjust=2.3,gp=gpar(fontsize=18,font=1))) 
#Pido que me guarde la figura creada en una ruta especifica:
ggsave(filename="figura1.jpg", 
       path = "Salidas_1/", EST_1,
       width=16, height=8) 



####################################################################################################################



#Ahora clasifico los resultados obtenidos segun los criterios de Koppen para clasificar las estaciones de
#acuerdo a su clima:
#La idea es ir guardando en vectores separados el criterio ppal, criterio secundario y criterio terciario

#Criterio principal:
crit1=c()
#Primero veo cuales cumplen E POLAR CLIMATES tmax<10
for(i in 1:24){ 
  m=max(MAX[i,],na.rm=T) #Tmax
  if(m<10){
    crit1[i]=i}}
crit1 #devuelve NULL, entonces E no va a influir en la clasificacion

#Seguno verifico cuales  cumplen B ARID CLIMATES Pann<10Pth
pth=c() #primero defino Pth (ppt umbral)
for(i in 1:24){ 
  m=mean(MEAN[i,],na.rm=T) #Tann
  pan=sum(RPCP[i,],na.rm=T) #Pann
  pw=mean(RPCP[i,c(6,7,8)]) #Ppt winter
  ps=mean(RPCP[i,c(12,1,2)]) #Ppt summer
  if((2/3)*pan >= pw){
    pth[i]=2*m}else if((2/3)*pan>=ps){
      pth[i]=2*m+28}else{
        pth[i]=2*m+14
      }}
for(i in 1:24){ #Veo que valores cumplen con B
  pan=sum(RPCP[i,],na.rm=T) #Pann
  if(pan<10*pth[i]){ 
    crit1[i]="B"}} 

#Tercero verifico A,C,D: EQUATORIAL CLIMATES tmin>=18ºc, WARM TEMPERATE CLIMATES tmin<18 & tmin>-3,
#SNOW CLIMATES tmin<=-3
for(i in 1:24){
  m=min(MIN[i,],na.rm=T) #Tmin
  if(m>=18 & is.na(crit1[i])){ #si ese minimo es >=18 y la posicion i no esta ocupada
    crit1[i]="A" #guardo la clasificacion A en la posicion i
  }else if(m<18 & m>(-3) & is.na(crit1[i])){
    crit1[i]="C"
  }else if(m<=(-3) & is.na(crit1[i])){
    crit1[i]="D"
  }}

#Ahora veo las subclasificaciones:
#BS STEPPE CIMATE Pann>5Pth 
#BW DESERT CIMATE Pann<=5Pth 
crit2=c()
for(i in 1:24){
  pan=sum(RPCP[i,],na.rm=T)#Pann
  p=5*pth[i]
  if(crit1[i]=="B" & pan>p){ #Solo si crit1 es B
    crit2[i]="S"
  }else if(crit1[i]=="B" & pan[i]<=p){ 
    crit2[i]="W"
  }}

#Cs WARM TEMPERATE CLIMATE WITH DRY SUMMER Psmin<Pwmin, Pwmax>3Psmin & Psmin<40
#Cw WARM TEMPERATE CLIMATE WITH DRY WINTER Pwmin<Psmin & Psmax>10Pwmin
#Cf WARM TEMPERATE CLIMATE, FULLY HUMID neither Cs nor Cw 
win=RPCP[,c(6,7,8)] #meses de invierno
sum=RPCP[,c(12,1,2)] #meses de verano
for(i in 1:24){
  min_w=min(win[i,],na.rm=T) #Pwmin
  max_w=max(win[i,],na.rm=T) #Pwmax
  min_s=min(sum[i,],na.rm=T) #Psmin
  max_s=max(sum[i,],na.rm=T) #Psmax
  if(crit1[i]=="C" & min_s<min_w & max_w>(3*min_s) & min_s<40){ #Si crit1 es C
    crit2[i]="s"
  }else if(crit1[i]=="C" & min_w<min_s & max_s>(10*min_w)){ 
    crit2[i]="w"
  }else if(crit1[i]=="C" & is.na(crit2[i])){ #Si crit1 es C y si la posicion i no esta ocupada
    crit2[i]="f"}}

#Ds SNOW CLIMATE WITH DRY SUMMER Psmin<Pwmin, Pwmax>3Psmin & Psmin<40
#Dw SNOW CLIMATE WITH DRYWINTER Pwmin<Psmin, Psmax>10*Pwmin
#Df SNOW CLIMATE, FULLY HUMID neither Ds nor Dw
for(i in 1:24){
  min_w=min(win[i,],na.rm=T) #Pwmin
  max_w=max(win[i,],na.rm=T) #Pwmax
  min_s=min(sum[i,],na.rm=T) #Psmin
  max_s=max(sum[i,],na.rm=T) #Psmax
  if(crit1[i]=="D" & min_s<min_w & max_w>(3*min_s) & min_s<40){ #Si crit1 es D
    crit2[i]="s"
  }else if(crit1[i]=="D" & min_w<min_s & max_s>(10*min_w)){
    crit2[i]="w"}else if(crit1[i]=="D" & is.na(crit2[i])){ #Si crit1 es D y la posicion i no esta ocupada
      crit2[i]="f"}}

#Sub-sub clasificaciones:
#Para la clasificacion primaria B
#h HOTE STEPPE/DESERT Tann>=18 para 
#k COLD STEPPE/DESERT Tann<18 
crit3=c()
for(i in 1:24){
  t=sum(MEAN[i,],na.rm=T)
  if(crit1[i]=="B" & t>=18){ #si crit1 es B
    crit3[i]="h"
  }else if(crit1[i]=="B" & t<18){
    crit3[i]="k"}}

#Para la clasificacion primaria C y D
#a HOT SUMMER Tmax>=22
for(i in 1:24){
  m=max(MAX[i,],na.rm=T) #Tmax
  if(m>=22 & crit1[i]!="B"){ #Si crit1 NO es B
    crit3[i]="a"}}

#b WARM SUMMER not a & at least 4Tmon>=10 (temperatura mayor a 10 durante al menos cuatro meses)
for(i in 1:24){
  m=max(MAX[i,],na.rm=T)
  T1=MEAN[,c(1,2,3,4)] #selecciono periodos de 4 meses
  T2=MEAN[,c(5,6,7,8)]
  T3=MEAN[,c(9,10,11,12)]
  if(is.na(crit3[i]) & T1[i,1]>=10 & T1[i,2]>=10 & T1[i,3]>=10 & crit1[i]!="B" ){ #si las temperaturas de los 4 periodos es >=0
    #si la posicion i no esta ocupada y crit1 es C o D
    crit3[i]="b"}else if(is.na(crit3[i]) & T2[i,1]>=10 & T2[i,2]>=10 & T2[i,3]>=10 & crit1[i]!="B" ){
      crit3[i]="b"
    }else if(is.na(crit3[i]) & T3[i,1]>=10 & T3[i,2]>=10 & T3[i,3]>=10 & crit1[i]!="B"){
      crit3[i]="b"}}

#c COOL SUMMER AND COOL WINTER not b & Tmin>-38
#d EXTREMELY CONTINENTAL like c but Tmin<=-38
for(i in 1:24){
  m=min(MIN[i,],na.rm=T)
  if(m>(-38) & is.na(crit3[i])& crit1[i]!="B"){ #si m>-38, si la posicion i no esta ocupada y si crit1 es C o D
    crit3[i]="c"
  }else if(m<=(-38) & is.na(crit3[i]) & crit1[i]!="B"){
    crit3[i]="d"
  }}


#Vuelco todas las clasificaciones en un mapa de Argentina
#Primero creo el mapa:

world <- map_data("world") #guardo en world los datos del mapa "world"
arg=world[world$region=="Argentina",]  #selecciono de world solo los datos de arg
mi_mapa <- geom_path(data = arg, aes(long, lat, group = group), #guardo el geom_path para llamarlo mas facil
                     size = 0.1)

#Guardo en una tabla las estaciones con sus respectivas lons, lats y sus clasificaciones:

lon_est=c(-64.2,-59.2,-57.4,-59.5,-60.2,-57.4,-68.1,-62.1,-71.1,-71.1,-68.4,-65.1,-63,-71.1,-68.5,
          -65.2,-71,-67.3,-70.1,-66,-67.5,-69.2,-67.5,-68.2)
lat_est=c(-36.3,-37.1,-36.21,-37.4,-38.2,-37.5,-38.5,-38.4,-40,-41,-41.1,-40.4,-40.5,-42.6,-43.5,
          -43.1,-46.3,-45.5,-48.5,-47.4,-49.2,-51.4,-53.5,-54.5)
nombres=c("santa Rosa Aero","Tandil Aero","Dolores Aero", "Benito Juarez Aero",
          "Tres Arroyos","Mar del Plata Aero","Neuquen Aero","Bahia Blanca Aero",
          "Chapelco Aero","Bariloche Aero","Maquinchao","San Antonio Oeste Aero",
          "Viedma Aero","Esquel Aero", "Paso de los indios", "Trelew Aero",
          "Perito Moreno Aero","Comodoro Rivadavia Aero", "Gobernador Gregores Aero","Puerto Deseado Aero",
          "San Julian Aero", "Rio Gallegos Aero", "Rio Grande B.A.","Ushuaia Aero")

tabla2=data.frame(ESTACIONES=est,LONGITUD=lon_est,LATIUD=lat_est,NOMBRE=nombres, #estaciones y latitudes
                  CLASIFICACION=paste(crit1, crit2, crit3),stringsAsFactors = F) #clasificaciones

#Guardo esta tabla en un archivo ASCII:
#Genero un archivo de salida dentro del proyecto en la carpeta Salidas
file_out <- here("Salidas_1", "escritura_tabla.txt")

#Uso write.table para escribir el archivo ASCII
write.table(tabla2, file_out, quote = FALSE, row.names = FALSE,
            col.names = TRUE)

#Con la informacion organizada en un data.frame:
tabla2

#Ahora agrego las clasificaciones al mapa

map=ggplot() +
  mi_mapa + #mapa de argentina como base
  labs(title="Clasificación Climática de Köppen-Geiger", #Agrego un titulo
       x="Longitud", y="Latitud")+
  geom_text(aes(x = -60, y = -48, #Agrego una leyenda dentro del mapa
                label = "Mapa de\n Argentina"), 
            stat = "unique", size=5, color="grey")+
  geom_label(data=tabla2, aes(x=lon_est, y=lat_est, #agrego carteles con las clasificaciones
                              label = CLASIFICACION)) 

ggsave(filename="mapa_arg.jpg", #guardo como imagen en Salidas 
       path = "Salidas_1/", map,
       width=6, height=7) #tamaño
