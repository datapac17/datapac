library("dplyr")
library("stringr")
library("purrr")
library("janitor")
library("sf")
library("ggplot2")
library("viridis")
#Mapa interactivo
library("leaflet")
#Táboas
library("kableExtra")
#01_abrir_datos----
#PARROQUIAS DE LALÍN
parroquias<-st_read(here::here("content","01_datos_iniciais/capas/lalin_parroquias.shp"),
                      options = "ENCODING=ISO8859-1") %>% st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  clean_names() %>% rename(nome=nombre) %>% mutate(nome=factor(tolower(nome)))

#Posición de Lalín no total de concellos de Galicia
concellos_galicia_pob<-read.csv("http://www.ige.eu/igebdt/igeapi/datos/5230/0:2017,1:0,2:0,9915:12:15006:15010:15066:15085:15011:15067:15072:15073:15007:15056:15014:15019:15029:15041:15040:15043:15068:15003:15009:15026:15027:15032:15039:15048:15063:15902:15064:15091:15090:15001:15005:15008:15017:15021:15030:15031:15058:15075:15015:15018:15050:15069:15070:15004:15022:15035:15036:15049:15051:15054:15055:15076:15081:15087:15023:15028:15034:15037:15052:15020:15053:15042:15057:15062:15071:15024:15038:15047:15059:15060:15084:15086:15901:15025:15044:15061:15002:15012:15013:15078:15082:15088:15089:15033:15065:15074:15046:15079:15080:15083:15016:15092:15093:15045:15077:27901:27006:27012:27034:27037:27045:27009:27016:27060:27004:27018:27035:27011:27014:27020:27023:27028:27039:27049:27056:27002:27902:27019:27027:27030:27063:27013:27038:27064:27066:27025:27005:27048:27051:27061:27029:27046:27053:27054:27017:27050:27052:27024:27026:27042:27043:27055:27057:27062:27001:27007:27010:27015:27022:27033:27044:27065:27021:27008:27031:27041:27047:27058:27059:27003:27032:27040:32001:32007:32043:32055:32036:32037:32006:32030:32041:32042:32051:32011:32013:32019:32035:32045:32061:32065:32074:32076:32005:32012:32016:32062:32067:32077:32078:32082:32089:32090:32032:32002:32008:32026:32031:32052:32054:32058:32059:32075:32079:32081:32087:32003:32004:32010:32018:32022:32025:32027:32040:32046:32069:32023:32049:32057:32080:32014:32020:32024:32033:32047:32056:32064:32066:32068:32084:32029:32044:32063:32070:32009:32015:32017:32038:32060:32072:32073:32083:32088:32021:32028:32039:32050:32053:32071:32085:32091:32034:32048:32086:32092:36023:36036:36048:36054:36055:36005:36010:36015:36032:36044:36040:36056:36030:36031:36034:36042:36050:36020:36016:36024:36047:36052:36059:36004:36008:36026:36029:36001:36009:36013:36014:36002:36007:36902:36012:36025:36041:36043:36038:36058:36006:36022:36901:36027:36028:36046:36051:36060:36061:36011:36017:36018:36003:36019:36021:36033:36035:36037:36039:36045:36049:36053:36057") %>% 
  as.tbl() %>% slice(-1) %>% select(-c(Tempo,Sexo,Grupos.de.idade,CodEspazo,DatoT)) %>% arrange(desc(DatoN))  


#Datos do nomenclátor 2017 por lugares
ubicacion_arquivo<-here::here("content","01_datos_iniciais/2018-11-11_lalin_lugares_17.csv")
nomenclator_17 <- read.csv2(ubicacion_arquivo,skip=3) %>% as.tbl() %>% 
  filter_all(all_vars(is.na(.)==F)) %>% clean_names() %>% select(-c(provincia,concello))

parroquias_cod<-nomenclator_17 %>%filter(es==0) %>%  select(ec,nome) %>% distinct() %>% 
  rename(parroquia=nome)
parroquias_lug<-nomenclator_17 %>% inner_join(parroquias_cod,by="ec")%>% filter(es!=0)

#Número de lugares por parroquia
parroquias_lugares<-parroquias_lug %>% 
  group_by(parroquia) %>%  count(parroquia) %>% arrange(desc(n))

#Poboación por parroquias
nomenclator_17<-parroquias_lug %>% group_by(parroquia) %>% summarise(total=sum(total,na.rm=T)) %>% 
  rename(nome=parroquia) %>% 
  mutate(nome=factor(tolower(nome)))


#Reaxuste dos niveis da capa de parroquias, nos nomes destas
levels(parroquias$nome)[levels(parroquias$nome)%in%levels(nomenclator_17$nome)==F]<-
  setdiff(levels(nomenclator_17$nome),levels(parroquias$nome))

#Capa de parroquias unida a t?boa de datos do nomencl?tor 
#Antes ? necesario dividir a poboaci?n en grupos num?ricos para poder comparar visualmente
cuantis<-quantile(nomenclator_17$total,probs=c(0,0.1,0.25,0.5,0.75,0.9,1))

etiquetas <- map( 1:length(cuantis),~paste0(round(cuantis[.x]), " - ",round(cuantis[.x + 1]))) %>% 
  unlist() %>% .[-length(.)]

nomenclator_shp<-nomenclator_17 %>% inner_join(.,parroquias,by="nome")%>% 
  mutate(grupos=cut(total, 
    breaks = cuantis,
    labels=etiquetas,
    include.lowest = T)) %>% st_sf()



#Gr?ficos----


#Gr?fico est?tico
proba<-nomenclator_shp %>% ggplot()+geom_sf(aes(fill=grupos))+
  scale_fill_viridis(
    option = "magma",
    name = "Grupos de poboaci?n",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
#Leaflet
pal <- colorBin("YlOrRd", domain = nomenclator_shp$total, bins = cuantis)
labels <- sprintf(
  "<strong>%s</strong><br/>%g habitantes</sup>",
  nomenclator_shp$nome, nomenclator_shp$total
) %>% lapply(htmltools::HTML)


mapa_interactivo<-leaflet()  %>% 
  setView(lng =  -8.110976, lat=42.661413,zoom=11) %>% 
  addPolygons(data=nomenclator_shp,fillColor = ~pal(total),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
label = labels,
labelOptions = labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto"))%>%   addLegend("bottomright", pal = pal, round(nomenclator_shp$total),
                                      labels= c("23-52", "52-96","96-161","161-239","239-429", "429-10670"),
                                      title = "Rangos de poboaci?n",
                                      opacity = 1
  )
  

#Taboa de lugares por parroquia

taboa_parroquias_lugares<-
  kable(parroquias_lugares,col.names=c("Parroquia","Número de lugares")) %>%
  kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
  column_spec(1, bold = T) %>% 
  scroll_box(width = "1000px", height = "400px")




