library("dplyr")
library("purrr")
library("janitor")
library("sf")
library("ggplot2")
library("viridis")
#Mapa interactivo
library("leaflet")
#01_abrir_datos----
#Datos de parroquias
parroquias<-st_read(here::here("content","01_datos_iniciais/capas/lalin_parroquias.shp"),
                      options = "ENCODING=ISO8859-1") %>% st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  clean_names() %>% rename(nome=nombre) %>% mutate(nome=factor(tolower(nome)))

#Datos do nomenclátor 2017
ubicacion_arquivo<-here::here("content","01_datos_iniciais/2018-11-07_nomenc_Lalin17.csv")
nomenclator_17 <- read.csv(ubicacion_arquivo,skip=3) %>% as.tbl() %>% 
  filter_all(all_vars(is.na(.)==F)) %>% clean_names() %>% select(nome,total) %>% 
  mutate(nome=factor(tolower(nome)))
#Reaxuste dos niveis da capa de parroquias, nos nomes destas
levels(parroquias$nome)[levels(parroquias$nome)%in%levels(nomenclator_17$nome)==F]<-
  setdiff(levels(nomenclator_17$nome),levels(parroquias$nome))

#Capa de parroquias unida a táboa de datos do nomenclátor 
#Antes é necesario dividir a poboación en grupos numéricos para poder comparar visualmente
cuantis<-quantile(nomenclator_17$total,probs=c(0,0.1,0.25,0.5,0.75,0.9,1))

etiquetas <- map( 1:length(cuantis),~paste0(round(cuantis[.x]), " - ",round(cuantis[.x + 1]))) %>% 
  unlist() %>% .[-length(.)]

nomenclator_shp<-nomenclator_17 %>% inner_join(.,parroquias,by="nome")%>% 
  mutate(grupos=cut(total, 
    breaks = cuantis,
    labels=etiquetas,
    include.lowest = T)) %>% st_sf()



#Gráficos----


#Gráfico estático
proba<-nomenclator_shp %>% ggplot()+geom_sf(aes(fill=grupos))+
  scale_fill_viridis(
    option = "magma",
    name = "Grupos de poboación",
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
  direction = "auto"))







