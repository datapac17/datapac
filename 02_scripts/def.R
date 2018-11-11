library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(extrafont)
library(ggplot2)
#font_import()
#carpetas<-c("01_datos_iniciais/","01_datos_iniciais/taboas","01_datos_iniciais/capas","02_scripts","03_datos_finais,",
#            "03_datos_finais/taboas","03_datos_finais/capas","04_rmd","05_documentos")
#sapply(carpetas,dir.create)


#Abrir datos--------
#Función para acadar unha taboa operativa a través das taboas de excel do ige
taboa_ige_excel<-function(taboa){
  #N?mero de columnas menos unha
  columnas_na<-ncol(taboa)-1
  #Li?as con nomes do factor que se usar? para agrupar
  lin_na<-which(apply(is.na(taboa[,-1]),1,sum)==columnas_na)
  #Li?as onde se atopan os valores v?lidos
  lin_val<-which(apply(is.na(taboa[,-1])==F,1,sum)==columnas_na)
  taboa_parcial<-taboa %>% 
    filter( between(row_number(),lin_na[1], lin_val[length(lin_val)])) %>% 
    setNames(paste("columna",letters[1:ncol(taboa)],sep="_"))
  #Nomes dos valores da variable factorial
  nomes_na<-taboa_parcial[which(apply(is.na(taboa_parcial),1,sum)==columnas_na),] %>%
    first()
  var_rep<-(lin_na[2]-lin_na[1])-1
  taboa_final<-taboa_parcial %>% filter(!columna_a%in%nomes_na) %>% 
    mutate(columna_a=rep(nomes_na,each=var_rep))
  taboa_final
}
#Abrir datos-----
ubicacion_arquivo<-here::here("content","01_datos_iniciais/2018-01-06_defuncions_idades.xls")
defuncions <- read_xls(ubicacion_arquivo) %>%
  remove_empty("rows") %>% taboa_ige_excel(.) %>%
  setNames(c("idade", "ano", "homes", "mulleres")) %>% 
  mutate_at(vars("ano","homes","mulleres"),as.numeric)



#Gráficos----
library(plotly)
p1<-defuncions %>% 
  gather(sexo,persoas,homes:mulleres) %>%
  filter(idade=="De máis de 69 anos") %>% 
  ggplot(.,aes(ano,persoas,fill=sexo))+
  geom_bar(stat="identity",position="dodge")+theme_minimal()+
  labs(x="",y="Número de defuncións")+
  scale_x_continuous(breaks=seq(1997,2016,1))+
  scale_y_continuous(breaks=seq(1,120,5))+
  scale_fill_grey(guides(title=""))+
  theme(legend.position = "bottom",text = element_text(family="Nunito Black"))
barplot_interactivo <- ggplotly(p1,width=1000,height = 560) %>% 
  layout(legend=list(orientation="h",x=0.4,y=-0.2))
#Gráfica interactiva

p2<-defuncions %>% 
  gather(sexo,persoas,homes:mulleres) %>%
  ggplot(.,aes(idade,persoas,fill=sexo))+
  geom_point(aes(size=persoas,frame=ano))+theme_minimal()+
  labs(x="",y="Número de defuncións")+
  scale_y_continuous(breaks=seq(1,120,5))+
  scale_fill_grey(guides(title=""))+
  theme(legend.position = "bottom",
        text = element_text(family="Nunito Black"),
        axis.title = element_text(size=20),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=15),
        panel.grid = element_line(color="white"))
scatter_animado <- ggplotly(p2,width=1000,height = 560) %>% 
  layout(legend=list(orientation="h",x=0.4,y=-0.4))