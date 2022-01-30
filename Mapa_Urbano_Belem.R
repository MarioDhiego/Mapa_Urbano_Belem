############### MAPAS URBANOS ##################################################################
# Foram gerado Mapas Urbanos com base cartograficas do IBGE + 
# Censo demografico de 2010


############## CARREGAR PACOTES ################################################################
library(geobr)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(ggmap)
library(rgdal)
library(RCurl)
library(curl)
library(tmaptools)
library(RColorBrewer)
library(colorspace)
library(ggspatial)

library(ggthemes)
################################################################################################

############# Baixar dados por Estado ########################################################## 
############ CE ################################################################################
Fortaleza <- read_census_tract(code_tract = 'CE') %>%
             dplyr::filter(name_muni=="Fortaleza" & zone=="URBANO") %>%
             mutate(Cod_setor=as.numeric(code_tract))
             
plot(Fortaleza$geom)
#################################################################################################

########### PA ##################################################################################
Belem <- read_census_tract(code_tract = 'PA', year = 2020) %>%
  dplyr::filter(name_muni=="Belém" & zone=="URBANO") %>%
  mutate(Cod_setor=as.numeric(code_tract))

plot(Belem$geom)



Manaus <- read_census_tract(code_tract = 'AM', year = 2010) %>%
  dplyr::filter(name_muni=="Manaus" & zone=="URBANO") %>%
  mutate(Cod_setor=as.numeric(code_tract))

plot(Manaus$geom)

#################################################################################################



########### DEFINIR DIRETORIO ####################################################################
setwd('C:/Users/mario Dhiego/Documents/Mapa_Urbano_Belem/Mapa_Urbano_Belem')


########## CARREGAR BANCO DE DADOS FORTALEZA #############################################################
censo.fortaleza <- read_excel('Basico_CE.xls') %>%
                   select(Cod_setor,V002) %>%
                   mutate(pop=cut(x = V002, 
                                  breaks=c(0,500,1000,1500,2000,2500,3000,Inf),
                                  labels=c('0-500','500-1000','1000-1500','1500-2000',
                                           '2000-2500','2500-3000','>3000')))
censo.fortaleza

#######################################################################################################


########## CARREGAR BANCO DE DADOS BELÉM #############################################################
censo.Belem <- read_excel('Basico_PA.xls') %>%
               select(Cod_setor,V002) %>%
               mutate(pop=cut(x = V002,
                              breaks=c(0,500,1000,1500,2000,2500,3000,Inf),
                              labels=c('0-500','500-1000','1000-1500','1500-2000',
                                       '2000-2500','2500-3000','>3000')))

censo.Belem


censo.Manaus <- read_excel('Basico_AM.xls') %>%
  select(Cod_setor,V002) %>%
  mutate(pop=cut(x = V002,
                 breaks=c(0,500,1000,1500,2000,2500,3000,Inf),
                 labels=c('0-500','500-1000','1000-1500','1500-2000',
                          '2000-2500','2500-3000','>3000')))

censo.Manaus




##############################################################################################

###### Classe do objeto ######################################################################
class(censo.fortaleza$Cod_setor)
class(censo.Belem$Cod_setor)
#############################################################################################


####### JUNTAR AS BASESE DE DADOS ###########################################################
Dados1 <- inner_join(Fortaleza,censo.fortaleza) %>%
          na.omit()  

Dados2 <- inner_join(Belem,censo.Belem) %>%
  na.omit()

Dados3 <- inner_join(Manaus,censo.Manaus) %>%
  na.omit()

############################################################################################

############### Mapa Fortaleza ############################################################
base.fortaleza <- get_stamenmap(bbox=as.numeric(geocode_OSM("Fortaleza")$bbox),
                                zoom=12,force=TRUE)
ggmap(base.fortaleza)

ggmap(base.fortaleza)+
  geom_sf(data = Dados1,inherit.aes = FALSE, aes(fill=pop) )+
  scale_fill_manual(values =brewer.pal(7,'YlOrBr'))+
  labs(x=NULL,y=NULL, 
         fill='Populacao de Fortalesa',
         title='Mapa Urbano')+
  annotation_scale()+
  annotation_north_arrow(location='tr', 
                         style = north_arrow_fancy_orienteering())+
  theme(legend.position= c(.9,.13),
        legend.key.height= unit(2,'mm'),
        legend.key.width = unit(3,'mm'))
  
ggsave(filename = 'Mapa_Urbano_for.png',
       height = 15,
       width = 15,
       units = 'cm',
       scale = 0.94)
#################################################################################



############### Mapa Belem ####################################################
base.Belem <- get_stamenmap(bbox = as.numeric(geocode_OSM("Belém")$bbox),
                                zoom = 14, 
                                force=TRUE)
ggmap(base.Belem)

ggmap(base.Belem)+
  geom_sf(data = Dados2,inherit.aes=FALSE,aes(fill=pop))+
  scale_fill_manual(values=brewer.pal(7,'YlOrBr'))+
  labs(x=NULL,y=NULL, 
         fill='População',
         title='Mapa Urbano de Belém',
       caption = 'Fonte: Censo/IBGE-2010', size=8)+
  annotation_scale()+
  theme_minimal()+
  annotation_north_arrow(location='tr', 
                         style = north_arrow_fancy_orienteering())+
  theme(legend.position= c(0.9,0.1),
        legend.key.height= unit(2,'mm'),
        legend.key.width = unit(2,'mm'))

ggsave(filename = 'Mapa_Urbano_bel.png',
       height = 15,
       width = 15,
       units = 'cm',
       scale = 0.94)
###############################################################################



############### Mapa Manaus ####################################################
base.Manaus <- get_stamenmap(bbox = as.numeric(geocode_OSM("Manaus")$bbox),
                            zoom = 12, 
                            force=TRUE)
ggmap(base.Manaus)

ggmap(base.Manaus)+
  geom_sf(data = Dados2,inherit.aes=FALSE,aes(fill=pop))+
  scale_fill_manual(values=brewer.pal(7,'YlOrBr'))+
  labs(x=NULL,y=NULL, 
       fill='População',
       title='Mapa Urbano de Manaus',
       caption = 'Fonte: Censo/IBGE-2010', size=8)+
  annotation_scale()+
  annotation_north_arrow(location='tr', 
                         style = north_arrow_fancy_orienteering())+
  theme(legend.position= c(0.9,0.1),
        legend.key.height= unit(2,'mm'),
        legend.key.width = unit(2,'mm'))

ggsave(filename = 'Mapa_Urbano_manaus.png',
       height = 15,
       width = 15,
       units = 'cm',
       scale = 0.94)
###############################################################################







