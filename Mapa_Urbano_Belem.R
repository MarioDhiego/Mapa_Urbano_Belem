############### MAPAS URBANOS ##################################################################
# Foram gerado Mapas Urbanos com base cartograficas do IBGE e Censo demogr?fico de 2010


############## CARREGAR PACOTES ################################################################
library(geobr)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(ggmap)
library(tmaptools)
library(RColorBrewer)
library(ggspatial)
################################################################################################

############# Baixar dados por Estado ########################################################## 
############ CE ################################################################################
fortaleza <- read_census_tract(code_tract = 'CE') %>%
             dplyr::filter(name_muni=="Fortaleza" & zone=="URBANO") %>%
             mutate(Cod_setor=as.numeric(code_tract))
             
             
plot(fortaleza$geom)

########### PA ##################################################################################
belem <- read_census_tract(code_tract = 'PA') %>%
  dplyr::filter(name_muni=="Belém" & zone=="URBANO")

plot(belem$geom)
#################################################################################################



########### DEINIR DIRETÓRIO ####################################################################
setwd('C:/Users/mario Dhiego/Documents/Mapas_Urbanos')


########## CARREGAR BANCO DE DADOS #############################################################
censo.fortaleza <- read_excel('Basico_CE.xls') %>%
                   select(Cod_setor,V002) %>%
                   mutate(pop=cut(x = V002, 
                                  breaks=c(0,500,1000,1500,2000,2500,3000,Inf),
                                  labels=c('0-500','500-1000','1000-1500','1500-2000',
                                           '2000-2500','2500-3000','>3000')))

censo.fortaleza
##############################################################################################

###### Classe do objeto ######################################################################
class(censo.fortaleza$Cod_setor)
#############################################################################################


####### JUNTAR AS BASESE DE DADOS ###########################################################
dados <- inner_join(fortaleza,censo.fortaleza) %>%
          na.omit()  
############################################################################################

############### Mapa Fortaleza ############################################################
base.fortaleza <- get_stamenmap(bbox=as.numeric(geocode_OSM("Fortaleza")$bbox),
                                zoom=12, 
                                force=TRUE)

ggmap(base.fortaleza)+
  geom_sf(data = dados,inherit.aes=FALSE,aes=(fill=pop))+
  scale_fill_manual(values=brewer.pal(6,'YlorBr'))+
  labels(x=NULL,y=NULL, 
         fill='População de Fortalesa',
         title='Mapa Urbano')+
  annotation_scale()+
  annotation_north_arrow(location='tr', 
                         style = north_arrow_orienteering())+
  theme(legend.position= c(0.9,0.13),
        legend.key.height= unit(2,'mm'),
        legend.key.width = unit(3,'mm'))
  
ggsave(filename = 'Mapa_Urbano_for.png',
       height = 15,
       width = 15,
       units = 'cm',
       scale = 0.94)
#################################################################################



############### Mapa Belem ####################################################
base.belem <- get_stamenmap(bbox = as.numeric(geocode_OSM("Belém")$bbox),
                                zoom = 12, 
                                force=TRUE)

ggmap(base.belem)+
  geom_sf(data = dados,inherit.aes=FALSE,aes=(fill=pop))+
  scale_fill_manual(values=brewer.pal(6,'YlorBr'))+
  labels(x=NULL,y=NULL, 
         fill='População de Fortalesa',
         title='Mapa Urbano')+
  annotation_scale()+
  annotation_north_arrow(location='tr', 
                         style = north_arrow_orienteering())+
  theme(legend.position= c(0.9,0.13),
        legend.key.height= unit(2,'mm'),
        legend.key.width = unit(3,'mm'))

ggsave(filename = 'Mapa_Urbano_bel.png',
       height = 15,
       width = 15,
       units = 'cm',
       scale = 0.94)
###############################################################################









