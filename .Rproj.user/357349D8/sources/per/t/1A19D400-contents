#=================================
# OSAR
# FUNCOES PARA VISUALIZACAO
#
# claudioalvesmonteiro
#================================


# Tema para Graficos
tema_massa <- function (base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(size=12,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"))
}

#==============================#
# importar dados

# limpar string
cleanStr <- function(string){
  library(stringi); library(stringr);
  string = stri_trans_general(string, "latin-ascii")
  string = toupper(string)
  return(string)
}

# GRAFICO DE BARRA
barPLOT <- function(data, nominal, variable, nome, nome2){
  
  # ordenar
  nominal = factor(nominal, levels=nominal[order(variable)])
  
  # plotar
  p = ggplot(data = data, aes(x = nominal , y = variable))+
    geom_bar(stat = 'identity', fill = "#5c4963")+
    geom_text(aes(label=variable),  size = 3.8, vjust = 0.4, hjust=-0.08)+
    labs(x = "", y = paste(nome2))+
    tema_massa()+
    coord_flip()
  
  # salvar
  ggsave(paste0(nome, "_BAIRRO.png"), p, path = "Indicador/versao2/resultados", width = 9, height = 12, units = "in")
  
  return(p)
  
}



#===== Funcao p/ Mapa =====#
mapa.funcao <- function(shape, data, variable, maintitle, legendtitle, savename) { 
  library(stringi); library(ggplot2)
  
  data$variavel = variable
  
  # merge data with shapefile
  if (savename == 'renda'| savename == 'rendalog'){
    shp_data <- merge(shape, data, by = "NM_MUNICIP", all = T)
    
    }  else {
      shp_data <- merge(shape, data, by = "code", all = T)
  }
  
  # definir labels no mapa (3 maiores, 3 menores)
  #shp_data$variavel[is.na(shp_data$variavel)] = 0
  shp_data = shp_data[order(shp_data$variavel, decreasing = T),]
  shp_data$bairros_detasq = 1
  shp_data$bairros_detasq[1:7] = ""
  #shp_data$bairros_detasq[c(length(shp_data)-3):c(length(shp_data))] = ""
  shp_data$bairros_detasq = with(shp_data, paste0(shp_data$bairros_detasq, shp_data$NM_MUNICIP))
  shp_data$bairros_detasq_cod = grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] = ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity = fortify(shp_data, region = "NM_MUNICIP")
  localidade = shp_data@data$NM_MUNICIP
  # extrair centroides dos poligonos
  centroids.df = as.data.frame(coordinates(shp_data))
  names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades
  # base para plotagem
  variavel = shp_data@data$variavel
  nomes_centroides = shp_data$bairros_detasq
  map_dataframe = data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot = ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = shp_data$variavel),colour = grey(0.96),  map = data_fortity, size=0.2) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) +
    scale_fill_viridis(name = legendtitle, option = 'D') +
    # scale_fill_gradient(name = legendtitle, low="lightgreen", high= "darkblue")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 2.7, color = "black") +
    labs(title = maintitle, x = "Longitude", y = "Latitude")+
    coord_fixed(1) +
    theme_nothing(legend = T)+
    theme(#legend.position="bottom",
      #legend.key.size = unit(0.7, "cm"),
      #legend.text = element_text(size = 14, hjust = 3, vjust = 3)
      #legend.title = element_text(size = 15, face = "plain"),
      #title = element_text(size = 15, face = "bold")
    )
  
  return(plot)
}
