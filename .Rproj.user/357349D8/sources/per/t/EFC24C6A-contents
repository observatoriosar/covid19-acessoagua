#===================================
# OSAR
# observatoriosar.wordpress.com
#
# github.com/claudioalvesmonteiro
#===================================

# carregar pacotes
pacotes = c("ggrepel","readr","readxl", "stringr", 
             "dplyr", "viridis", "maps", "raster",
             "ggmap", "ggrepel", "sp", "maptools",
             'ggplot2',  "stringi" )
lapply(pacotes, library, character.only = T)
source('codes/mapas_source.R')

# carregar dados
snis = read.csv('data/SNIS.csv')
pop = read.csv('data/popidade_2015.txt', sep=';')
shape_pe = shapefile("data/pe_municipios/26MUE250GC_SIR.shp")
renda <- read_excel("data/tabela3279.xlsx")

#=============================
# preprocessamento
#=============================

# remover virgula e transformar em numerico
snis$duracao_paralisacoes = str_replace(snis$IN072...Duração.média.das.paralisações, ',','.')
snis$duracao_paralisacoes = as.numeric(snis$duracao_paralisacoes )
snis$code = snis$Código.do.Município

# selecionar casos com dados
snis = snis[!is.na(snis$duracao_paralisacoes),]

# remover ultimo digito para combinacao
shape_pe@data$code = substr(shape_pe@data$CD_GEOCMU,1,nchar(shape_pe@data$CD_GEOCMU)-1)

# criar taxa pop maior que 65 anos
pop = mutate(pop, pop_65oumais = X65.a.69.anos + X70.a.74.anos+ X75.a.79.anos+ X80.anos.ou.mais)
pop = mutate(pop, taxa_65oumais = round(( pop_65oumais /Total)*100,3) )

# selecionar codigo do municipio e remover do nome
pop$code = substr(pop$Município, 1, 6)
pop$nome = substr(pop$Município, 8, nchar(as.character(pop$Município)))

# selecionar pop de recife
pop = pop[str_starts(pop$code, '26'), ]

# padronizar nome dos municipios para renda
renda$NM_MUNICIP = stri_trans_general(renda$Local, 'Latin-ASCII')%>%
                    str_replace('[(]', '')%>%
                    str_replace('[)]', '')%>%
                    str_replace(' PE', '')%>%
                    toupper()
shape_pe$NM_MUNICIP = stri_trans_general(shape_pe$NM_MUNICIP, 'Latin-ASCII')

# criar log da renda
renda$renda_log = log(renda$`Valor do rendimento nominal médio mensal das pessoas de 10 anos ou mais de idade (Reais)`)

#==================
# visualizacao
#==================

# funcao para criar visualizacao em mapa e barras
mapAndBar = function(shape, data, variable, namevariable, legendname, savename){
  
  #--- MAPA
  mapa.funcao(shape, data, variable, '', legendname, savename)
  ggsave(paste0('resultados/',savename,"_MAPA.png"), width = 6, height = 6, units = "in")
  
  # ---BARRA
  # ordenar
  namevariable = factor(namevariable, levels = unique(namevariable[order(variable)]) )
  
  ggplot(data = data, aes(x = namevariable , y = variable))+
    geom_bar(stat = 'identity', fill = "#5c4963")+
    geom_text(aes(label=round(variable,2)),  size = 3.5, vjust = 0.4, hjust=-0.08)+
    labs(x = "", y = legendname)+
    tema_massa()+
    theme(axis.text.y = element_text(size=9,face="plain"))+
    coord_flip()
  ggsave(paste0('resultados/',savename,"_BARRA.png"), width = 10, height = 14, units = "in")
  
}

mapAndBar(shape_pe, snis, snis$duracao_paralisacoes, snis$Município, 'Duração Média das\nParalizações (horas)', 'horas_paralizacoes')
mapAndBar(shape_pe, pop, pop$taxa_65oumais, pop$nome, '% de Idosos', 'prop_idosos')
mapAndBar(shape_pe, pop, log(pop$pop_65oumais), pop$nome, 'Número de Idosos', 'log_pop_idosos')
mapAndBar(shape_pe, pop, pop$pop_65oumais, pop$nome, 'Número de Idosos', 'pop_idosos')

renda = renda[2:186,] # remover linhas sem dados
mapAndBar(shape_pe, renda, renda$`Valor do rendimento nominal médio mensal das pessoas de 10 anos ou mais de idade (Reais)`, renda$NM_MUNICIP, 'Renda Média', 'renda')
mapAndBar(shape_pe, renda, renda$renda_log, renda$NM_MUNICIP, 'Logaritmo da\nRenda Média', 'rendalog')

#========================
# REGIAO METROPOLITANA
#========================

df = merge(shape_pe, snis, by='code')
df = df[df$NM_MUNICIP == 'RECIFE' |
          df$NM_MUNICIP == 'OLINDA' |
          df$NM_MUNICIP == 'ABREU E LIMA' |
          df$NM_MUNICIP == 'ARACOIABA' |
          df$NM_MUNICIP == 'CABO DE SANTO AGOSTINHO' |
          df$NM_MUNICIP == 'CAMARAGIBE' |
          df$NM_MUNICIP == 'GOIANA' |
          df$NM_MUNICIP == 'IGARASSU' |
          df$NM_MUNICIP == 'ILHA DE ITAMARACA' |
          df$NM_MUNICIP == 'IPOJUCA' |
          df$NM_MUNICIP == 'ITAPISSUMA' |
          df$NM_MUNICIP == 'JABOATAO DOS GUARARAPES' |
          df$NM_MUNICIP == 'MORENO' |
          df$NM_MUNICIP == 'PAULISTA' |
          df$NM_MUNICIP == 'SAO LOURENCO DA MATA' 
          ,]

mapAndBar(df, snis, snis$duracao_paralisacoes2, snis$code, 'Duração Média das\nParalizações (horas)', 'metro_paralizacao')


#========================
# MATRIX DE CORRELACAO
#========================

# renomear nome em renda
pop$NM_MUNICIP =  stri_trans_general(pop$nome , 'Latin-ASCII')%>%toupper()

# combinar
data = merge(pop[,c('code', 'NM_MUNICIP', 'pop_65oumais', 'taxa_65oumais')], snis[,c('code', "duracao_paralisacoes2")], by='code')
data = merge(data, renda[,c(3:5)], by='NM_MUNICIP')
colnames(data)[6] = 'renda_media'

# funcao para normalizacao
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# aplicar normalizacao nas variaveis numericas
data[,c(3:7)] = data.frame(sapply(data[,c(3:7)], function(x) range01(x)))

# executar e visualizar matrix de correlacao
mydatacor = cor(data[,c(3:7)])
library(corrplot)
corrplot(mydatacor)



