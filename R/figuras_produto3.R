######################################################################
####################### Script - Figuras do produto 3 - 2021 #########
########### V.1.0 - Desenvolvido por Mikael Lemos ####################
######################################################################

# Bibliotecas

#install.packages('gtools')
library(gtools)

#install.packages('dplyr')
library(dplyr)

#install.packages("tidyr")
library(tidyr)

#install.packages("data.table")
library(data.table)

#install.packages('stringr')
library(stringr)

#install.packages('Amelia')
library(Amelia)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("lubridate")
library(lubridate)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("read.dbc")
library(read.dbc)

#install.packages("foreign")
library(foreign)

#install.packages("openxlsx")
library(openxlsx)

#install.packages("forcats")
library(forcats)

#install.packages("readr")
library(readr)

#install.packages("fs")
library(fs)

#install.packages("stringi")
library(stringi)

#install.packages('Rcpp')
library(Rcpp)

#install_formats()
#install.packages('rio')
library(rio)

library(RColorBrewer)

install.packages('ggmap')
library(ggmap) 

# Carregando tabelas de indicadores por tipo de hepatite

lista_tbls <- import_list("C:/Users/lemos/Downloads/AMA/produtos_opas/2021/produto3/tabela_produto_3 _FIGURAS.xlsx", setclass = "tbl", rbind = TRUE)

# tabela/figura indicadores por tipo e ano

tbl_figura1 <- lista_tbls %>% group_by(lista_tbls$`Tipo de hepatite`, lista_tbls$`Início da apuração`)
tbl_figura1_n <- tbl_figura1 %>% summarise(n = n())

tbl_figura1_n <- select(tbl_figura1_n, hepatite = "lista_tbls$`Tipo de hepatite`", ano = "lista_tbls$`Início da apuração`", n )

# Figura 1 - tipo de hepatite , ano , n

ggplot(data=tbl_figura1_n, aes(x=ano, y=n, fill=hepatite))  +  
  geom_bar(stat="identity", position= position_dodge2(width = 0.9, preserve = "single"))+ 
  geom_text(aes(label=n), vjust=-0.3, position =  position_dodge2(width = 0.9, preserve = "single"), size=4) + 
  facet_grid(~ano, scales = "free_x", space = "free_x", switch = "x") +
  theme_minimal() +  scale_x_continuous(name ="",breaks= tbl_figura1_n$ano) +
  scale_y_continuous(name="Frequência") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),legend.position = "bottom")  + scale_fill_discrete(name = "Indicadores")

# tabela/figura indicadores por ano

tbl_figura2_n <- table(lista_tbls$`Início da apuração`)
tbl_figura2_n <- as.data.frame(tbl_figura2_n)

# Figura 2 - indicador por ano

ggplot(data=tbl_figura2_n, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",width=0.2, fill="#5b9ad5")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+ xlab("") + ylab("Frequência")+
  theme_minimal()

# tabela/figura indicadores por origem de dados 

lista_tbls$`Origem dos dados do indicador`[lista_tbls$`Origem dos dados do indicador` == "Coordenações estaduais de saúde e as coordenações dos programas estaduais de hepatite"] <- "Coord. de saúde e programas estaduais de hepatite"
lista_tbls$`Origem dos dados do indicador`[lista_tbls$`Origem dos dados do indicador` == "Planilha de tratamentos e medicamentos das hepatites virais - Área de logística - MS/SVS/DCCI"] <- "Planilha de tratamentos e medicamentos-Área de logística"

tbl_figura3_n <- table(lista_tbls$`Origem dos dados do indicador`)
tbl_figura3_n <- as.data.frame(tbl_figura3_n)

# Figura 3 - indicador por origem de dados 

ggplot(data=tbl_figura3_n, aes(x=reorder(Var1, +Freq), y=Freq)) +
  geom_bar(stat="identity",width=0.5, fill="#5b9ad5")+
  geom_text(aes(label=Freq), vjust=+0.3,hjust=-0.2, size=3.0)+ xlab("Fontes de dados") + ylab("Frequência")+
  theme_minimal() +
  coord_flip()


# tabela/figura indicadores por área responsável

tbl_figura4_n <- table(lista_tbls$`Área responsável pelo monitoramento do indicador`)
tbl_figura4_n <- as.data.frame(tbl_figura4_n)

# Figura 4 - indicador por área responsável

ggplot(data=tbl_figura4_n, aes(x=reorder(Var1, +Freq), y=Freq)) +
  geom_bar(stat="identity",width=0.4, fill="#5b9ad5")+
  geom_text(aes(label=Freq), vjust=+0.3,hjust=-0.2, size=3.0)+ xlab("Áreas responsáveis") + ylab("Frequência")+
  theme_minimal() +
  coord_flip()

# tabela/figura indicadores por periodicidade

tbl_figura5_n <- table(lista_tbls$Periodicidade)
tbl_figura5_n <- as.data.frame(tbl_figura5_n)

# Figura 5 - indicador por ano

ggplot(data=tbl_figura5_n, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity",width=0.1, fill="#5b9ad5")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+ xlab("Periodicidade") + ylab("Frequência")+
  theme_minimal()


# Figura 6 - indicadores por hepatite , ano, porcentagem

ggplot(tbl_figura1, aes(x=as.factor(tbl_figura1$`Início da apuração`), fill=as.factor(tbl_figura1$`Tipo de hepatite`)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), width=0.5, position="stack" , size=4)  + theme_minimal()+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(accuracy = 1,..count../tapply(..count.., ..x.. ,sum)[..x..]) ),stat="count",colour = "black", position = position_stack(vjust = .5)) +
  ylab('') + scale_y_continuous(labels = scales::percent) + theme(legend.text = element_text(colour="black", size=11) ,axis.text.y =element_text(size=11), axis.text.x=element_text(vjust=7,size=11) ,axis.title.x = element_blank(),
                                                                  axis.title.y = element_text(size = 11), legend.position = "bottom")  + scale_fill_discrete(name = "Indicadores")

# Figura 7 - indicadores por hepatite , ano, porcentagem

tbl_figura1$`Origem dos dados do indicador`[tbl_figura1$`Origem dos dados do indicador` == "Coordenações estaduais de saúde e as coordenações dos programas estaduais de hepatite"] <- "Coord. de saúde e programas estaduais de hepatite"
tbl_figura1$`Origem dos dados do indicador`[tbl_figura1$`Origem dos dados do indicador` == "Planilha de tratamentos e medicamentos das hepatites virais - Área de logística - MS/SVS/DCCI"] <- "Planilha de tratamentos e medicamentos-Área de logística"

ggplot(tbl_figura1, aes(x=as.factor(tbl_figura1$`Área responsável pelo monitoramento do indicador`), fill=as.factor(tbl_figura1$`Origem dos dados do indicador`)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), width=0.35, position="stack" , size=7)  + theme_minimal()+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(accuracy = 1,..count../tapply(..count.., ..x.. ,sum)[..x..]) ),stat="count",colour = "black", position = position_stack(vjust = .5)) +
  ylab('') + scale_y_continuous(labels = scales::percent) + theme(legend.text = element_text(colour="black", size=12) ,axis.text.y =element_text(size=12), axis.text.x=element_text(vjust=7,size=12) ,
                                                                  axis.title.y = element_text(size = 12), legend.position = "bottom")  + scale_fill_discrete(name = "Fonte dos dados")+  coord_flip() + xlab("Indicadores por áreas-MS") + ylab("Porcentagem - fonte de dados")
# Figura 8 - pie chart - periodicidade dos indicadores

periodicidade <- data.frame(col1 = c(57,12),
                         col2 = c("Anual", "Quadrimestral")) %>%
  mutate(col2 = factor(col2),
        
         cf = cumsum(col1),
         mid = cf - col1 / 2,
         label = paste0(col2, " ", round(col1 / sum(col1) * 100, 0), "%"))


            
pie(periodicidade$col1 , labels = c("Anual-83%", "Quadrimestral-17%"), border="white", col=myPalette )

# Figura 9 - pie chart - pct indicadores por ano

ind_ano <- data.frame(col1 = c(24,34,4,7),
                            col2 = c("2010", "2019", "2020", "2021")) %>%
  mutate(col2 = factor(col2),
         
         cf = cumsum(col1),
         mid = cf - col1 / 2,
         label = paste0(col2, " ", round(col1 / sum(col1) * 100, 0), "%"))



pie(ind_ano$col1 , labels = c("2010-35%", "2019-49%", "2020-6%", "2021-10%"), border="white", col=myPalette )


