#script básico
#programas para análise estatística
library(tidyverse)
library(ggplot2)
library(cowplot)
library(patchwork)
library (tidyverse)
library (magrittr)
library(pacman)
library(ggsignif)
library(readr)

## seleciona o diretório de trabalho/salvamento
 setwd("F:/analises doutorado")

## eliminação dos controles para analise de correlação
## Filter (pacote tydiverse): seleciona sub grupo para analises
so_tak<-(banco_tak)%>%
  filter(grupo=="tak")


## graficos de dispersão

#geom_smooth: faz a linha da regressão; é necessário colocar a "method=lm"
#geom_texto: insere texto dentro do gráfico; aes(estética): determina a coordenada da frase usando referêncial cartesiano.

d1<- ggplot(data = so_tak, aes (y = fss_score, x = mfis_score)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "MFIS score (0-80)") +
  geom_text(aes(y= 70, x = 0, label= "rho = 0.691 P = < 0.0001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()
 
d2<- ggplot(data = so_tak, aes (y = fss_score, x = mfis_dfis)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "MFIS- Physical domain (0-36)") +
  geom_text(aes(y= 70, x = 0, label= "rho = 0.714 P = < 0.0001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()
d3<-ggplot(data = so_tak, aes (y = fss_score, x = mfis_dcog)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "MFIS- Cognitive domain (0-40)") +                                                                                                                                       
  geom_text(aes(y= 70, x = 0, label= "rho = 0.547  P = < 0.0001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()
d4<-ggplot(data = so_tak, aes (y = fss_score, x = mfis_dpsi)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "MFIS- Psychosocial domain (0-8)") +
  geom_text(aes(y= 70, x = 0, label= "rho = 0.485 P = < 0.001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()
d5<-ggplot(data = so_tak, aes (y = fss_score, x = vas_fad)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "VASf (0-10)") +
  geom_text(aes(y= 70, x = 0, label= "rho = 0.587 P = < 0.0001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

# gráficos relacionados a doença (FSS)

d6<-ggplot(data = so_tak, aes (y = fss_score, x = tempo_doenca)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "Disease duration (years)") +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.294 P = 0.033"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d7<- ggplot(data = so_tak, aes (y = fss_score, x =haq_score)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "HAQ score (0-3)") +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.578  P = < 0.0001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d8<-ggplot(data = so_tak, aes (y = fss_score, x =ipac_mets)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "MET (min/week)") +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.203 P = 0.144"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d9<-ggplot(data = so_tak, aes (y = fss_score, x =pcr)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "C-reactive protein (mg/L)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.304 P = 0.027"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d10<-ggplot(data = so_tak, aes (y = fss_score, x =vhs)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = "ESR (mm/1st hour)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.146 P = 0.298"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d11<-ggplot(data = so_tak, aes (y = fss_score, x =pred_dia_mg)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "FSS score (0-63)", x = " Prednisone (mg/day)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.273 P = 0.048"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

#graficos realcionados a doença (MFIS)

d12<-ggplot(data = so_tak, aes (y = mfis_score, x = tempo_doenca)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "Disease duration (years)") +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.078 P = 0.600"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d13<- ggplot(data = so_tak, aes (y = mfis_score, x =haq_score)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "HAQ score (0-3)") +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.481 P = < 0.001"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d14<-ggplot(data = so_tak, aes (y = mfis_score, x =ipac_mets)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "MET (min/week)") +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.085 P = 0.544"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d15<-ggplot(data = so_tak, aes (y = mfis_score, x = pcr)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "C-reactive protein (mg/L)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.005 P = 0.972"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d16<-ggplot(data = so_tak, aes (y = mfis_score, x =vhs)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "ESR (mm/1st hour)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = -0.074 P = 0.600"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

d17<-ggplot(data = so_tak, aes (y = mfis_score, x =pred_dia_mg)) +
  geom_point(size=1.5, color="black")+
  geom_smooth(method=lm, color="grey30")+
  labs(y = "MFIS score (0-84)", x = "Prednisone (mg/day)" ) +
  geom_text(aes(y= 80, x = 0, label= "rho = 0.319 P = 0.020"), hjust=0, vjust=1, size=4, color="grey30")+
  theme_classic()

## criacão de objeto para plotagem (plot_grid)
# salvar em tiff (ggsave)

P_mfis_fss_g <-plot_grid(d1, d2, d3, d4,d5, nrow=2, ncol=3,labels=c("A", "B","C","D","E"))
ggsave ("figura 2.tiff", width =9, height=6.5, dpi=600)
Tak <-plot_grid(d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17, nrow=3, ncol=4,labels=c("A", "B","C","D","E", "F", "G", "H", "I", "J", "K", "L"))
ggsave ("figura 3.tiff", width =12, height=10, dpi=600)

