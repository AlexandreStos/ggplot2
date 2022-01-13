# bibliotecas
library(ggplot2)
library(cowplot)
library(patchwork)
library (tidyverse)
library (magrittr)
library(pacman)
library(ggsignif)
library(readr)

# Escolha do diretório
setwd("F:/analises doutorado")

# Escolha do banco
readxl::read_excel("banco_tak.xlsx")
banco_tak <- readxl::read_excel("banco_tak.xlsx") 


##Alteração de nome de variavel para gráficos

banco_tak$grupo  <- factor (banco_tak$grupo,
                            
                            labels = c("Control", "Takayasu"),
                            
                            levels = c("controle_tak","tak"))



######################################### 

# Graficos 

g1 <- ggplot(data = banco_tak, aes (y = fss_score, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7,  fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = 0.018")) +
  labs(y = "FSS score (0-63)", x = "") +
  theme_classic()
g2 <- ggplot(data = banco_tak, aes (y = vas_fad, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7, fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = 0.362")) +
  labs(y = "VASf (0-10)", x = "") +
  theme_classic()
g3 <- ggplot(data = banco_tak, aes (y = mfis_score, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7, fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = 0.001")) +
  labs(y = " MFIS score (0-84)", x = "") +
  theme_classic()
g4 <- ggplot(data = banco_tak, aes (y = mfis_dfis, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7, fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = < 0.001")) +
  labs(y = " MFIS- Physical domain (0-36)", x = "") +
  theme_classic()
g5 <- ggplot(data = banco_tak, aes (y = mfis_dcog, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7, fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = 0.177")) +
  labs(y = " MFIS- Cognitive domain (0-40)", x = "") +
  theme_classic()

g6 <- ggplot(data = banco_tak, aes (y = mfis_dpsi, x = grupo)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.5, height= 0.7, fill = "grey90",
               outlier.shape = 0.5, outlier.size = 1, show.legend = FALSE) +
  geom_signif(comparisons = list (c("Takayasu",
                                    "Control")),
              tip_length = 0.03,
              textsize = 3.5,
              annotations = c(" P = 0.004")) +
  labs(y = " MFIS- Psychosocial domain (0-40)", x = "") +
  theme_classic()

 

#plotagem em conjunto: instalar library(cowplot) e library(patchwork)
#nrow= nº de linhas; ncol= nº colunas
#labels: letras no gráfico



plot_grid(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3, labels=c("A","B","C","D","E","F"))


# para salvar em tiff ( publicação)
# não esquecer de salvar o nome com a extensão ".tiff"
# Dimensões depois do nome da figura, dpi: ver na revista 

ggsave ("figura teste.tiff", width = 12, height= 10, dpi=600)





