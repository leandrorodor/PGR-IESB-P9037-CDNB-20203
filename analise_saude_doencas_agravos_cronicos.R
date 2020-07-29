### Ambiente ##############################################################
getwd()
setwd("/Volumes/IESB/ciencia_de_dados/P9037-CDNB-20203/trabalho_grupo")
save.image("ENV_P9037-CDNB-20203.Rdata")

### Bibliotecas ###########################################################
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
#suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(microbenchmark)
library(formattable)
library(forcats)
library(gmodels)

### Dicionário de dados ###################################################
dic_dados_SIM <- read_delim("/Volumes/IESB/ciencia_de_dados/P9127-CDNB-20203/projeto_pratico/dic_dados_SIM.csv", 
                            "|", escape_double = FALSE, trim_ws = TRUE)
View(dic_dados_SIM)
read_tsv("./ETLSIM.DORES.csv",n_max=1)

### Base de dados 2003/2004 ###############################################
ETLSIM_DORES <- read_delim("/Volumes/IESB/ciencia_de_dados/P9127-CDNB-20203/projeto_pratico/ETLSIM.DORES.csv", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
View(ETLSIM_DORES)

ETLSIM_DORES_2004 <- read_delim("/Volumes/IESB/ciencia_de_dados/P9127-CDNB-20203/projeto_pratico/ETLSIM.DORES_2004.csv", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
View(ETLSIM_DORES)
### Variáveis de estudo SIM 2014 ##############################################
selecao_var_SIM_2014 <- ETLSIM_DORES_2014 %>% 
  select(
    ano_obito,
    def_sexo,
    data_obito,
    def_loc_ocor,
    CAUSABAS,
    def_circ_obito,
    ocor_SIGLA_UF,
  )
print(selecao_var_SIM_2014)

### Variáveis de estudo SIM 2015 ##############################################
selecao_var_SIM_2015 <- ETLSIM_DORES_2015 %>% 
  select(
    ano_obito,
    def_sexo,
    data_obito,
    def_loc_ocor,
    CAUSABAS,
    def_circ_obito,
    ocor_SIGLA_UF,
  )
print(selecao_var_SIM_2015)

### Variáveis de estudo SIM 2016 ##############################################
selecao_var_SIM_2016 <- ETLSIM_DORES_2016 %>% 
  select(
    ano_obito,
    def_sexo,
    data_obito,
    def_loc_ocor,
    CAUSABAS,
    def_circ_obito,
    ocor_SIGLA_UF,
  )
print(selecao_var_SIM_2016)
### Variáveis de estudo SINASC 2014 ##############################################
selecao_var_SINASC_2014 <- ETLSINASC_DNRES_2014 %>% 
  select(
    LOCNASC,
    def_loc_nasc,
    codanomal_capitulo,
    DTNASC,
    data_nasc,
    ano_nasc,
    SEXO,
    def_sexo,
    def_raca_cor,
    IDANOMAL,
    def_anomalia,
    nasc_SIGLA_UF,
    codanomal_capitulo,
    codanomal_grupo,
    codanomal_categoria,
    codanomal_subcategoria,
  )
print(selecao_var_SINASC_2014)

### Variáveis de estudo SINASC 2015 ##############################################
selecao_var_SINASC_2015 <- ETLSINASC_DNRES_2015 %>% 
  select(
    LOCNASC,
    def_loc_nasc,
    codanomal_capitulo,
    DTNASC,
    data_nasc,
    ano_nasc,
    SEXO,
    def_sexo,
    def_raca_cor,
    IDANOMAL,
    def_anomalia,
    nasc_SIGLA_UF,
    codanomal_capitulo,
    codanomal_grupo,
    codanomal_categoria,
    codanomal_subcategoria,
  )
print(selecao_var_SINASC_2015)

### Variáveis de estudo SINASC 2016 ##############################################
selecao_var_SINASC_2016 <- ETLSINASC_DNRES_2016 %>% 
  select(
    LOCNASC,
    def_loc_nasc,
    codanomal_capitulo,
    DTNASC,
    data_nasc,
    ano_nasc,
    SEXO,
    def_sexo,
    def_raca_cor,
    IDANOMAL,
    def_anomalia,
    nasc_SIGLA_UF,
    codanomal_capitulo,
    codanomal_grupo,
    codanomal_categoria,
    codanomal_subcategoria,
  )
print(selecao_var_SINASC_2016)

### Normatização correção ##########################################################

## Remover linhas repetidas
selecao_var_SIM<-distinct(selecao_var_SIM)

## Remover ocorrências com campos vazios
selecao_var_SIM<-drop_na(selecao_var_SIM)

# Remover linhas scontendo "Ignorado" para a variável def_sexo
#for(i in 1:nrow(selecao_var_SIM)){
#  if(selecao_var_SIM[i,2] == "Ignorado")
#    selecao_var_SIM <- selecao_var_SIM[-i, ]
#}

## Eliminação de gênero (Ignorado)
## Filtragem por tipo de óbito (Suicídio)
## Simplificação de CID
## Filtragem UF (DF)

df.variaveis<-selecao_var_SIM%>%
  select(ano_obito,idade_obito_calculado,def_sexo,def_raca_cor,def_escol,def_circ_obito,ocor_SIGLA_UF,CAUSABAS)%>%
  mutate(cid_simplificado= str_sub(selecao_var_SIM$CAUSABAS, end = -2))%>%
  mutate(def_cid_simplificado = recode(cid_simplificado,
                                       "X61" = "Drogas Anticonvulsivantes, Sedativos, Hipnóticos, Antiparkinsonianos e Psicotrópicos",
                                       "X62" = "Narcóticos e Psicodislépticos (alucinógenos)",
                                       "X64" = "Drogas, Medicamentos e Substâncias Biológicas",
                                       "X68" = "Pesticidas",
                                       "X65" = "Auto-intoxicação Voluntária Por Álcool",
                                       "X69" = "Produtos Químicos e Substâncias Nocivas Não Especificadas",
                                       "X70" = "Enforcamento, Estrangulamento e Sufocação",
                                       "X71" = "Afogamento e Submersão",
                                       "X72" = "Disparo de Arma de Fogo de Mão",
                                       "X74" = "Disparo de Outra Arma de Fogo e de Arma de Fogo Não Especificada",
                                       "X76" = "Lesão Autoprovocada Intencionalmente Pela Fumaça, Pelo Fogo e Por Chamas",
                                       "X75" = "Lesão Autoprovocada Por Dispositivo Explosivo",
                                       "X78" = "Objeto Cortante ou Penetrante",
                                       "X80" = "Precipitação de um Lugar Elevado",
                                       "X81" = "Precipitação ou Permanência Diante de um Objeto em Movimento",
                                       "X83" = "Lesão Provocada Por Outros Meios Especificados",
                                       "X84" = "Lesão Autoprovocada Por Meios Não Especificados",)) %>%    
  filter(def_circ_obito %in%'Suicídio') %>%
  filter(def_sexo %in%c('Masculino','Feminino')) %>%
  filter(ocor_SIGLA_UF %in%c('DF')) %>%  
  group_by(ano_obito,idade_obito_calculado,def_sexo,def_raca_cor,def_escol,def_circ_obito,ocor_SIGLA_UF,cid_simplificado,def_cid_simplificado) %>%
  ungroup()
count()
df.variaveis


### Frequências - óbito por suicídio x CID simplificada ##############
## Tabela de frequências absolutas
freq_abs_cid <-table(df.variaveis$def_cid_simplificado)
freq_abs_cid
## Tabela de frequências relativas
freq_rel_cid <-prop.table(freq_abs_cid)
freq_rel_cid
## Tabela porcentagem
freq_porc_cid <- 100 *prop.table(freq_abs_cid)
freq_porc_cid
## Totais
freq_abs_cid <-c(freq_abs_cid, sum(freq_abs_cid))
freq_rel_cid <-c(freq_rel_cid, sum(freq_rel_cid))
freq_porc_cid <-c(freq_porc_cid,sum(freq_porc_cid))
names(freq_abs_cid)[17] <-"Total"
## Tabela frequencias SEXO

freq_cid <- cbind(freq_abs_cid,
                  freq_rel_cid=round(freq_rel_cid, digits=2),
                  freq_porc_cid=round(freq_porc_cid, digits = 2))

freq_cid

pdf("freq_cid.pdf", height=11, width=15)
grid.table(freq_cid)
dev.off()

## Criando um data.frame a partir de uma tabela
freq_cid<-as.data.frame.matrix(freq_cid)

### Renomeando cabeçalho
colnames(freq_cid)
names(freq_cid)[names(freq_cid) == "freq_abs_cid"] <- "Frequência absoluta"
names(freq_cid)[names(freq_cid) == "freq_rel_cid"] <- "Frequência relativa"
names(freq_cid)[names(freq_cid) == "freq_porc_cid"] <- "Percentual"

glimpse(freq_cid)

## Tabela formatada usando "formattable"
formattable(freq_cid, 
            align = c("l",rep("r", NCOL(freq_cid) - 1)),
            list(
              `Frequência absoluta` = color_tile("#fff0da", "#ffa500"),
              `Frequência relativa` = color_bar("#87cefa"),
              `Percentual` = color_tile("#f6dbd6", "#c98e85")))

### Frequências - óbito por suicídio x sexo ############################
## Tabela de frequências absolutas
freq_abs_sexo <-table(df.variaveis$def_sexo)
freq_abs_sexo
## Tabela de frequências relativas
freq_rel_sexo <-prop.table(freq_abs_sexo)
freq_rel_sexo
## Tabela porcentagem
freq_porc_sexo <- 100 *prop.table(freq_abs_sexo)
freq_porc_sexo
## Totais
freq_abs_sexo <-c(freq_abs_sexo, sum(freq_abs_sexo))
freq_rel_sexo <-c(freq_rel_sexo, sum(freq_rel_sexo))
freq_porc_sexo <-c(freq_porc_sexo,sum(freq_porc_sexo))
names(freq_abs_sexo)[3] <-"Total"


## Tabela frequencias SEXO
freq_sexo <- cbind(freq_abs_sexo,
                   freq_rel_sexo=round(freq_rel_sexo, digits=2),
                   freq_porc_sexo=round(freq_porc_sexo, digits = 2))

pdf("freq_sexo.pdf", height=11, width=15)
grid.table(freq_sexo)
dev.off()

## Criando um data.frame a partir de uma tabela
freq_sexo<-as.data.frame.matrix(freq_sexo)

## Renomeando cabeçalho
colnames(freq_sexo)
names(freq_sexo)[names(freq_sexo) == "freq_abs_sexo"] <- "Frequência absoluta"
names(freq_sexo)[names(freq_sexo) == "freq_rel_sexo"] <- "Frequência relativa"
names(freq_sexo)[names(freq_sexo) == "freq_porc_sexo"] <- "Percentual"

glimpse(freq_sexo)

## Tabela formatada usando "formattable"
formattable(freq_sexo, 
            align = c("l",rep("r", NCOL(freq_sexo) - 1)),
            list(
              `Frequência absoluta` = color_tile("#fff0da", "#ffa500"),
              `Frequência relativa` = color_bar("#9ad4b3"),
              # `Frequência relativa` = color_bar("#87cefa"),
              `Percentual` = color_tile("#f6dbd6", "#c98e85")))

### Frequências - óbito por suicídio x raça ##################
## Tabela de frequências absolutas
freq_abs_raca_cor <-table(df.variaveis$def_raca_cor)
freq_abs_raca_cor

## Tabela de frequências relativas
freq_rel_raca_cor <-prop.table(freq_abs_raca_cor)
freq_rel_raca_cor

## Tabela porcentagem
freq_porc_raca_cor <- 100 *prop.table(freq_abs_raca_cor)
freq_porc_raca_cor

## Totais
freq_abs_raca_cor <-c(freq_abs_raca_cor, sum(freq_abs_raca_cor))
freq_rel_raca_cor <-c(freq_rel_raca_cor, sum(freq_rel_raca_cor))
freq_porc_raca_cor <-c(freq_porc_raca_cor,sum(freq_porc_raca_cor))
names(freq_abs_raca_cor)[5] <-"Total"

## Tabela frequencias ESCOLARIDADE
freq_raca_cor <- cbind(freq_abs_raca_cor,
                  freq_rel_raca_cor=round(freq_rel_raca_cor, digits=2),
                  freq_porc_raca_cor=round(freq_porc_raca_cor, digits = 2))

freq_raca_cor
pdf("freq_raca_cor.pdf", height=11, width=15)
grid.table(freq_raca_cor)
dev.off()

## Criando um data.frame a partir de uma tabela
freq_raca_cor<-as.data.frame.matrix(freq_raca_cor)

## Renomeando cabeçalho
colnames(freq_raca_cor)
names(freq_raca_cor)[names(freq_raca_cor) == "freq_abs_raca_cor"] <- "Frequência absoluta"
names(freq_raca_cor)[names(freq_raca_cor) == "freq_rel_raca_cor"] <- "Frequência relativa"
names(freq_raca_cor)[names(freq_raca_cor) == "freq_porc_raca_cor"] <- "Percentual"

glimpse(freq_raca_cor)

## Tabela formatada usando "formattable"
formattable(freq_raca_cor, 
            align = c("l",rep("r", NCOL(freq_raca_cor) - 1)),
            list(
              `Frequência absoluta` = color_tile("#fff0da", "#ffa500"),
              `Frequência relativa` = color_bar("#87cefa"),
              `Percentual` = color_tile("#f6dbd6", "#c98e85")))

### Frequências - óbito por suicídio x faixa etária ##################
## Tabela de frequências absolutas
df.variaveis<-df.variaveis%>%
  select(ano_obito,idade_obito_calculado,def_sexo,def_raca_cor,def_escol,def_circ_obito,ocor_SIGLA_UF,CAUSABAS,cid_simplificado,def_cid_simplificado)%>%
  mutate(faixa_etaria=cut(c(df.variaveis$idade_obito_calculado), breaks = c(5,9,14,29,39,49,59,69,79,80,120),
                          labels = c("5-9","10-14","15-19","20-29","30-39","40-49","50-59","60-69","70-79","80 ou mais"),
                          include.lowest = TRUE))%>%
  group_by(ano_obito,idade_obito_calculado,faixa_etaria,def_sexo,def_raca_cor,def_escol,def_circ_obito,ocor_SIGLA_UF,cid_simplificado,def_cid_simplificado) %>%
  ungroup()
count()

freq_abs_fe <-table(df.variaveis$faixa_etaria)
freq_abs_fe

## Tabela de frequências relativas
freq_rel_fe <-prop.table(freq_abs_fe)
freq_rel_fe

## Tabela porcentagem
freq_porc_fe <- 100 *prop.table(freq_abs_fe)
freq_porc_fe

## Totais
freq_abs_fe <-c(freq_abs_fe, sum(freq_abs_fe))
freq_rel_fe <-c(freq_rel_fe, sum(freq_rel_fe))
freq_porc_fe <-c(freq_porc_fe,sum(freq_porc_fe))
names(freq_abs_fe)[11] <-"Total"

## Tabela frequencias ESCOLARIDADE
freq_fe <- cbind(freq_abs_fe,
                       freq_rel_fe=round(freq_rel_fe, digits=2),
                       freq_porc_fe=round(freq_porc_fe, digits = 2))

freq_fe
pdf("freq_fe.pdf", height=11, width=15)
grid.table(freq_fe)
dev.off()

## Criando um data.frame a partir de uma tabela
freq_fe<-as.data.frame.matrix(freq_fe)

## Renomeando cabeçalho
colnames(freq_fe)
names(freq_fe)[names(freq_fe) == "freq_abs_fe"] <- "Frequência absoluta"
names(freq_fe)[names(freq_fe) == "freq_rel_fe"] <- "Frequência relativa"
names(freq_fe)[names(freq_fe) == "freq_porc_fe"] <- "Percentual"

glimpse(freq_fe)

## Tabela formatada usando "formattable"
formattable(freq_fe, 
            align = c("l",rep("r", NCOL(freq_fe) - 1)),
            list(
              `Frequência absoluta` = color_tile("#fff0da", "#ffa500"),
              `Frequência relativa` = color_bar("#87cefa"),
              `Percentual` = color_tile("#f6dbd6", "#c98e85")))

### Frequências - óbito por suicídio x escolaridade ##################
## Tabela de frequências absolutas
freq_abs_esc <-table(df.variaveis$def_escol)
freq_abs_esc

## Tabela de frequências relativas
freq_rel_esc <-prop.table(freq_abs_esc)
freq_rel_esc

## Tabela porcentagem
freq_porc_esc <- 100 *prop.table(freq_abs_esc)
freq_porc_esc

## Totais
freq_abs_esc <-c(freq_abs_esc, sum(freq_abs_esc))
freq_rel_esc <-c(freq_rel_esc, sum(freq_rel_esc))
freq_porc_esc <-c(freq_porc_esc,sum(freq_porc_esc))
names(freq_abs_esc)[7] <-"Total"

## Tabela frequencias ESCOLARIDADE
freq_esc <- cbind(freq_abs_esc,
                  freq_rel_esc=round(freq_rel_esc, digits=2),
                  freq_porc_esc=round(freq_porc_esc, digits = 2))

freq_esc
pdf("freq_esc.pdf", height=11, width=15)
grid.table(freq_esc)
dev.off()

## Criando um data.frame a partir de uma tabela
freq_esc<-as.data.frame.matrix(freq_esc)

## Renomeando cabeçalho
colnames(freq_esc)
names(freq_esc)[names(freq_esc) == "freq_abs_esc"] <- "Frequência absoluta"
names(freq_esc)[names(freq_esc) == "freq_rel_esc"] <- "Frequência relativa"
names(freq_esc)[names(freq_esc) == "freq_porc_esc"] <- "Percentual"

glimpse(freq_esc)

## Tabela formatada usando "formattable"
formattable(freq_esc, 
            align = c("l",rep("r", NCOL(freq_esc) - 1)),
            list(
              `Frequência absoluta` = color_tile("#fff0da", "#ffa500"),
              `Frequência relativa` = color_bar("#87cefa"),
              `Percentual` = color_tile("#f6dbd6", "#c98e85")))


### Distribuição - óbito x idade #######################################
# Calculando histograma
df.variaveis
hist_idade=hist(df.variaveis$idade_obito_calculado, as.factor(df.variaveis$def_sexo) breaks=25  , plot=F)
# Colorinfo o vetor 
my_color= ifelse(hist_idade$breaks < 21, rgb(0.2,0.8,0.5,0.5),
                 ifelse (hist_idade$breaks >=65, "purple", rgb(0.2,0.2,0.2,0.2) ))
# Resultado
plot(hist_idade, col=my_color , border=F , main="Distribuição óbito por idade" , ylab= "Frequência", xlab="Idade óbito", xlim=c(-0,120) )

### Distribuição - óbito x sexo x ano ###################################

t.obito_sexo_uf_ano<-df.variaveis %>% 
  filter(def_circ_obito %in%
           'Suicídio') %>% 
  filter(def_sexo %in%
           c('Masculino','Feminino')) %>%
  filter(ocor_SIGLA_UF %in%
           'DF') %>%  
  group_by(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_sexo) %>%
  select(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_sexo) %>%
  count()

t.obito_sexo_uf_ano

g.obito_sexo_uf_ano<-ggplot(t.obito_sexo_uf,aes(x=def_sexo,y=n,fill=factor(ano_obito)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Ano")+
  ggtitle("Óbito ocorridos por sexo e ano")+ # for the main titlexlab("Sexo") 
  xlab("Gêneros")+ylab("Óbitos por suicídio")+
geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)
print(g.obito_sexo_uf_ano)

g.obito_sexo_uf_ano<-ggplot(t.obito_sexo_uf,aes(x=def_sexo, y=n,fill=factor(ano_obito))) +
  geom_col(position = "dodge") +
  labs(title = "Suicídio por gênero",
       x = "Categorias",
       y = "Idade do óbito",
       fill = "Ano")+
  coord_flip()
print(g.obito_sexo_uf_ano)

### Distribuição - óbito x raça/cor x ano ###################################

t.obito_raca_uf_ano<-df.variaveis %>% 
  filter(def_circ_obito %in%
           'Suicídio') %>% 
    filter(ocor_SIGLA_UF %in%
           'DF') %>%  
  group_by(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_raca_cor) %>%
  select(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_raca_cor) %>%
  count()
t.obito_raca_uf_ano

g.obito_raca_uf_ano<-ggplot(t.obito_raca_uf_ano,aes(x=def_raca_cor,y=n,fill=factor(ano_obito)))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=n), position=position_dodge(width=0.9),vjust=-0.25)+
  scale_fill_discrete(name="Ano")+
  ggtitle("Óbito ocorridos por raça/cor e ano")+ # for the main titlexlab("Sexo") 
  xlab("Raça/CorGêneros")+
  ylab("Óbitos por suicídio")
print(g.obito_raca_uf_ano)

g.obito_sexo_uf_ano<-ggplot(t.obito_raca_uf,aes(x=def_raca_cor, y=n,fill=factor(ano_obito))) +
  geom_col(position = "dodge") +
  labs(title = "Suicídio por raça/cor",
       x = "Categorias",
       y = "Idade do óbito",
       fill = "Ano")+

print(g.obito_raca_uf_ano)

### Distribuição - óbito x sexo x ano ###################################

t.obito_sexo_uf<-selecao_var_SIM %>% 
  filter(def_circ_obito %in%
           'Suicídio') %>% 
  filter(def_sexo %in%
           c('Masculino','Feminino')) %>%
  filter(ocor_SIGLA_UF %in%
           'DF') %>%  
  group_by(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_sexo,def_raca_cor) %>%
  select(ano_obito,ocor_SIGLA_UF,def_circ_obito,def_sexo,def_raca_cor) %>%
  count()

t.sexo_obito_uf

g.obito_sexo_uf_ano<-ggplot(t.obito_sexo_uf,aes(x=def_sexo,y=n,fill=factor(ano_obito)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Ano")+
  ggtitle("Óbito ocorridos por sexo e ano")+ # for the main titlexlab("Sexo") 
  ylab("Óbitos por suicídio")
print(g.obito_sexo_uf)
### Distribuição - óbito x faixa etária x ano ###################################

t.obito_faixaetaria_uf<-df.variaveis %>% 
  filter(def_circ_obito %in%
           'Suicídio') %>% 
  filter(def_sexo %in%
           c('Masculino','Feminino')) %>%
  filter(ocor_SIGLA_UF %in%
           'DF') %>%  
  group_by(ano_obito,faixa_etaria) %>%
  select(ano_obito,faixa_etaria) %>%
  count()

t.obito_faixaetaria_uf

g.obito_faixaetaria_uf<-ggplot(t.obito_faixaetaria_uf,aes(x=ano_obito,y=n,fill=factor(faixa_etaria)))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=n), position=position_dodge(width=0.9),vjust=-0.25)+
  scale_fill_discrete(name="Faixa etária")+
  ggtitle("Óbitos ocorridos por faixa etária")+ # for the main titlexlab("Sexo") 
  xlab("Anos")+
  ylab("Óbitos por suicídio")
print(g.obito_faixaetaria_uf)

g.obito_faixaetaria_uf<-ggplot(t.obito_faixaetaria_uf,aes(x=ano_obito,y=n,fill=factor(faixa_etaria)))+
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(label=n), position=position_stack(0.5))+
  scale_fill_discrete(name="Faixa etária")+
  ggtitle("Óbitos ocorridos por faixa etária")+ # for the main titlexlab("Sexo") 
  xlab("Anos")+
  ylab("Óbitos por suicídio")
print(g.obito_faixaetaria_uf)

### Associação - chi-quadrado ###################################

assoc_sexo_raca <- df.variaveis %>% select(def_raca_cor, def_sexo) 
as.matrix(table(assoc_sexo_raca))

chisq.test(as.matrix(table(assoc_sexo_raca)))

# Ho: p1=p2=3=p4=p5
# Ha: p1!=p2!=p3=!p5

### Frequencias - Geral #################################
# 2-Way Frequency Table

attach(analise)
analise <- table(def_sexo,def_raca_cor) # A como linhas, B como colunas
margin.table(analise, 1) # Frequências A (resumido sobre B)
margin.table(analise, 2) # Frequências B (resumido sobre A)
prop.table(analise) # Percentual céluas
prop.table(analise, 1) # Percentual linhas
prop.table(analise, 2) # Percentual colunas

# 3-Way Frequency Table
analise <- table(def_sexo,def_raca_cor,def_escol)
ftable(analise)
summary(analise) # chi-square test of indepedence

# 3-Way Frequency Table
analise <- xtabs(~def_sexo+def_raca_cor+def_escol, data=analise)
ftable(analise) # print table
summary(analise) # chi-square test of indepedence

library(MASS)
analise <- xtabs(~def_sexo+def_raca_cor, data=analise)

library(vcd)
mosaic(analise, shade = TRUE, legend=TRUE)
