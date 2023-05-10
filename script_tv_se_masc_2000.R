##################################################################
### Script para construção de tábua de vida ######################
### Nome: Kleber Oliveira ########################################
### Data: 18 de março de 2023 ####################################
##################################################################


### Libraries

library(tidyverse)
library(ggplot2)
library(xtable)



## Insere manualmente os dados

tabua_se_masc_2000 <- data.frame(idade=factor(c('<1','1-4','5-9','10-14','15-19','20-24','25-29', '30-34',
                                         '35-39','40-44','45-49','50-54', '55-59', '60-64','65-69',
                                         '70-74','75-79', '80w'),
                                       levels = c('<1','1-4','5-9','10-14','15-19','20-24','25-29', '30-34',
                                                  '35-39','40-44','45-49','50-54', '55-59', '60-64','65-69',
                                                  '70-74','75-79', '80w')),
                                 x=as.numeric(c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)),
                                 n=as.numeric(c(1,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,0)), 
                                 mortes=c(1886,242,107,103,253,351,354,392,421,453,495,569,622,683,
                                          836,924,980,2518),
                                 populacao=c(39566,15711,19492,204359,204490,176462,146537,133306,
                                             115906,92308,75466,62269,48330,39318,30697,23431,17044,
                                             20150))


saveRDS(tabua_se_masc_2000, file="tabua_se_masc_2000.RData")    ## salva o arquivo

## Taxa de mortalidade por idade

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(nmx = mortes/populacao)                                        ## constrói a coluna nmx

tabua_se_masc_2000$nmx <- round(tabua_se_masc_2000$nmx, digits = 6)     ## formata o número de casas decimais


## Fator de separação
nr_linhas <- nrow(tabua_se_masc_2000)
tabua_se_masc_2000$nax <- 2.5

#### para 0a1
if(tabua_se_masc_2000$nmx[1]>=0.107){
  tabua_se_masc_2000$nax[1]=0.33
} else{
  tabua_se_masc_2000$nax[1]=0.045+2.684*tabua_se_masc_2000$nmx[1]
}


#### para 1a4

if(tabua_se_masc_2000$nmx[2]>=0.107){
  tabua_se_masc_2000$nax[2]=1.352
  } else{
    tabua_se_masc_2000$nax[2] = 1.651-2.816*tabua_se_masc_2000$nmx[1]
  }

#### para +a80
tabua_se_masc_2000$nax[nr_linhas] <- 1/tabua_se_masc_2000$nmx[nr_linhas]

## Probabilidade de morte

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(nqx=(n*nmx)/(1+(n-nax)*nmx))

tabua_se_masc_2000$nqx[nr_linhas] <- 1 


## Probabilidade de sobrevivência

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(npx = 1 - nqx)

## Sobrevivente à idade exata x

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(lx = 100000* cumprod(c(1, npx[-nr_linhas])))

tabua_se_masc_2000$lx <- round(tabua_se_masc_2000$lx, digits = 0)


## Óbitos na coorte de 100 mil nascidos vivos

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(ndx = c(-diff(lx), lx[nr_linhas]))

tabua_se_masc_2000$ndx <- round(tabua_se_masc_2000$ndx, digits = 0)



## Pessoas anos vividos

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(nLx = n*(lx-ndx)+nax*ndx)

tabua_se_masc_2000$nLx <- round(tabua_se_masc_2000$nLx, digits = 0) 

## ou

# tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  #  mutate(sobr = lead(lx, 1)*n+nax*ndx)

tabua_se_masc_2000$sobr[nr_linhas] <- tabua_se_masc_2000$ndx[nr_linhas]/tabua_se_masc_2000$nmx[nr_linhas]


## Pessoas anos a serem vividos

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(Tx = sum(nLx) - cumsum(nLx) + nLx)

## Esperança de vida ao nascer

tabua_se_masc_2000 <- tabua_se_masc_2000 %>% 
  mutate(ex = Tx/lx)

tabua_se_masc_2000$ex <- round(tabua_se_masc_2000$ex, digits = 1)


saveRDS(tabua_se_masc_2000, file = 'tabua_se_masc_2000.RData')

tabua_se_masc_2000 <- readRDS("tabua_se_masc_2000.RData")


## Gerando o latex

print(xtable(tabua_se_masc_2000, digits = c(0,0,0,0,0,0,4,3,4,4,0,0,0,0,0)), format.args=list(big.mark=".", decimal.mark=","), include.rownames=FALSE)


##### Gráfico nqx ----
library(ggplot2)

graf_nqx <- tabua_se_masc_2000 %>% 
  ggplot(aes(x=idade,y=nqx, group=1)) +
  geom_line() +
  scale_y_log10()

graf_nqx

tabua_se_masc_2000 <- readRDS(file="tabua_se_masc_2000.RData")
