library(tidyverse)
library(readxl)
library(writexl)

load("dados.RData")

t<-bind_rows(tcu_total_2011,tcu_total_2012,tcu_total_2013,tcu_total_2014,
             tcu_total_2015,tcu_total_2016,tcu_total_2017,
             tcu_total_2018,tcu_total_2019,tcu_2020)

# ano do processo, ano do julgado e duração

t<-t %>% mutate(Ano_processo=str_sub(Processo,9,12))
t<-t %>% mutate(Ano_julgado = str_sub(Data,7,10))

t$Data1<-t$Data
lubridate::dmy(t$Data1)
t$Data1<-lubridate::dmy(t$Data1)
glimpse(t)
t$Datap<-("01/01/")
t$Datapr<-paste(t$Datap,t$Ano_processo)
t$Data2<-lubridate::dmy(t$Datapr)
t$Duracao1<-t$Data1-t$Data2
table(t$Duracao1)
t$Duracaoa<-t$Duracao1/365
summary(t$Duracaoa)
mean(t$Duracaoa)
mean(t$Duracao1)
t<-t %>% mutate(Duracaoa=as.double((Duracaoa)))
t<-t %>% mutate(Duracao1=as.double((Duracao1)))

t<-t %>% mutate(Ano_julgado=as.integer((Ano_julgado)))
t<-t%>% mutate(Ano_processo=as.integer(Ano_processo))
t<-t %>% mutate(Duracao=Ano_julgado-Ano_processo)
t<-t %>% mutate(Duracao=as.integer(Duracao))
t$Data2<-lubridate::dmy(t$Data1)

write_xlsx(tcu_total_2011,"xlsx/tcu_total_2011.xlsx")

t3<-t1
t3$Tipo<-NULL
t3$Título<-NULL
t3$Data<-NULL
t3$Sumário<-NULL
t3$Processo<-NULL
t3$`Interessado / Responsável / Recorrente`<-NULL
t3$Entidade<-NULL
t3$`Representante do Ministério Público`<-NULL
t3$`Unidade Técnica`<-NULL
t3$`Representante Legal`<-NULL
t3$Assunto<-NULL
t3$`Endereço do Arquivo`<-NULL
t3$Duracao<-t5$Duracaoa
t3$multa<-t5$multa

t3 <- t3[!is.na(t3$multa),]
t3$sigiloso<-t4$sigiloso
t3<-subset(t3,t3$sigiloso != "TRUE")
t3$sigiloso<-NULL

table(t4$sigiloso)

t6<-t3

t6<- t6 %>% mutate(multa1=case_when(str_detect(multa,
                                               "FALSE")~"não",TRUE~"sim"))
t6$multa<-t6$multa1
t6$multa1<-NULL

#multa

t$multa<-str_detect(t$Sumário,"(?i)(multa)")
table(t$multa)

t<- t %>% mutate(multas=case_when(str_detect(multa,"FALSE")~"não",TRUE~"sim"))

t$sigiloso<-str_detect(t$Sumário,"(?i)(Documento classificado como sigiloso)")
table(t$sigiloso)

glimpse(t$multa)
t$multa[is.na(t$multa)] <- TRUE

t4 <- t[!is.na(t$multa),]


tt4<-anti_join(t,t4, by="Título")

summary(t$multa)
summary(t4$multa)

t4<-subset(t4,t4$sigiloso != "TRUE")

t5<-t
t<-t4

#distribuições

table(t$Relator)
Relator1<-as.data.frame(table(t$Relator))
Relator1<-Relator1 %>% mutate(proporcao=Freq/51183)
Relator1<-Relator1 %>% mutate(porcentagem=Freq*100/51183)

glimpse(Relator1)

write_xlsx(Relator1,"xlsx/Relator1.xlsx")

table(t$`Tipo de processo`)
Tipo_de_processo1<-as.data.frame(table(t$`Tipo de processo`))
Tipo_de_processo1<-Tipo_de_processo1 %>% mutate(proporcao=Freq/51183)
Tipo_de_processo1<-Tipo_de_processo1 %>% mutate(porcentagem=Freq*100/51183)

write_xlsx(Tipo_de_processo1,"xlsx/Tipo_de_processo1.xlsx")

table(t$multa)

table(t$Ano_processo)
Ano_processo1<-as.data.frame(table(t$Ano_processo))
Ano_processo1<-Ano_processo1 %>% mutate(proporcao=Freq/51183)
Ano_processo1<-Ano_processo1 %>% mutate(porcentagem=Freq*100/51183)
write_xlsx(Ano_processo1,"xlsx/Ano_processo1.xlsx")

table(t$Ano_julgado)
Ano_julgado1<-as.data.frame(table(t$Ano_julgado))
Ano_julgado1<-Ano_julgado1 %>% mutate(proporcao=Freq/51183)
Ano_julgado1<-Ano_julgado1 %>% mutate(porcentagem=Freq*100/51183)
write_xlsx(Ano_julgado1,"xlsx/Ano_julgado1.xlsx")

table(t$Duracao)
Duracao<-as.data.frame(table(t$Duracao))
write_xlsx(Duracao,"xlsx/Duracao.xlsx")

t1<-subset(t,Ano_processo>0)
t2<-t
t<-t1

table(t2$Ano_processo)
t2t1<-anti_join(t2,t1, by="Título")

#graficos

#grafico de para variaveis categoricas

t %>%
  ggplot(aes(x = Relator)) +
  labs(y="Frequência")+
  geom_bar()+
  coord_flip()

t$Relator = factor(t$Relator,
                   levels=names(sort(table(t$Relator), decreasing=TRUE)))
g = ggplot(t, aes(y=Relator))
g + geom_bar()

Tipo_de_processo300 %>%
  ggplot(aes(x =Tipo_de_processo)) +
  labs(y="Frequência",x="Tipo de processo")+
  geom_bar()+
  coord_flip()

Tipo_de_processo300 %>%
  ggplot(aes(x =Tipo_de_processo)) +
  labs(y="Frequência",x="Tipo de processo")+
  geom_bar()+
  coord_flip()

Tipo_de_processo300$Tipo_de_processo =
  factor(Tipo_de_processo300$Tipo_de_processo,
         levels=names(sort(table(Tipo_de_processo300$Tipo_de_processo),
                           decreasing=TRUE)))
g = ggplot(Tipo_de_processo300, aes(y=Tipo_de_processo))
g + geom_bar()

Tipo_de_processo300 <- t %>%
  group_by(t$`Tipo de processo`) %>%
  filter(n()>300) %>%
  ungroup() %>%
  droplevels()



t %>%
  ggplot(aes(y =multas)) +
  labs(x="Frequência",y="Aplicação de multa no julgado")+
  geom_bar()+
  coord_flip()

#pizza

ggplot(relator_pizza, aes(x="", y=porcentagem, fill=Relator)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Greys")+
  theme_minimal()

ggplot(relator_pizza, aes(x="", y=porcentagem,
                          fill=reorder(Relator, porcentagem))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Greys")+
  labs(fill="Relator")+
  theme_minimal()

ggplot(pizza_tipo, aes(x="", y=porcentagem,
                       fill=reorder(`Tipo de Processo`,porcentagem))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Greys")+
  labs(fill="Tipo de processo")+
  theme_minimal()

multa<-as.data.frame(table(t$multa))
multa$porcentagem=multa$Freq*100/51183
multa<- multa %>% mutate(multa=case_when(str_detect(Var1,
                                                    "FALSE")~"não",TRUE~"sim"))

sd(t$Duracaoa)
var(t$Duracaoa)

ggplot(multa, aes(x="", y=porcentagem, fill=multa)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Greys")+
  theme_minimal()


#gráficos para variaveis quantitativas

Ano_processo1995 %>%
  ggplot(aes(x =`Ano_processo`)) +
  labs(y="Frequência", x="Ano de abertura do processo")+
  geom_bar()

t %>%
  ggplot(aes(x =`Ano_processo`)) +
  labs(y="Frequência", x="Ano do processo")+
  geom_bar()


Ano_processo2000<-subset(t,t$Ano_processo > 1995)

t %>%
  ggplot(aes(x =`Ano_julgado`)) +
  labs(y="Frequência", x="Ano do julgado")+
  scale_x_continuous(breaks = c(2011,2015,2020))+
  geom_bar()

t %>%
  ggplot(aes(x =Duracao)) +
  labs(y="Frequência", x="Duração do processo")+
  geom_bar()

stripchart(dados, method = "jitter", jitter = 0.1)
stripchart(t$Ano_processo)

#histograma

ggplot(Duracaoa20,aes(x=Duracaoa)) +  geom_histogram(binwidth = 1)+
  labs(y="Frequência", x="Duração do processo")
Duracaoa20<-subset(t,t$Duracaoa <15)


#medidas resumo

glimpse(t)

summary(t$Ano_processo)

sum(t$Ano_processo)

105059766/52196
103021751/51183

summary(t$Ano_julgado)

sum(t$Ano_julgado)

105205638/52196
103165915/51183

summary(t$Duracao)

sum(t$Duracao)

145872/52196

summary(t$Duracaoa)
sum(t$Duracaoa)

170599.7/51183

summary(t$Relator)
mean(Relator$Freq)
sd(Relator$Freq)
cv<-function(x){coef<-sd(x)/mean(x)*100}
cv(Relator$Freq)

#medidas de dispersão

var(t$Ano_processo)
sd(t$Ano_processo)

var(t$Ano_julgado)
sd(t$Ano_julgado)

var(t$Duracao)
sd(t$Duracao)

var(t$Duracaoa)
sd(t$Duracaoa)

#box plot

ggplot(t, aes(y=Ano_processo, x=1))+
  geom_jitter(alpha=0.02)+
  geom_boxplot()+
  scale_x_discrete(breaks = c(0))+
  labs( y="ano do processo",x="",caption = "N=51.183")

ggplot(t, aes(y=Ano_julgado, x=1))+
  geom_jitter(alpha=0.02)+
  geom_boxplot()+
  scale_x_discrete(breaks = c(0))+
  scale_y_continuous(breaks = c(2011,2015,2020))+
  labs( y="ano do julgado",x="",caption = "N=51.183")

ggplot(t, aes(y=Duracaoa, x=1))+
  geom_jitter(alpha=0.02)+
  geom_boxplot()+
  scale_x_discrete(breaks = c(0))+
  labs( y="Duração do processo (anos)",x="",caption = "N=51.183")

# analise bidimensional

Tipo_de_processo1000 <- t %>%
  group_by(t$`Tipo de processo`) %>%
  filter(n()>1000) %>%
  ungroup() %>%
  droplevels()

table(Tipo_de_processo1000$`Tipo de processo`)

relator_x_tipo<-as.data.frame(table(t$Relator,t$`Tipo de processo`))

relator_x_tipo1000<-as.data.frame(table(Tipo_de_processo1000$Relator,
                                        Tipo_de_processo1000$Tipo_de_processo))

rt<-spread(relator_x_tipo,key=Var1,value=Freq)
rtt<-spread(relator_x_tipo,key=Var2,value=Freq)

rt1000<-spread(relator_x_tipo1000,key=Var2,value=Freq)

spread(relator_x_tipo,key=Var2,value=Freq)


rt1000$soma<-apply(X = rt1000[,-c(1)], MARGIN = 1, FUN = sum)
rt1000p<-rt1000[,-c(1)]*100/rt1000$soma

rt1000t<-rt1000%>%inner_join(Relator1,by="Var1")
rt1000t$outros<-rt1000t$Freq-rt1000t$soma
rt1000p<-rt1000t/rt1000t$Freq
rt1000p$proporcao<-NULL
rt1000p$porcentagem<-NULL
rt1000p$relator<-Relator1$Var1

write_xlsx(rt1000p,"xlsx/rt1000p.xlsx")

glimpse(rt1000)

chisq.test(t$Relator,t$Tipo_de_processo)
chisq.test(t$Relator,t$multa)
chisq.test(t$multa,t$Tipo_de_processo)

#graf bidim ggplot qualitativo

ggplot(t,aes(y=Relator, fill=multas))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("multa", values = c("sim" = "black", "não" = "white"))+
  labs(y="Relator", x="aplicação de multa")+theme(legend.position="top")

ggplot(Tipo_de_processo300,aes(y=Tipo_de_processo, fill=multas))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("multa", values = c("sim" = "black", "não" = "white"))+
  labs(y="Tipo de processo", x="aplicação de multa")+theme(legend.position="top")

#analise bidimensional multa

tipo_multa<-as.data.frame(table(t$`Tipo de processo`,t$multa))
tmt<-spread(tipo_multa,key=Var2,value=Freq)

tm300<-as.data.frame(table(Tipo_de_processo300$`Tipo de processo`,
                           Tipo_de_processo300$multa))
tm300t<-spread(tm300,key=Var2,value=Freq)


rel_multa<-as.data.frame(table(t$Relator,t$multa))
rmt<-spread(rel_multa,key=Var2,value=Freq)

# tabelas

tapply(t$Duracaoa, t$Tipo_de_processo, FUN = mean, na.rm = TRUE)
tipo_dur<-as.data.frame(tapply(t$Duracaoa, t$Tipo_de_processo, FUN = mean, na.rm = TRUE))

tapply(t$Duracaoa, t$Relator, FUN = mean, na.rm = TRUE)
rel_dur<-as.data.frame(tapply(t$Duracaoa, t$Relator, FUN = mean, na.rm = TRUE))

tmt$proporcao<-tmt$"TRUE"/(tmt$"FALSE"+tmt$"TRUE")
tmt$percentual<-100*tmt$"TRUE"/(tmt$"FALSE"+tmt$"TRUE")

tdm<-bind_cols(tipo_dur,tmt)
rdm<-bind_cols(rel_dur,rmt)

tdm$duracao<-tdm$`tapply(t$Duracaoa, t$Tipo_de_processo, FUN = mean, na.rm = TRUE)`
rdm$duracao<-rdm$`tapply(t$Duracaoa, t$Relator, FUN = mean, na.rm = TRUE)`

relator_multa<-as.data.frame(table(t$Relator,t$multa))
rmt<-spread(relator_multa,key=Var2,value=Freq)

rmt$soma<-rmt$"FALSE"+rmt$"TRUE"
rmtp<-rmt[,-c(1)]*100/rmt$soma
write_xlsx(rmtp,"xlsx/rmtp.xlsx")

tm300t$soma<-tm300t$"FALSE"+tm300t$"TRUE"
tm300tp<-tm300t[,-c(1)]*100/tm300t$soma
write_xlsx(tm300tp,"xlsx/tm300tp.xlsx")

write_xlsx(rt,"xlsx/rt.xlsx")

write_xlsx(tm300t,"xlsx/tm300t.xlsx")

table(t$multa)

#grafico de dispersao

ggplot(t, aes(y = `Ano_julgado`, x = `Ano_processo`))+
  labs(y="Ano do julgado", x="Ano do processo")+
  scale_y_continuous(breaks = c(2011,2015,2020)) +geom_point()

ggplot(t, aes(y = `Ano_julgado`, x = `Ano_processo`))+
  labs(y="Ano do julgado", x="Ano do processo")+
  scale_y_continuous(breaks = c(2011,2015,2020)) +geom_point()+
  geom_jitter(alpha=0.02)

ggplot(t, aes(y = `Duracaoa`, x = `Ano_processo`)) +
  labs(y="Duração do processo", x="Ano do processo")+
  geom_point()

ggplot(t, aes(y = `Duracaoa`, x = `Ano_processo`)) +
  labs(y="Duração do processo", x="Ano do processo")+
  geom_point()+
  geom_jitter(alpha=0.02)

ggplot(t, aes(y = `Duracaoa`, x = `Ano_julgado`))+
  labs(y="Duração do processo", x="Ano do julgado")+
  scale_x_continuous(breaks = c(2011,2015,2020)) +geom_point()

ggplot(t, aes(y = `Duracaoa`, x = `Ano_julgado`))+
  labs(y="Duração do processo", x="Ano do julgado")+
  scale_x_continuous(breaks = c(2011,2015,2020)) +geom_point()+
  geom_jitter(alpha=0.02)

#correlação

cor(t$Ano_julgado,t$Ano_processo)
cor(t$Ano_julgado,t$Duracao)
cor(t$Ano_processo,t$Duracao)

summary(t$Duracao)

# variaveis qualitativas x quantitativas

tce <- subset(t,`Tipo de processo`=="TOMADA DE CONTAS ESPECIAL (TCE)")
summary(tce$Duracaoa)

apos<- subset(t,`Tipo de processo`=="APOSENTADORIA (APOS)")
summary(apos$Duracaoa)

repr<-subset(t,`Tipo de processo`=="REPRESENTAÇÃO (REPR)")
summary(repr$Duracaoa)

ra<-subset(t,`Tipo de processo`=="RELATÓRIO DE AUDITORIA (RA)")
summary(ra$Duracaoa)

pciv<-subset(t,`Tipo de processo`=="PENSÃO CIVIL (PCIV)")
summary(pciv$Duracaoa)

pc<-subset(t,`Tipo de processo`=="PRESTAÇÃO DE CONTAS (PC)")
summary(pc$Duracaoa)

tp6<-bind_rows(tce,apos,repr,ra,pciv,pc)

summary(t$Duracaoa)


sd(tce$Duracaoa)
sd(apos$Duracaoa)
sd(repr$Duracaoa)
sd(ra$Duracaoa)
sd(pciv$Duracaoa)
sd(pc$Duracao)

sd(t$Duracaoa)

nao<-subset(t,multa=="FALSE")
sim<-subset(t,multa=="TRUE")
summary(nao$Duracaoa)
summary(sim$Duracaoa)
sd(nao$Duracaoa)
sd(sim$Duracaoa)


# quali-quanti boxplot

tp<-bind_rows(tce,apos,repr,ra,pciv,pc)
tp3<-bind_rows(tce,apos,repr)
tp<-bind_rows(tce,apos,repr,ra,pciv,pc)
tp4<-bind_rows(tce,apos,repr,ra)

par(mfrow=c(3,2))

ggplot(tp, aes(y=Duracaoa,x=1))+
  geom_jitter(alpha=0.02)+
  labs(y="Duração do processo",x="Tipo de processo")+
  geom_boxplot()+
  scale_x_discrete(breaks = c(0))+
  facet_wrap(vars(`Tipo de processo`))

ggplot(t, aes(y=Duracaoa,x=1))+
  geom_jitter(alpha=0.02)+
  labs(y="Duração do processo",x="relator")+
  geom_boxplot()+
  scale_x_discrete(breaks = c(0))+
  facet_wrap(vars(Relator))

ggplot(t,aes(x = multas, y = Duracaoa))+
  geom_jitter(alpha=0.02)+
  geom_boxplot() +
  labs(x="Multa", y="Duração do processo")

#regressões lineares tdm rdm


lm.fit=lm(tdm$duracao~tdm$percentual,data=tdm)

coef(lm.fit)
summary(lm.fit)

predict(lm.fit,data.frame(percentual=1:33),
        interval="confidence")



tdm <- tdm %>% mutate(percentual=as.numeric(percentual))

glimpse(tdm)

ggplot(mapping = aes(tdm$percentual,tdm$duracao)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title="Tipo de Processo",x="Porcentual de aplicação de multa",
       y="Duração do processo")

lm.fit=lm(rdm$duracao~rdm$percentual,data=rdm)

ggplot(mapping = aes(rdm$duracao,rdm$percentual)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(y="Porcentual de aplicação de multa", x="Duração do processo")


#Regressao linear

coef(lm.fit)
summary(lm.fit)
confint(lm.fit)

lm.fit=lm(Duracaoa~Ano_processo, data = t)

lm.fit=lm(duracao~percentual, data = rdm)



predict(lm.fit,data.frame(Ano_processo=1979:2020),
        interval="confidence")

lm.fit=lm(t$Duracaoa~t$Ano_julgado,
          data = t)

lm.fit=lm(t$Duracaoa~t$Ano_processo+t$Ano_julgado,data = t)

predict(lm.fit,data.frame(Ano_processo=2005,Ano_julgado=2010),
        interval="confidence")

-2.70-0.9906400*2010+0.9922487*2020

#graficos regressão

ggplot(mapping = aes(t$Ano_processo,t$Duracaoa)) +
  geom_point() +
  geom_smooth(method ="lm", color="black")+
  labs(y="Duração do processo", x="Ano do processo")

ggplot(mapping = aes(tdm$percentual,tdm$duracao)) +
  geom_point() +
  geom_smooth(method ="lm", color="black")+
  labs(title="Tipo de processo",y="Duração do processo",
       x="Porcentual de aplicação de multa")

ggplot(mapping = aes(rdm$percentual,rdm$duracao)) +
  geom_point() +
  geom_smooth(method ="lm", color="black")+
  labs(title="Relator",y="Duração do processo",
       x="Porcentual de aplicação de multa")+
  scale_y_continuous(breaks = c(0,3,6,9),limits = c(0,12))

ggplot(mapping = aes(t$Ano_processo,t$Duracaoa)) +
  geom_point() +
  geom_smooth(method ="lm", color="black")+
  labs(y="Duração do processo", x="Ano do processo")+
  geom_jitter(alpha=0.005)

ggplot(mapping = aes(tdm$percentual,tdm$duracao)) +
  geom_point() +
  geom_smooth(method = "lm", color="black")+
  labs(title="Tipo de processo",
       y="Duração do processo", x="Porcentual de aplicação de multa")+
  geom_segment(aes(x = tdm$percentual, y = tdm$duracao,
                   xend = tdm$percentual,
                   yend = predict(lm(tdm$duracao ~ tdm$percentual))),
               color="black")

ggplot(mapping = aes(t$Ano_processo,t$Duracaoa)) +
  geom_point() +
  geom_smooth(method = "lm", color="black")+
  labs(y="Duração do processo", x="Ano do processo")+
  geom_segment(aes(x = t$Ano_processo, y = t$Duracaoa,
                   xend = t$Ano_processo, yend = predict(lm(t$Duracaoa ~ t$Ano_processo))),
               color="black")+
  geom_jitter(alpha=0.005)

ggplot(mapping = aes(tdm$percentual,tdm$duracao)) +
  geom_point() +
  geom_smooth(method = "lm", color="black")+
  geom_hline(yintercept = mean(tdm$duracao))+
  labs(title="Tipo de processo",
       y="Duração do processo", x="Porcentual de aplicação de multa")+
  geom_segment(aes(x = tdm$percentual, y = tdm$duracao,
                   xend = tdm$percentual,
                   yend = mean(tdm$duracao)))

mean(tdm$duracao)

ggplot(mapping = aes(t$Ano_processo,t$Duracaoa)) +
  geom_point() +
  geom_smooth(method = "lm", color="black")+
  geom_hline(yintercept = mean(t$Duracaoa))+
  labs(y="Duração do processo", x="Ano do processo")+
  geom_segment(aes(x = t$Ano_processo, y = t$Duracaoa,
                   xend = t$Ano_processo, yend = mean(t$Duracaoa)),color="black")+
  scale_y_continuous(breaks = c(0,3.3,10,20,30))+
  geom_jitter(alpha=0.005)

ggplot(mapping = aes(t$Ano_julgado,t$Duracaoa))+geom_point()+
  geom_smooth(method = "lm",color="black")+
  labs(y="Duração do processo", x="Ano do julgado")+
  scale_x_continuous(breaks = c(2011,2015,2020))+
  geom_jitter(alpha=0.005)

ggplot(mapping = aes(t$Ano_processo,t$Ano_julgado)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(y="Ano do julgado", x="Ano do processo")+
  scale_y_continuous(breaks = c(2011,2015,2020))

ggplot(t, aes(y = `Ano_julgado`, x = `Ano_processo`))+
  labs(y="Ano do julgado", x="Ano do processo") +geom_point()+
  geom_smooth(method = "lm")

# residuos

plot(resid(lm.fit) ~ predict(lm.fit),pch=16)
abline(0,0,col="red")

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))

# coeficientes rl multipla

coef(lm.fit)
summary(lm.fit)

lm.fit=lm(t$Duracaoa~t$Ano_processo+t$Ano_julgado,
          data = t)

# reg lin variaveis qualitativas

lm.fit=lm(t$Duracaoa~t$multa,
          data = t)

coef(lm.fit)
summary(lm.fit)


lm.fit=lm(t$Duracaoa~t$Relator,
          data = t)

lm.fit=lm(Duracaoa~`Tipo de processo`,
          data = t)

names(lm.fit)

vif(lm.fit)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

coeftp<-as.data.frame(coef(lm.fit))

write_xlsx(coeftp,"xlsx/coeftp.xlsx")

glimpse((coeftp))

mean(t$Duracaoa)

coefrel<-as.data.frame(coef(lm.fit))
write_xlsx(coefrel,"xlsx/coefrel.xlsx")

lm.fit=lm(t$Duracaoa~t$Ano_processo+t$Ano_julgado+t$multa+
            t$Relator+t$`Tipo de processo`,
          data = t)

lm.fit=lm(t$Duracaoa~t$Ano_julgado+t$multa+
            t$Relator+t$`Tipo de processo`,
          data = t)

lm.fit=lm(t$Duracaoa~t$multa+
            t$Relator+t$`Tipo de processo`,
          data = t)

coef(lm.fit)

summary(lm.fit)



lm.fit=lm(t$Duracaoa~t$Ano_processo+t$multa+
            t$Relator+t$`Tipo de processo`,
          data = t)

lm.fit=lm(t$Duracaoa~t$Ano_processo+t$multa,
          data = t)

lm.fit=lm(t$Duracaoa~t$Ano_julgado+t$multa,
          data = t)

ggplot(t,aes(x = Ano_processo, y = Duracaoa, shape = multas)) +
  geom_smooth(method = "lm",color="black",aes(linetype=multas)) +
  scale_x_continuous(limits = c(1995,2020)) +
  labs(y="Duração do processo", x="Ano do processo",linetype="Multa")

ggplot(t,aes(x = Ano_julgado, y = Duracaoa, shape = multas)) +
  geom_smooth(method = "lm",color="black",aes(linetype=multas)) +
  scale_x_continuous(breaks = c(2011,2015,2020),limits = c(2011,2020)) +
  scale_y_continuous(breaks = c(0,3,6,9),limits = c(0,12))+
  labs(y="Duração do processo", x="Ano do julgado",linetype="Multa")


# regressão logística

t$Tipo_de_processo<-t$`Tipo de processo`

t <- t %>% mutate(multa1=case_when(str_detect(multa,
                                              "FALSE")~"0",TRUE~"1"))

t <- t %>% mutate(multa1=as.integer(multa1))

glm.fit=glm(ty$multa1~ty$Duracaoa+
              ty$Tipo_de_processo+ty$Relator,
            data=ty ,family =binomial)

coefrlogmtdr<-as.data.frame(coef(glm.fit))
write_xlsx(coefrlogmtdr,"xlsx/coefrlogmtdr.xlsx")

glm.fit=glm(t$multa1~t$Ano_processo+t$Ano_julgado+t$Duracaoa+
              t$Relator+t$Tipo_de_processo,
            data=t ,family =binomial)

coef(glm.fit)
summary(glm.fit)

glm.probs=predict(glm.fit ,type ="response")
glm.probs [1:10]

-2.43774502+1*0.04295037-2.22170545-0.51611751

glm.pred=rep("0",51183)
glm.pred[glm.probs >.5]="1"
glm.pred[1:10]
table(glm.pred,t$multa1)

mean(glm.pred==t$multa1)

coefrlog<-as.data.frame(coef(glm.fit))

tx.2019<-subset(tx, tx$Ano_julgado == "2019")
tx.2020<-subset(tx, tx$Ano_julgado == "2020")
tx.2019<-bind_rows(tx.2019,tx.2020)
dim(tx.2019)

train <- train %>% mutate(multa1=case_when(str_detect(multa,
                                                      "FALSE")~"0",TRUE~"1"))

train <- train %>% mutate(multa1=as.integer(multa1))


glm.fit=glm(multa1~Duracaoa,
            data=train ,family =binomial)

glm.fit=glm(multa1~Tipo_de_processo,
            data=train ,family =binomial)

glm.fit=glm(multa1~Relator,
            data=train ,family =binomial)

glm.fit=glm(multa1~Duracaoa+Relator+Tipo_de_processo,
            data=train ,family =binomial)

#

glm.probs =predict (glm.fit,tx.2019,type="response")

glm.pred=rep("0",11846)
glm.pred[glm.probs>.5]="1"
table(glm.pred,tx.2019$multa1)
mean(glm.pred==tx.2019$multa1)


#graficos regressão logística


ggplot(t, aes(x=Duracaoa, y=glm.probs)) +
  geom_point()+
  labs(x="Duração do processo (anos)", y="Probabilidade de multa")


plot(sort(glm.probs), xlab="Decisões", ylab="Probabilidade de multa")




