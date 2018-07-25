library(dplyr)
library(ggplot2)
library(readxl)
library(tidyquant)  
library(cranlogs) 
library(magrittr)
#lee los datos
HistoricosFB1 <- read_excel("Andes/Historicos Admin (1).xlsx", sheet = "FB")
HistoricosFB2 <- read_excel("Andes/data facebook uniandes admon desde enero.xlsx")
HistoricosAdw <- read_excel("Andes/Historicos Admin (1).xlsx", sheet = "Adw")
HistoricosLinkedin <- read_excel("Andes/Historicos Admin (1).xlsx", sheet = "Linkedin")
#nombra los datos para hacer mas facil filtrar
names(HistoricosFB2)=c("date","campaign","ad_set_name","campaign-objet","amount","Impressions","Reach","engagements","clicks","videoviews","Frecuency","Leads")
names(HistoricosFB1)=c("date","campaign","ad_set_name","amount","clicks","Impressions","Leads","Likes","engagements")
namesF=c("date","campaign","amount","Impressions","clicks")
names(HistoricosAdw)=c("date","campaign","amount","clicks","Impressions","Conversions","videoviews")
names(HistoricosLinkedin)=c("date","campaign","amount","Impressions","clicks","Leads")
#filtra los datos que son comunes entre las bases
NewFB1<-HistoricosFB1[namesF]
NewFB2<-HistoricosFB2[namesF]
NewLinkedin<-HistoricosLinkedin[namesF]
NewAdw<-HistoricosAdw[namesF]
#cambia a pesos el gasto de linkedin y une las beses
NewLinkedin[,3]<-NewLinkedin[,3]*2896.25
NewData=rbind(NewFB1,NewFB2,NewAdw,NewLinkedin)
#aprovecha el orden de las bases y crea una nueva columna con el medio del que vinen
large=dim(NewData)[1]
i=1
large=dim(NewData)
largeFB=dim(HistoricosFB1)[1]+dim(HistoricosFB2)[1]
largeAdw=dim(HistoricosAdw)[1]
largelink=dim(HistoricosLinkedin)[1]
Medio=matrix(0, large, 1)
for (i in 1:largeFB)
{(Medio[i,1]<-"FB")}
for (i in 1:largeAdw) 
{(Medio[largeFB+i,1]<-"Adw")}
for (i in 1:largelink)
{(Medio[largeFB+largeAdw+i,1]<-"Linkedin")}
NewData=data.frame(NewData,Medio)
#agrupa los datos por medio y fechas sumando la totalidad de las impresiones y costos
Fechas=NewData %>% 
  group_by(date,Medio) %>%
  summarise(amount = sum(amount), 
            Impressions = sum(Impressions) ,clicks=sum(clicks))
#agrupa los datos por medio y campaña sumando la totalidad de las impresiones y costos
campañas=NewData %>% 
  group_by(campaign,Medio) %>%
  summarise(amount = sum(amount), 
            Impressions = sum(Impressions) ,clicks=sum(clicks))
#############por Fechas
#grafica las impresiones por cada dia 
dat<-data.frame(date=Fechas[,1],Impressions=Fechas[,4],amount=Fechas[,3],Medio=Fechas[,2])
ggplot(data=dat,aes(x = date, y = Impressions, color = Medio)) +
  geom_point() +
  geom_smooth(method = "loess") + 
  labs( x = "", 
        y = "Impresiones") +
  facet_wrap(~ Medio, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")
#grafica las impresiones contra los costes de cada dia 

ggplot(data=dat,aes(x = amount, y = Impressions, color = Medio)) +
  geom_point() +
  geom_smooth(method = "loess") + 
  labs( x = "Amount", 
        y = "Impresiones") +
  facet_wrap(~ Medio, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")
#crea una funcion para almacenar datos estadisticos necesarios
custom_stat_fun <- function(x, na.rm = TRUE, ...) {
  c(mean    = mean(x, na.rm = na.rm),
    stdev   = sd(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...)) 
}
# genera una nueva base de datos con las estadisticas agrupando por cada semana
options(digits = 4)
set.seed(3366)
nums  <- c(10 + 1.5*rnorm(10), NA)
probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)
custom_stat_fun(nums, na.rm = TRUE, probs = probs)
stats_dat <- NewData %>%
  tq_transmute(
    select = Impressions,
    mutate_fun = apply.weekly, 
    FUN = custom_stat_fun,
    na.rm = TRUE,
    probs = probs
  )
stats_dat
#grafica la media de cada semana contra las impresiones 
ggplot(data=stats_dat, aes(x = date, y = `50%`)) +
  # Ribbon
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), 
              color = palette_light()[[1]], fill = palette_light()[[1]], alpha = 0.5) +
  # Points
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) + 
  # Aesthetics
  labs(title = "Media de impresiones por semana", x = "",
       subtitle = "Range of 1st and 3rd quartile to show volatility",
       y = "Media de impresiones diarias por semana") +
  expand_limits(y = 0) + 
  scale_color_tq(theme = "dark") +
  theme_tq() +
  theme(legend.position="none")
####################### por campaña
#grafica las impresiones por cada campaña
dat<-data.frame(campaign=campañas[,1],Impressions=campañas[,4],amount=campañas[,3],Medio=campañas[,2])
ggplot(data=dat,aes(x = campaign, y = Impressions, color = Medio)) +
  geom_point() +
  geom_smooth(method = "loess") + 
  labs( x = "", 
        y = "Impresiones") +
  facet_wrap(~ Medio, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")
#grafica costos contra impresiones por campaña
ggplot(data=dat,aes(x = amount, y = Impressions, color = Medio)) +
  geom_point() +
  geom_smooth(method = "loess") + 
  labs( x = "Amount", 
        y = "Impresiones") +
  facet_wrap(~ Medio, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")
