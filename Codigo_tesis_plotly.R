############################################################################
#                                                                          #
#                         Introducción a Plotly                            #
#                                                                          #
############################################################################

library(tidyverse)
library(plotly)

data = data.frame(variable1 = seq(1, 100, 1), 
                  variable2 = rnorm(100, 0, 1)) 

p <- plot_ly(data,  
             x = ~ variable1,  
             y = ~variable2,  
             type = 'scatter',  
             mode = 'markers', 
             marker = list(color = c("blue"))) 
p 

data = data.frame(variable1 = seq(1, 100, 1), 
                  variable2 = rnorm(100, 0, 1),
                  variable3 = rnorm(100, 5, 1))

p <- plot_ly(data,  
             x = ~ variable1,  
             y = ~variable2,  
             name = "variable 2",
             type = 'scatter',  
             mode = 'markers', 
             marker = list(color = c("blue"))) %>%  
  add_trace(y = ~variable3, 
            name = "variable 3",
            mode = "lines", 
            line   = list(color = c("red")),
            marker = list(color = c("red"))) %>%
  layout(title = "Ejemplo de gráfico", 
         xaxis = list(title ="x"), 
         yaxis = list(title = "y")) 
p 

data = data.frame(variable1 = seq(1, 100, 1), 
                  variable2 = rnorm(100, 0, 1),
                  variable3 = rnorm(100, 5, 1)) 

p <- plot_ly(data,  
             x = ~ variable1,
             y = ~variable2, 
             name = "variable 2",
             type = 'scatter',  
             size = ~variable3, 
             color = I("blue"), 
             opacity = 0.8 ) %>%
  add_lines(y = ~fitted(loess(variable2 ~ variable1)) , 
            name = "variable 3", 
            line   = list(color = c("blue"))) %>%  
  layout(title = "Ejemplo de gráfico", 
         xaxis = list(title ="x"), 
         yaxis = list(title = "y"))
p 

p <- plot_ly(iris,  
             x = ~Sepal.Length, 
             y = ~Sepal.Width, 
             type = "scatter", 
             color = ~Species, 
             mode = "markers") %>%
  layout(title="Segmentación por  color") 
p 

subplot( 
  plot_ly(iris,
          x = ~Sepal.Length, 
          y = ~Sepal.Width, 
          type = "scatter", 
          color = ~Species, 
          mode = "markers") %>%
    layout(showlegend = FALSE),
  plot_ly(iris,  
          x = ~Sepal.Length, 
          y = ~Petal.Length, 
          type = "scatter", 
          color = ~Species, 
          mode = "markers") %>%
    layout(title="Subgráfico", 
           showlegend = TRUE) 
) 

data <- data.frame(categorias = c("categoría 1", "categoría 2", "categoría 3") ,  
                   variable1 =  c(40, 60, 30),  
                   variable2 =  c(55, 67, 23)) 
p <- plot_ly(data,  
             x = ~categorias,  
             y = ~variable1,  
             type = 'bar',  
             name = 'variable 1') %>%  
  add_trace(y = ~variable2,  
            name = 'variable 2') %>%  
  layout(yaxis = list(title = 'Count'), 
         barmode = 'group') 
p 

############################################################################
#                                                                          #
#                Base de datos de incidencia delictiva                     #
#                                                                          #
############################################################################


#Filtrado y partición de los datos:
library(tidyverse)

IDM_NM_sep22 <- read.csv("Descargas/IDM_NM_sep22.csv", encoding = "latin1")
levels(as.factor(IDM_NM_sep22$Tipo.de.delito))
variables = c("Aborto", "Abuso sexual", "Acoso sexual", "Corrupción de menores", 
              "Feminicidio", "Hostigamiento sexual", "Incesto" , 
              "Incumplimiento de obligaciones de asistencia familiar", 
              "Otros delitos que atentan contra la libertad y la seguridad sexual", 
              "Trata de personas", "Violación equiparada", "Violación simple",
              "Violencia de género en todas sus modalidades distinta a la violencia familiar", 
              "Violencia familiar")

IDM_NM_sep22 <- IDM_NM_sep22 %>% filter(Tipo.de.delito %in% variables)
IDM_NM_sep22$total <- rowSums(IDM_NM_sep22[,10:21], na.rm = TRUE)

Delitos_Genero_2015 <- IDM_NM_sep22 %>% filter(Año == "2015")
Delitos_Genero_2016 <- IDM_NM_sep22 %>% filter(Año == "2016")
Delitos_Genero_2017 <- IDM_NM_sep22 %>% filter(Año == "2017")
Delitos_Genero_2018 <- IDM_NM_sep22 %>% filter(Año == "2018")
Delitos_Genero_2019 <- IDM_NM_sep22 %>% filter(Año == "2019")
Delitos_Genero_2020 <- IDM_NM_sep22 %>% filter(Año == "2020")
Delitos_Genero_2021 <- IDM_NM_sep22 %>% filter(Año == "2021")
Delitos_Genero_2022 <- IDM_NM_sep22 %>% filter(Año == "2022")

write.csv(Delitos_Genero_2015, file = "Delitos_Genero_2015.csv", row.names = FALSE)
write.csv(Delitos_Genero_2016, file = "Delitos_Genero_2016.csv", row.names = FALSE)
write.csv(Delitos_Genero_2017, file = "Delitos_Genero_2017.csv", row.names = FALSE)
write.csv(Delitos_Genero_2018, file = "Delitos_Genero_2018.csv", row.names = FALSE)
write.csv(Delitos_Genero_2019, file = "Delitos_Genero_2019.csv", row.names = FALSE)
write.csv(Delitos_Genero_2020, file = "Delitos_Genero_2020.csv", row.names = FALSE)
write.csv(Delitos_Genero_2021, file = "Delitos_Genero_2021.csv", row.names = FALSE)
write.csv(Delitos_Genero_2022, file = "Delitos_Genero_2022.csv", row.names = FALSE)

#ARMAR BASE COMPLETA 

años = seq(2015, 2022, 1)
for(i in años){
  if(i == 2015){
    Delitos_Genero <- read.csv(paste("https://raw.githubusercontent.com/valemtzsolano/graficos-interactivosPlotly/main/Delitos_Genero_",i,".csv", sep = ""), encoding = "UFT8")
  }else{
    data_aux = read.csv(paste("https://raw.githubusercontent.com/valemtzsolano/graficos-interactivosPlotly/main/Delitos_Genero_",i,".csv", sep = ""), encoding = "UFT8")
    Delitos_Genero <- rbind(Delitos_Genero, data_aux)
  }
}
rm(data_aux)


#EJEMPLO 1: Gráfico de lineas
datos <-Delitos_Genero %>% filter(Tipo.de.delito == "Feminicidio") %>% 
  select(Año, total) %>% group_by(Año) %>% summarise(Total = sum(total))

p <- plot_ly(datos, x = ~Año, 
             y = ~Total, 
             type = "scatter", 
             mode = "markers",
             line = list(color=c("#FF5733")),
             marker=list(color=c("#FF5733")))  %>% 
  layout(title = "Total de Feminicidios",
         xaxis = list(title =""),
         yaxis = list(title = "Total de casos reportados"))
p

# EJEMPLO 2: GRÁFICO DE BARRAS

Feminicidios <-Delitos_Genero %>% 
  #Filtro para seleccionar solo feminicidios
  filter(Tipo.de.delito == "Feminicidio") %>%
  #Seleccionamos los meses y el año
  select(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto,
         Septiembre, Octubre, Noviembre, Diciembre, Año) %>% 
  #Creamos una base larga para hacer el gráfico
  gather("meses", "total", 1:12) %>% group_by(Año, meses) %>% 
  #Calculamos el total de feminicidios
  summarise(Total = sum(total)) %>% 
  #Ordenamos los meses
  mutate(meses = factor(meses, levels = c("Enero", "Febrero", "Marzo", "Abril", 
                                          "Mayo", "Junio", "Julio", "Agosto",
                                          "Septiembre", "Octubre","Noviembre", "Diciembre")),
         Año = as.character(Año))


p <- plot_ly(Feminicidios, 
             x = ~meses, 
             y = ~Total,
             type = "bar",
             color = ~Año) %>% 
  layout(title = "Total de Feminicidios",
         xaxis = list(title =""),
         yaxis = list(title = "Total mensual"))
p

# EJEMPLO 3: GRÁFICO DE DISPERSIÓN

#Se seleccionan los delitos para el año 2021
a <-Delitos_Genero %>% filter(Año == "2021") %>% 
  #Se selecciona el tipo de delito
  filter(Tipo.de.delito == "Feminicidio") %>% 
  #se agrupa por muncipio y se calcula el total para cada municipio.
  select(Cve..Municipio, total) %>% group_by(Cve..Municipio) %>% summarise(Total = sum(total))
#Se procede igual en el código siguiente
b <-Delitos_Genero %>% filter(Año == "2021") %>% 
  filter(Tipo.de.delito == "Abuso sexual") %>% 
  select(Cve..Municipio, total) %>% group_by(Cve..Municipio) %>% summarise(Total = sum(total))

#Se unen ambas bases de datos con un inner join
datos = a %>% inner_join(b, by = "Cve..Municipio")

#se realiza el gráfico de dispersión
p <- plot_ly(datos, x = ~Total.x, y = ~Total.y, 
             type = "scatter",
             name = "Total municipal",
             mode = "markers") %>% 
  #Se agrega la linea de regresión
  add_trace(y = ~fitted(lm(Total.y ~ Total.x)),
            mode = "lines",
            name = "Estimación") %>% 
  layout(title = "Relación entre Femincidios y abuso sexual 2021",
         xaxis = list(title ="Total de Feminicidios"),
         yaxis = list(title = "Total de casos de abuso sexual"))
p

# EJEMPLO 4 GRÁFICO DE CAJAS
#https://ceey.org.mx/movilidad-social-en-mexico-las-cinco-regiones/
sur = c("Guerrero","Oaxaca","Chiapas", "Veracruz de Ignacio de la Llave",
        "Tabasco", "Campeche", "Yucatán" , "Quintana Roo" )

datos<-Delitos_Genero %>% filter(Año == "2021") %>% 
  filter(Tipo.de.delito == "Violación equiparada" ) %>% 
  filter(Entidad %in% sur) %>% select(Entidad, total)

datos2<-Delitos_Genero %>% filter(Año == "2021") %>% 
  filter(Tipo.de.delito == "Violación simple" ) %>% 
  filter(Entidad %in% sur) %>% select(Entidad, total)

p <- subplot(
  plot_ly(datos,  x = ~total, color = ~Entidad,
          type = "box") %>% 
    layout(xaxis = list(title = "Violación Equiparada")),
  plot_ly(datos2,  x = ~total, color = ~Entidad,
          type = "box") %>% 
    layout(title = "Delitos de Violación en el sur de México en 2021",
           yaxis = list(title =""),
           xaxis = list(title = "Violación Simple"),
           showlegend = FALSE),
  titleY = TRUE, titleX = TRUE
)
p

# EJEMPLO 5 Distribución de aborto

centro = c("Guanajuato","Querétaro", "Hidalgo", "México", "Ciudad de México",
           "Tlaxcala", "Puebla", "Morelos", "Jalisco", "Aguascalientes", "Colima",
           "Michoacán de Ocampo", "San Luis Potosí")

Delitos_Genero <-Delitos_Genero %>% mutate(región = ifelse(Entidad %in% sur, "SUR",
              ifelse(Entidad %in% centro, "CENTRO","NORTE")))

datos=Delitos_Genero %>% filter(Tipo.de.delito == "Aborto") %>% 
  filter(Año == 2015) %>% 
  select(total, región) %>% filter(total > 0)
datos2=Delitos_Genero %>% filter(Tipo.de.delito == "Aborto") %>% 
  filter(Año == 2021) %>% 
  select(total, región) %>% filter(total > 0)


p <- subplot(
  plot_ly(datos, x = ~total, color = ~región, 
          type = "histogram", alpha = 0.7,
          showlegend = F) %>% 
    layout(barmode = "overlay",
           title = "",
           xaxis = list(title ="Distribución del aborto 2015"),
           yaxis = list(title = "Frecuencia")),
  plot_ly(datos2, x = ~total, color = ~región, 
          type = "histogram", alpha = 0.7) %>% 
    layout(barmode = "overlay",
           title = "Distribución del delito de aborto",
           xaxis = list(title ="Distribución del aborto 2021"),
           yaxis = list(title = "Frecuencia")),
  titleY = TRUE, titleX = TRUE
)
p



