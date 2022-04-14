
# Extraccion de datos

install.packages("dplyr")
install.packages('ggplot1')
install.packages("wrapr")
library(dplyr)
library(ggplot2)
library(wrapr)



clientes<- read.csv(file="files/clientes.csv",header = TRUE,sep = ",", encoding = "UTF-8" )
pedidos<- read.csv(file="files/pedido.csv",header = TRUE,sep = ",", encoding = "UTF-8" )
detallePedido<- read.csv(file="files/detallePedido.csv",header = TRUE,sep = ",", encoding = "UTF-8" )

# Union clientes con pedidos
df = clientes %>% inner_join(pedidos,by="CODIGO_CLIENTE")

# Datos
datos = df %>% inner_join(detallePedido,by="CODIGO_PEDIDO")

# Exploracion de datos
glimpse(datos)
str(datos)
head(datos)
tail(datos)
class(datos)
summary(datos)
which(datos == "CODIGO_CLIENTE")
names(datos)
colnames(datos)

datos2 <- datos
str(datos2)
# Convercion de datos a factor
datos2 <- as.data.frame(unclass(datos2),stringsAsFactors = TRUE)
str(datos2)

#Conversion de datos a date
datos3 <- datos
datos3$FECHA_PEDIDO = as.Date(datos3$FECHA_PEDIDO,"%Y-%m-%d")
str(datos3)

#Conversion de datos a integer
datos4 <- datos
datos4$CANTIDAD = as.integer(datos4$CANTIDAD)
str(datos4)

#Conversion de datos a double
datos5 <- datos
datos5$PRECIO_UNIDAD = as.double(datos5$PRECIO_UNIDAD)
str(datos5)


# Table
table(datos5$PAIS)

# Datos nulos
anyNA(datos5)

# Boxplot valores atipicos
boxplot(datos5$PRECIO_UNIDAD)

# Hits distrubuccion de datos
hist(datos$PRECIO_UNIDAD)

# Eliminar datos nulos 
datos6 <- datos
datos6 <- datos6[!is.null(datos6$PRECIO_UNIDAD) && datos6$CANTIDAD != '']
View(datos6)


# Creacion de nuevo conjunto
datos6 <- data.frame(nombre_cliente = datos$NOMBRE_CLIENTE,ciudad = datos$CIUDAD,
                     region = datos$REGION, pais = datos$PAIS, fecha_pedido = datos$FECHA_PEDIDO, estado = datos$ESTADO,
                     cantidad = datos$CANTIDAD, precio_unidad = datos$PRECIO_UNIDAD, codigo_producto = datos$CODIGO_PRODUCTO)
View(datos6)

# Creacion de total ventas

datos6$total_ventas <- datos$CANTIDAD * datos$PRECIO_UNIDAD 
datos6$fecha_pedido <- as.Date(datos6$fecha_pedido)
View(datos6)
str(datos6)

# Creacion columna mes y ano
datos6$ano <- as.numeric(format(datos6$fecha_pedido,'%Y'))
View(datos6)

datos6$mes <- as.numeric(format(datos6$fecha_pedido,'%m'))
View(datos6)

# ggplot por ciudad

ggplot(datos6,aes(x = ciudad, y = total_ventas)) + geom_point()

#ggplot por fecha

ggplot(datos6 %>% filter(ano != 2006),aes(x = fecha_pedido, y = total_ventas)) + geom_point()


#ggplot por ciudad 
cantidad_clientes_ciudad <- datos6 %>%
  select("nombre_cliente","ciudad") %>%
  group_by(ciudad) %>%
  summarize(total_clientes = n() )


ggplot(cantidad_clientes_ciudad,aes(x = ciudad,y = total_clientes )) + geom_point()


#ggplot por pais
cantidad_clientes_pais <- datos6 %>%
  select("nombre_cliente","pais") %>%
  group_by(pais) %>%
  summarize(total_clientes = n() )

ggplot(cantidad_clientes_pais,aes(x = pais,y = total_clientes )) + geom_point()




