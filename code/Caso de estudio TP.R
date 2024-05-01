#limpia datos innecesarios
rm(list=ls(all=TRUE))
cat("\014")
graphics.off()

#Cargar librerias
library(jsonlite)
library(ggplot2)
library(dplyr)

setwd("C:/Users/milid/Documents/UPC/2024-01/Fundamentos en Data Science/TP")
hotel_data <- read.csv('hotel_bookings.csv', header=TRUE, stringsAsFactors = FALSE, sep=',', dec=',')
head(hotel_data)

#estructura del dataset
str(hotel_data)
#ultimos registro del dataset
tail(hotel_data)
#resumen estadistico
summary(hotel_data)
#nombres de columna
colnames(hotel_data)
#cantidad de filas y columnas
dim(hotel_data)
#conteo por categoria
table(hotel_data$hotel)


#DESARROLO DE PREGUNTAS

# 1. ¿Qué tipo de hotel, urbano o resort, tiene más reservas?
reservas_por_hotel <- hotel_data %>% 
  group_by(hotel) %>%
  summarise(total_reservas = n())

print("Reservas por tipo de hotel:")
print(reservas_por_hotel)

# Gráfico
ggplot(reservas_por_hotel, aes(x = hotel, y = total_reservas)) +
  geom_bar(stat = "identity") +
  labs(x = "Tipo de hotel", y = "Total de reservas", title = "Reservas por tipo de hotel")

# 2. ¿Está aumentando la demanda de reservas con el tiempo?
hotel_data$arrival_date_year <- as.factor(hotel_data$arrival_date_year)
reservas_por_año <- hotel_data %>%
  group_by(arrival_date_year) %>%
  summarise(total_reservas = n())

print("Tendencia de reservas a lo largo del tiempo:")
print(reservas_por_año)

# Gráfico
ggplot(reservas_por_año, aes(x = arrival_date_year, y = total_reservas)) +
  geom_bar(stat = "identity") +
  labs(x = "Año de llegada", y = "Total de reservas", title = "Tendencia de reservas a lo largo del tiempo")

# 3. ¿Cuándo son las temporadas de alta y baja demanda?
temporadas_demandadas <- hotel_data %>%
  mutate(total_noches = stays_in_weekend_nights + stays_in_week_nights) %>%
  group_by(arrival_date_month) %>%
  summarise(promedio_noches = mean(total_noches))

print("Promedio de noches de estancia por mes:")
print(temporadas_demandadas)

# Gráfico
ggplot(temporadas_demandadas, aes(x = arrival_date_month, y = promedio_noches)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes de llegada", y = "Promedio de noches de estancia", title = "Promedio de noches de estancia por mes")

# 4. ¿Cuándo es menor la demanda de reservas?
meses_menos_demandados <- hotel_data %>%
  mutate(total_reservas = stays_in_weekend_nights + stays_in_week_nights) %>%
  group_by(arrival_date_month) %>%
  summarise(total_reservas = n()) %>%
  arrange(total_reservas)

print("Meses con menor demanda de reservas:")
print(head(meses_menos_demandados))

# Gráfico
ggplot(meses_menos_demandados, aes(x = arrival_date_month, y = total_reservas)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes de llegada", y = "Total de reservas", title = "Meses con menor demanda de reservas")

# 5. ¿Cuántas reservas incluyen niños y/o bebés?
reservas_con_niños <- hotel_data %>%
  filter(children > 0 | babies > 0)

num_reservas_con_niños <- nrow(reservas_con_niños)
print("Número de reservas que incluyen niños y/o bebés:")
print(num_reservas_con_niños)

# Gráfico
ggplot() +
  geom_bar(data = data.frame(con_niños = c("Sí", "No"), num_reservas = c(num_reservas_con_niños, nrow(hotel_data) - num_reservas_con_niños)), aes(x = "", y = num_reservas, fill = con_niños), stat = "identity") +
  coord_polar("y") +
  labs(fill = "Incluye niños y/o bebés", x = NULL, y = NULL, title = "Proporción de reservas con niños y/o bebés") +
  theme_void()

# 6. ¿Es importante para los huéspedes contar con espacios de estacionamiento?
espacios_estacionamiento <- hotel_data %>%
  group_by(required_car_parking_spaces) %>%
  summarise(total_reservas = n())

print("Reservas por número de espacios de estacionamiento requeridos:")
print(espacios_estacionamiento)

# Gráfico
ggplot(espacios_estacionamiento, aes(x = required_car_parking_spaces, y = total_reservas)) +
  geom_bar(stat = "identity") +
  labs(x = "Espacios de estacionamiento requeridos", y = "Total de reservas", title = "Reservas por número de espacios de estacionamiento requeridos")

# 7. ¿En qué meses del año se producen más cancelaciones de reservas?
cancelaciones_por_mes <- hotel_data %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(total_cancelaciones = n())

print("Cancelaciones de reservas por mes:")
print(cancelaciones_por_mes)

# Gráfico 
ggplot(cancelaciones_por_mes, aes(x = arrival_date_month, y = total_cancelaciones)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes de llegada", y = "Total de cancelaciones", title = "Cancelaciones de reservas por mes")

#PREGUNTA ADICIONAL
# Calcular la tarifa diaria promedio (ADR) para diferentes tipos de habitaciones
ADR_por_tipo_habitacion <- hotel_data %>%
  group_by(assigned_room_type) %>%
  summarise(ADR_promedio = mean(adr))

print("Tarifa diaria promedio (ADR) por tipo de habitación:")
print(ADR_por_tipo_habitacion)

# Gráfico
ggplot(ADR_por_tipo_habitacion, aes(x = assigned_room_type, y = ADR_promedio)) +
  geom_bar(stat = "identity") +
  labs(x = "Tipo de habitación asignada", y = "Tarifa diaria promedio (ADR)", title = "Tarifa diaria promedio (ADR) por tipo de habitación")


#PRE-PROCESAMIENTO
#identificacion de datos faltantes 
datos_faltantes <- sum(is.na(hotel_data))
#eliminar datos faltantes
hotel_data_limpia <- na.omit(hotel_data)

#datos atipicos(outliers)
outliers <- function(x) {
  if (!is.numeric(x)) {
    stop("La variable no es numérica.")
  }
  x <- na.omit(x) # Eliminar valores NA si los hay
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(x[x < lower_bound | x > upper_bound])
}

# Ejemplo: Detectar outliers en la variable 'adr' (tarifa diaria promedio)
outliers_adr <- outliers(hotel_data$adr)

# Imprimir los valores outliers
print("Valores outliers de la variable 'adr':")
print(outliers_adr)

#escritura de datos limpios
write.csv(hotel_data_limpia, "data_limpia.csv", row.names = FALSE)

# Gráfico de dispersión entre 'lead_time' y 'adr'
ggplot(hotel_data_limpia, aes(x = lead_time, y = adr)) +
  geom_point() +
  ggtitle("Relación entre Lead Time y ADR")

# Gráfico de caja para 'adr'
ggplot(hotel_data_limpia, aes(x = hotel, y = adr)) +
  geom_boxplot() +
  ggtitle("Distribución de ADR por Tipo de Hotel")


