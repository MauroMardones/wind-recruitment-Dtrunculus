# Vector con todos los paquetes que necesitas
paquetes <- c(
  "readr", "dplyr", "lubridate", "stringr", "purrr",
  "ggplot2", "tidyr", "gridExtra", "viridis", "scales"
)


purrr::walk(paquetes, library, character.only = TRUE)


# Ruta al directorio
directorio <- "~/IEO/Environmental_data"

# Vector con nombres de archivos
archivos <- c(
  "21405_40638_5028023_WIND_20130101124124_20250723114124.csv",
  "21405_40639_5027023_WIND_20130101124130_20250723114130.csv",
  "21405_40640_5029023_WIND_20130101124134_20250723114134.csv",
  "21405_40641_5030023_WIND_20130101124138_20250723114138.csv"
)

# Construir rutas completas
rutas_completas <- file.path(directorio, archivos)

# Función para leer y limpiar cada archivo
leer_archivo_viento <- function(archivo) {
  readr::read_tsv(archivo, skip = 1,
                  col_names = c("fecha_raw", "velocidad_viento", "direccion_grados"),
                  show_col_types = FALSE) %>%
    mutate(
      fecha_raw = stringr::str_trim(fecha_raw),  # quitar espacios si los hay
      fecha = lubridate::parse_date_time(fecha_raw, orders = "Y m d H", tz = "UTC"),
      velocidad_viento = as.numeric(velocidad_viento),
      direccion_grados = as.numeric(direccion_grados)
    ) %>%
    dplyr::select(fecha, velocidad_viento, direccion_grados)
}

# Leer y combinar todos los archivos
datos_viento <- purrr::map_dfr(rutas_completas, leer_archivo_viento)
glimpse(datos_viento)



datos_limpios <- datos_viento %>%
  filter(!is.na(fecha),
         !is.na(velocidad_viento),
         !is.na(direccion_grados),
         velocidad_viento != -999.9,
         direccion_grados != -999.9)

# Paso 1: Clasificar

### Grafico de Rosa de Vientos

# Crear la clasificación de sectores con grados
sectores_clasificacion <- data.frame(
  sector = c("Norte", "Noreste", "Este", "Sureste", "Sur", "Suroeste", "Oeste", "Noroeste"),
  grados_inicio = c(337.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5),
  grados_fin = c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5),
  tipo_viento = c("Otro", "Otro", "Levante", "Levante", "Otro", "Otro", "Poniente", "Otro"),
  # Valor constante para mostrar todos los sectores igual
  valor = 1,
  # Etiquetas con los grados
  etiqueta_grados = c("337.5°-22.5°", "22.5°-67.5°", "67.5°-112.5°", "112.5°-157.5°", 
                      "157.5°-202.5°", "202.5°-247.5°", "247.5°-292.5°", "292.5°-337.5°")
) %>%
  mutate(
    sector = factor(sector, levels = c("Norte", "Noreste", "Este", "Sureste", 
                                       "Sur", "Suroeste", "Oeste", "Noroeste")),
    color_sector = case_when(
      tipo_viento == "Levante" ~ "#E74C3C",   # Rojo
      tipo_viento == "Poniente" ~ "#3498DB",  # Azul  
      TRUE ~ "#95A5A6"                        # Gris
    )
  )

rosa_polar_clasificacion <- ggplot(sectores_clasificacion, aes(x = sector, y = valor, fill = tipo_viento)) +
  geom_col(width = 0.9, color = "white", size = 1) +
  scale_fill_manual(
    values = c("Levante" = "#E74C3C", "Poniente" = "#3498DB", "Otro" = "#95A5A6"),
    name = "Clasificación"
  ) +
  # Añadir etiquetas con los grados
  geom_text(aes(label = paste0(sector, "\n", etiqueta_grados)), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3, fontface = "bold", lineheight = 0.8) +
  labs(
    title = "Rosa de Vientos - Clasificación por Sectores",
    subtitle = "Levante: Sur (157.5°-202.5°) y Sureste (112.5°-157.5°) | Poniente: Oeste (247.5°-292.5°)",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  coord_polar(theta = "x", start = -pi/8, direction = 1) +
  theme(
    panel.grid.major.x = element_line(color = "lightblue", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "lightblue", linetype = "dotted"),
    axis.text.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 1.3))

# Mostrar rosa polar
rosa_polar_clasificacion



# Ahora clasificar
datos_clasificados <- datos_limpios %>%
  mutate(
    tipo_viento = case_when(
      velocidad_viento >= 4 & velocidad_viento <= 40 &
        direccion_grados >= 67.5 & direccion_grados <= 157.5 ~ "Levante",
         velocidad_viento >= 8 & velocidad_viento <= 40 &
        direccion_grados >= 247.5 & direccion_grados <= 292.5 ~ "Poniente",
      
      TRUE ~ "Otro"
    )
  )


# Transformar a velocidad media para histograma (clasificación por intervalos)
datos_vel <- datos_clasificados %>%
  filter(!is.na(velocidad_viento)) %>%
  mutate(clase_velocidad = cut(
    velocidad_viento,
    breaks = seq(0, 24, by = 3),
    include.lowest = TRUE,
    right = FALSE
  )) %>%
  count(clase_velocidad) %>%
  mutate(porcentaje = 100 * n / sum(n))

# Clasificar direcciones de viento en puntos cardinales
puntos_cardinales <- function(grados) {
  case_when(
    grados >= 337.5 | grados < 22.5 ~ "N",
    grados >= 22.5 & grados < 67.5 ~ "NE",
    grados >= 67.5 & grados < 112.5 ~ "E",
    grados >= 112.5 & grados < 157.5 ~ "SE",
    grados >= 157.5 & grados < 202.5 ~ "S",
    grados >= 202.5 & grados < 247.5 ~ "SW",
    grados >= 247.5 & grados < 292.5 ~ "W",
    grados >= 292.5 & grados < 337.5 ~ "NW"
  )
}

datos_dir <- datos_clasificados %>%
  filter(!is.na(direccion_grados)) %>%
  mutate(direccion_cardinal = puntos_cardinales(direccion_grados)) %>%
  count(direccion_cardinal) %>%
  mutate(porcentaje = 100 * n / sum(n)) %>%
  mutate(direccion_cardinal = factor(direccion_cardinal, 
                                     levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))



# Gráfico 1: Velocidad media
g1 <- ggplot(datos_vel, aes(x = clase_velocidad, y = porcentaje)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Velocidad Media (m/s)", y = "Frecuencia %") +
  theme_minimal(base_size = 13)


# Definir tipo de viento por punto cardinal
datos_dir <- datos_clasificados %>%
  filter(!is.na(direccion_grados)) %>%
  mutate(
    direccion_cardinal = puntos_cardinales(direccion_grados),
    tipo_viento = case_when(
      direccion_cardinal %in% c("E", "SE") ~ "Levante",
      direccion_cardinal %in% c( "W") ~ "Poniente",
      TRUE ~ "Otro"
    )
  ) %>%
  count(direccion_cardinal, tipo_viento) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    direccion_cardinal = factor(
      direccion_cardinal,
      levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    )
  )

# Nuevo gráfico 2: Dirección cardinal con colores por tipo de viento
g2 <- ggplot(datos_dir, aes(x = direccion_cardinal, 
                            y = porcentaje, 
                            fill = tipo_viento)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Dirección de Proce", y = "Frecuencia %") +
  scale_fill_manual(values = c("Levante" = "#d62728", "Poniente" = "#1f77b4")) +
  coord_polar(theta = "x") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

g1 + g2


# Filtramos solo Levante y Poniente
datos_eventos <- datos_clasificados %>%
  filter(tipo_viento %in% c("Levante", "Poniente")) %>%
  arrange(fecha) %>%
  group_by(tipo_viento) %>%
  mutate(
    dif_hora = as.numeric(difftime(fecha, lag(fecha), units = "hours")),
    nueva_serie = is.na(dif_hora) | dif_hora > 1,
    grupo_evento = cumsum(nueva_serie)
  ) %>%
  ungroup()

# Calcular duración y características por evento
resumen_eventos <- datos_eventos %>%
  group_by(tipo_viento, grupo_evento) %>%
  summarise(
    inicio = min(fecha),
    fin = max(fecha),
    duracion_dias = as.numeric(difftime(fin, inicio, units = "days")) + 1,
    velocidad_media_episodio = mean(velocidad_viento, na.rm = TRUE),
    velocidad_maxima = max(velocidad_viento, na.rm = TRUE),
    .groups = "drop"
  )
grafico_duracion_intensidad <- ggplot(resumen_eventos,
                                      aes(x = duracion_dias, y = velocidad_media_episodio,
                                          color = tipo_viento, size = velocidad_maxima)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("Levante" = "#E74C3C", "Poniente" = "#3498DB")) +
  scale_size_continuous(name = "Vel. Máxima\n(m/s)", range = c(1, 8)) +
  labs(
    title = "Relación Duración vs Intensidad de Eventos",
    subtitle = "Tamaño del punto = velocidad máxima del episodio",
    x = "Duración (días)",
    y = "Velocidad Media del Episodio (m/s)",
    color = "Tipo de Viento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  ) +
  facet_wrap(~tipo_viento)

# Mostrar gráfico
grafico_duracion_intensidad



# Crear año, mes, y día como columnas separadas
resumen_dias <- datos_clasificados %>%
  mutate(
    fecha_dia = as_date(fecha),
    año = year(fecha),
    mes = month(fecha, label = TRUE, abbr = TRUE)
  ) %>%
  filter(tipo_viento %in% c("Levante", "Poniente")) %>%
  group_by(año, mes, tipo_viento, fecha_dia) %>%
  summarise(.groups = "drop") %>%
  count(año, mes, tipo_viento, name = "dias_con_evento")

ggplot(resumen_dias, aes(x = mes, y = dias_con_evento, fill = tipo_viento)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~año, ncol = 3) +
  scale_fill_manual(values = c("Levante" = "red", "Poniente" = "blue")) +
  labs(
    title = "Días con eventos de Levante y Poniente por mes y año",
    x = "Mes", y = "Días con evento de viento",
    fill = "Tipo de viento"
  ) +
  theme_minimal(base_size = 14)



grafico_barras_mes <- ggplot(resumen_dias, aes(x = mes, y = dias_con_evento,
                                               fill = tipo_viento)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Levante" = "#E74C3C", "Poniente" = "#3498DB")) +
  labs(
    title = "Eventos de Levante y Poniente por Mes",
    x = "",
    y = "Dias con eventos",
    fill = "Tipo de Viento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  facet_wrap(~año)+
  geom_text(aes(label = dias_con_evento), position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3)

# Mostrar el gráfico
grafico_barras_mes


# Separar data para Levante y Poniente
levante_data <- resumen_dias %>% filter(tipo_viento == "Levante")
poniente_data <- resumen_dias %>% filter(tipo_viento == "Poniente")

# Heatmap Levante
p1 <- ggplot(levante_data, aes(x = mes, y = factor(año), fill = dias_con_evento)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Días\nLevante", option = "B", direction = -1) +
  labs(title = "Eventos de Levante", x = "Mes", y = "Año") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

# Heatmap Poniente
p2 <- ggplot(poniente_data, aes(x = mes, y = factor(año), fill = dias_con_evento)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Días\nPoniente", option = "E", direction = -1) +
  labs(title = "Eventos de Poniente", x = "Mes", y = "Año") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

egg::ggarrange(p1, p2, ncol = 2)



# Crear columna con fecha mensual (usamos el primer día del mes)
serie_mensual <- resumen_dias %>%
  mutate(
    mes_num = match(mes, month.abb),  # Convertir mes abreviado a número
    año_mes = as.Date(paste(año, mes_num, "01", sep = "-"))
  )

# Crear el gráfico
grafico_lineas_mensual <- ggplot(serie_mensual, aes(x = año_mes, y = dias_con_evento, color = tipo_viento)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Levante" = "#E74C3C", "Poniente" = "#3498DB")) +
  labs(
    title = "Evolución Temporal de Eventos",
    subtitle = "Serie mensual de episodios de Levante y Poniente",
    x = "Fecha",
    y = "Número de Eventos por Mes",
    color = "Tipo de Viento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months")

# Mostrar el gráfico
grafico_lineas_mensual


# PASO 9: Preparar datos para serie anual
# Agrupar por año y tipo de viento
serie_anual <- resumen_dias %>%
  group_by(año, tipo_viento) %>%
  summarise(dias_con_evento = sum(dias_con_evento), .groups = "drop")

# Crear gráfico de líneas anual
grafico_lineas_anual <- ggplot(serie_anual, aes(x = año, y = dias_con_evento, color = tipo_viento)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Levante" = "#E74C3C", "Poniente" = "#3498DB")) +
  labs(
    title = "Evolución Anual de Eventos",
    subtitle = "Total de días con episodios de Levante y Poniente por año",
    x = "Año",
    y = "Número de Días con Evento",
    color = "Tipo de Viento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "top"
  ) +
  scale_x_continuous(breaks = unique(serie_anual$año))

# Mostrar gráfico
grafico_lineas_anual



#### INDICE LEVANTE

datos_clasificados2 <- datos_clasificados %>%
  mutate(es_levante = if_else(tipo_viento == "Levante", 1, 0))

# Agregar columnas año y mes
datos_clasificados2 <- datos_clasificados2 %>%
  mutate(
    año = year(fecha),
    mes = month(fecha)
  )

# Índice mensual Levante (proporción de horas Levante)
indice_levante_mensual <- datos_clasificados2 %>%
  group_by(año, mes) %>%
  summarise(
    horas_levante = sum(es_levante),
    total_horas = n(),
    indice_levante = horas_levante / total_horas
  ) %>%
  ungroup()

# Índice anual Levante (proporción de horas Levante)
indice_levante_anual <- datos_clasificados2 %>%
  group_by(año) %>%
  summarise(
    horas_levante = sum(es_levante),
    total_horas = n(),
    indice_levante = horas_levante / total_horas
  ) %>%
  ungroup()




# Gráfico mensual
grafico_mensual <- ggplot(indice_levante_mensual, aes(x = as.Date(paste(año, mes, "01", sep = "-")), y = indice_levante)) +
  geom_col(fill = "#E74C3C", alpha = 0.7) +
  geom_line(color = "black", size = 1) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Índice Mensual de Levante",
    x = "Fecha",
    y = "Índice Levante (proporción de horas)",
    caption = "Índice adimensional entre 0 y 1"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafico_mensual




