# Análisis del Viento de Levante y su relacion con variables poblacionales de coquina *Donax truncukus* en el Parque de Doñana, Costa de Andalucía

Un análisis comprehensive del fenómeno meteorológico del viento de Levante y su impacto en variables poblacionales de coquina *Donax trunculus*.

## Descripción

Este proyecto presenta un análisis detallado del viento de Levante, un fenómeno meteorológico característico del sur de España, específicamente en la zona de El Puerto de Santa María. El estudio combina datos meteorológicos, análisis estadístico y documentación histórica para proporcionar una comprensión integral de este importante factor climático regional.

##  Estructura del Proyecto

```
├── apa.csl                             # Estilo de citación APA
├── code/                               # Scripts y código de análisis
├── data/                               # Datos meteorológicos y datasets
├── docs/                               # Documentación adicional
├── Donax.bib                           # Referencias bibliográficas
├── figures/                            # Gráficos y visualizaciones
├── Levante_Analysys.Rmd                # Código fuente R Markdown
├── README.md                           # Este archivo
├── titulo.sty                          # Estilo personalizado LaTeX
└── wind-recruitment-Dtrunculus.Rproj   # Proyecto R
```

### Paquetes de R requeridos:
```r
paquetes <- c(
  "readr", "dplyr", "lubridate", "stringr", "purrr",
  "ggplot2", "tidyr", "gridExtra", "viridis", "scales",
  "formatR"
)

purrr::walk(paquetes, library, character.only = TRUE)
```

### Compilar el documento completo:

   ```r
   # En la consola de R
   rmarkdown::render("Levante_Analysys.Rmd")
   ```

## Contenido del Análisis

- **Caracterización meteorológica** del viento de Levante
- **Análisis temporal** y patrones estacionales
- **Impacto en la actividad pesquera** y marisquera
- **Correlaciones** con otros factores ambientales
- **Visualizaciones** y mapas de distribución
- **Modelado predictivo** y tendencias

## Datos

Los datos utilizados en este análisis incluyen:
- Series temporales meteorológicas (2013-2015)
- Registros históricos de viento
- Datos de temperatura y presión atmosférica
- Información oceanográfica complementaria
- Variables poblacionales del proyecto FEMP 04.

## Estilo y Formato

El documento utiliza:
- **Estilo APA** para citaciones (`apa.csl`)
- **Estilos LaTeX personalizados para portada de reporte autocontenido** `Levante_Analysys.pdf` (`titulo.sty`, `titulo2.sty`)
- **Formato académico** profesional

## Contribuciones

1. Fork el repositorio
2. Crea una rama para tu feature (`git checkout -b feature/tunombreproy`)
3. Commit tus cambios (`git commit -m 'Add some tunombreproy'`)
4. Push a la rama (`git push origin feature/tunombreproy`)
5. Abre un Pull Request


## Contacto

Para preguntas o colaboraciones, por favor contacta a través de los issues del repositorio.
