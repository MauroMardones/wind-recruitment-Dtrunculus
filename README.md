# Análisis del Viento de Levante en el Parque de Doñana, Andalucía

Un análisis comprehensive del fenómeno meteorológico del viento de Levante y su impacto en variables poblacionales de coquina *Donax trunculus*.

## Descripción

Este proyecto presenta un análisis detallado del viento de Levante, un fenómeno meteorológico característico del sur de España, específicamente en la zona de El Puerto de Santa María. El estudio combina datos meteorológicos, análisis estadístico y documentación histórica para proporcionar una comprensión integral de este importante factor climático regional.

##  Estructura del Proyecto

```
├── apa.csl                              # Estilo de citación APA
├── code/                                # Scripts y código de análisis
├── data/                                # Datos meteorológicos y datasets
├── docs/                                # Documentación adicional
├── Donax.bib                           # Referencias bibliográficas
├── figures/                            # Gráficos y visualizaciones
├── Levante_Analysys.Rmd                # Código fuente R Markdown
├── README.md                           # Este archivo
├── titulo.sty                          # Estilo personalizado LaTeX
└── wind-recruitment-Dtrunculus.Rproj   # Proyecto R
```

## 🔧 Requisitos

### Software necesario:
- **R** (versión 4.0 o superior)
- **RStudio** (recomendado)
- **LaTeX** (MiKTeX, TeX Live, o MacTeX)
- **Pandoc** (incluido con RStudio)

### Paquetes de R requeridos:
```r
paquetes <- c(
  "readr", "dplyr", "lubridate", "stringr", "purrr",
  "ggplot2", "tidyr", "gridExtra", "viridis", "scales",
  "formatR"
)

purrr::walk(paquetes, library, character.only = TRUE)
```

## 🚀 Uso

### Compilar el documento completo:

1. **Abrir el proyecto en RStudio:**
   ```bash
   # Abrir el archivo .Rproj
   wind-recruitment-Dtrunculus.Rproj
   ```

2. **Compilar el documento R Markdown:**
   ```r
   # En la consola de R
   rmarkdown::render("Levante_Analysys.Rmd")
   ```

### Ejecutar análisis específicos:
```r
# Cargar scripts individuales desde la carpeta code/
source("code/data_processing.R")
source("code/analysis.R")
source("code/visualization.R")
```

## 📊 Contenido del Análisis

- **Caracterización meteorológica** del viento de Levante
- **Análisis temporal** y patrones estacionales
- **Impacto en la actividad pesquera** y marisquera
- **Correlaciones** con otros factores ambientales
- **Visualizaciones** y mapas de distribución
- **Modelado predictivo** y tendencias

## 📁 Datos

Los datos utilizados en este análisis incluyen:
- Series temporales meteorológicas (2013-2015)
- Registros históricos de viento
- Datos de temperatura y presión atmosférica
- Información oceanográfica complementaria

> **Nota:** Los datos específicos se encuentran en la carpeta `data/` y están documentados individualmente.

## 📖 Documentación

- **Documento principal:** `Levante_Analysys.pdf`
- **Código fuente:** `Levante_Analysys.Rmd`
- **Figuras:** Directorio `figures/`
- **Referencias:** `Donax.bib`

## 🎨 Estilo y Formato

El documento utiliza:
- **Estilo APA** para citaciones (`apa.csl`)
- **Estilos LaTeX personalizados** (`titulo.sty`, `titulo2.sty`)
- **Formato académico** profesional

## Contribuciones

1. Fork el repositorio
2. Crea una rama para tu feature (`git checkout -b feature/AmazingFeature`)
3. Commit tus cambios (`git commit -m 'Add some AmazingFeature'`)
4. Push a la rama (`git push origin feature/AmazingFeature`)
5. Abre un Pull Request


## 📧 Contacto

Para preguntas o colaboraciones, por favor contacta a través de los issues del repositorio.

---

## Actualización del Documento

Para mantener el análisis actualizado:

```bash
# Actualizar datos
Rscript code/update_data.R

# Recompilar documento
Rscript -e "rmarkdown::render('Levante_Analysys.Rmd')"
```

*Última actualización: $(date)*