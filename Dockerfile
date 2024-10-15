# Usa la imagen base de Shiny
FROM rocker/shiny:latest

# Instalar paquetes adicionales si es necesario
RUN R -e "install.packages(c('shiny', 'plotly', 'shinydashboard', 'ggplot2', 'dplyr', 'reshape2', 'tidyr', 'RColorBrewer', 'viridis', 'data.table', 'paletteer', 'openxlsx', 'DT'), dependencies=TRUE)"

# Copia todos los archivos al contenedor
COPY ./ /srv/shiny-server/

# Exponer el puerto predeterminado
EXPOSE 3838

# Comando para iniciar el servidor Shiny
CMD ["/usr/bin/shiny-server"]

