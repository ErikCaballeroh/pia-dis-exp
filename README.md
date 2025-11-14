# Aplicación Shiny
Esta es una aplicación web interactiva creada con R Shiny.

## Estructura del Proyecto

```
.
├── app.R      # Archivo principal de la aplicación
├── server.R   # Lógica del servidor
└── ui.R       # Interfaz de usuario
```

## Requisitos

- R (versión 4.0.0 o superior)
- Paquetes de R:
  - shiny

## Instalación

1. Clona este repositorio:
```bash
git clone https://github.com/ErikCaballeroh/pia-dis-exp.git
```

2. Instala los paquetes necesarios en R:
```R
install.packages("shiny")
```

## Uso

Para ejecutar la aplicación:

1. Abre R o RStudio
2. Establece el directorio de trabajo en la carpeta del proyecto
3. Ejecuta:
```R
shiny::runApp("app")
```