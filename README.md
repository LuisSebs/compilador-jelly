[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/g15yszWF)

# Proyecto: Compilador jelly

<img src="https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExYmN4ODIxNzBrOWM1OXFqOWhkODRjbXRoYXRzdXN3c2YwejNoa2htNyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/3ov9k5wdvgTEm4yY0w/giphy.gif"/>

# Puntos implementados

- **(+7 puntos)** Ejercicio principal: Proceso que convierte un archivo en código **jelly** a un archivo con código **java**.
- **(+2 puntos)** Ejercicio 1: Definicion y uso de Variables globales
- **(+2 puntos)** Ejercicio 4: Script para el compilador de jelly desde la línea de comandos

# Ejecución

El comando para compilar un programa es el siguiente: 

```bash
    racket jelly-compiler.rkt -f <nombre-del-archivo>.jly
```

Al compilar un programa se generara un archivo **java** en la carpeta `java_files/`. Puedes obtener mayor informacion del compilador al ejecutar el siguiente comando:

```bash
    racket jelly-compiler.rkt -h
```

## Ejemplos

En la carpeta `jelly_files/` puedes encontrar ejemplos de archivos **jelly**, para compilarlos ejecuta los siguientes comandos:

```bash
racket jelly-compiler.rkt -f jelly_files/example.jly
```

```bash
racket jelly-compiler.rkt -f jelly_files/example2.jly
```

```bash
racket jelly-compiler.rkt -f jelly_files/variablesGlobales.jly
```


