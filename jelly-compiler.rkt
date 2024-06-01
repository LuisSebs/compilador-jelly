#! /usr/bin/env racket
#lang nanopass

(require "proyecto.rkt"
          parser-tools/yacc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Puntos Extra : Script para el compilador jelly
;; Autor: Arrieta Mancera Luis Sebastian
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; filename-param es el nombre de un archivo jelly a compilar, valor por default: ""
(define filename-param (make-parameter ""))

(define parser
  (command-line
   #:usage-help
   "Este script de Racket está diseñado para compilar programas escritos en el lenguaje Jelly y generar archivos en Java. A través de una única instrucción en la línea de comandos, el script ejecuta todo el proceso de compilación, implementando los conceptos y técnicas aprendidos a lo largo del curso de compiladores."

   #:once-each
   [("-f" "--filename") FILENAME
    "FILENAME establece el nombre del archivo jelly a compilar, e.g. <nombre-del-archivo>.jly"
    (filename-param FILENAME)]

   #:args () (void)))

;; Procesa los argumentos de la linea de comandos
(define (main args)
  (if (eq? "" args)
      (display "Ingresa el nombre de un archivo jelly")
      (begin
        (jelly-compiler args)
        (printf "~a\n" "Successful build ʕ•ᴥ•ʔ."))))

;; Ejecutamos la funcion principal con los argumentos de la linea de comandos;;
(main (filename-param))