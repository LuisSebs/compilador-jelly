#lang nanopass
(require "parser.rkt"
          parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRÁCTICA 4 : Syntáxis abstracta
;; Autor: Arrieta Mancera Luis Sebastian
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definición del Árbol de Sintaxis Abstracta
(define (syntax-tree expr)
  (match expr

    [(num v) v] ;; Números CHECK CHECK

    [(id nombre) nombre] ;; Cadenas CHECK CHECK

    [(bool booleano) booleano] ;; Booleanos CHECK CHECK

    [(return e) (string-append "(return " (syntax-tree e) ")")] ;; return CHECK CHECK

    [(auto-incremento nombre)
     (string-append "(= " (syntax-tree nombre) " (+ " (syntax-tree nombre) " 1))")] ;; a++ CHECK CHECK

    [(auto-decremento nombre)
     (string-append "(= " (syntax-tree nombre) " (- " (syntax-tree nombre) " 1))")] ;; a-- CHECK CHECK

    [(op-ternario cond arg1 arg2) ;; Operador ternario CHECK CHECK
     (string-append "(" "? " (syntax-tree cond) " "
                     (syntax-tree arg1) " "       
                     (syntax-tree arg2)
                     ")")]
    
    [(bin-op op arg1 arg2) ;; Operador binario CHECK CHECK
     (string-append "(" (symbol->string op) " "
                     (syntax-tree arg1) " "
                     (syntax-tree arg2)
                     ")")]
    
    [(un-op op arg1) ;; Operador unario CHECK CHECK
     (string-append "(" (symbol->string op) " "
                     (syntax-tree arg1)
                     ")")]
    [(array '()) "(array )"] ;; Arreglo vacio CHECK CHECK
    [(array l) (string-append "(array "(string-join (map syntax-tree l) " ") ")")] ;; Arreglos CHECK CHECK

    [(if-completo cond entonces sino) ;; if completo CHECK CHECK
     (string-append "(if " (syntax-tree cond) " { " 
                           (string-join (map syntax-tree entonces)) " } { "
                           (string-join (map syntax-tree sino)) " })")]
    
    [(if-chiquito cond entonces) ;; if corto CHECK CHECK
     (string-append "(if " (syntax-tree cond) " { "
                    (string-join (map syntax-tree entonces)) " })")]
    
    [(while-loop guardia ten) ;; while CHECK CHECK
     (string-append "(while " (syntax-tree guardia) " { " 
                    (string-join (map syntax-tree ten) " ") " })" )]
    
    [(declaracion t id v) ;; Declaraciones simples CHECK CHECK
     (string-append "(decl " (syntax-tree id) " " (string-downcase (symbol->string t)) " " (syntax-tree v) ")")]

    [(declaracion-mult t l) ;; Declaraciones múltiples CHECK CHECK
     (string-append "(decl-mult " (string-join (map syntax-tree l) " ") " " (string-downcase (symbol->string t)) ")")]

    [(asignacion l) ;; Asignación CHECK CHECK
     (string-append "(= " (string-join (map syntax-tree l) " ") ")")]

    [(asignacion-comp ta v1 v2) ;; Asignaciones compuestas CHECK CHECK
     ;(string-append "(" (symbol->string ta) " " (syntax-tree v1) " "(syntax-tree v2) ")")
     (string-append "(= " (syntax-tree v1) " (" (string (string-ref (symbol->string ta) 0)) " " (syntax-tree v1) " " (syntax-tree v2) "))")]    

    [(llamada-arr nombre pos) ;; Llamadas a arreglos CHECK CHECK
     (string-append "(array-call " nombre " " (syntax-tree pos) ")" )]

    [(funcion nombre '() cuerpo);; Funcion sin parametros CHECK CHECK
     (string-append "(function " nombre " [] { " (string-join (map syntax-tree cuerpo) " ") " })")]

    [(llamada nombre argumentos) ;; Llamadas a funciones CHECK CHECK
     (string-append "(call " nombre " " (string-join (map syntax-tree argumentos) " ") ")")]
    
    [(funcion nombre parametros cuerpo) ;; Funciones CHECK CHECK
     (string-append "(function " nombre " [" (string-join (map syntax-tree parametros) " ") "] { " (string-join (map syntax-tree cuerpo) " ") " })")]
    
    [(metodo nombre parametros regreso cuerpo) ;; Métodos CHECK
     (string-append "(method " nombre " [" (string-join (map syntax-tree parametros) " ") "] " (string-downcase (symbol->string regreso)) " { " (string-join (map syntax-tree cuerpo) " ") " })")]

    [(main cuerpo) ;; Main CHECK CHECH
     (string-append "(main { " (string-join (map syntax-tree cuerpo) " ") " })")]

    ['() ""] ;; Vital para recorrer las listas
    [(list h t) (string-append (syntax-tree h)
                               (syntax-tree t))]

    [(programa lista-instrucciones) ;; CHECK CHECK
     (string-append "(program " (string-join (map syntax-tree lista-instrucciones) " ") ")")]
  )
)

;; Pruebas para syntax-tree
(define (ejecuta-pruebas-st [l pruebas])
  (for/list ([i l])
    (st i)))

;; Ejemplo pdf p4
(define ejemplo-pdf-p4 "
gcd(a:int, b:int):int{
    while (a != 0){
        if (a < b)
            b = b - a
        else
            a = a - b
    }
    return b
}
")

;; Función para probar:
;; Ejemplo: (st "2 + 3")
;; Recibe una cadena y genera su sintax-tree
(define (st str)
  (syntax-tree (parsea str)))