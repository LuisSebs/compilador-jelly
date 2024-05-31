#lang nanopass

(require "type-system.rkt"
          parser-tools/yacc)
(require "symbol-table.rkt"
          parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROYECTO : Traduccion a java
;; Integrantes:
;; - Arrieta Mancera Luis Sebastián
;; - Góngora Ramírez Dania Paula
;; - Martínez Hernández Zuriel Enrique
;; - Villafán Flores María Fernanda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Programa
(define (to-java-program ir)
  (nanopass-case (jelly Programa) ir
                 [(program ,firm* ...) (string-join (map (lambda (v) (to-java-firm v)) firm*) "\n")]))

;; Firma
(define (to-java-firm ir)
  (nanopass-case (jelly Firma) ir
                 [,m (string-append "public static void main(String[] args) {\n" (to-java-m m) "\n}")] ;; CHECK
                 [,meth (to-java-meth meth)]
                 [,fun (to-java-fun fun)]
                 [,dec (string-append "static " (to-java-dec dec) ";")]
                 [,dec-mult (string-append "static " (to-java-dec-mult dec-mult) ";")]))

;; Main
(define (to-java-m ir) ;; CHECK
  (nanopass-case (jelly Main) ir
                 [(main ,block) (to-java-block block)]))

;; Metodo
(define (to-java-meth ir)
  (nanopass-case (jelly Metodo) ir
                 [(method ,i (,arg* ...) ,ty ,block) (string-append "static " (to-java-ty ty) " " (symbol->string i) "(" (string-join (map (lambda (v) (to-java-arg v)) arg*) ", ") "){\n" (to-java-block block) "\n}" )]
                 ))

;; Funcion
(define (to-java-fun ir)
  (nanopass-case (jelly Funcion) ir
                 [(function ,i (,arg* ...) ,block) (string-append "static void " (symbol->string i) "(" (string-join (map (lambda (v) (to-java-arg v)) arg*) ", ") "){\n" (to-java-block block) "\n}")]
                 ))

;; Argumento
(define (to-java-arg ir)
  (nanopass-case (jelly Argumento) ir
                 [(decl ,i ,ty) (string-append (to-java-ty ty) " " (symbol->string i))]
                 ))


;; Bloque
(define (to-java-block ir) ;; CHECK
  (nanopass-case (jelly Bloque) ir
                 [(,stmt* ...) (string-append (string-join (map (lambda (v) (to-java-stmt v)) stmt*) ";\n") ";") ]
                 ))

;; Sentencia
(define (to-java-stmt ir)
  (nanopass-case (jelly Sentencia) ir
                 [,e (to-java-e e)] ;; CHECK
                 [,dec (to-java-dec dec)] ;; CHECK
                 [,dec-mult (to-java-dec-mult dec-mult)] ;; CHECK
                 [,struct (to-java-struct struct)] ;; CHECK
                 [(return ,e) (string-append "return " (to-java-e e))])) ;; CHECK

;; Expresion
(define (to-java-e ir) ;; CHECK
  (nanopass-case (jelly Expr) ir
                 [,c (if (number? c)
                         (number->string c)
                         (if (eq? 'True c)
                             "true"
                             "false"))] ;; CHECK
                 [,i (symbol->string i)] ;; CHECK
                 [,arr (to-java-arr arr)] ;; CHECK
                 [,cal (to-java-cal cal)] ;; CHECK
                 [,cal-arr (to-java-cal-arr cal-arr)] ;; CHECK
                 [,assign (to-java-assign assign)] ;; CHECK
                 [(,ou ,e) (string-append "(" (to-java-ou ou) (to-java-e e) ")")] ;; CHECK
                 [(,ob ,e0 ,e1) (string-append "(" (to-java-e e0) (to-java-ob ob) (to-java-e e1) ")")] ;; CHECK
                 [(? ,e0 ,e1 ,e2) (string-append "(" (to-java-e e0) " ? " (to-java-e e1) " : " (to-java-e e2) ")")] ;; CHECK
                 ))

;; Arreglo
(define (to-java-arr ir) ;; CHECK
  (nanopass-case (jelly Arreglo) ir
                 [(array ,e* ...) (string-append "{" (string-join (map (lambda (v) (to-java-e v)) e*) ", ") "}")]))

;; Llamada a funcion
(define (to-java-cal ir) ;; CHECK
  (nanopass-case (jelly Llamada) ir
                 [(call ,i ,e* ...) (if (primitive-cal? i)
                                        (if (eq? 1 (length e*))
                                            (string-append (string-join (map (lambda (v) (to-java-e v)) e*)) "." (symbol->string i))
                                            (display "Otro caso"))
                                        (string-append (symbol->string i) "(" (string-join (map (lambda (v) (to-java-e v)) e*) ", ") ")"))]))

;; Llamada a arreglo
(define (to-java-cal-arr ir) ;; CHECK
  (nanopass-case (jelly Llamadaarr) ir
                 [(array-call ,i ,e) (string-append (symbol->string i) "[" (to-java-e e) "]")]))

;; Asignacion
(define (to-java-assign ir) ;; CHECK
  (nanopass-case (jelly Asignacion) ir
                 [(= ,e0 ,e1) (string-append (to-java-e e0) " = " (to-java-e e1))]))

;; Declaracion
(define (to-java-dec ir) ;; CHECK
  (nanopass-case (jelly Declaracion) ir
                 [(decl ,i ,ty) (string-append (to-java-ty ty) " " (symbol->string i))]
                 [(decl ,i ,ty ,e) (string-append (to-java-ty ty) " " (symbol->string i) " = " (to-java-e e))]
                 ))

;; Declaracion multiple
(define (to-java-dec-mult ir) ;, CHECK
  (nanopass-case (jelly Declaracionmult) ir
                 [(decl-mult ,i* ... ,ty) (string-append (to-java-ty ty) " " (string-join (map symbol->string i*) ", ") )]
                 ))

;; Estructura
(define (to-java-struct ir) ;; CHECK
  (nanopass-case (jelly Estructura) ir
                 [(if ,e ,block) (string-append "if (" (to-java-e e) "){\n"(to-java-block block)"\n}") ]
                 [(if ,e ,block0 ,block1) (string-append "if (" (to-java-e e) "){\n" (to-java-block block0) "\n} else {\n" (to-java-block block1) "\n}")]
                 [(while ,e ,block) (string-append "while (" (to-java-e e) "){\n" (to-java-block block) "\n}")]
                 ))

;; Tipo
(define (to-java-ty ty)
  (cond 
    [(eq? 'int ty) "int"]
    [(eq? 'boolean ty) "boolean"]
    [(eq? 'int-arr ty) "int[]"]
    [(eq? 'bool-arr) "boolean[]"]))

;; Operador unario
(define (to-java-ou ou)
  (cond
    [(eq? 'not ou) "!"]))

;; Operador binario
(define (to-java-ob ob)
  (cond
    [(eq? 'and ob) "&&"]
    [(eq? 'or ob) "||"]
    [else (symbol->string ob)]))

;; Funciones primitivas
(define (primitive-cal? i)
  (memq i '(length)))

;; Funcion que compila un programa de jelly a java
(define (jelly-compiler path)
  (let* (
         [file-content (file->string path)] ;; Convertimos el contenido del archivo en string
         [parseo (pj file-content)]
         [renombre (rename-var parseo)]
         [tabla (symbol-table renombre (make-hash))])
    (type-check renombre tabla)
    (let* (
           [filename (upper (extract-filename path))]
           [java-file (open-output-file (string-append "java_files/" filename ".java") #:exists 'truncate)])
      (display (set-class filename (to-java-program renombre)) java-file)
      (close-output-port java-file))))

;; Complementa agregando el nombre de la clase a un codigo en jelly
(define (set-class filename code)
  (string-append "public class " filename "{\n" code "\n}"))

;; Extraer el nombre de un archivo
(define (extract-filename file)
  (let* ([re1 #rx"[a-zA-Z0-9]+.jly"]
         [re2 #rx"[a-zA-Z0-9]+"]
         [match1 (regexp-match re1 file)]
         [match2 (regexp-match re2 (first match1))])
    (first match2)))

;; Escribe la primer letra en mayusculas
(define (upper str)
  (string-append (string-upcase (substring str 0 1)) (substring str 1)))

;; Prueba:
(define compilar-example-jly (jelly-compiler "jelly_files/example.jly"))

;;Variables Globales
(define compilar-variablesGlobales-jly (jelly-compiler "jelly_files/variablesGlobales.jly"))

;; Otra prueba
;; (define compilar-example2-jly (jelly-compiler "jelly_files/example2.jly"))