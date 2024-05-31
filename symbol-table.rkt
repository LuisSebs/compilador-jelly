#lang nanopass
(require "syntax-tree.rkt"
          parser-tools/yacc)
(require "parser.rkt"
          parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRÁCTICA 5 : Tabla de simbolos
;; Integrantes:
;; - Arrieta Mancera Luis Sebastián
;; - Góngora Ramírez Dania Paula
;; - Martínez Hernández Zuriel Enrique
;; - Villafán Flores María Fernanda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Da el syntax tree de una cadena del lenguaje jelly
(define (st str)
  (syntax-tree (parsea str)))

;; Tranformar la cadena del syntax tree, lo del syntax tree debe pasar antes aquí
(define (tr str)
  (read (open-input-string str)))

;; Definicion para probar el nuevo parser
(define (pj x)
  (parser-jelly (tr (st x))))

;; Juicios
(define (constante? c) (or (number? c) (memq c '(True False))))
(define (op-binario? ob) (memq ob '(+ - * / % != == < > <= >= and or)))
(define (op-unario? on) (memq on '(not)))
(define (tipo? ty) (memq ty '(int boolean int-arr bool-arr)))
(define (id? i) (symbol? i))

;; Lenguaje jelly
(define-language jelly
  ;; Terminales
  (terminals
   (constante (c))
   (op-binario (ob))
   (op-unario (ou))
   (tipo (ty))
   (id (i)))
  ;; Programa CHECK CHECK
  (Programa (p)
            (program firm* ...)
            e
            struct
            )
  ;; Firma CHECK CHECK
  (Firma (firm)
         m
         meth
         fun
         dec
         dec-mult)
  ;; Main CHECK CHECK
  (Main (m)
        (main block))
  ;; Metodo CHECK CHECK
  (Metodo (meth)
          (method i (arg* ...) ty block))
  ;; Funcion CHECK CHECK
  (Funcion (fun)
           (function i (arg* ...) block))  
  ;; Argumento CHECK CHECK
  (Argumento (arg)
              (decl i ty))
  ;; Bloque CHECK CHECK
  (Bloque (block)
          (stmt* ...))
  ;; Sentencia CHECK CHECK
  (Sentencia (stmt)
             e
             dec
             dec-mult             
             struct
             (return e))
  ;; Estructura CHECK CHECK
  (Estructura (struct)
              (if e block)
              (if e block0 block1)
              (while e block))
  ;; Declaracion CHECK CHECK
  (Declaracion (dec)
               (decl i ty)
               (decl i ty e))
  ;; Declaracion multiple CHECK CHECK
  (Declaracionmult (dec-mult)
                   (decl-mult i* ... ty))
  ;; Asignacion CHECK
  (Asignacion (assign)
              (= e0 e1))
  ;; Expresion CHECK
  (Expr (e)        
        c
        i        
        arr
        cal
        cal-arr
        assign
        (ou e)
        (ob e0 e1)
        (? e0 e1 e2))
  ;; Llamada a funcion CHECK
  (Llamada (cal)
           (call i e* ...))
  ;; Llamada a arreglo CHECK
  (Llamadaarr (cal-arr)
              (array-call i e))
  ;; Arreglo CHECK
  (Arreglo (arr)
           (array e* ...)))

;; Parser Jelly
(define-parser parser-jelly jelly)

;; EJEMPLO PARA PROBAR EL SEGUNDO PARSER
;; (pj program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Funciones para obtener variables   :;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Programa 
(define (vars-p ir)
  (nanopass-case (jelly Programa) ir
                 [(program ,firm* ...) (let ([variables (mutable-set)])
                                         (for-each (lambda (v) (set-union! variables v)) (map vars-firm firm*))
                                         variables)]))

;; Firma
(define (vars-firm ir)
  (nanopass-case (jelly Firma) ir
                 [,m (vars-m m)]
                 [,meth (vars-meth meth)]
                 [,fun (vars-fun fun)]
                 [,dec (vars-dec dec)]
                 [,dec-mult (vars-dec-mult dec-mult)]))

;; Main
(define (vars-m ir) ;; CHECK 
  (nanopass-case (jelly Main) ir
                 [(main ,block) (vars-block block)]))

;; Metodo
(define (vars-meth ir)
  (nanopass-case (jelly Metodo) ir
                 [(method ,i (,arg* ...) ,ty ,block) (let ([variables (mutable-set)])
                                                       (for-each (lambda (v) (set-union! variables v)) (map vars-arg arg*))
                                                       (set-union! variables (vars-block block))
                                                       variables)]))

;; Funcion
(define (vars-fun ir)
  (nanopass-case (jelly Funcion) ir
                 [(function ,i (,arg* ...) ,block) (let ([variables (mutable-set)])
                                                     (for-each (lambda (v) (set-union! variables v)) (map vars-arg arg*))
                                                     (set-union! variables (vars-block block))
                                                     variables)]))
;; Argumento
(define (vars-arg ir)
  (nanopass-case (jelly Argumento) ir
                 [(decl ,i ,ty) (mutable-set i)]))

;; Bloque
(define (vars-block ir) ;; CHECK
  (nanopass-case (jelly Bloque) ir
                 [(,stmt* ...) (let ([variables (mutable-set)])
                                 (for-each (lambda (v) (set-union! variables v)) (map vars-stmt stmt*))
                                 variables)]))

;; Sentencia
(define (vars-stmt ir)
  (nanopass-case (jelly Sentencia) ir
                 [,e (vars-e e)] ;; CHECK
                 [,dec (vars-dec dec)] ;; CHECK
                 [,dec-mult (vars-dec-mult dec-mult)] ;; CHECK
                 [,struct (vars-struct struct)] ;; CHECK
                 [(return ,e) (vars-e e)])) ;; CHECK

;; Expresion
(define (vars-e ir)
  (nanopass-case (jelly Expr) ir
                 [,c (mutable-set)] ;; CHECK
                 [,i (mutable-set i)] ;; CHECK
                 [,arr (vars-arr arr)] ;; CHECK
                 [,cal (vars-cal cal)] ;; CHECK
                 [,cal-arr (vars-cal-arr cal-arr)] ;; CHECK
                 [,assign (vars-assign assign)] ;; CHECK
                 [(,ou ,e) (vars-e e)] ;; CHECK
                 [(,ob ,e0 ,e1) (let ([variables (mutable-set)]) ;; CHECK
                                  (set-union! variables (vars-e e0))
                                  (set-union! variables (vars-e e1))
                                  variables)]
                 [(? ,e0 ,e1 ,e2) (let ([variables (mutable-set)]) ;; CHECK
                                    (set-union! variables (vars-e e0))
                                    (set-union! variables (vars-e e1))
                                    (set-union! variables (vars-e e2))
                                    variables)]))

;; Arreglo
(define (vars-arr ir) ;; CHECK
  (nanopass-case (jelly Arreglo) ir
                 [(array ,e* ...) (let ([variables (mutable-set)])
                                    (for-each (lambda (v) (set-union! variables v)) (map vars-e e*))
                                    variables)]))

;; Llamada a funcion
(define (vars-cal ir) ;; CHECK
  (nanopass-case (jelly Llamada) ir
                 [(call ,i ,e* ...) (let ([variables (mutable-set)])
                                      (for-each (lambda (v) (set-union! variables v)) (map vars-e e*))
                                      variables)]))

;; Llamada a arreglo
(define (vars-cal-arr ir) ;; CHECK
  (nanopass-case (jelly Llamadaarr) ir
                 [(array-call ,i ,e) (let ([variables (mutable-set)])
                                       (set-union! variables (mutable-set i))
                                       (set-union! variables (vars-e e))
                                       variables)]))
;; Asignacion
(define (vars-assign ir) ;; CHECK
  (nanopass-case (jelly Asignacion) ir
                 [(= ,e0 ,e1) (let ([variables (mutable-set)])
                                (set-union! variables (vars-e e0))
                                (set-union! variables (vars-e e1))
                                variables)]))

;; Declaracion
(define (vars-dec ir) ;; CHECK
  (nanopass-case (jelly Declaracion) ir
                 [(decl ,i ,ty) (mutable-set i)]
                 [(decl ,i ,ty ,e) (let ([variables (mutable-set)])
                                     (set-union! variables (mutable-set i))
                                     (set-union! variables (vars-e e))
                                     variables)]))

;; Declaracion multiple
(define (vars-dec-mult ir) ;; CHECK
  (nanopass-case (jelly Declaracionmult) ir
                 [(decl-mult ,i* ... ,ty) (let ([variables (mutable-set)])
                                            (for-each (lambda (v) (set-union! variables (mutable-set v))) i*)
                                            variables)]))

;; Estructura
(define (vars-struct ir) ;; CHECK
  (nanopass-case (jelly Estructura) ir
                 [(if ,e ,block) (let ([variables (mutable-set)])
                                   (set-union! variables (vars-e e))
                                   (set-union! variables (vars-block block))
                                   variables)]
                 [(if ,e ,block0 ,block1) (let ([variables (mutable-set)])
                                         (set-union! variables (vars-e e))
                                         (set-union! variables (vars-block block0))
                                         (set-union! variables (vars-block block1))
                                         variables)]
                 [(while ,e ,block) (let ([variables (mutable-set)])
                                      (set-union! variables (vars-e e))
                                      (set-union! variables (vars-block block))
                                      variables)]))

;; Programa parseado
(define p (parser-jelly '(program
  (main ((decl i int (= zzzz (+ zzzz 1))) (= zzzz (+ zzzz 1)) (decl r int (call gdc i zzz))))
  (method
   gdc
   ((decl var1 int) (decl var2 int))
   int
   ((while (!= var1 0) ((if (< var1 var2) ((= var2 (- var2 var1))) ((= var1 (- var1 var2)))))) (return b)))
  (function
   sort
   ((decl a int-arr))
   ((decl i int 0)
    (decl n int (call length a))
    (while
     (< i n)
     ((decl j int i)
      (while
       (> j 0)
       ((if (> (array-call a (- j 1)) (array-call a j))
          ((decl swap int (array-call a j)) (= (array-call a j) (array-call a (- j 1))) (= (array-call a (- j 1)) swap)))
        (= j (- j 1))))
      (= i (+ i 1)))))))))

;; Variables de todo un programa
(define variables-de-programa (vars-p p))

;; Contador para nombres de variables
(define c 0)

;; Genera un nuevo nombre de variable
(define (nueva)
  (let* ([str-num (number->string c)] [str-sim (string-append "var_" str-num)])
    (set! c (add1 c))
    (string->symbol str-sim)))

;; Renombra las variables de un conjunto de variables
(define (asigna vars)
  (let ([tabla (make-hash)])
    (set-for-each vars
                  (lambda (v) (hash-set! tabla v (nueva))))
    tabla))

;; Renombramos las variables del conjunto generado anteriormente
;;(define variables-de-programa-renombradas (asigna variables-de-programa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Renombre de variables

(define-pass rename-var : jelly (ir) -> jelly ()
  ;; Programa
  (Programa : Programa (ir) -> Programa ()
            [(program ,firm* ...) (let* ([vars (vars-p ir)]
                                         [tabla (asigna vars)])
                                    `(program ,(map (lambda (v) (Firma v tabla)) firm*) ...))])
  ;; Firma
  (Firma : Firma (ir h) -> Firma ()
        [,m `,(Main m h)]
        [,meth `,(Metodo meth h)]
        [,fun `,(Funcion fun h)]
        [,dec `,(Declaracion dec h)]
        [,dec-mult `,(Declaracionmult dec-mult h)])
  ;; Main
  (Main : Main (ir h) -> Main ()
        [(main ,block) `(main ,(Bloque block h))])
  ;; Metodo
  (Metodo : Metodo (ir h) -> Metodo () ;; CHECK
          [(method ,i (,arg* ...) ,ty ,block) `(method ,i (,(map (lambda (v) (Argumento v h)) arg*) ...) ,ty ,(Bloque block h))])
  
  ;; Funcion
  (Funcion : Funcion (ir h) -> Funcion ()
           [(function ,i (,arg* ...) ,block) `(function ,i (,(map (lambda (v) (Argumento v h)) arg*) ...) ,(Bloque block h))])
  ;; Argumento
  (Argumento : Argumento (ir h) -> Argumento ()
             [(decl ,i ,ty) `(decl ,(hash-ref h i) ,ty)])
  ;; Bloque
  (Bloque : Bloque (ir h) -> Bloque ()
          [(,stmt* ...) `(,(map (lambda (v) (Sentencia v h)) stmt*) ...)])
  ;; Sentencia
  (Sentencia : Sentencia (ir h) -> Sentencia ()
             [,e `,(Expr e h)] ;; CHECK
             [,dec `,(Declaracion dec h)] ;; CHECK
             [,dec-mult `,(Declaracionmult dec-mult h)] ;; CHECK
             [,struct `,(Estructura struct h)] ;; CHECK
             [(return ,e) `(return ,(Expr e h))]) ;; CHECK
  ;; Declaracion
  (Declaracion : Declaracion (ir h) -> Declaracion () ;; CHECK
               [(decl ,i ,ty) `(decl ,(hash-ref h i) ,ty)]
               [(decl ,i ,ty ,e) `(decl ,(hash-ref h i) ,ty ,(Expr e h))])
  ;; Declaracion multiple
  (Declaracionmult : Declaracionmult (ir h) -> Declaracionmult () ;; CHECK
                   [(decl-mult ,i* ... ,ty) `(decl-mult ,(map (lambda (v) (hash-ref h v)) i*) ... ,ty)])
  ;; Estructura
  (Estructura : Estructura (ir h) -> Estructura () ;; CHECK
              [(if ,e ,block) `(if ,(Expr e h) ,(Bloque block h))]
              [(if ,e ,block0 ,block1) `(if ,(Expr e h) ,(Bloque block0 h) ,(Bloque block1 h))]
              [(while ,e ,block) `(while ,(Expr e h) ,(Bloque block h))])
  ;; Expresion
  (Expr : Expr (ir h) -> Expr ()
        [,c ir] ;; CHECK
        [,i `,(hash-ref h i)] ;; CHECK
        [,arr `,(Arreglo arr h)] ;; CHECK
        [,cal `,(Llamada cal h)] ;; CHECK
        [,cal-arr `,(Llamadaarr cal-arr h)] ;; CHECK
        [,assign `,(Asignacion assign h)] ;; CHECK
        [(,ou ,e) `(,ou ,(Expr e h))] ;; CHECK
        [(,ob ,e0 ,e1) `(,ob ,(Expr e0 h) ,(Expr e1 h))] ;; CHECK
        [(? ,e0 ,e1 ,e2) `(? ,(Expr e0 h) ,(Expr e1 h) ,(Expr e2 h))]) ;; CHECK
  ;; Arreglo
  (Arreglo : Arreglo (ir h) -> Arreglo () ;; CHECK
           [(array ,e* ...) `(array ,(map (lambda (v) (Expr v h)) e*) ...)])
  ;; Llamada
  (Llamada : Llamada (ir h) -> Llamada () ;; CHECK
           [(call ,i ,e* ...) `(call ,i ,(map (lambda (v) (Expr v h)) e*) ...)])
  ;; Llamada a arreglo
  (Llamadaarr : Llamadaarr (ir h) -> Llamadaarr () ;; CHECK
              [(array-call ,i ,e) `(array-call ,(hash-ref h i) ,(Expr e h))])
  ;; Asignacion
  (Asignacion : Asignacion (ir h) -> Asignacion () ;; CHECK
              [(= ,e0 ,e1) `(= ,(Expr e0 h) ,(Expr e1 h))]))

;; Renombramos las variables de un programa
(define p-renombrado (rename-var p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tabla de simbolos

;; Programa
(define (symbol-table ir tb) ;; CHECK
  (nanopass-case (jelly Programa) ir
                 [(program ,firm* ...) (begin
                                         (for-each (lambda (v) (get-symbol-table-firm v tb)) firm*)
                                         tb) ]))
;; Firma
(define (get-symbol-table-firm ir tb) ;; CHECK
  (nanopass-case (jelly Firma) ir
                 [,m (get-symbol-table-m m tb)] ;; CHECK
                 [,meth (get-symbol-table-meth meth tb)] ;; CHECK
                 [,fun (get-symbol-table-fun fun tb)]
                 [,dec (get-symbol-table-dec dec tb)]
                 [,dec-mult (get-symbol-table-dec-mult dec-mult tb)]))  ;; CHECK

;; Main
(define (get-symbol-table-m ir tb) ;; CHECK
  (nanopass-case (jelly Main) ir
                 [(main ,block) (get-symbol-table-block block tb)]))

;; Metodo
(define (get-symbol-table-meth ir tb) ;; CHECK
  (nanopass-case (jelly Metodo) ir
                 [(method ,i (,arg* ...) ,ty ,block) (begin
                                                       (for-each (lambda (v) (get-symbol-table-arg v tb)) arg*)
                                                       (hash-set! tb i ty) ;; Agregamos el nombre del metodo a la tabla hash
                                                       (get-symbol-table-block block tb))]))
;; Funcion
(define (get-symbol-table-fun ir tb) ;; CHECK
  (nanopass-case (jelly Funcion) ir
                 [(function ,i (,arg* ...) ,block) (begin
                                                     (for-each (lambda (v) (get-symbol-table-arg v tb)) arg*)
                                                     (hash-set! tb i 'unit) ;; Agregamos el nombre de la funcion a la tabla hash con 'unit
                                                     (get-symbol-table-block block tb))]))

;; Argumento
(define (get-symbol-table-arg ir tb) ;; CHECK
  (nanopass-case (jelly Argumento) ir
                 [(decl ,i ,ty) (hash-set! tb i ty)]))

;; Bloque
(define (get-symbol-table-block ir tb) ;; CHECK
  (nanopass-case (jelly Bloque) ir
                 [(,stmt* ...) (for-each (lambda (v) (get-symbol-table-stmt v tb)) stmt*)]))

;; Sentencia
(define (get-symbol-table-stmt ir tb) ;; CHECK
  (nanopass-case (jelly Sentencia) ir
                 [,e tb] ;; CHECK
                 [,dec (get-symbol-table-dec dec tb)] ;; CHECK
                 [,dec-mult (get-symbol-table-dec-mult dec-mult tb)] ;; CHECK
                 [,struct (get-symbol-table-struct struct tb)] ;; CHECK
                 [(return ,e) tb])) ;; CHECK

;; Declaracion
(define (get-symbol-table-dec ir tb) ;; CHECK
  (nanopass-case (jelly Declaracion) ir
                 [(decl ,i ,ty) (hash-set! tb i ty)]
                 [(decl ,i ,ty ,e) (hash-set! tb i ty)]))

;; Declaracion-multiple
(define (get-symbol-table-dec-mult ir tb) ;; CHECK
  (nanopass-case (jelly Declaracionmult) ir
                 [(decl-mult ,i* ... ,ty) (for-each (lambda (v) (hash-set! tb v ty)) i*)]))

;; Estructura
(define (get-symbol-table-struct ir tb) ;; CHECK
  (nanopass-case (jelly Estructura) ir
                 [(if ,e ,block) (begin
                                   (get-symbol-table-block block tb))]
                 [(if ,e ,block0 ,block1) (begin
                                            (get-symbol-table-block block0 tb)
                                            (get-symbol-table-block block1 tb))]
                 [(while ,e ,block) (begin
                                      (get-symbol-table-block block tb))]))

;; Tabla de simbolos de un programa
(define tabla-de-simbolos-p (symbol-table p-renombrado (make-hash)))