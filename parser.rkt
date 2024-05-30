#lang nanopass
(require "lexer.rkt"
          parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRÁCTICA 3 : Parser
;; Integrantes:
;; - Arrieta Mancera Luis Sebastián
;; - Góngora Ramírez Dania Paula
;; - Martínez Hernández Zuriel Enrique
;; - Villafán Flores María Fernanda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2+3

;;     +
;;    / \
;;   2   +
;;      / \
;;     3   4

;; (+ 2 (+ 3 4))


;;     expr
;;     / | \
;;  expr + expr
;;   |      |
;;  const  const
;;   |      |
;;   2      3


;; Definición de Estructuras
(define-struct num (v) #:transparent)
(define-struct id (i) #:transparent)
(define-struct bool (b) #:transparent)
(define-struct array (l) #:transparent)
(define-struct return (e) #:transparent)
(define-struct bin-op (op v1 v2) #:transparent)
(define-struct un-op (un-op v1) #:transparent)
(define-struct if-completo (guardia ten elze) #:transparent)
(define-struct if-chiquito (guardia ten) #:transparent)
(define-struct auto-incremento (nombre) #:transparent)
(define-struct auto-decremento (nombre) #:transparent)
(define-struct while-loop (guarda c) #:transparent)
(define-struct op-ternario (cond v1 v2) #:transparent)
(define-struct declaracion (t id v) #:transparent)
(define-struct declaracion-mult (t l) #:transparent)
(define-struct asignacion (l) #:transparent)
(define-struct asignacion-comp (ta v1 v2) #:transparent)
(define-struct llamada (nombre argumentos) #:transparent)
(define-struct llamada-arr (nombre pos) #:transparent)
(define-struct funcion (nombre parametros cuerpo) #:transparent)
(define-struct metodo (nombre parametros regreso cuerpo) #:transparent)
(define-struct main (cuerpo) #:transparent)
(define-struct programa (lista-instrucciones) #:transparent)

;; Definición del Parser
(define jelly-parser
    (parser
        [start program]
        [end EOF]
        [tokens contenedores vacios]
        [error (lambda (tok-ok? tok-name tok-value)
                 (raise-syntax-error 'error
                                     "no fue posible procesar un token"
                                     (if tok-value tok-value tok-name)))]
        
        ;; Para esto se utilizó la tabla del pdf de la práctica
        [precs  (nonassoc LP RP LC RC Q DDOT NOT)
                (left ELSE)
                (left OR)
                (left AND)
                (left EQ DIF)
                (left MAX MIN MAXEQ MINEQ)
                (left ADD SUBS)
                (left MULT DIV MOD)
                ]

        [grammar

         ;; Una constante puede ser:
         [const
          [(const-num) $1]  ;; Constante numérica
          [(const-bool) $1] ;; Constante booleana
          ]
         
         ;; Constante numérica
         [const-num
          [(NUM) (num $1)]
          ]
         ;; Constante booleana
         [const-bool
          [(BOOL) (bool $1)]
          ]

         ;; Variable
         [var
          [(ID) (id $1)]
          ]

         ;; Estructuras de control
         [struct
           [(WHILE condicion stmt)(while-loop $2 (list $3))] ;; while sin corchetes
           [(WHILE condicion bloque)(while-loop $2 $3)] ;; while con corchetes
           [(IF condicion stmt) (if-chiquito $2 (list $3))]  ;; if sin corchetes
           [(IF condicion bloque) (if-chiquito $2 $3)]  ;; if con corchetes
           [(IF condicion stmt ELSE stmt ) (if-completo $2 (list $3) (list $5))] ;; if sc else sc
           [(IF condicion bloque ELSE bloque ) (if-completo $2 $3 $5)] ;; if cc else cc
           [(IF condicion stmt ELSE bloque ) (if-completo $2 (list $3) $5)] ;; if sc else cc
           [(IF condicion bloque ELSE stmt ) (if-completo $2 $3 (list $5))] ;; if cc else sc
           ]

          ;; condicion
          [condicion
            [(val) $1]
          ]
         
         ;; return puede regresa un valor
         [ret 
          [(RETURN val) (return $2)]
          ]

         ;; Una sentencia puede ser:
         [stmt
          [(struct) $1]              ;; Estructura de control
          [(ret) $1]                 ;; return
          [(dec) $1]                 ;; Declaración
          [(dec-mult) $1]            ;; Declaracion multiple
          [(assign) (asignacion $1)] ;; Asignación múltiple
          [(assign-comp) $1]         ;; Asignación compuesta
          [(val) $1]                 ;; Valor
          ]

         ;; Asignación
         [assign
          [(val ASSIGN assign) (list* $1 $3)] ;; Asignación múltiple
          [(val ASSIGN val) (list $1 $3)]     ;; Asignación
          ]

         ;; Asignación compuesta
         [assign-comp
          [(var INCCOMP val) (asignacion-comp '+= $1 $3)] ;; +=
          [(var DECCOMP val) (asignacion-comp '-= $1 $3)] ;; -=
          ]

         ;; Un valor puede ser:
         [val
          [(expr-bool) $1]                  ;; Expresión booleana
          [(expr-num) $1]                   ;; Expresión numérica
          [(arr) $1]                        ;; Arreglo
          [(var INCR) (auto-incremento $1)] ;; Un autoincremento
          [(var DECR) (auto-decremento $1)] ;; Un autodecremento
          [(var) $1]                        ;; Variable          
          [(call-arr) $1]                   ;; Llamada-arreglo
          [(op-tern) $1]                    ;; Operador ternario          
          [(LP val RP) $2]                  ;; Valor entre paréntesis
          [(call) $1]                       ;; Llamada
          ]

         ;; Arreglo
         [arr
          [(LC RC) (array '())]
          [(LC elems RC) (array $2)]
          ]

         ;; Elementos de un arreglo
         [elems
          [(val COMMA elems) (list* $1 $3)]
          [(val) (list $1)]
          ]

         ;; Llamada a función
         [call
          [(ID LP RP) (llamada $1 '())]
          [(ID LP args RP) (llamada $1 $3)]
          ]

         ;; Llamada a un arreglo
         [call-arr
          [(ID LB val RB) (llamada-arr $1 $3)]
          ]

         ;; Argumentos
         [args
          [(val COMMA args) (list* $1 $3)]
          [(val) (list $1)]
          ]

         ;; Operador ternario
         [op-tern
          [(val Q val DDOT val) (op-ternario $1 $3 $5)]
          ]

         ;; Una expresión booleana puede ser:
         [expr-bool
          [(val EQ val) (bin-op '== $1 $3)]    ;; ==
          [(val DIF val) (bin-op '!= $1 $3)]   ;; !=
          [(val AND val) (bin-op 'and $1 $3)]  ;; and
          [(val OR val) (bin-op 'or $1 $3)]    ;; or
          [(val MAX val) (bin-op '> $1 $3)]    ;; >
          [(val MIN val) (bin-op '< $1 $3)]    ;; <
          [(val MAXEQ val) (bin-op '>= $1 $3)] ;; >=
          [(val MINEQ val) (bin-op '<= $1 $3)] ;; <=
          [(NOT val) (un-op 'not $2)]          ;; not
          [(const-bool) $1]                    ;; constante booleana
          ]                   

         ;; Una expresión numérica puede ser:
         [expr-num
          [(val ADD val) (bin-op '+ $1 $3)]  ;; +
          [(val SUBS val) (bin-op '- $1 $3)] ;; -
          [(val MULT val) (bin-op '* $1 $3)] ;; *
          [(val DIV val) (bin-op '/ $1 $3)]  ;; /
          [(val MOD val) (bin-op '% $1 $3)]  ;; %
          [(const-num) $1]                   ;; constante numérica
          ]

         ;; Bloque de código: {a++ b++ r--}
         [bloque
          [(LC lineas RC) $2]
          ]

         [lineas
          [(stmt lineas) (list* $1 $2)] ;; Sentencias
          [(stmt) (list $1)]            ;; Sentencia
          [() empty]
          ]

         ;; Parámetros
         [params
          [(dec COMMA params) (list* $1 $3)] ;; Múltiples
          [(dec) (list $1)]                  ;; Único
          ]
         
         ;; Declaración simple
         [dec
          [(var DDOT typ) (declaracion $3 $1 empty)]          ;; Declaración sin asignación
          [(var DDOT typ ASSIGN val) (declaracion $3 $1 $5)]  ;; Declaración con asignación
          ]

         ;; Declaración múltiple
         [dec-mult
          [(typ mult-dec) (declaracion-mult $1 $2)]
          ]
         
         [mult-dec
          [(var COMMA mult-dec) (list* $1 $3)]
          [(var) (list $1)]
          ]

         ;; Tipo de dato
         [typ
          [(INT)    'INT]             ;; Entero
          [(BOOLEAN)   'BOOLEAN]      ;; Booleano
          [(INT LB RB) 'INT-ARR]      ;; Arreglo de enteros
          [(BOOLEAN LB RB) 'BOOL-ARR] ;; Arreglo de booleanos
          ]

         ;; Métodos
         [meth
          [(ID LP RP DDOT typ bloque) (metodo $1 empty $5 $6)]
          [(ID LP params RP DDOT typ bloque) (metodo $1 $3 $6 $7)]
          ]

         ;; Funciones
         [func
          [(ID LP RP bloque) (funcion $1 empty $4)]
          [(ID LP params RP bloque) (funcion $1 $3 $5)]          
          ]

         ;; Main
         [m
          [(MAIN bloque) (main $2)]
          ]

         ;; Procedimientos
         [proc
          [(func) $1] ;; Funciones          
          [(meth) $1] ;; Métodos
          ]

         ;; Programa es:
         [program ;; Una lista de procedimientos con un main
          [(m list-procs) (programa (append (list $1) $2))]
          [(list-procs m list-procs) (programa (append $1 (list $2) $3))]
          [(list-procs m) (programa (append $1 (list $2)))]
          ]

         [list-procs
          [(proc list-procs) (list* $1 $2)]
          [(proc) (list $1)]
          [() empty]
          ]
         ]
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             EJEMPLOS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arreglos
(define arr1 "main{ {} }")
(define arr2 "main{ {1,2,3,4,5} }")
(define arr3 "main{ {1,True,a} }")

;; Expresiones booleanas
(define bool1 "main{ True }")
(define bool2 "main{ True == False }")
(define bool3 "main{ (True == False) }")
(define bool4 "main{ (True == False) != False }")
(define bool5 "main{ False & (True | False) }")
(define bool6 "main{ (False != True) | (True == False) }")
(define bool7 "main{ (False != True) & !(True == False) | (False) }")

;; Expresiones aritméticas
(define arit1 "main{ 2 }")
(define arit2 "main{ 1 + 2 }")
(define arit3 "main{ 1 + 2 * 4 - 3 / 5 }")
(define arit4 "main{ 1 - (2 + 3) }")
(define arit5 "main{ (1 + 3) * 2 }")
(define arit6 "main{ (1 + 3) * (2 / 4) }")
(define arit7 "main{ (1 + 3) * (2 / (4 - 5)) }")

;; Declaraciones
(define dec1 "main{ temp1:int = 2 }")
(define dec2 "main{ temp_2:int }")
(define dec3 "main{ dummyVar:boolean }")
(define dec4 "main{ i:int = f(x) }")
(define dec5 "main{ a:int[] = {1,2,3,4} }")
(define dec6 "main{ b:boolean[] = {True} }")

;; Asignaciones
(define asign1 "main{ x = x + 1 }")
(define asign2 "main{ b = !b }")

;; Estructuras de control
(define if-in        ;; if
  "main{
     if(1<z) {
       a++
       n++}
     else {
       b++
       r++}
   }")

;; while
(define while-in
  "main{
     while (x < 0) {
         if(1<z) {
           a++
           n++}
         else {
           b++
           r++}
     }
   }")

;; Llamadas a funciones
(define call1 "main{ f() }")
(define call2 "main{ f(x) }")
(define call3 "main{ f(a, b) }")
(define call4 "main{ f(a, True, 2) }")

;; Métodos
(define meth1
  "main{} gcd (var1:int, var2:int): int {
    while(var1 != 0) {
        if (var1 < var2) var2 = var2 - var1
        else var1 = var1 - var2
    }
    return b
   }")

(define meth2
  "main{} gdc(var1:int, var2:int): int {
    while var1 != 0 {
        if (var1 < var2) var2 = var2 - var1
        else var1 = var1 - var2
    }
    return b
   }")

(define meth3
  "main{} gdc(var1:int, var2:int) : int {
    while (var1 != 0) {
        if (var1 < var2) var2 = var2 - var1
        else var1 = var1 - var2
    }
   }")

;; Método
(define fun
  "main{} gdc(var1:int, var2:int) {
     while (x < 0)  {
       if(1<z) {
         a++
         n++}
       else {
         b++
         r++}
     }
   }")

;; Main
(define prin
  "main {
    i:int = zzzz++
    zzzz += 1
    r:int = gdc(i,zzz)
   }")

;; programa
(define program "//Comentario 1
  main {
    i:int = zzzz++
    zzzz += 1
    r:int = gdc(i,zzz)
  }

  {- Comentario 2
     Comentario 2 -}

  gdc(var1:int, var2:int): int {
     while var1 != 0 {
        if (var1 < var2) var2 = var2 - var1
        else var1 = var1 - var2
     }
     return b
  }

  sort(a:int []) {
     i:int = 0
     n:int = length(a)

     while i < n {
        j:int = i
        while j > 0 {
           if a[j-1] > a [j] {
              swap:int = a[j]
              a[j] = a[j-1]
              a[j-1] = swap
           }
           j--
        }
        i++
     }
  }")




;; Sintaxis adicional

;; Declaración múltiple
(define dec-mult "main{ int a, b, c }")

;; Asignación múltiple
(define asign-mult "main{ a = b = c = 0 }")

;; Asignación compuesto
(define as-add "main{ a+=5 }")
(define as-subs "main{ a-=5 }")

;; Autoincremento / Autodecremento
(define a-inc "main{ a++ }")
(define a-dec "main{ a-- }")

;; if corto (operador ternario)
(define if-corto "main{ c:int = a > b ? a : b }")

;; Lista de pruebas
(define pruebas (list
                 arr1
                 arr2
                 arr3
                 bool1
                 bool2
                 bool3
                 bool4
                 bool5
                 bool6
                 bool7
                 arit1
                 arit2
                 arit3
                 arit4
                 arit5
                 arit6
                 arit7
                 dec1
                 dec2
                 dec3
                 dec4
                 dec5
                 dec6
                 asign1
                 asign2
                 if-in
                 while-in
                 call1
                 call2
                 call3
                 call4
                 meth1
                 meth2
                 meth3
                 fun
                 prin
                 program
                 dec-mult
                 asign-mult
                 as-add
                 as-subs
                 a-inc
                 a-dec
                 if-corto
                 ))

;; Función que parsea todas las pruebas de la lista de pruebas
;; (ejecuta-pruebas)
(define (ejecuta-pruebas [l pruebas])
  (for/list ([i l])
    (parsea i)))


(define (lex-this lexer input) (lambda () (lexer input)))

(define (parsea in)
        (let ([in-s (open-input-string in)])
        (jelly-parser (lex-this jelly-lex in-s))))