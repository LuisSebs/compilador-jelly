#lang nanopass

(require "symbol-table.rkt"
          parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRÁCTICA 6 : Sistema de tipos
;; Integrantes:
;; - Arrieta Mancera Luis Sebastián
;; - Góngora Ramírez Dania Paula
;; - Martínez Hernández Zuriel Enrique
;; - Villafán Flores María Fernanda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cosas que faltan y no afectan para esta practica:
;; Declaracion multiple (dec-mul) CHECK
;; operador ternario (? e0 e1 e2) CHECK


;; Programa
(define (type-check ir tb)
  (nanopass-case (jelly Programa) ir
                 [(program ,firm* ...) (begin
                                         (for-each (lambda (v) (get-type-firm v tb)) firm*)
                                         'unit)]))

;; Firma
(define (get-type-firm ir tb) ;; CHECK
  (nanopass-case (jelly Firma) ir
                 [,m (get-type-m m tb)]
                 [,meth (get-type-meth meth tb)]
                 [,fun (get-type-fun fun tb)]))

;; Main
(define (get-type-m ir tb) ;; CHECK
  (nanopass-case (jelly Main) ir
                 [(main ,block) (get-type-block block tb)]))

;; Metodo
(define (get-type-meth ir tb) ;; CHECK
  (nanopass-case (jelly Metodo) ir
                 [(method ,i (,arg* ...) ,ty ,block) (if (eq? ty (get-type-block block tb))
                                                         'unit
                                                         (error "Error metodo"))]))

;; Funcion
(define (get-type-fun ir tb) ;; CHECK
  (nanopass-case (jelly Funcion) ir
                 [(function ,i (,arg* ...) ,block) (begin
                                                     (get-type-block block tb)
                                                     'unit)]))

;; Bloque
(define (get-type-block ir tb) ;; CHECK
  (nanopass-case (jelly Bloque) ir
                 [(,stmt* ...) (let ([tipos (map (lambda (v) (get-type-stmt v tb)) stmt*)])
                                 (last tipos))]))

;; Sentencia
(define (get-type-stmt ir tb)
  (nanopass-case (jelly Sentencia) ir
                 [,e (get-type-e e tb)]
                 [,dec (get-type-dec dec tb)]
                 [,dec-mult (get-type-dec-mult dec-mult tb)]
                 [,struct (get-type-struct struct tb)]
                 [(return ,e) (get-type-e e tb)]))

;; Asignacion
(define (get-type-assign ir tb) ;; CHECK
  (nanopass-case (jelly Asignacion) ir
                 [(= ,e0 ,e1) (let* ([ty-e0 (get-type-e e0 tb)]
                                     [ty-e1 (get-type-e e1 tb)])
                                (cond
                                  [(and (eq? 'int-arr ty-e0) (eq? 'int ty-e1)) 'unit]
                                  [(and (eq? 'bool-arr ty-e0) (eq? 'boolean ty-e1)) 'unit]
                                  [(eq? ty-e0 ty-e1) 'unit]
                                  [else (error "Error asignacion")]))]))

;; Declaracion
(define (get-type-dec ir tb) ;; CHECK
  (nanopass-case (jelly Declaracion) ir
                 [(decl ,i ,ty) 'unit]
                 [(decl ,i ,ty ,e) (if (eq? ty (get-type-e e tb))
                                       'unit
                                       (error "Error declaracion"))]))

;; Declaración multiple
(define (get-type-dec-mult ir tb)
  (nanopass-case (jelly Declaracionmult) ir
                 [(decl-mult ,i* ... ,ty) 'unit]))

;; Estructura
(define (get-type-struct ir tb) ;; CHECK
  (nanopass-case (jelly Estructura) ir
                 [(if ,e ,block) (if (eq? 'boolean (get-type-e e tb))
                                     (begin
                                       (get-type-block block tb)
                                       'unit)
                                     (error "Error if corto"))]
                 [(if ,e ,block0 ,block1) (if (eq? 'boolean (get-type-e e tb))
                                              (begin
                                                (get-type-block block0 tb)
                                                (get-type-block block1 tb)
                                                'unit)
                                              (error "Error if largo"))]
                 [(while ,e ,block) (if (eq? 'boolean (get-type-e e tb))
                                        (begin
                                          (get-type-block block tb)
                                          'unit)
                                        (error "Error while"))]))


;; Expresion
(define (get-type-e ir tb)
  (nanopass-case (jelly Expr) ir
                 [,c (if (number? c)
                         'int
                         (if (memq c '(True False))
                             'boolean
                             (error "No existe tipo para esta expr")))]
                 [,i (hash-ref tb i)]
                 [,arr (get-type-arr arr tb)]
                 [,cal (get-type-cal cal tb)]
                 [,cal-arr (get-type-cal-arr cal-arr tb)]
                 [,assign (get-type-assign assign tb)]
                 [(,ou ,e) (if (eq? 'boolean (get-type-e e tb))
                               'boolean
                               (error "Error operacion unaria"))]
                 [(,ob ,e0 ,e1) (let* ([ty-e0 (get-type-e e0 tb)]
                                       [ty-e1 (get-type-e e1 tb)])
                                  (cond
                                    [(and (memq ob '(+ - * / %)) (eq? 'int ty-e0) (eq? 'int ty-e1)) 'int]
                                    [(and (memq ob '(== != < <= > >=)) (eq? 'int ty-e0) (eq? 'int ty-e1)) 'boolean]
                                    [(and (memq ob '(== != and or)) (eq? 'boolean ty-e0) (eq? 'boolean ty-e1)) 'boolean]
                                    [else (error "Error operacion binaria")]))]
                 [(? ,e0 ,e1 ,e2) (let* ([ty-e0 (get-type-e e0 tb)]
                                         [ty-e1 (get-type-e e1 tb)]
                                         [ty-e2 (get-type-e e2 tb)])
                                    (cond
                                      [(and (eq? 'boolean ty-e0) (eq? ty-e1 ty-e2)) ty-e1]))]))

;; Arreglo
(define (get-type-arr ir tb) ;; CHECK
  (nanopass-case (jelly Arreglo) ir
                 [(array ,e* ...) (let ([tipos (map (lambda (v) (get-type-e v tb)) e*)])
                                    (if (andmap (lambda (ty) (eq? ty (car tipos))) (cdr tipos))
                                        (if (eq? 'int (car tipos))
                                            'int-arr
                                            'bool-arr)
                                        (error "Error arreglo")))]))

;; Llamada a funcion
(define (get-type-cal ir tb) ;; CHECK
  (nanopass-case (jelly Llamada) ir
                 [(call ,i ,e* ...) (begin
                                      (if (and
                                           (eq? 'length i)
                                           (eq? 1 (length e*))
                                           (or
                                            (eq? 'int-arr (get-type-e (first e*) tb))
                                            (eq? 'int-bool (get-type-e (first e*) tb))))
                                          'int
                                          (hash-ref tb i)))]))

;; Llamada a arreglo
(define (get-type-cal-arr ir tb) ;; CHECK
  (nanopass-case (jelly Llamadaarr) ir
                 [(array-call ,i ,e) (if (eq? 'int (get-type-e e tb))
                                         (cond
                                           [(eq? 'int-arr (hash-ref tb i)) 'int]
                                           [(eq? 'bool-arr (hash-ref tb i)) 'boolean])
                                         (error "Error llamada a arreglo"))]))

;; Γ ⊢ n ∶ int
(define j1 (let* ([parseo (pj "main{ 2 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ true ∶ bool
(define j2 (let* ([parseo (pj "main{ True }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ false ∶ bool
(define j3 (let* ([parseo (pj "main{ False }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ x ∶ τ
(define j4 (let* ([parseo (pj "main{ var1:int var1 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ −e ∶ int NO ES NECESARIO PARA ESTA PRACTICA
;; Γ ⊢ !e ∶ bool
(define j6 (let* ([parseo (pj "main{ !True }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ length(e) ∶ int
(define j7 (let* ([parseo (pj "main{ arreglo:int[] = {1, 2, 3} n:int = length(arreglo) }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ {e1 , ..., en } ∶ τ []
(define j8 (let* ([parseo (pj "main{ arreglo:int[] = {1, 2, 3} }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ e1 [e2] ∶ τ
(define j9 (let* ([parseo (pj "main{ arreglo:int[] = {1, 2, 3} arreglo[1] }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ e1 ⊕ e2 ∶ int
(define j10 (let* ([parseo (pj "main{ (1 + 2 * 4 - 5) / 2 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ e1 ⊖ e2 ∶ bool
(define j11 (let* ([parseo (pj "main{ (2 * 2) > (3 / 3) }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ e1 ⊖ e2 ∶ bool
(define j12 (let* ([parseo (pj "main{ (True == True) & (True != False)  }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ f (e) ∶ T'
(define j13 (let* ([parseo (pj " main{ var1:boolean = even(2) } even(numero:int) : boolean { return (numero % 2) == 0 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ f (e1 , ..., en ) ∶ T'
(define j14 (let* ([parseo (pj " main{ var1:int = sum(2) } sum(num1:int, num2:int) : int { return num1 + num2 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ x = e ∶ unit
(define j15 (let* ([parseo (pj " main{ var1:int var1 = 2 }")]
                  [renombre (rename-var parseo)]
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ if(e) s ∶ unit
(define j16 (let* ([parseo (pj " main{ flag:boolean = True if(2 > 1){ flag = False }}")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ while(e) s ∶ unit
(define j17 (let* ([parseo (pj " main{ flag:boolean = True if(2 > 1){ flag = False } else { flag = True }}")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ while(e) s ∶ unit
(define j18 (let* ([parseo (pj " main{ count:int = 0 while(count < 10){ count++ } }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ e1 [e2] = e3 ∶ unit
(define j19 (let* ([parseo (pj " main{ arreglo:int[] = {1, 2, 3} arreglo[0] = arreglo[0] * 2 }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ f (e) ∶ unit
(define j20 (let* ([parseo (pj " main{ miFuncion(2) } miFuncion(num:int){ var1:int = 1 + num var1++ }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Γ ⊢ f (e1 , ..., en ) ∶ unit
(define j21 (let* ([parseo (pj " main{ miFuncion(1, 2, 3) } miFuncion(num1:int, num2:int, num3:int){ var1:int = num1 + num2 + num3 }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Declaracion multiple
(define jdec-mult (let* ([parseo (pj " main{ int a, b, c a = 0 b = 1 c = 2 }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Operador ternario
(define jop-tern (let* ([parseo (pj " main{ x:int = (2>1) ? 1 : 2+3 }")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])
             (type-check renombre tabla)))

;; Verificamos los tipos y sentencias todo un programa
(define prueba-final (let* ([parseo (pj
                                     " main{
                                             arr:int[] = {1, 2, 3}
                                             sort(arr)
                                       }

                                       sort(a:int[]){
                                           i:int = 0
                                           n:int = length(a)
                                           while i < n {
                                               j:int = i
                                               while j > 0 {
                                                   if a[j-1] > a[j]{
                                                       swap:int = a[j]
                                                       a[j] = a[j-1]
                                                       a[j-1] = swap
                                                   }
                                                   j = j-1
                                               }
                                               i = i + 1
                                           }
                                       }
                                       ")]
                  [renombre (rename-var parseo)] 
                  [tabla (symbol-table renombre (make-hash))])             
             (type-check renombre tabla)))
