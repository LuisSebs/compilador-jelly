#lang nanopass
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRÁCTICA 2 : Lexer y Tokens
;; Integrantes:
;;; Autor: Arrieta Mancera Luis Sebastian
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   NOTA: Hasta este punto el lexer reconoce todos los archivos, no sé si 
   falte agregar más tokens del lenguaje y más consideraciones,
   además que no "reconoce" ni sabe que hacer con lexemas no reconocidos
|# 

;; Punto 1. Lexer
(define-tokens contenedores (ID BOOL NUM)) ;; Tokens que tendrán atributo
(define-empty-tokens vacios ;; Tokens vacios
  (ADD     ;; +
   MULT    ;; *
   DIV     ;; /
   MOD     ;; %
   SUBS    ;; -
   DIF     ;; !=
   EQ      ;; ==
   MIN     ;; <
   MAX     ;; >
   MINEQ   ;; <=
   MAXEQ   ;; >=
   AND     ;; &
   OR      ;; |
   NOT     ;; !
   Q       ;; ?
   INCR    ;; ++
   DECR    ;; --
   INCCOMP ;; +=
   DECCOMP ;; -=
   IF      ;; if
   ELSE    ;; else
   WHILE   ;; while
   LP      ;; (
   RP      ;; )
   LB      ;; [
   RB      ;; ]
   LC      ;; {
   RC      ;; }
   MAIN    ;; main
   DDOT    ;; :
   COMMA   ;; ,
   ASSIGN  ;; =
   INT     ;; int
   BOOLEAN ;; boolean
   RETURN  ;; return
   EOF     ;; eof
  )
)

;; Definición del Lexer
(define jelly-lex
  (lexer
   [whitespace (jelly-lex input-port)]
   [(:: "boolean")(token-BOOLEAN)] ;; boolean
   [(:: "return") (token-RETURN)]  ;; return
   [(:: "if")     (token-IF)]      ;; if
   [(:: "else")   (token-ELSE)]    ;; else
   [(:: "while")  (token-WHILE)]   ;; while
   [(:: "main")   (token-MAIN)]    ;; main
   [(:: "int")    (token-INT)]     ;; int
   [(:: #\+)      (token-ADD)]     ;; +
   [(:: #\*)      (token-MULT)]    ;; *
   [(:: "/")      (token-DIV)]     ;; /
   [(:: #\%)      (token-MOD)]     ;; %
   [(:: #\-)      (token-SUBS)]    ;; -
   [(:: "!=")     (token-DIF)]     ;; !=
   [(:: "==")     (token-EQ)]      ;; ==
   [(:: "<")      (token-MIN)]     ;; <
   [(:: ">")      (token-MAX)]     ;; >
   [(:: "<=")     (token-MINEQ)]   ;; <=
   [(:: ">=")     (token-MAXEQ)]   ;; >=
   [(:: "&")      (token-AND)]     ;; &
   [(:: "|")      (token-OR)]      ;; |
   [(:: "!")      (token-NOT)]     ;; !
   [(:: "?")      (token-Q)]       ;; ?
   [(:= 2 "+")    (token-INCR)]    ;; ++
   [(:= 2 "-")    (token-DECR)]    ;; --
   [(:: "(")      (token-LP)]      ;; (
   [(:: ")")      (token-RP)]      ;; )
   [(:: "[")      (token-LB)]      ;; [
   [(:: "]")      (token-RB)]      ;; ]
   [(:: "{")      (token-LC)]      ;; {
   [(:: "}")      (token-RC)]      ;; }
   [(:: ":")      (token-DDOT)]    ;; :
   [(:: ",")      (token-COMMA)]   ;; ,
   [(:: "=")      (token-ASSIGN)]  ;; =
   [(:: "+=")     (token-INCCOMP)] ;; +=
   [(:: "-=")     (token-DECCOMP)] ;; -=
   [(eof)         (token-EOF)]     ;; eof 

   ;; Contenedores   
   [(:: (char-range #\a #\z)
        (:* (:or (char-range #\a #\z)
                 (char-range #\A #\Z)
                 (:or numeric alphabetic #\_))))
    (token-ID lexeme)]

   [(:or "True" "False") (token-BOOL lexeme)]
   [(:+ numeric) (token-NUM lexeme)]
   
;; Punto 4. La expresión regular para los comentarios
   ;; Esto se usa para comentarios de Java y C
   [(:: "{-" (complement (:: any-string "-}" any-string)) "-}") (jelly-lex input-port)]
   ;; Comentarios de una línea
   [(:: "//" (:* (char-complement #\newline))) (jelly-lex input-port)]

;; Punto 3. La expresión regular para un lexema no reconocido
  )
)

;; Punto 2. Para resolver este punto, se transforma el archivo de texto
;;          a cadena y esa cadena se le pasa a una función que enlista los
;;          tokens

#|
   Lee un archivo y lo transforma a cadena
|#
(define (doc-string ruta)
  (call-with-input-file ruta
    (lambda (input-port)
      (port->string input-port))))

#|
   Función que lee cadenas del lenguaje y enlista los tokens, no sabe que
   hacer con lexemas no reconocidos.
   Por ejemplo, se puede probar con el siguiente archivo:
      (define archivo (doc-string "ejemplos/gcd.jly"))
      (probar archivo)
|#
(define (probar s)
  (let* ([input (open-input-string s)])
    (let loop ((tokens '()))
      (let ((lexer-output (jelly-lex input)))
        (if (eof-object? lexer-output)
            (reverse tokens) 
            (begin
              (set! tokens (cons lexer-output tokens))
              (if (eof-object? (peek-char input))
                  (reverse tokens) 
                  (loop tokens))))))))