#lang eopl


;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local y procedimientos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa> :=  <expresion>
;;                 un-programa (exp)
;;  <expresion>    ::= <numero>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>

;;  <programa> :=  <expresion>
;;                 un-programa (exp)
;;
;;  <expresion> := <numero>
;;               numero-lit (num)
;;
;;              := "\""<texto> "\""
;;               texto-lit (txt)
;;
;;              := <identificador>
;;               var-exp (id)
;;
;;              := Si <expresion> entonces <expresion> sino <expression> finSi
;;                condicional-exp (test-exp true-exp false-exp)
;;
;;              := declarar (<identificador> = <expresion> (;)) {<expresion>}
;;                variableLocal-exp (ids exps cuerpo)
;;
;;              := procedimiento (<identificador>* ',') haga <expresion> finProc
;;                prodecimiento-exp (ids cuerpo)
;;
;;              := evaluar <expresion> (<expresion> ',')* finEval
;;                app-exp (exp epxs)
;;
;;              ::= let {<dentificador> = <expresion>}* in <expresion>
;;                let-exp (ids rands body)
;;
;;              := (<expresion> <primitiva-binaria> <expresion>)
;;                primapp-bin-exp (exp1 prim-binaria exp2)
;;
;;              := <primitiva-unaria> (<expresion>)
;;               primapp-un-exp (prim-unaria exp)
;;
;;              := let-recursivo {<identificador> (<identificador>* ',') = <expresion>}* in <expresion>
;;               let-recursivo-exp (proc-names ids bodies body)
;;
;;  <primitiva-binaria> :=  + (primitiva-suma)
;;                      :=  ~ (primitiva-resta)
;;                      :=  / (primitiva-div)
;;                      :=  * (primitiva-multi)
;;                      :=  concat (primitiva-concat)
;;
;;  <primitiva-unaria> :=  longitud (primitiva-longitud)
;;                     :=  add1 (primitiva-add1)
;;                     :=  sub1 (primitiva-sub1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TENER EN CUENTA
;;<numero>: Debe definirse para valores decimales y enteros (positivos y negativos)
;;<texto>: Debe definirse para cualquier texto escrito en racket
;;<identificador>: En este lenguaje todo identificador iniciará con el símbolo  @, es decir las variables @x y @z son válidas

;******************************************************************************************
;Especificación Léxica
(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier
     ("@" letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." (arbno digit)) number)
    (number
     ("-" digit (arbno digit) "." (arbno digit)) number)
    (string
     ("\"" (arbno (not #\")) "\"") string)
    ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion ("\"" string "\"") texto-lit)
    (expresion (identifier) var-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSi")
               condicional-exp)
    (expresion ("declarar" "(" (arbno identifier) ")" "haga" expresion "finProc")
               procedimiento-exp)
    (expresion ("let" (arbno identifier = expresion) "en" expresion "finLet")
               let-exp)
    (expresion ("evaluar" expresion "("(arbno expresion) ")" "finEvaluar")
               app-exp)
    (expresion ("let-recursivo"(arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
               letrec-exp)
    (expresion
     ("(" expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion
     (primitiva-binaria "(" (separated-list expresion ",")")")
     primapp-exp)
    (expresion
     (primitiva-unaria "(" expresion ")") primapp-un-exp)
    
    ; características adicionales
    ;(expresion ("proc" "(" (separated-list identifier ",") ")" expression)
     ;           proc-exp)
    ;(expresion ( "(" expression (arbno expression) ")")
     ;           app-exp)
    ;;;;;;

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("-") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;;;;;;

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ))

;evaluar-expresion: <expresion> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (string) "\"" string "\"")
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc)))))))


;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (evaluar-programa pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))




;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

;*******************************************************************
;Datatype cerradora (procVal)
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)
   )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ambiente inicial
