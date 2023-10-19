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
;;  <expresion> := <numero>
;;               numero-lit (num)
;;              := "\""<texto> "\""
;;               texto-lit (txt)
;;              := <identificador>
;;               var-exp (id)
;;              := (<expresion> <primitiva-binaria> <expresion>)
;;               primapp-bin-exp (exp1 prim-binaria exp2)
;;              := <primitiva-unaria> (<expresion>)
;;               primapp-un-exp (prim-unaria exp)
;;
;;  <primitiva-binaria> :=  + (primitiva-suma)
;;                      :=  ~ (primitiva-resta)
;;                      :=  / (primitiva-div)
;;                      :=  * (primitiva-multi)
;;                      :=  concat (primitiva-concat)
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
    (comentario
     ("%" (arbno (not #\newline))) skip)
    (identificador
     ("@" letter (arbno (or letter digit "?"))) symbol)
    (numero
     (digit (arbno digit)) numero)
    (numero
     ("-" digit (arbno digit)) numero)
    (numero
     (digit (arbno digit) "." (arbno digit)) numero)
    (numero
     ("-" digit (arbno digit) "." (arbno digit)) numero)
    (texto
     ("\"" (arbno (not #\")) "\"") texto)
    ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion
     ("(" expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion
     (primitive "(" (separated-list expresion ",")")")
     primapp-exp)
    (expresion
     (primitive "(" expression ")") primapp-un-exp)
    
    ; características adicionales
    (expresion ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expresion ( "(" expression (arbno expression) ")")
                app-exp)
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
