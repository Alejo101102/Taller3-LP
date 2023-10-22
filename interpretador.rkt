#lang eopl
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; TALLER #3 - FUNDAMENTOS DE COMPILACION E INTERPRETACION DE LP


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; INTEGRANTES
; Alejandro Guerrero Cano (2179652)- Alejo101102
; alejandro.cano@correounivalle.edu.co

; Juan David Loaiza Santiago (2177570)- JuanLoaiza007
; juan.loaiza.santiago@correounivalle.edu.co

; Juan Sebastian Muñoz Rojas (2177436)- sebastianmr18
; juan.munoz.rojas@correounivalle.edu.co


;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; DESARROLLO


;; Primera gramática


(define gramatica
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression "\""(string)"\"" texto-lit)
    (expression ("@" identifier) var-exp)
    (expression
     ("(" expression primitiva-binaria expression ")")
     primapp-bin-exp)
    (expression
     (primitive "(" expression ")")
     primapp-un-exp)
    (primitive ("+") primitiva-suma)
    (primitive ("-") primitiva-resta)
    (primitive ("/") primitiva-div)
    (primitive ("*") primitiva-multi)
    (primitive ("concat") primitiva-concat)
    (primitive ("longitud") primitiva-longitud)
    (primitive ("add1") primitiva-add1)
    (primitive ("sub1") primitiva-sub1)))