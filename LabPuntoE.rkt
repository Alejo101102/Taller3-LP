#lang eopl


;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local y procedimientos

;; La definición BNF para las expresiones del lenguaje:
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
     ("\"" (arbno (not #\")) "\"") symbol)
    ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (string) texto-lit)
    (expresion (identifier) var-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI")
               condicional-exp)
    (expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expresion "finProc")
               procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)
    (expresion ("let-recursivo" "{" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "}" "en" expresion)
               letrec-exp)
    (expresion
     ("(" expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")") primapp-un-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
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
      (texto-lit (string) string)
      (var-exp (id) (buscar-variable env id))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (evaluar-expresion test-exp env))
                           (evaluar-expresion true-exp env)
                           (evaluar-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                         (let ((values (eval-rands exps env)))
                           (let ((extended-env (extend-env ids values env)))
                             (evaluar-expresion cuerpo extended-env))))
      (procedimiento-exp (ids body)
                         (cerradura ids body env))
      (app-exp (rator rands)
               (let ((proc (evaluar-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                     (extend-env-recursively proc-names idss bodies env)))
      (primapp-bin-exp (exp1 exp-bin exp2)
                       (let ((val1 (evaluar-expresion exp1 env))
                             (val2 (evaluar-expresion exp2 env)))
                         (apply-binary-primitive val1 exp-bin val2)))

      (primapp-un-exp (exp-un exp)
                      (let ((val (evaluar-expresion exp env)))
                        (apply-unary-primitive exp-un val))))))

; funciones auxiliares para aplicar evaluar-expresion a cada caso
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

(define apply-binary-primitive
  (lambda (val1 bin-op val2)
    (cases primitiva-binaria bin-op
      (primitiva-suma () (+ val1 val2))
      (primitiva-resta () (- val1 val2))
      (primitiva-div () (/ val1 val2))
      (primitiva-multi () (* val1 val2))
      (primitiva-concat () (string-append val1 val2)))))

(define apply-unary-primitive
  (lambda (un-op val)
    (cases primitiva-unaria un-op
      (primitiva-longitud () (string-length val))
      (primitiva-add1 () (+ val 1))
      (primitiva-sub1 () (- val 1)))))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;*******************************************************************
;Procedimientos
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
                 (evaluar-expresion body (extend-env ids args env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ambientes

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env ambiente?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> ambiente
;función que crea un ambiente vacío
(define empty-env
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> ambiente -> ambiente
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                        (list-ref bodies pos)
                                                        env)
                                             (buscar-variable old-env sym)))))))

;Ambiente inicial

(define ambiente-inicial
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))


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
                   (evaluar-expresion body (ambiente-inicial))))))

#| Punto f
declarar (
   @integrantes= procedimiento() % No recibe nada
                    haga "Alejandro-Juan-Y-Sebastian" % Retorna el String
                 finProc
){
   evaluar @integrantes() finEval % Evalua el procedimiento
}
|#


#|
% Procedimiento que retorna un String
declarar (
   @tomaString= procedimiento() % No recibe nada
                    haga "Un String" % Retorna el String
                 finProc
){
   evaluar @tomaString() finEval % Evalua el procedimiento
}


% Procedimiento que retorna un procedimiento
declarar(
   @procExterno= procedimiento()
                    haga
                    procedimiento() haga "Soy un procedimiento interno no evaluado" finProc
                 finProc
){
   evaluar @procExterno () finEval % Hay que evaluar el procExterno para que retorne el procedimiento interno
}


% Procedimiento que recibe un procedimiento y lo evalua
declarar (
   @procEjemplo= procedimiento() haga "Soy un procedimiento interno no evaluado" finProc;
   @procEvaluador= procedimiento(@procNoEval)
                      haga
                         declarar()
                         {
                            evaluar @procNoEval () finEval
                         }
                   finProc  
){
   evaluar @procEvaluador (@procEjemplo) finEval
}


% Concatena "Hola" al principio de una @palabra - Funciona pero hay que actualizar el evaluador
declarar(
   
   @palabra= "Mundo";   
   @funcConcatenarHola= procedimiento(@s)haga
                         declarar(@sufijo= "Hola")
                         {
                            (@sufijo concat @s)
                         }
                        finProc
){
   evaluar @funcConcatenarHola (@palabra) finEval
}


|#
;; @saludar = //un procedimiento que recibe un procedimiento y retorna
;; otro procedimiento que le agrega el string "Hola:" a la invocación del
;; procedimiento que recibió en sus argumentos.
(interpretador)