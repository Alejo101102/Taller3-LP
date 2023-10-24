#lang eopl

;;Ejecuta esto en el archivo del interpretador!!!

#| Punto f
% Forma correcta creo* ----- No Funciona
declarar (
   @integrantes= procedimiento() % No recibe nada
                    haga "Alejandro-Juan-Y-Sebastian" % Retorna el String
                 finProc;
   @saludar= procedimiento(@proc)      % Recibe un procedimiento
                           haga
                             declarar(
                                @sufijo= "Hola: ";
                                @palabra= declarar()
                                          {evaluar @proc () finEval}      % Evalua el proceso para obtener el String
                             )
                             {
                               procedimiento() haga (@sufijo concat @palabra) finProc      % Retorna el proceso que concatena
                             }
                             finProc;
   @decorate= evaluar @saludar(@integrantes) finEval      % Creacion del decorador
){
   evaluar @decorate () finEval      % Deberá retornar "Hola Alejandro-Juan-Y-Sebastian"     
}


% Forma incorrecta creo* ----- Funciona
declarar (
   @integrantes= procedimiento() % No recibe nada
                    haga "Alejandro-Juan-Y-Sebastian" % Retorna el String
                 finProc;
   @saludar= procedimiento(@proc)      % Recibe un procedimiento
                           haga
                             declarar(
                                @sufijo= "Hola: ";
                                @palabra= declarar()
                                          {evaluar @proc () finEval}      % Evalua el proceso para obtener el String
                             )
                             {
                               procedimiento() haga (@sufijo concat @palabra) finProc      % Retorna el proceso que concatena
                             }
                             finProc
                 
){
   declarar(
      @decorate= evaluar @saludar(@integrantes) finEval      % Creacion del decorador
   ){
      evaluar @decorate () finEval      % Deberá retornar "Hola Alejandro-Juan-Y-Sebastian" 
   }
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

% Recibe una palabra
% Concatena "Hola" al principio de una @palabra
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


% Recibe un procedimiento que retorna una @palabra
% Retorna un proceso que concatena "Hola" al principio de una @palabra
% Modifica "procedimiento() haga (@sufijo concat @palabra) finProc" para ver si el proceso funciona bien
declarar(
   @palabraEnProc= procedimiento() % No recibe nada
                    haga "nueva palabra" % Retorna el String
                 finProc;
   @funcConcatenarHola= procedimiento(@proc) % Recibe un procedimiento
                           haga
                             declarar(
                                @sufijo= "Hola ";
                                @palabra= declarar()
                                          {evaluar @proc () finEval}
                             )
                             {
                               procedimiento() haga (@sufijo concat @palabra) finProc
                             }
                             finProc

){
   evaluar @funcConcatenarHola (@palabraEnProc) finEval
}


|#
;; @saludar = //un procedimiento que recibe un procedimiento y retorna
;; otro procedimiento que le agrega el string "Hola:" a la invocación del
;; procedimiento que recibió en sus argumentos.
