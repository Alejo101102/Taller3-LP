#lang eopl

;;Ejecuta esto en el archivo del interpretador!!!

#| Punto F
% Forma incorrecta creo* ----- Funciona
declarar (
   @integrantes= procedimiento() % No recibe nada
                    haga "Alejandro-Juan-Y-Sebastian" % Retorna el String
                 finProc;
   @saludar= procedimiento(@proc)      % Recibe un procedimiento
                           haga
                             declarar(
                                @sufijo= "Hola: ";
                                @prefijo= " Estudiantes de FLP";
                                @palabra= declarar()
                                          {evaluar @proc () finEval}      % Evalua el proceso para obtener el String
                             )
                             {
                               procedimiento() haga ((@sufijo concat @palabra) concat @prefijo) finProc      % Retorna el proceso que concatena
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