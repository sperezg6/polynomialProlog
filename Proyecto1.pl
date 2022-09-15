:- style_check(-singleton).

%   =============================================
%   ==      PROYECTO 1: IA                     ==
%   ==      Equipo:                            ==
%   ==         <Santiago Pérez      188475>    ==
%   ==         <Mauricio de Ariño   191280>    ==
%   ==         <Alan Amaya          191165>    ==
%   =============================================
%   Se definieron varias reglas que permiten rea-
%   lizar operaciones básicas con polinomios.
%   Para representar un polinomio se utiliza una
%   lista con sus coeficientes en orden ascendente
%   según el grado de cada término.
%   Por ejemplo, la lista [1,2,3,4] representa al
%   polonomio 1 + 2x + 3x^2 + 4x^3.

%   ===== numTerminos(i,o) ======
%   <<< Esta es una regla auxiliar >>>
%   Calcula el número de términos de una 
%   expresión polinomial
%   numTerminos(X,N) donde:
%       X -> una liste con los coeficientes del
%           polinomio.
%       N -> el numero de elementos en la lista
%               = num. de terminos.
%
%   ejemplo:
%   numTerminos([1,1,1,1],X).
%       |   X = 4.
numTerminos([],0).
numTerminos([_|Y],N):-
    numTerminos(Y,N1),
    N is N1+1.

%   ===== grado(i,o) =====
%   Indica el grado del polinomio.
%   grado(X,G) donde:
%       X -> es la lista de coeficientes.
%       G -> es el grado del polinomio.
%   
%   ejemplo:
%   grado([0,0,3],X).
%       |   X = 2.
grado([],0).
grado([_|Y],G):-
    numTerminos(Y,N),
    G is N.

%   ===== suma(i,i,o) =====
%   Suma de polinomios.
%   suma(A,B,C) donde:
%       A -> lista de coefs. del primer pol.
%       B -> lista de coefs. del segundo pol.
%       C -> lista de coefs. de la suma de los polinomios
%
%   ejemplo:
%   suma([1,2,0],[0,2,3],X).
%       |   X = [1,4,3].
suma(A,[],A).
suma([], B, B).
suma([X0|X1],[Y0|Y1],[S|PS]):-
    S is X0+Y0,
    suma(X1,Y1,PS).
    

%   ===== resta(i,i,o) =====
%   Resta de polinomios.
%   resta(A,B,C) donde:
%       A -> lista de coefs. del primer pol.
%       B -> lista de coefs. del segundo pol.
%       C -> lista de coefs. de la resta de los polinomios
%
%   ejemplo:
%   resta([1,2,3,4],[0,2,1,1],X). 
%       |   X = [1,0,2,3].
resta([],[],[]).
resta([], Y, Z).
resta(X,[],Z).
resta([X0|X1],[Y0|Y1],[S|PS]):-
    S is X0-Y0,
    resta(X1,Y1,PS).
    


%   ===== multPorEscalar(i,i,o) =====
%   <<< Esta es una regla auxiliar >>>
%   Realiza la multiplicación de un escalar por un
%   polinomio
%   multPorEscalar(A,B,R) donde:
%       A -> número escalar
%       B -> coeficientes del polinomio
%       R -> coeficientes del polinomio resultante
multPorEscalar(_,[],[]):- !.
multPorEscalar(A,[H|B], [Rh|Rb]) :-
   Rh is A*H,
   multPorEscalar(A, B, Rb).

%   ===== producto(i,i,o) =====
%   Realiza la multiplicación de dos polinomios
%   producto(A,B,R) donde:
%       A -> coeficientes del primer polinomio 
%       B -> coeficientes del segundo polinomio
%       R -> coeficientes del polinomio resultante
producto([A|Y],B,R):-
    multPorEscalar(A,B,Rt),
    productoRec(Y,B,Rt,R).

%   ===== productoRec(i,i,i,o) =====
%   <<< Esta es una regla auxiliar >>>
%   Realiza la recursión del producto de polinomios
%   productoRec(A,B,Act,R) donde:
%       A -> un número escalar (coef. del primer pol.)
%       B -> lista incrementada de coeficientes del
%           segundo polinomio (el incremento emula el
%           aumento del grado en la multiplicación)
%       Act -> la suma actual de polinomios
%       R -> el resultado de la suma de polinomios con 
%           el polinomio actual
productoRec([],_,Y,Y):- !.
productoRec([A|Y],B,Act,R):-
    append(Act, [0], RAct),
    append([0],B,B2),
    multPorEscalar(A,B2,Rt),
    suma(RAct,Rt,R2),
    productoRec(Y,B2,R2,R),
    !.

%   ===== composicion(i,i,o) =====
%   Composicion de dos polinomios.
%   Realiza la composicion de dos polinomios a y b 
%   de la forma a(b(x)).
%   composicion(A,B,R) donde:
%       A -> coeficientes del primer polinomio
%       B -> coeficientes del segundo polinomio
%       R -> coeficientes del polinomio resultante de 
%           la composicion.
composicion([],_,[]).
composicion([A0|A1],B,C):-
    composicion(A1,B,T),
    producto(B,T,P),
    suma([A0],P,C),
    !.

%   ===== evaluacion(i,i,i,o) =====
%   evaluación del polinomio.
%   evaluacion(A,G,X,R) donde 
%       A -> lista de coeficientes
%       G -> grado del primer termino del polinomio
%               (SIEMPRE INICIA EN 0).
%       X -> valor que se va evaluar,
%       R -> valor del polinomio evaluado
%
%   ejemplo:
%   evaluacion([2,1,1,1],0,2,X).
%       |   X = 16.
evaluacion([],_,_,0).
evaluacion([A|Y],G,X,R):-
    R1 is A*X^G,
    N is G + 1,
    evaluacion(Y,N,X,Rt),
    R is R1+Rt.

%   ===== derivada(i,o) =====
%   Devuelve la derivada del polinomio
%   derivada(A,R) donde:
%       A -> coeficientes del polinomio
%       R -> coeficientes de la derivada
derivada([A|Y],R):-
    derivadaRec(Y,1,R).

%   ===== derivadaRec(i,i,o) =====
%   <<< Esta es una regla auxiliar >>>
%   Realiza la parte recursiva de derivada
%   derivadaRec(A,G,R) donde:
%       A -> coeficientes del polinomio
%       G -> grado del término
%       R -> Resultado de la derivada
derivadaRec([],_,[]).
derivadaRec([A|Y],G,[Rh|Rb]):-
    Rh is A*G,
    N is G + 1,
    derivadaRec(Y,N,Rb).

%   ===== genTerm(i,i,o) =====
%   <<< Esta es una regla auxiliar >>>
%   Genera la expresión de cada término del polinomio
%   para construir la expresión final.
%   genTerm(A,G,R) donde:
%       A -> es el coeficiente del término
%       G -> es el grado del término
%       R -> es el término expresado en el tipo atom

%   Caso en que el coeficiente es 0. i.e. 0x^n = 0
genTerm(0,_,R):- R = ''.

%   Caso en que el grado es 0. i.e. ax^0 = a
genTerm(A,0,R):-
    term_to_atom(A,R).

%   Caso en que el coeficiente es 1. i.e. 1x^n = x^n
genTerm(1,G,R):-
    G > 1 ->
    term_to_atom(x^G,Res),
    atom_concat(' + ',Res,R);
    %   Si el grado tambien es 1 entonces 1x = x
    term_to_atom(x,Res),
    atom_concat(' + ',Res,R).

%   Caso en que el grado es 1. i.e. ax^1 = ax
genTerm(A,1,R):-
    term_to_atom(A*x,Res),
    atom_concat(' + ',Res,R).

%   Caso general
genTerm(A,G,R):-
    A > 0 ->
    term_to_atom(A*x^G,Res),
    atom_concat(' + ',Res,R);
    term_to_atom(A*x^G,R).

%   ===== toString(i,i,o) =====
%   Devuelve una expresión en términos de x para imprimir
%   en pantalla.
%   toString([A],G,R) donde
%       [A] -> es la lista de coeficientes
%       G -> es el grado del primer término 
%           (SIEMPRE INICIA 0)
%       R -> es la expresión en un tipo atom de prolog
toString([A],G,R):- 
    genTerm(A,G,R).
toString([A|Y],G,R):-   
    genTerm(A,G,R1),
    N is G + 1,
    toString(Y,N,Rt),
    atom_concat(R1, Rt, R),
    !.

%   === Polinomios de prueba ===
p1([1,2,3,4]).
p2([5,0,3,0]).

main:-
    p1(P1),
    p2(P2),
    write('polinomio1 = '),
    toString(P1,0,R),
    write(R),nl,
    write('polinomio2 = '),
    toString(P2,0,R1),
    write(R1),nl,
    suma(P1,P2,X),
    write('suma de polinomios = '),
    toString(X,0,R2),
    write(R2),nl,
    evaluacion(P1,0,2,S),
    write('Evaluacion P1(2)= '),
    write(S),nl,
    write('Derivada P2= '),
    derivada(P2,S1),
    toString(S1,0,R3),
    write(R3),nl,
    write('Segunda derivada P2= '),
    derivada(S1,S2),
    toString(S2,0,R4),
    write(R4),nl,
    write('Grado del polinomio1 = '),
    grado(P1,G),
    write(G),nl,
    write('Producto de P1 y P2 = '),
    producto(P1,P2,S3),
    toString(S3,0,R5),
    write(R5),nl,
    write('Producto de un Polinomio por un Escalar P1*2 = '),
    multPorEscalar(2,P1,S4),
    toString(S4,0,R6),
    write(R6),nl,
    write('Composicion de P1 y P2 = '),
    composicion(P1,P2,S5),
    toString(S5,0,R7),
    write(R7),nl,
    !.


main.