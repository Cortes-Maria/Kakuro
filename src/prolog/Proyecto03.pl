/*
Obtiene la linea de una matriz en la posicion N
E: La lista que representa la matriz, numero de fila por traer
S: devuelve la fila solicitada de la matriz enviada
*/
row(Sudoku, N, Row) :-
    N0 is N-1,
    N1 is N0*9+1, nth1(N1, Sudoku, X1),
    N2 is N0*9+2, nth1(N2, Sudoku, X2),
    N3 is N0*9+3, nth1(N3, Sudoku, X3),
    N4 is N0*9+4, nth1(N4, Sudoku, X4),
    N5 is N0*9+5, nth1(N5, Sudoku, X5),
    N6 is N0*9+6, nth1(N6, Sudoku, X6),
    N7 is N0*9+7, nth1(N7, Sudoku, X7),
    N8 is N0*9+8, nth1(N8, Sudoku, X8),
    N9 is N0*9+9, nth1(N9, Sudoku, X9),
    Row = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

/*
Obtiene la columna de una matriz en la posicion N
E: La lista que representa la matriz, numero de columna por traer
S: devuelve la columna solicitada de la matriz enviada
*/
column(Sudoku, N, Column) :-
    N1 is 0*9+N, nth1(N1, Sudoku, X1),
    N2 is 1*9+N, nth1(N2, Sudoku, X2),
    N3 is 2*9+N, nth1(N3, Sudoku, X3),
    N4 is 3*9+N, nth1(N4, Sudoku, X4),
    N5 is 4*9+N, nth1(N5, Sudoku, X5),
    N6 is 5*9+N, nth1(N6, Sudoku, X6),
    N7 is 6*9+N, nth1(N7, Sudoku, X7),
    N8 is 7*9+N, nth1(N8, Sudoku, X8),
    N9 is 8*9+N, nth1(N9, Sudoku, X9),
    Column = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

/*
Imprime una lista, en este caso es una fila de la matriz
E: la lista por imprimir
S: N/A
*/
imprimeLinea([H|[]]) :- write(H), nl, !.
imprimeLinea([H|T]) :- write(H), write('\t '), imprimeLinea(T).

/*
Imprime en pantalla un kakuro
E: una lista que representa el kakuro
*/
imprimirKakuro(Kak) :- 
            row(Kak, 1, L1), imprimeLinea(L1),
            row(Kak, 2, L2), imprimeLinea(L2),
            row(Kak, 3, L3), imprimeLinea(L3),
            row(Kak, 4, L4), imprimeLinea(L4),
            row(Kak, 5, L5), imprimeLinea(L5),
            row(Kak, 6, L6), imprimeLinea(L6),
            row(Kak, 7, L7), imprimeLinea(L7),
            row(Kak, 8, L8), imprimeLinea(L8),
            row(Kak, 9, L9), imprimeLinea(L9).

/*
Devuelve la cola de una lista en una posicion indicada
E: la lista a recortar, un contador, la posicion donde recortar
S: la lista ya recortada
*/
colaLista([_|T], I, N, R) :- I == N, R = T, !.
colaLista([_|T], I, N, R) :- I2 is I+1, colaLista(T, I2, N, R).

/*
Colocan en una posicion de la matriz un elemento
E: un kakuro, posicion X Y donde modificar el elemento, contador I J para iterar la matriz, elemento a insertar
S: el kakuro con el elemento ya insertado en la posicion deseada
*/
modificarElemento(_, _, _, R, 10, _, _) :- R = [], !.
modificarElemento(K, X, Y, R, I, J, S) :- X == I, J == Y, I1 is I+1,
                                          modificarElemento(K, X, Y, R1, I1, 1, S),
                                          row(K, I, L2),
                                          colaLista(L2, 1, J, L3),
                                          append([S|L3], R1, R), !.

modificarElemento(K, X, Y, R, I, J, S) :- X == I,  J1 is J+1,
                                          row(K, I, L1), 
                                          nth1(J, L1, E), 
                                          modificarElemento(K, X, Y, R1, I, J1, S), 
                                          append([E], R1, R), !.

modificarElemento(K, X, Y, R, I, J, S) :- I1 is I+1, row(K, I, L),
                                          modificarElemento(K, X, Y, R1, I1, J, S),
                                          append(L, R1, R).

modEl(X, Y, S, R, K) :- modificarElemento(K, X, Y, R, 1, 1, S).

/*Funcion de la biblioteca list_util*/
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).

split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :-
    % cannot optimize here because (+, -, -, -) would be wrong,
    % which could possibly be a useful generator.
    N > 0.
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
    split_at_(Xs, N0, Take, Rest).



kakuroIntermedio(1, Y) :- Y = [x, x, x, a, a, x, x, a, a,
                               x, a, c, 0, 0, x, c, 0, 0,
                               b, 3, 4, 5, 6, c, 0, 0, 0,
                               b, 0, 0, b, 0, 0, 0, 0, x,
                               x, x, x, b, 0, 0, x, x, x,
                               x, x, a, c, 0, 0, x, a, a,
                               x, c, 0, 0, 0, 0, c, 0, 0,
                               b, 0, 0, 0, b, 0, 0, 0, 0,
                               b, 1, 0, x, b, 0, 0, x, x].

kakuroInicial(1, Y) :- Y = [x, x, x, a, a, x, x, a, a,
                            x, a, c, 0, 0, x, c, 0, 0,
                            b, 3, 1, 5, 6, c, 0, 0, 0,
                            b, 0, 0, b, 0, 0, 0, 0, x,
                            x, x, x, b, 0, 0, x, x, x,
                            x, x, a, c, 0, 0, x, a, a,
                            x, c, 0, 0, 0, 0, c, 0, 0,
                            b, 0, 0, 0, b, 0, 0, 0, 0,
                            b, 2, 0, x, b, 0, 0, x, x].

kakuroInicial(2, Y) :- Y = [x, x, x, x, x, a, a, x, x,
                            x, a, a, x, b, 0, 0, a, a,
                            b, 0, 0, a, b, 0, 0, 0, 0,
                            b, 0, 0, 0, a, a, c, 0, 0,
                            x, b, 0, 0, 0, 0, 0, 0, x,
                            x, c, 0, 0, 0, 0, 0, 0, a,
                            b, 0, 0, a, a, b, 0, 0, 0,
                            b, 0, 0, 0, 0, x, b, 0, 0,
                            x, x, b, 0, 0, x, x, x, x].

kakuroInicial(3, Y) :- Y = [x, a, a, x, a, a, x, x, x,
                            b, 0, 0, c, 0, 0, a, x, x,
                            b, 0, 0, 0, 0, 0, 0, x, x,
                            x, c, 0, 0, c, 0, 0, a, a,
                            b, 0, 0, b, 0, 0, b, 0, 0,
                            b, 0, 0, c, 0, 0, c, 0, 0,
                            x, x, b, 0, 0, c, 0, 0, a,
                            x, x, b, 0, 0, 0, 0, 0, 0,
                            x, x, x, b, 0, 0, b, 0, 0].


/*
Cuenta la cantidad de ceros seguidos en una lista
E: la lista
S: cantidad de ceros seguidos
*/
contarAux([0|T], CantB) :- contarAux(T, R1), CantB is R1+1, !.
contarAux(_, CantB) :- CantB is 0.

/*
Retorna una lista con numeros del 1 al 9 desordenados aleatoriamente, se le indica cuantos elementos quiere
E: La cantidad de elementos que queremos obtener
S: lista con la cantidad de numeros deseados, del 1 al 9 sin repetirse y desordenados aleatoriamente
*/
numerosRandom(C, L) :- random_permutation([1,2,3,4,5,6,7,8,9], L1), split_at(C, L1, L, _).
numerosRandom(C, L) :- numerosRandom(C, L).


/*
Crea una lista con los numeros para saber si tienen numeros repetidos o no, esto lo verifica solo en las columnas
E: Lista, numero indicando si el anterior fue un numero o no, contador, iterador de lista.
S: retorna una lista con los numeros encontrados, esto excluye todo lo que no sean numeros del 1 al 9.
*/
creaLista([0|T], L, _, C, Y) :- C1 is C+1, creaLista(T, L, 0, C1, Y), !.
creaLista([H|T], L, _, C, Y) :- member(H, [1,2,3,4,5,6,7,8,9]), C1 is C+1, creaLista(T, L1, 1, C1, Y), append([H], L1, L), !.
creaLista([_|T], L, 0, C, Y) :- C1 is C+1, creaLista(T, L, 0, C1, Y), !.
creaLista([_|_], L, _, C, Y) :- Y is C, append([], [], L), !.

/*
Revisa una lista para ver si hay numeros repetidos
E: una columna, un iterador de la columna
S: True si no hay numeros repetidos, False si los hay
*/
revisaColumnasAux([], _) :- !.
revisaColumnasAux(_, Y) :- Y > 8, !.
revisaColumnasAux(Columna, Y) :- split_at(Y, Columna, _, L), creaLista(L, L1, 0, Y, Y1), is_set(L1), revisaColumnasAux(Columna, Y1).

/*
Revisa todas las columnas para ver si en alguna hay numeros repetidos
E: el kakuro
S: True si no hay nada repetido en ninguna columna, False si si hay repetidos
*/
revisaColumnas(K) :- 
                    column(K, 2, C2A), append(C2A, [x], C2B), revisaColumnasAux(C2B, 1),
                    column(K, 3, C3A), append(C3A, [x], C3B), revisaColumnasAux(C3B, 1),
                    column(K, 4, C4A), append(C4A, [x], C4B), revisaColumnasAux(C4B, 1),
                    column(K, 5, C5A), append(C5A, [x], C5B), revisaColumnasAux(C5B, 1),
                    column(K, 6, C6A), append(C6A, [x], C6B), revisaColumnasAux(C6B, 1),
                    column(K, 7, C7A), append(C7A, [x], C7B), revisaColumnasAux(C7B, 1),
                    column(K, 8, C8A), append(C8A, [x], C8B), revisaColumnasAux(C8B, 1),
                    column(K, 9, C9A), append(C9A, [x], C9B), revisaColumnasAux(C9B, 1), !.

/*
Inserta en cada fila numeros del 1 al 9 sin repetirlos
E: una linea para insertarle numeros, posicion X Y, el kakuro
S: retorna el kakuro con la linea con los numeros ya insertados
*/
insertarNumeros([_|T], _, _, K, R) :- T == [], R = K, !.
insertarNumeros([0|T], X, Y, K, R) :- contarAux([0|T], CantB), numerosRandom(CantB, L), 
                                      insertarNumerosAux(K, L, X, Y, K1), Y1 is Y+1, 
                                      row(K1, X, T1), 
                                      split_at(Y, T1, _, T2), 
                                      insertarNumeros(T2, X, Y1, K1, R), !.
insertarNumeros([_|T], X, Y, K, R) :- T \= [], Y1 is Y+1, insertarNumeros(T, X, Y1, K, R).

/*
Funcion auxiliar de insertaNumeros, es la que inserta en cada posicion
E: el kakuro, Lista con elementos, posicion X Y
R: retorna el kakuro con el elemento insertado
*/
insertarNumerosAux(K, [H|T], X, Y, R) :- T == [], modEl(X, Y, H, R, K), !.
insertarNumerosAux(K, [H|T], X, Y, R) :- modEl(X, Y, H, R1, K), Y1 is Y+1, insertarNumerosAux(R1, T, X, Y1, R), !.

/*
Funcion principal de insertarNumeros, el predicado de abajo es para que cuando en el primero las
columnas no cumplan con numeros no repetidos, entonces entra al segundo y este haria que se vuelvan
a generar los numeros
E: La linea, posicion XY de inicio, el kakuro
S: el kakuro con los numeros ya insertados
*/
insNum(Linea, X, Y, K, R) :- insertarNumeros(Linea, X, Y, K, R).
insNum(Linea, X, Y, K, R) :- insNum(Linea, X, Y, K, R).

kakuroFinal(
            [x    , x     , x     , x      , x     , 14/x  , 14/x  , x     , x    ,
             x    , 11/x  , 30/x  , x      , x/11  , 6     , 5     , 33/x  , 13/x ,
             x/7  , 6     , 1     , 12/x   , x/30  , 8     , 9     , 7     , 6    ,
             x/19 , 5     , 6     , 8      , 13/x  , 11/x  , 15/11 , 4     , 7    ,
             x    , x/29  , 4     , 3      , 9     , 6     , 5     , 2     , x    ,
             x    , 9/26  , 3     , 1      , 4     , 5     , 7     , 6     , 9/x  ,
             x/12 , 3     , 9     , 13/x   , 4/x   , x/12  , 3     , 5     , 4    ,
             x/23 , 6     , 7     , 9      , 1     , x     , x/14  , 9     , 5    ,
             x    , x     , x/7   , 4      , 3     , x     , x     , x     , x    ]).

/*
Suma los numeros de X cantidad de espacios con numeros de 1 al 9
E: lista con los numeros y demas elementos
S: retorna la suma de todos esos numeros que se encuentran seguidos
*/
sumarNumeros([H|T], Suma) :- member(H, [1,2,3,4,5,6,7,8,9]), sumarNumeros(T, R1), Suma is R1+H, !.
sumarNumeros(_, Suma) :- Suma is 0.

/*
Recibe una posicion en el kakuro, y verifica si hay numeros a la derecha o abajo,
si los hay entonces obtiene la suma para colocarlo en la posicion como las clues para el kakuro
E: una linea, una columna, el elemento que hay en el kakuro en la posicion XY
S: retorna el elemento que deberia insertarse en la posicion XY que recibio de parametro.
*/
contar(_, Columna, a, X, _, E) :- split_at(X, Columna, _, L1), 
                                  sumarNumeros(L1, Suma), 
                                  E = Suma/x, !.

contar(Linea, _, b, _, Y, E) :- split_at(Y, Linea, _, L1), 
                                sumarNumeros(L1, Suma), 
                                E = x/Suma, !.

contar(Linea, Columna, c, X, Y, E) :- split_at(X, Columna, _, L1), 
                                      sumarNumeros(L1, Suma1), 
                                      split_at(Y, Linea, _, L2), 
                                      sumarNumeros(L2, Suma2),
                                      E = Suma1/Suma2, !.
contar(_, _, X, _, _, E) :- E = X.

/*
Se encarga de colocar las sumas de los numeros en sus correspondientes casillas
E: kakuro resuelto, kakuro sin resolver, posicion XY
S: retorna tanto el kakuro resuelto como el no resuelto, ya con los numeros sumados colocados.
*/
colocaNumeros(KR, KSR, 10, _, R1, R2) :- R1 = KR, R2 = KSR, !.
colocaNumeros(KR, KSR, X, 10, R1, R2) :- X1 is X+1, colocaNumeros(KR, KSR, X1, 1, R1, R2), !.
colocaNumeros(KR, KSR, X, Y, R1, R2) :- row(KR, X, Linea),
                             column(KR, Y, Columna),
                             nth1(Y, Linea, Elemento),
                             contar(Linea, Columna, Elemento, X, Y, E),
                             modEl(X, Y, E, KR1, KR),
                             insertaNumeroKSR(KSR, E, X, Y, KSR1),
                             Y1 is Y+1,
                             colocaNumeros(KR1, KSR1, X, Y1, R1, R2).

/*
Se encarga de colocar en el kakuro sin solucion los numeros en las casillas correspondientes
E: el kakuro, el elemento a insertar, posicion XY
S: el kakuro con el numero ya insertado
*/
insertaNumeroKSR(K, E, X, Y, R) :- member(E, [1,2,3,4,5,6,7,8,9]),
                                modEl(X, Y, 0, R, K), !.
insertaNumeroKSR(K, E, X, Y, R) :- modEl(X, Y, E, R, K), !.


/*
Se encarga de generar aleatoriamente un kakuro
E: N/A
S: retorna dos kakuros, uno con la solucion y otro sin ella
*/
generarKakuro(R1, R2) :- random(1, 4, Num), kakuroInicial(Num, K), revisaColumnas(K),
                 row(K, 2, L2),  insNum(L2, 2, 1, K, K1),  revisaColumnas(K1),
                 row(K1, 3, L3), insNum(L3, 3, 1, K1, K2), revisaColumnas(K2),
                 row(K2, 4, L4), insNum(L4, 4, 1, K2, K3), revisaColumnas(K3),
                 row(K3, 5, L5), insNum(L5, 5, 1, K3, K4), revisaColumnas(K4),
                 row(K4, 6, L6), insNum(L6, 6, 1, K4, K5), revisaColumnas(K5),
                 row(K5, 7, L7), insNum(L7, 7, 1, K5, K6), revisaColumnas(K6),
                 row(K6, 8, L8), insNum(L8, 8, 1, K6, K7), revisaColumnas(K7),
                 row(K7, 9, L9), insNum(L9, 9, 1, K7, K8), revisaColumnas(K8),
                 colocaNumeros(K8, K, 1, 2, R1, R2),
                 write('Kakuro sin resolver'), nl, imprimirKakuro(R2), nl,
                 write('Kakuro resuelto'), nl, imprimirKakuro(R1), nl, !.

/*
Verifica el estado de un kakuro
E: recibe el kakuro que se esta jugando y el kakuro con la solucion de este
S: retorna la cantidad de errores que tiene el jugador, y la cantidad de campos pendientes de rellenar
*/
verificarKakuro(K1, K2, Errores, CamposPendientes) :- cuentaVacios(K1, 2, CamposPendientes), cuentaErrores(K1, K2, 2, Errores).

/*
Cuenta la cantidad de espacios vacios en una fila
E: la lista
S: cantidad de espacios vacios de esa fila
*/
cuentaVaciosAux(L, Total) :- L == [], Total is 0, !.
cuentaVaciosAux([0|T], Total) :- cuentaVaciosAux(T, T1), Total is T1+1, !.
cuentaVaciosAux([_|T], Total) :- cuentaVaciosAux(T, Total), !.

/*
Cuenta la cantidad de espacios en blanco de un kakuro entero
E: el kakuro, un iterador X para desplazarse entre filas
S: cantidad de espacios vacios en todo el kakuro
*/
cuentaVacios(_, 10, 0) :- !.
cuentaVacios(K, X, Total) :- row(K, X, Linea), cuentaVaciosAux(Linea, T1), X1 is X+1, cuentaVacios(K, X1, T2), Total is T1+T2, !.

/*
Cuenta la cantidad de errores que tiene el jugador en el kakuro que esta jugando
E: el kakuro que se esta jugando, el kakuro con la solucion, iterador X de filas
S: retorna la cantidad de errores que tiene el jugador
*/
cuentaErrores(_, _, 10, 0) :- !.
cuentaErrores(K1, K2, X, Total) :- row(K1, X, L1), row(K2, X, L2), cuentaErroresAux(L1, L2, T1), X1 is X+1, cuentaErrores(K1, K2, X1, T2), Total is T1+T2.

/*
Cuenta la cantidad de errores en una fila especifica
E: la fila del kakuro que se resuelve, la fila del kakuro con la solucion
S: cantidad de errores en esa fila especifica*/
cuentaErroresAux(L, _, Total) :- L == [], Total is 0.
cuentaErroresAux([H1|T1], [H2|T2], Total) :- member(H1, [1,2,3,4,5,6,7,8,9]), H1 \= H2, cuentaErroresAux(T1, T2, Total1), Total is Total1+1, !.
cuentaErroresAux([_|T1], [_|T2], Total) :- cuentaErroresAux(T1, T2, Total).