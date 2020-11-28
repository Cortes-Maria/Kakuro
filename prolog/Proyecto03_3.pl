
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

sodoku( [4,x,x,x,6,x,9,1,x,
         2,x,x,x,x,7,x,5,x,
         x,9,x,8,x,x,x,2,x,
         x,x,1,6,x,9,x,x,2,
         x,8,x,x,x,x,x,6,3,
         x,7,x,x,4,x,x,x,x,
         7,x,3,x,x,8,x,9,x,
         x,x,x,x,3,x,4,x,5,
         x,4,x,9,x,x,6,x,x]).

kakuro([x    , 17/x  , 16/x  , x     , x     , 12/x , 6/x  , x    , x    ,
        x/16 , 9     , 7     , 23/x  , x/4   , 3    , 1    , 26/x , 23/x ,
        x/23 , 8     , 9     , 6     , 29/29 , 9    , 5    , 7    , 8    ,
        x    , x     , 18/17 , 9     , 8     , 23/x , x/17 , 8    , 9    ,
        x    , 27/27 , 3     , 8     , 7     , 9    , 23/8 , 2    , 6    ,
        x/9  , 7     , 2     , x/24  , 1     , 6    , 8    , 9    , x    ,
        x/4  , 3     , 1     , 8/21  , 4     , 8    , 9    , 17/x , 16/x ,
        x/22 , 8     , 4     , 1     , 9     , x/23 , 6    , 8    , 9    ,
        x/24 , 9     , 8     , 7     , x     , x    , x/16 , 9    , 7     ]).

imprimeLinea([H|[]]) :- write(H), nl, !.
imprimeLinea([H|T]) :- write(H), write('\t '), imprimeLinea(T).

escribeLineas(Kak) :- 
            row(Kak, 1, L1), imprimeLinea(L1),
            row(Kak, 2, L2), imprimeLinea(L2),
            row(Kak, 3, L3), imprimeLinea(L3),
            row(Kak, 4, L4), imprimeLinea(L4),
            row(Kak, 5, L5), imprimeLinea(L5),
            row(Kak, 6, L6), imprimeLinea(L6),
            row(Kak, 7, L7), imprimeLinea(L7),
            row(Kak, 8, L8), imprimeLinea(L8),
            row(Kak, 9, L9), imprimeLinea(L9).

imprimirKakuro :- kakuro(X), 
                  escribeLineas(X).

kakuroInicial([x,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n,
               n,n,n,n,n,n,n,n,n]).

colaLista([_|T], I, N, R) :- I == N, R = T, !.
colaLista([_|T], I, N, R) :- I2 is I+1, colaLista(T, I2, N, R).

%modificarElemento(K, X, Y, R, I, 10, S) :- I1 is I+1, modificarElemento(K, X, Y, R, I1, 1, S), !.
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


editarColumnas(X, _, _, _, K, R) :- X > 9, R = K, !.
editarColumnas(X, Y, I, C, K, R) :- C1 is C+1, I == C1, modEl(X, Y, x, R, K).
editarColumnas(X, Y, I, C, K, R) :- X1 is X+1, I1 is I+1,
                                    editarColumnas(X1, Y, I1, C, K, R1),
                                    modEl(X, Y, 0, R, R1).

editarFilas(X, Y, I, C, K, R, Y2, E1, _) :- editarFilasAux(X, Y, I, C, K, R1, Y2, N), N == 1, Y1 is Y-1, modEl(X, Y1, E1, R, R1), write('BElemento: '), write(E1), nl, write('Kakuro:'), nl, escribeLineas(R), nl.
editarFilas(X, Y, I, C, K, R, Y2, _, E2) :- editarFilasAux(X, Y, I, C, K, R1, Y2, _), Y1 is Y-1, modEl(X, Y1, E2, R, R1), write('MElemento: '), write(E2), nl, write('Kakuro:'), nl, escribeLineas(R), nl.
editarFilasAux(X, Y, _, C, K, R, Y2, N) :- estaMala(X, Y, C, K), R = K, N is 2, Y2 is Y+1, write('Entre\n'), !.
editarFilasAux(_, Y, _, _, K, R, _, N) :- Y > 9, R = K, N is 1, !.
editarFilasAux(X, Y, I, C, K, R, Y2, N) :- I == C, modEl(X, Y, 0, R, K), Y2 is Y+1, N is 1.
editarFilasAux(X, Y, I, C, K, R, Y2, N) :- Y1 is Y+1, I1 is I+1,
                                     editarFilasAux(X, Y1, I1, C, K, R1, Y2, N),
                                     modEl(X, Y, 0, R, R1).

%comprueba que las columnas que siguen no tengan x
estaMala(X, Y, C, K) :- comprobarColumnas(X, Y, K), comprobarUltimaColumna(X, Y, C, K), !.
estaMala(X, Y, _, K) :- comprobarColumnas(X, Y, K), !.
estaMala(X, Y, C, K) :- comprobarUltimaColumna(X, Y, C, K), !.

comprobarColumnas(X, Y, K) :- Y1 is Y-1, row(K, X, L1), split_at(Y1, L1, _, L2), member(x, L2), !.
comprobarUltimaColumna(X, Y, C, K) :- row(K, X, L1), C1 is Y+C, nth1(C1, L1, E), E==0, !.

%generarLineas(K, 1) :- row(K, 1, L).
generarCasillas(Num, X) :- Num < 10, X is 2, !.
generarCasillas(Num, X) :- Num > 23, random(4, 6, X), !.
generarCasillas(Num, X) :- Num > 17, random(3, 5, X), !.
generarCasillas(_, X) :- random(2, 4, X).

%primera linea con numero
editarTablero(1, Y, 1, K, R) :- random(4, 28, NumR),
                                generarCasillas(NumR, NumC),
                                E = NumR/x,
                                modEl(1, Y, E, R1, K),
                                editarColumnas(2, Y, 1, NumC, R1, R), write('1\n'), escribeLineas(R).
%primera linea sin numero
editarTablero(1, Y, _, K, R) :- modEl(1, Y, x, R, K), write('1N\n'), escribeLineas(R).

%si la casilla tiene un 0 no se pondria nada
editarTablero(X, Y, _, K, R, Y1) :- row(K, X, L1),
                                   nth1(Y, L1, E),
                                   E == 0, Y1 is Y+1, R = K, write('2\n'), !.

%si se esta en la primera columna no deberia haber numeros abajo
editarTablero(X, 1, 1, K, R, Y1) :- modEl(X, 1, x, R, K),
                                    Y1 is 2, write('3\n'), !.
editarTablero(X, 1, 3, K, R, Y1) :- random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, 1, E, R1, K),
                                    Y2 is 2,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('4\n'), !.

%cuando se esta en los ultimos 4 campos se insertaria una x
editarTablero(X, Y, 3, K, R, Y1) :- Y>7, X>7, modEl(X, Y, x, R, K),
                                    Y1 is Y+1, write('5\n'), !.

%----------------------------------------------------------
%cuando se esta en la columna 6 y numero derecha, este deberia ser pequeno
editarTablero(X, 6, 2, K, R, Y1) :- random(4, 20, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, 6, E, R1, K),
                                    Y2 is 7,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('6\n'), !.

%columna 6 y fila  mayor a 7 numeros pequenos derecha
editarTablero(X, 6, 3, K, R, Y1) :- X>7, random(4, 20, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, 6, E, R1, K),
                                    Y2 is 7,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('7\n'), !.
%random(4, 20, NumR)
%columna 6, fila 6
editarTablero(X, 6, 3, K, R, Y1) :- X==6, random(4, 20, NumR1),
                                    random(4, 20, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 6, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 7,
                                    editarColumnas(X1, 6, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('8\n'), !.
%columna 6, fila 7
editarTablero(X, 6, 3, K, R, Y1) :- X==7, random(4, 14, NumR1),
                                    random(4, 20, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 6, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 7,
                                    editarColumnas(X1, 6, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('9\n'), !.

%cuando se esta en la fila 6 y numero abajo, este deberia ser pequeno
editarTablero(X, 6, 3, K, R, Y1) :- random(4, 28, NumR1),
                                    random(4, 20, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 6, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 7,
                                    editarColumnas(X1, 6, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('10\n'), !.







%cuando se esta en la columna 7 y numero derecha, este deberia ser pequeno
editarTablero(X, 7, 2, K, R, Y1) :- random(4, 14, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, 7, E, R1, K),
                                    Y2 is 8,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('11\n'), !.

%columna 7 fila mayor a 7 
editarTablero(X, 7, 3, K, R, Y1) :- X>7, random(4, 14, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, 7, E, R1, K),
                                    Y2 is 8,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('12\n'), !.

%columna 7, fila 6
editarTablero(X, 7, 3, K, R, Y1) :- X==6, random(4, 20, NumR1),
                                    random(4, 14, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 7, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 8,
                                    editarColumnas(X1, 7, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('13\n'), !.
%columna 7, fila 7
editarTablero(X, 7, 3, K, R, Y1) :- X==7, random(4, 14, NumR1),
                                    random(4, 14, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 7, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 8,
                                    editarColumnas(X1, 7, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('14\n'), !.


%cuando se esta en la columna 7 y numero derecha, este deberia ser pequeno
editarTablero(X, 7, 3, K, R, Y1) :- random(4, 28, NumR1),
                                    random(4, 14, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, 7, E, R1, K),
                                    X1 is X+1,
                                    Y2 is 8,
                                    editarColumnas(X1, 7, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('15\n'), !.

%--------------------------------------------------------------------


%cuando se esta en la fila 6 y numero abajo, este deberia ser pequeno
editarTablero(6, Y, 1, K, R, Y1) :- random(4, 20, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(6, Y, E, R1, K),
                                    X1 is 7,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('16\n'), !.

%fila 7 columna mayor a 6 numeros pequenos abajo y x derecha
editarTablero(6, Y, 3, K, R, Y1) :- Y>7, random(4, 20, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(6, Y, E, R1, K),
                                    X1 is 7,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('17\n').

%cuando se esta en la fila 6 y numero abajo, este deberia ser pequeno
editarTablero(6, Y, 3, K, R, Y1) :- random(4, 20, NumR1),
                                    random(4, 28, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(6, Y, E, R1, K),
                                    X1 is 7,
                                    Y2 is Y+1,
                                    editarColumnas(X1, Y, 1, NumC1, K, R1),
                                    editarFilas(6, Y2, 1, NumC2, R1, R, Y1, E, E1), write('18\n'), !.


%cuando se esta en la fila 7 y numero abajo, este deberia ser pequeno
editarTablero(7, Y, 1, K, R, Y1) :- random(4, 14, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(7, Y, E, R1, K),
                                    X1 is 8,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('19\n'), !.

%fila 7 columna mayor a 7 numeros pequenos abajo y x derecha
editarTablero(7, Y, 3, K, R, Y1) :- Y>7, random(4, 14, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(7, Y, E, R1, K),
                                    X1 is 8,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('20\n').

%cuando se esta en la fila 7 y numero abajo, este deberia ser pequeno
editarTablero(7, Y, 3, K, R, Y1) :- random(4, 14, NumR1),
                                    random(4, 28, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(7, Y, E, R1, K),
                                    X1 is 8,
                                    Y2 is Y+1,
                                    editarColumnas(X1, Y, 1, NumC1, K, R1),
                                    editarFilas(7, Y2, 1, NumC2, R1, R, Y1, E, E1), write('21\n'), !.


%si se esta en las ultimas 2 columnas no deberia haber numeros a la derecha
/*editarTablero(X, 9, 2, K, R, Y1) :- modEl(X, 9, x, R, K),
                                    Y1 is 10, !.*/
editarTablero(X, Y, 2, K, R, Y1) :- Y>7, modEl(X, Y, x, R, K),
                                    Y1 is Y+1, write('22\n'), !.





%si se esta en las ultimas 2 filas no deberian haber numeros abajo
/*editarTablero(9, Y, 1, K, R, Y1) :- modEl(9, Y, x, R, K),
                                    Y1 is Y+1, !.*/
editarTablero(X, Y, 1, K, R, Y1) :- X>7, modEl(X, Y, x, R, K),
                                    Y1 is Y+1, write('23\n'), !.


%dos numeros ultimas dos columnas solo deberia haber abajo
/*editarTablero(X, 9, 3, K, R, Y1) :- random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(X, 9, E, R1, K),
                                    X1 is X+1,
                                    editarColumnas(X1, 9, 1, NumC, R1, R),
                                    Y1 is 10, !.*/
/*si se esta despues de la columna 7 y en la fila 8, deberia ser un numero del 4 al 9
editarTablero(X, Y, 3, K, R, Y1) :- Y>7, X==7, random(4, 10, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(X, Y, E, R1, K),
                                    X1 is X+1,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, !.*/

editarTablero(X, Y, 3, K, R, Y1) :- Y>7, random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(X, Y, E, R1, K),
                                    X1 is X+1,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('24\n'), !.

%dos numeros ultima fila solo deberian haber a la derecha
/*editarTablero(9, Y, 3, K, R, Y1) :- random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    modEl(9, Y, E, R1, K),
                                    Y2 is Y+1,
                                    editarFilas(9, Y2, 1, NumC, R1, R, Y1), !.*/
editarTablero(X, Y, 3, K, R, Y1) :- X>7, random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, Y, E, R1, K),
                                    Y2 is Y+1,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('25\n'), !.

%cualquier linea, numero abajo
editarTablero(X, Y, 1, K, R, Y1) :- random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = NumR/x,
                                    modEl(X, Y, E, R1, K),
                                    X1 is X+1,
                                    editarColumnas(X1, Y, 1, NumC, R1, R),
                                    Y1 is Y+1, write('26\n'), !.

%cualquier linea, numero derecha
editarTablero(X, Y, 2, K, R, Y1) :- random(4, 28, NumR),
                                    generarCasillas(NumR, NumC),
                                    E = x/NumR,
                                    %modEl(X, Y, E, R1, K),
                                    Y2 is Y+1,
                                    editarFilas(X, Y2, 1, NumC, K, R, Y1, E, x), write('27\n'), !.

%cualquier linea, numero abajo y derecha
editarTablero(X, Y, 3, K, R, Y1) :- random(4, 28, NumR1),
                                    random(4, 28, NumR2),
                                    generarCasillas(NumR1, NumC1),
                                    generarCasillas(NumR2, NumC2),
                                    E = NumR1/NumR2,
                                    E1 = NumR1/x,
                                    %modEl(X, Y, E, R1, K),
                                    Y2 is Y+1,
                                    X1 is X+1,
                                    editarColumnas(X1, Y, 1, NumC1, K, R1),
                                    editarFilas(X, Y2, 1, NumC2, R1, R, Y1, E, E1), write('28\n'), !.
                                    



%cualquier linea, no numero
editarTablero(X, Y, _, K, R, Y1) :- modEl(X, Y, x, R, K), Y1 is Y+1, write('29\n').

%primera linea
generaLinea(1, 10, R, K) :- R = K, !.
generaLinea(1, Y, R, K) :- random(1, 4, Num),
                           Y1 is Y+1,
                           generaLinea(1, Y1, R1, K),
                           editarTablero(1, Y, Num, R1, R).
%cualquier linea
generaLinea(_, 10, R, K) :- R = K.
generaLinea(X, Y, R, K) :- random(1, 4, Num), 
                           editarTablero(X, Y, Num, K, R1, Y1),
                           generaLinea(X, Y1, R, R1).


%llamar a esta funcion para generar
generarKakuro :- kakuroInicial(K), 
                 generaLinea(1, 2, R, K), 
                 generaLinea(2, 1, R1, R), 
                 generaLinea(3, 1, R2, R1),
                 generaLinea(4, 1, R3, R2),
                 generaLinea(5, 1, R4, R3),
                 generaLinea(6, 1, R5, R4),
                 generaLinea(7, 1, R6, R5),
                 generaLinea(8, 1, R7, R6),
                 generaLinea(9, 1, R8, R7),
                 escribeLineas(R8).


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