
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

editarColumnas(X, _, _, _, K, R) :- X > 9, R is K.
editarColumnas(X, Y, I, C, K, R) :- I == C, modEl(X, Y, 0, R, K).
editarColumnas(X, Y, I, C, K, R) :- X1 is X+1, I1 is I+1,
                                    editarColumnas(X1, Y, I1, C, K, R1),
                                    modEl(X, Y, 0, R, R1).

%generarLineas(K, 1) :- row(K, 1, L).
generarCasillas(Num, X) :- Num < 10, X is 2, !.
generarCasillas(Num, X) :- Num > 17, random(3, 5, X), !.
generarCasillas(Num, X) :- Num > 23, random(4, 6, X), !.
generarCasillas(_, X) :- random(2, 4, X).

editarTablero(1, Y, 1, K, R) :- random(4, 28, NumR),
                                generarCasillas(NumR, NumC),
                                E = NumR/x,
                                modEl(1, Y, E, R1, K),
                                editarColumnas(2, Y, 1, NumC, R1, R).

editarTablero(1, Y, _, K, R) :- modEl(1, Y, x, R, K).

generaLinea(1, 10, R, K) :- R = K.
generaLinea(1, Y, R, K) :- random(1, 4, Num),
                           Y1 is Y+1,
                           generaLinea(1, Y1, R1, K),
                           editarTablero(1, Y, Num, R1, R).
                           

generarKakuro :- kakuroInicial(K), generaLinea(1, 2, R, K), escribeLineas(R).

verLinea(Num) :- kakuro(X), row(X, Num, Row), write(Row).
verColumna(Num) :- kakuro(X), column(X, Num, Column), write(Column).

prueba :- kakuroInicial(K), generaLinea(1, 10, R, K), escribeLineas(R).