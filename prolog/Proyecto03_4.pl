
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

kakuroInicial([x,x,x,x,x,x,x,x,x,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n,
               x,n,n,n,n,n,n,n,n]).                      


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




kakuroInicial(1, Y) :- Y = [x, x, x, a, a, x, x, a, a,
                            x, a, c, 0, 0, x, c, 0, 0,
                            b, 0, 0, 0, 0, c, 0, 0, 0,
                            b, 0, 0, b, 0, 0, 0, 0, x,
                            x, x, x, b, 0, 0, x, x, x,
                            x, x, a, c, 0, 0, x, a, a,
                            x, c, 0, 0, 0, 0, c, 0, 0,
                            b, 0, 0, 0, b, 0, 0, 0, 0,
                            b, 0, 0, x, b, 0, 0, x, x].

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

contarAux([0|T], CantB) :- contarAux(T, R1), CantB is R1+1, !.
contarAux(_, CantB) :- CantB is 0.

contar(_, Columna, a, X, _, E) :- split_at(X, Columna, _, L1), 
                                  contarAux(L1, CantB), 
                                  rango(CantB, Num), 
                                  E = Num/x, !.

contar(Linea, _, b, _, Y, E) :- split_at(Y, Linea, _, L1), 
                                contarAux(L1, CantB), 
                                rango(CantB, Num), 
                                E = x/Num, !.

contar(Linea, Columna, c, X, Y, E) :- split_at(X, Columna, _, L1), 
                                      contarAux(L1, CantB1), 
                                      split_at(Y, Linea, _, L2), 
                                      contarAux(L2, CantB2),
                                      rango(CantB1, Num1),
                                      rango(CantB2, Num2),
                                      E = Num1/Num2, !.

contar(_, _, x, _, _, E) :- E = x.
contar(_, _, 0, _, _, E) :- E = 0.


colocaNumeros(K, 10, _, R) :- R = K, !.
colocaNumeros(K, X, 10, R) :- X1 is X+1, colocaNumeros(K, X1, 1, R), !.

colocaNumeros(K, X, Y, R) :- row(K, X, Linea),
                             column(K, Y, Columna),
                             nth1(Y, Linea, Elemento),
                             contar(Linea, Columna, Elemento, X, Y, E),
                             modEl(X, Y, E, R1, K),
                             Y1 is Y+1,
                             colocaNumeros(R1, X, Y1, R).


rango(0, Num) :- Num = x.
rango(2, Num) :- random(3, 18, Num).
rango(3, Num) :- random(6, 25, Num).
rango(4, Num) :- random(10, 31, Num).
rango(5, Num) :- random(15, 36, Num).
rango(6, Num) :- random(21, 40, Num).

prueba :- random(1, 4, X), kakuroInicial(X, K), colocaNumeros(K, 1, 1, R), escribeLineas(R).