neighbor(X,Y,NX,Y):-
    NX is X-1,
    cell(NX,Y).

neighbor(X,Y,NX,Y):-
    NX is X+1,
    cell(NX,Y).

neighbor(X,Y,X,NY) :-
    NY is Y-1,
    cell(X,NY).

neighbor(X,Y,X,NY) :-
    NY is Y+1,
    cell(X,NY).

neighbors(X,Y,L) :-
    findall(neighbor(NX,NY),neighbor(X,Y,NX,NY),L).


size(Rows,Columns).
size(8,8).
wall(Rows,Columns).
wall(1,6).
wall(2,2).
wall(2,3).
wall(3,7).
wall(4,1).
wall(4,5).
wall(5,4).
wall(5,8).
wall(6,2).
wall(7,6).
wall(7,7).
wall(8,3).
wall_num(1,6,1).
wall_num(2,2,3).
wall_num(3,7,0).
wall_num(5,4,4).
wall_num(5,8,0).
wall_num(6,2,2).
wall_num(7,6,1).

light(1,1).
light(1,5).
lght(1,7).


cell(X,Y):-X>0,X<9,Y>0,Y<9.

neighbors(X,Y,L):-cell(X,Y),
cell(X+1,Y),X1 is X + 1,Y1 is Y  ,L =[X1,Y1];
cell(X-1,Y),X1 is X - 1,Y1 is Y  ,L=[X1,Y1];
cell(X,Y+1),X1 is X    ,Y1 is Y + 1 ,L=[X1,Y1];
cell(X,Y-1),X1 is X    ,Y1 is Y - 1 ,L=[X1,Y1].

yneighbors(X,Y,L):-cell(X,Y+1),L=[X,Y].

grid_row(R,N,(R,N)) :-size(8,8),\+wall_num(R,N,_),\+wall(R,N).
grid_row(R,N,Row) :-M is N + 1,cell(R, M),grid_row(R, M, Row).
grid_row(R,N,Row) :- M is N-7,R1 is R + 1 ,cell(R1, M),grid_row(R1, M,Row).
collect(R,N,M):-findall((Row),grid_row(R,N,Row),M).


:- call1(......, L1), call2(.......,L2), append(L1,L2,L).


cellRight(X,Y,L):-  X1 is X + 1, cell(X1,Y), not(wall(X1,Y)), not(wall_num(X1,Y,_)), cellRight(X1,Y,L1), append([[X1,Y]],L1,L).

cellRight(_,_,[]).

cellLeft(X,Y,L):-  X1 is X - 1, cell(X1,Y), not(wall(X1,Y)), not(wall_num(X1,Y,_)), cellLeft(X1,Y,L1), append([[X1,Y]],L1,L).

cellLeft(_,_,[]).



exRow(X,Y,L):- cellRight(X,Y,L1), cellLeft(X,Y,L2), append(L1,L2,L).



cellUp(Y,X,L):- Y1 is Y + 1, cell(Y1,X), not(wall(Y1,X)), not(wall_num(Y1,X,_)), cellUp(Y1,X,L1), append([[Y1,X]],L1,L).

cellUp(_,_,[]).

cellDown(Y,X,L):-  Y1 is Y - 1, cell(Y1,X), not(wall(Y1,X)), not(wall_num(Y1,X,_)), cellDown(Y1,X,L1), append([[Y1,X]],L1,L).

cellUp(_,_,[]).


exRow(Y,X,L):-cellUp(Y,X,L1),cellDown(Y,X,L2), append(L1,L2,L).
