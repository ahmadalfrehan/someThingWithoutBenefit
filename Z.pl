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
light(1,7).
cell(X,Y):-X>0,X<9,Y>0,Y<9.

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



%rowAnd:- call1(......, L1), call2(.......,L2), append(L1,L2,L).

%second algorithm


cellRight(X,Y,L):-  X1 is X + 1, cell(X1,Y), not(wall(X1,Y)), not(wall_num(X1,Y,_)), cellRight(X1,Y,L1), append([[X1,Y]],L1,L).

cellRight(_,_,[]).

cellLeft(X,Y,L):-  X1 is X - 1, cell(X1,Y), not(wall(X1,Y)), not(wall_num(X1,Y,_)), cellLeft(X1,Y,L1), append([[X1,Y]],L1,L).

cellLeft(_,_,[]).

exRow(X,Y,L):- cellRight(X,Y,L1), cellLeft(X,Y,L2), append(L1,L2,L).

cellUp(Y,X,L):- Y1 is Y + 1, cell(Y1,X), not(wall(Y1,X)), not(wall_num(Y1,X,_)), cellUp(Y1,X,L1), append([[Y1,X]],L1,L).

cellUp(_,_,[]).

cellDown(Y,X,L):-  Y1 is Y - 1, cell(Y1,X), not(wall(Y1,X)), not(wall_num(Y1,X,_)), cellDown(Y1,X,L1), append([[Y1,X]],L1,L).

cellDown(_,_,[]).

%counter([],0).
%counter([T|H],C):-C1 is C+1,counter(H,C1).

exRow(Y,X,L):-cellUp(Y,X,L1),cellDown(Y,X,L2), append(L1,L2,L).



lenLight([H|T],N) :- lenLight(T,X),((isLight(H))-> N is X+1; N is X).


isLighted(X,Y):-light(X,Y);(emptyCell(X,Y),allCells(X,Y,Z),lenLight(Z,B),B > 0).


%THIRD ALGORITHM

%count the light

lengthLight([],0).

lengthLight([H|T],N):- lengthLight(T,N1),((isLight(H))-> N is N1+1; N is N1).

light([N,M|_]):-light(N,M).

cellRightForLight(_,_,[],0).

cellRightForLight(X,Y,L,R):-X1 is X + 1,cell(X1,Y),lengthLight(L,R),
    cellRightForLight(Y,X1,L1),
% append([[Y,X1]],L1,L).
%

cellLeftForLight(_,_,[],0).

cellLeftForLight(X,Y,L,R):-  X1 is X - 1, cell(X1,Y),lengthLight(L,R),
    cellLeftForLight(X1,Y,L1).% append([[X1,Y]],L1,L).

% exRowLight(X,Y,L,R):- cellRightForLight(X,Y,L1,R),
% cellLeftForLight(X,Y,L2,R), append(L1,L2,L).
%
cellUpForLight(_,_,[],0).

cellUpForLight(Y,X,L,R):- Y1 is Y + 1,cell(Y1,X),lengthLight(L,R),cellUpForLight(Y1,X,L1,R).    %append([[Y1,X]],L1,L).

cellDownForLight(_,_,[],0).

cellDownForLight(Y,X,L,R):-Y1 is Y - 1,cell(Y1,X),LengthLight(L,R),
    cellDownForLight(Y1,X,L,R).

%Fourth algorithm


cellRightFourth(_,_).

%islight(X,Y):-light(X,Y).

cellRightFourth(X,Y):-X1 is X + 1, cell(X1,Y),not(wall(X1,Y)),
    not(wall_num(X1,Y,_)),
    islight(X,Y);cellRightFourth(X1,Y,L1).


cellLeftFourth(_,_).
%islight(X,Y):-light(X,Y).
cellLeftFourth(X,Y):-X1 is X - 1,cell(X1,Y),not(wall(X1,Y)),
    not(wall_num(X1,Y,_)),
    islight(X,Y);cellLeftFourth(X1,Y,L1).


%exRow(X,Y,L):- cellRight(X,Y,L1), cellLeft(X,Y,L2), append(L1,L2,L).

cellUpFourth(_,_).
%islight(X,Y):-light(X,Y).
cellUpFourth(Y,X):- Y1 is Y + 1, cell(Y1,X), not(wall(Y1,X)),
    not(wall_num(Y1,X,_)),islight(X,Y);cellUpFourth(Y1,X);islight(X,Y).

%append([[Y1,X]],L1,L).

cellDownFourth(_,_).
%islight(X,Y):-light(X,Y).
cellDownFourth(Y,X):-  Y1 is Y - 1, cell(Y1,X), not(wall(Y1,X)),
    not(wall_num(Y1,X,_)),islight(X,Y);cellDown(Y1,X,L1).

allWeAreNotTheCell(X,Y):-
    cellRightFourth(X,Y);
cellLeftFourth(X,Y);
cellUpFourth(X,Y);
cellDownFourth(X,Y).

%append([[Y1,X]],L1,L).


%
% exRowLight(Y,X,L):-cellUpForLight(Y,X,L1),cellDownForLight(Y,X,L2),
% append(L1,L2,L).
%









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
























