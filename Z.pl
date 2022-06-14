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
