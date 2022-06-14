
doSolve(InitialBoard, Board):-
	setupBoard(InitialBoard, Board), !,
	trivialSolver(Board),
	solve(Board),
	!.

% Trivial solver places lights around number tiles that only have one possible solution
trivialSolver(puzzle(size(C,R), board(B), tBoard(TB), lines(L), walls(W), tiles(T))) :-
	flatten(T, Tiles),
	checkTiles(B, Tiles, Flag),
	not(var(Flag)), % If any light are placed a new number wall choulde have lights placed around itself
	trivialSolver(puzzle(size(C,R), board(B), tBoard(TB), lines(L), walls(W), tiles(T))).
trivialSolver(_).

checkTiles(_, [], _).
checkTiles(B, [tile(value(Tile), lines(Lines), walls(Walls))|Tiles], Flag) :-

	checkWallConstraint(B, tile(value(Tile), lines(Lines), walls(Walls)), Walls, Flag), !,
	checkTiles(B, Tiles, Flag).

checkWallConstraint(_, _, [], _).
checkWallConstraint(B, tile(value(Tile), lines(Lines), walls(Walls)), [[NumWall|Wall]|Tail], Flag) :-
	flatten(Wall, W),
	countFreeVars(W, NumFree),
	countLightsWalls(W, NumLights), !,
	Num is NumFree + NumLights,
	(NumWall >= Num, dif(NumWall, NumLights) ->
		(placeLight(tile(value(Tile), lines(Lines), walls(Walls))) ->
		setWalls([NumWall|Wall]),
			% writeBoard(B), nl,
			Flag is 1, % Flags to indicate that trivialSolver should be called again
			checkWallConstraint(B, tile(value(Tile), lines(Lines), walls(Walls)), Tail, Flag);
			checkWallConstraint(B, tile(value(Tile), lines(Lines), walls(Walls)), Tail, Flag)
		);
		checkWallConstraint(B, tile(value(Tile), lines(Lines), walls(Walls)), Tail, Flag)
	).

countFreeVars([],0).
countFreeVars([X|T],N) :- var(X), countFreeVars(T, N1), N is N1 + 1.
countFreeVars([_|T],N) :- countFreeVars(T,N).

solve(puzzle(size(_,_), board(Board), tBoard(_), lines(_), walls(Walls), tiles(S))) :-
	flatten(S, NewS),
	backtracking(NewS),
	checkNum(Walls),
	checkLitUp(Board),
	!.
backtracking([]).
backtracking([H|T]) :-
	placeLight(H),
	backtracking(T).
backtracking([_|T]) :-
	backtracking(T).

placeLight(tile(value(Tile), lines(Lines), walls(Walls))) :-
	var(Tile),
	checkLightsWallsLessThan(Walls),
	Tile = '*',
	setLines(Lines),	    % Light up intersecting tiles on given tiles row and column
	!.

checkLitUp(Board) :-
	flatten(Board, FlatBoard),
	not(checkLitUpLoop(FlatBoard)).
checkLitUpLoop([H|_]) :-
	var(H).
checkLitUpLoop([_|T]):-
	checkLitUpLoop(T).

% Light up intersecting tiles on given tiles row and column
setLines([]).
setLines([H|T]) :-
	setLine(H),
	setLines(T).
setLine([]).
setLine([H|T]) :-
	H = '+',
	setLine(T).
setLine([_|T]) :-
	setLine(T).

% Mark tiles that are no longer valid for light placement
setWalls(Walls):-
	checkNum(Walls),
	markWalls(Walls).
setWalls(_) :-
	true.
markWalls([]).
markWalls([[_|Walls]|Tail]):-
	flatten(Walls, FlattWalls),
	setLine(FlattWalls),
	markWalls(Tail).

checkLightsWallsLessThan([]).
checkLightsWallsLessThan([[Num|Walls]|Tail]):-
	checkLessThanNrOfLights(Num, Walls),
	checkLightsWallsLessThan(Tail).

checkLessThanNrOfLights(Num, A) :-
	flatten(A, Adjacent),
    countLightsWalls(Adjacent, NrOfLights), !,
	NrOfLights < Num.

% Checks that number constraint for all num tiles are correct
checkNum([]).
checkNum([[Num|Walls]|Tail]):-
	checkCorrectNrOfLights(Num, Walls),
	checkNum(Tail).

% Checks that nr of lights around a num tile is valid
checkCorrectNrOfLights(Num, A) :-
	flatten(A, Adjacent),
    countLightsWalls(Adjacent, NrOfLights), !,
	NrOfLights == Num.

% Counts number if * in a list while ignoring/skipping free vars
countLightsWalls([],0).
countLightsWalls([X|T],N) :- not(var(X)), X == '*', countLightsWalls(T, N1), N is N1 + 1.
countLightsWalls([_|T],N) :- countLightsWalls(T,N).

getValue(Board, RowNum, ColNum, Val) :-
    nth1(RowNum, Board, Row), nth1(ColNum, Row, Val).

% Returns True if given list is empty
is_empty(List) :- not(member(_, List)).

/***********************************************/
/********************* Setting Up the puzzle **/
/*********************************************/
% Transforms board to include additional datastructures
% Board     -  puzzle(size(Row,Col), board(B)
% NewBoard  -  puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(W))
setupBoard(Board, NewBoard):-
	transpose(Board, TransBoard),
	setupLines(TransBoard, LinesBoard),
	setupNums(LinesBoard, WallsBoard),
	setupSolver(WallsBoard, NewBoard),
	!.


% Adds tiles(S) to the datastructure
% This contains a free tiles on the board with its corresponding line and walls
setupSolver(puzzle(size(Col,Row), board(B), tBoard(TB), lines(Lines), walls(Walls)), puzzle(size(Col,Row), board(B), tBoard(TB), lines(Lines), walls(Walls), tiles(S))) :-
	createSolverMatrix(Col, Row, S),
	createSolverTiles(B, Walls, Lines, S), % List of tile(value(_), lines([line groups]))
	!.

createSolverMatrix(Col, Row, Board) :-
	length(Board, Row),
	createColumn(Board, Col),
	!.
createColumn([], _).
createColumn([H|T], Col) :-
	length(H, Col),
	createColumn(T, Col).


createSolverTiles([], _, _, []).
createSolverTiles([Bh|Bt], Walls, Lines, [Sh|St]) :-
	createSolverLine(Bh, Walls, Lines, Sh),		% Loop a line
	createSolverTiles(Bt, Walls, Lines, St).	% Next Row

createSolverLine([], _, _, []).
createSolverLine([Lh|Lt], Walls, Lines, [Rh|Rt]) :-
	checkLines(Lh, Lines, LineResult),	% Check tile
	checkWalls(Lh, Walls, WallResult),
	Rh = tile(value(Lh), lines(LineResult), walls(WallResult)),
	createSolverLine(Lt, Walls, Lines, Rt). % Next tile

checkLines(_, [], Result):-
	Result = [].
checkLines(Tile, [H|T], Result) :-
	freeMember(Tile, H),
	checkLines(Tile, T, Result1),
	append([H], Result1, Result).
checkLines(Tile, [_|T], Result) :-
	checkLines(Tile, T, Result1),
	Result = Result1.

checkWalls(_, [], Result) :-
	Result = [].
checkWalls(Tile, [H|T], Result) :-
	nth1(2, H, WallTiles),
	freeMember(Tile, WallTiles),
	checkWalls(Tile, T, Result1),
	append([H], Result1, Result).
checkWalls(Tile, [_|T], Result) :-
	checkWalls(Tile, T, Result1),
	Result = Result1.

% Checks if Elem is the given list, works with free variables, if not in list fail
freeMember(_, []) :- !, fail. % Readbility, not necesarry but shows that it fails if not match is met before the list is
freeMember(Elem, [H|_]) :-
	Elem == H, !.
freeMember(Elem, [_|T]) :-
	freeMember(Elem, T).

% Adds lines(L) to the datastructure
% This countains a list of all lines that are continuous
setupLines(puzzle(size(Col,Row), board(B), tBoard(TB)), puzzle(size(Col,Row), board(B), tBoard(TB), lines(L))) :-
	findLines(B, Lines),
	findLines(TB, TLines),
	append(Lines, TLines, L),
	!.

findLines([], []).
findLines([H|T], Result) :-
	findLines(T, Result1),
	splitLine(H, Line, Result2),
	(is_empty(Line) ->
		Result3 = Result2;
		append([Line], Result2, Result3)
	),
	append(Result3, Result1, Result).

splitLine([], Line, Result) :-
	Line = [],
	Result = [].
splitLine([H|T],  Line, Result) :-
	var(H),
	splitLine(T, Line1, Result1),
	append([H], Line1, Line),
	Result = Result1.
splitLine([_|T], Line, Result) :-
	splitLine(T, Line1, Result1),
	(is_empty(Line1) ->
		Result = Result1;
		append([Line1], Result1, Result)
	),
	Line = [].

% Adds walls(W) to the datastructure
% This countains a list of all number walls with its adjacent tiles
setupNums(puzzle(size(Row,Col), board(B), tBoard(TB), lines(L)), puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(R))):-
	findNums(B, Row, Col, Row, Col, R).

findNums(Board, _, _, 1, 1, Result):-
	getValue(Board, 1, 1, Val),
	getAdjacentIfNum(Board, 1, 1, Val, AdjacentList),
	append(_, AdjacentList, Result).
findNums(Board, Col, Row, 1, CurrentRow, Result):-
	R1 is CurrentRow - 1,
	findNums(Board, Col, Row, Col, R1, R),
	getValue(Board, CurrentRow, 1, Val),
	getAdjacentIfNum(Board, CurrentRow, 1, Val, AdjacentList),
	append(R, AdjacentList, Result).
findNums(Board, Col, Row, CurrentCol, CurrentRow, Result):-
	C1 is CurrentCol - 1,
	findNums(Board, Col, Row, C1, CurrentRow, R),
	getValue(Board, CurrentRow, CurrentCol, Val),
	getAdjacentIfNum(Board, CurrentRow, CurrentCol, Val, AdjacentList),
	append(R, AdjacentList, Result).


getAdjacentIfNum(Board, Col, Row, Num, List) :-
	integer(Num), % We only want to check if the tile is a number
	getAdjacentTiles(Board, Col, Row, R),
	append([], [[Num, R]], List).
getAdjacentIfNum(_, _, _, _, List) :-
	List = [].

getAdjacentTiles(Board, Col, Row, R) :-
    getAdjacentPositions(Col, Row, PosList),
	getTiles(Board, PosList, R).

getTiles(_, [], []).
getTiles(Board, [Pos|PosList], Result) :-
	getTiles(Board, PosList, Result1),
	checkPos(Board, Pos, R),
	!,
	append(Result1, [R], Result).

checkPos(Board, [Col|[Row|_]], R) :-
	getValue(Board, Col, Row, R).
checkPos(_, _, []).

getAdjacentPositions(X, Y, R) :-
	X1 is X - 1, X2 is X + 1,
	Y1 is Y - 1, Y2 is Y + 1,
	append([], [[X1, Y], [X2, Y], [X, Y1], [X, Y2]], R).

%   Transpose a matrix board
transpose(puzzle(size(Row,Column), board(B)), puzzle(size(Row,Column), board(B), tBoard(TB))):-
	trans(B, TB).
trans([],[]).
trans([[]|_], []):-!.
trans([S|R], [L|L1]) :-
    trans(S, R, L, M),
    trans(M, L1).
trans([], _,[],[]).
trans([S1|S2], [], [S1|L1], [S2|M]):-
    trans([], [], L1, M).
trans([S1|S2], [R1|R2], [S1|L1], [S2|M]):-
    trans(R1, R2, L1, M).


/********************************************/
/********************* writing the result **/
/******************************************/
writeFullOutput(puzzle(size(Row,Col), board(B), tBoard(_), lines(_), walls(_), tiles(_))):-
	write("size "), write(Row), write("x"), write(Col), nl,
	writeBoard(B), !.
writeFullOutput(P):- write('Cannot solve puzzle: '), write(P), nl.

writeBoard([]).
writeBoard([H|T]):-
	writeLine(H),
	writeBoard(T).

writeLine([]):- nl.
writeLine([Head|Tail]):-
	var(Head),
	write('_'),
	writeLine(Tail).
writeLine([Head|Tail]) :-
	Head == '+',
	write('_'),
	writeLine(Tail).
writeLine([Head|Tail]) :-
	write(Head),
	writeLine(Tail).


/********************************************/
/********************** reading the input **/
/******************************************/
readProblem(puzzle(size(Row,Col), board(Grid))) :-
	findKW(size),
	readInt(Row),
	readInt(Col),
	length(Grid, Col),
	readGridLines(Row,Grid).

findKW(KW):-
	string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):-
	peek_code(-1), !, fail.
findKW(KW):-
	get_code(_), findKW(KW).

readKW([]):-
	get_code(_).
readKW([H|T]):-
	get_code(H), readKW(T).

readGridLines(_,[]).
readGridLines(N,[H|T]):-
	length(H,N),
	readGridLine(H),
	readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):-
	get_code(M),
	translate(M,E),
	!,
	readGridLine(T).

translate(-1,'ERROR: EOF').
translate(95, _).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- name(E,[X]).
whitespace(10). whitespace(12). whitespace(32).

readHintLine(0).
readHintLine(N):-
	N>0,
	N1 is N-1,
	get_code(_),
	readHintLine(N1).

readInt(N):-
	get_code(M),
	handleCode(M,N).

handleCode(M,N):-
	is_number_code(M,N1),
	!,
	continueInt(N1,N).
handleCode(-1,_):-
	!,
	fail. /* EOF */
handleCode(_,N):-
	readInt(N).

continueInt(O,N):-
	get_code(M),
	is_number_code(M,M1),
	!,
	H is 10*O+M1,
	continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):-
	N>=48,
	N<58,
	N1 is N-48.
is_number_code(95,0).


/**********************************************************************************/
/*********************** global control: starting the algorithm and the reading **/
/********************************************************************************/
input_output(IF,OF):-
	current_prolog_flag(argv, ['--io',IF,OF]), !.
input_output(IF,OF):-
	inputFile(IF),
	outputFile(OF).

run :-
	input_output(IF, OF),
	see(IF),
	tell(OF),
\	findKW(puzzles),
	readInt(N),
	write('puzzles '), write(N), nl,
	solvePuzzles(N),
	told,
	seen,
	!.

run :-
	told,
	seen. /* close the files */

solvePuzzles(0).
solvePuzzles(N) :-
	N > 0,
	readProblem(P),
	doSolve(P, S),
	writeFullOutput(S),
	!,
	N1 is N-1,
	solvePuzzles(N1).

:- run.
:- halt.
