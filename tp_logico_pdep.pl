casillero(F,C):- between(1,8,F), between(1,8,C).

% Diagonal descendente desde una casilla inicial diagonalDescendente(FilInicial,ColInicial,Fil,Col):-
	casillero(FilInicial,ColInicial), casillero(Fil,Col),
	cantidadFilas(C), CNeg is -C,
	between(-8,8,D),
	Fil is FilInicial+D,
	Col is ColInicial+D.

% Diagonal ascendente desde una casilla inicial diagonalAscendente(FilInicial,ColInicial,Fil,Col):-
	casillero(FilInicial,ColInicial), casillero(Fil,Col),
	between(-8,8,D),
	Fil is FilInicial+D,
	Col is ColInicial-D.
