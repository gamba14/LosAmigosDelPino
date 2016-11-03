% Chess
% 8 |_|#|_|#|_|#|_|#|
% 7 |#|_|#|_|#|_|#|_|
% 6 |_|#|_|#|_|#|_|#|
% 5 |#|_|#|_|#|_|#|_|
% 4 |_|#|_|#|_|#|_|#|
% 3 |#|_|#|_|#|_|#|_|
% 2 |_|#|_|#|_|#|_|#|
% 1 |#|_|#|_|#|_|#|_|
%    a b c d e f g h

% Se usara el tablero como se plantea en el ejercio:
% 1 |_|#|_|#|_|#|_|#|
% 2 |#|_|#|_|#|_|#|_|
% 3 |_|#|_|#|_|#|_|#|
% 4 |#|_|#|_|#|_|#|_|
% 5 |_|#|_|#|_|#|_|#|
% 6 |#|_|#|_|#|_|#|_|
% 7 |_|#|_|#|_|#|_|#|
% 8 |#|_|#|_|#|_|#|_|
%    1 2 3 4 5 6 7 8
% Se modelaran las piezas sin tener en cuenta su color (salvo los peones)
% Los peones blancos empiezan de la fila 7 y avanzan hacia en direncion a 1
% Los peones nergos  empiezan de la fila 2 y avanzan hacia en direncion a 8
% NO se modelaran los movimientos 'peon al paso' ni 'enroque' 
% NO se aplica la regla "Los reyes NUNCA pueden estar uno al lado del otro"


esPieza(rey).
esPieza(reina).
esPieza(torre).
esPieza(caballo).
esPieza(alfil).
esPieza(peonBlanco).
esPieza(peonNegro).

esPeon(peonBlanco).
esPeon(peonNegro).

esBlancas(peonBlanco).
esNegras(peonNegro).

esPeonBlanco(Peon) :- esPeon(Peon), esBlancas(Peon).
esPeonNegro(Peon) :- esPeon(Peon), esNegras(Peon).



esCasillero(Fila,Columna) :- 
	between(1,8,Fila),
	between(1,8,Columna).

% Diagonal descendente desde una casilla inicial 
diagonalIzqADer(FilInicial,ColInicial,FilaDestino,ColDestino):-
	esCasillero(FilInicial,ColInicial),
	esCasillero(FilaDestino,ColDestino),
	between(-8,8,Desplazamiento),
	FilaDestino is FilInicial + Desplazamiento,
	ColDestino is ColInicial + Desplazamiento.

% Diagonal ascendente desde una casilla inicial 
% no son ascendentes ni descendentes porque no tienen sentido, solo direccion. Me resulta menos confuso elegir por el orden en el que arrojan los resultados.
diagonalDerAIzq(FilInicial,ColInicial,FilaDestino,ColDestino):-
	esCasillero(FilInicial,ColInicial),
	esCasillero(FilaDestino,ColDestino),
	between(-8,8,D), % no es 7? porque el minimo es 1. 1+8 fallaria. 
	% Lo salva esCasillero
	FilaDestino is FilInicial+D,
	ColDestino is ColInicial-D.

casilleros(F,C,FA,CA) :-
	esCasillero(F,C),
	esCasillero(FA,CA).

casillerosDistintos(F,C,FA,CA):-
	casilleros(F,C,FA,CA),
	not(mismoCasillero(F,C,FA,CA)).

mismoCasillero(F,C,F,C).

% -- 1 --
atrasAdelante(1).
atrasAdelante(-1).

variacionAvanceCaballos(2,1).
variacionAvanceCaballos(1,2).


enPerimetro(FilaAmenazada,ColumnaAmenazada,Fila,Columna) :-
    between(-1,1,DesplazamientoFila),
    between(-1,1,DesplazamientoColumna),
    FilaAmenazada is Fila + DesplazamientoFila,
    ColumnaAmenazada is Columna + DesplazamientoColumna. % puede no sumar lo mismo 


perpendicular(Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	estaEnLaMismaLinea(Fila,Columna,FilaAmenazada,ColumnaAmenazada).

perpendicular(Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	estaEnLaMismaColumna(Fila,Columna,FilaAmenazada,ColumnaAmenazada).

estaEnLaMismaLinea(F,_,F,_).
estaEnLaMismaColumna(_,C,_,C). % no es determinista por el contexto de aplicacion (reducido y accesorio)

estaEnDiagonal(Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	diagonalDerAIzq(Fila,Columna,FilaAmenazada,ColumnaAmenazada).

estaEnDiagonal(Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	diagonalIzqADer(Fila,Columna,FilaAmenazada,ColumnaAmenazada).

% amenaza Rey
amenaza(rey,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	enPerimetro(FilaAmenazada,ColumnaAmenazada,Fila,Columna).

% amenaza torre
amenaza(torre,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	perpendicular(Fila,Columna,FilaAmenazada,ColumnaAmenazada).
	% no es muy expresivo, pero las lineas perpendiculares que convergen en la casilla origen son las amenazadas en el tablero, nombre mejor?

% amenaza alfil
amenaza(alfil,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	estaEnDiagonal(Fila,Columna,FilaAmenazada,ColumnaAmenazada).

% amenaza reina
amenaza(reina,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	amenaza(rey,Fila,Columna,FilaAmenazada,ColumnaAmenazada).

amenaza(reina,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	amenaza(torre,Fila,Columna,FilaAmenazada,ColumnaAmenazada).

amenaza(reina,Fila,Columna,FilaAmenazada,ColumnaAmenazada) :-
	amenaza(alfil,Fila,Columna,FilaAmenazada,ColumnaAmenazada).


% amenaza peon
amenaza(Peon, Fila, Columna, FilaAmenazada, ColumnaAmenazada) :-
	esPeonBlanco(Peon),
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	atrasAdelante(DesplazamientoColumna),
	Columna is ColumnaAmenazada + DesplazamientoColumna,
	Fila is FilaAmenazada + 1.

amenaza(Peon, Fila, Columna, FilaAmenazada, ColumnaAmenazada) :-
	esPeonNegro(Peon),
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	atrasAdelante(DesplazamientoColumna),
	Columna is ColumnaAmenazada + DesplazamientoColumna,
	Fila is FilaAmenazada - 1.

% amenaza caballo
% Usando la logica avanza 2 filas y  1 columnas (con la otra llega al mismo lugar)
amenaza(caballo, Fila, Columna, FilaAmenazada, ColumnaAmenazada) :-
	casillerosDistintos(Fila,Columna,FilaAmenazada,ColumnaAmenazada),
	atrasAdelante(DireccionF),
	atrasAdelante(DireccionC),
	variacionAvanceCaballos(AvanceF, AvanceC),
	FilaAmenazada    is Fila    + (AvanceF * DireccionF),
	ColumnaAmenazada is Columna + (AvanceC * DireccionC).


% -- 2--
cuantosAmenaza(Pieza,Fila,Columna,CantidadAmenazados) :-
	esPieza(Pieza),
	esCasillero(Fila,Columna),
	findall(esCasillero(FilaAmenazada,ColumnaAmenazada),amenaza(Pieza,Fila,Columna,FilaAmenazada,ColumnaAmenazada),Amenazados),
	length(Amenazados,CantidadAmenazados).

cuantosLibres(Pieza,Fila,Columna,CasillerosLibres) :-
	cuantosAmenaza(Pieza,Fila,Columna,CantidadAmenazados),
	CasillerosLibres is 64 - CantidadAmenazados - 1. % la pieza no amenaza su casillero 

% -- 2b -- 
movimientoKasparov(Pieza,FilaOptima,ColumnaOptima)  :-
	esPieza(Pieza),
	esCasillero(FilaOptima,ColumnaOptima),
	cuantosAmenaza(Pieza,FilaOptima,ColumnaOptima,CantidadAmenazados),
	ningunoLaSupera(Pieza,CantidadAmenazados).

ningunoLaSupera(Pieza, CantidadAmenazados) :-
	esPieza(Pieza),
	forall(cuantosAmenaza(Pieza,_,_,OtraCantidadAmenazados),noSupera(OtraCantidadAmenazados,CantidadAmenazados)).

noSupera(UnaCantidad,CantidadMaxima) :-
	UnaCantidad =< CantidadMaxima.

% -- 3 --

puedenEstar(UnaPieza,Fila,Columna,PiezaDistinta,FilaDistinta,ColumnaDistinta) :-
	sonPiezasDistintas(UnaPieza,PiezaDistinta),
	casillerosDistintos(Fila,Columna,FilaDistinta,ColumnaDistinta),
	not(amenaza(UnaPieza,Fila,Columna,FilaDistinta,ColumnaDistinta)),
	not(amenaza(PiezaDistinta,FilaDistinta,ColumnaDistinta,Fila,Columna)).

sonPiezasDistintas(P,Q) :-
	sonPiezas(P,Q),
	not(mismaPieza(P,Q)).

sonPiezas(P,Q) :-
	esPieza(P), esPieza(Q).

mismaPieza(P,P).

% -- 4 --
ocupa(reina,5,5).
ocupa(torre,2,1).
ocupa(torre,3,4).
ocupa(peonBlanco,7,1).
ocupa(peonNegro,2,1).

% puedeEstar(Pieza,Fila,Columna) :- ocupa(Pieza,Fila,Columna).
puedeEstar(Pieza, Fila, Columna) :-
	esCasillero(Fila,Columna),
	forall(ocupa(UnaPieza,UnaFila,UnaColumna),puedenEstar(UnaPieza,UnaFila,UnaColumna,Pieza,Fila,Columna)).
