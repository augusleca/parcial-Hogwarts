% PARTE 1 --------------------------------------

% Modelando magos -> mago/4(Nombre,TipoSangre,[Caracteristicas]).
mago(harry,mestiza,[corajudo,amistoso,orgulloso,inteligente]).
mago(draco,pura,[inteligente,orgulloso]).
mago(hermione,impura,[inteligente,orgulloso,responsable]).

% Odiaria quedar en que casa -> odiariaQuesar/2(Nombre,CasaOdiada).
odiariaQuedar(harry,slytherin).
odiariaQuedar(draco,hufflepuff).

% Modelando casas -> casa/2(NombreCasa,Interes)
casa(gryffindor,corajudo).
casa(slytherin,orgulloso).
casa(slytherin,inteligente).
casa(ravenclaw,inteligente).
casa(ravenclaw,responsable).
casa(hufflepuff,amistoso).

% 1)
permiteEntrar(slytherin,Mago):-
    mago(Mago,Sangre,_),
    Sangre \= impura.

permiteEntrar(Casa,Mago):-
    casa(Casa,_),
    Casa \= slytherin,
    mago(Mago,_,_).

% 2)
caracterApropiadoParaEntrar(Mago,Casa):-
    mago(Mago,_,_),
    casa(Casa,_),
    forall(casa(Casa,Interes),
        tieneCaracteristica(Mago,Interes)).

tieneCaracteristica(Mago,Caracteristica):-
    mago(Mago,_,Caracteristicas),
    member(Caracteristica, Caracteristicas).

% 3)
puedeQuedarEn(hermione,gryffindor).

puedeQuedarEn(Mago,Casa):-
    permiteEntrar(Casa,Mago),
    caracterApropiadoParaEntrar(Mago,Casa),
    not(odiariaQuedar(Mago,Casa)).

%4) -> Sacado de la resolucion de los profes
cadenaDeAmistades(ListaMagos):-
    todosAmistosos(ListaMagos),
    cadenaDeCasas(ListaMagos).

todosAmistosos(ListaMagos):-
    forall(member(Mago,ListaMagos),
        esAmistoso(Mago)).

esAmistoso(Mago):-
    tieneCaracteristica(Mago,amistoso).

cadenaDeCasas([Mago1,Mago2 | MagosSiguientes]):- % -> Planteamos predicado con lista generica
    puedeQuedarEn(Mago1,Casa),
    puedeQuedarEn(Mago2,Casa),
    cadenaDeCasas([Mago2 | MagosSiguientes]).

cadenaDeCasas([_]). % -> Caso base en caso de que la lista sea un solo elem.
cadenaDeCasas([]). % -> Caso base en caso de que la lista esta vacia.

% PARTE 2 --------------------------------------
% -> De los profesores, tomé el punto 1)a) y con esa base segui yo, ya que no utilice functores
esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).
casa(Casa):- casa(Casa,_).

accion(harry,anduvoFueraDeCama).
accion(hermione,irA(tercerPiso)).
accion(hermione,irA(bibliotecaRes)).
accion(harry,irA(bosque)).
accion(harry,irA(tercerPiso)).
accion(draco,irA(mazmorras)).
accion(ron,buenaAccion(50, ganarAjedrez)).
accion(hermione,buenaAccion(50, salvarAmigos)).
accion(harry,buenaAccion(60,vencerAVoldemort)).
% Punto 4) -----------
accion(hermione,respondio(dondeSeEncuentraBezoar,20,snape)).
accion(hermione,respondio(comoLevitaUnaPluma,25,flitwick)).
% -----------------
hizoAlgunaAccion(Mago):- accion(Mago,_).

puntajePorAccion(anduvoFueraDeCama,-50).
puntajePorAccion(irA(Lugar),Puntaje):- lugarProhibido(Lugar,Puntaje).
puntajePorAccion(buenaAccion(Puntaje,_),Puntaje).
% Punto 4) -----------
puntajePorAccion(respondio(_,Dificultad,snape),Puntaje):- Puntaje is Dificultad/2.
puntajePorAccion(respondio(_,Puntaje,Profesor),Puntaje):- Profesor \= snape.
% -----------------

lugarProhibido(bosque,-50).
lugarProhibido(bibliotecaRes,-10).
lugarProhibido(tercerPiso,-75).

% 1)
% a)
buenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

hizoAlgoMalo(Mago):-
    accion(Mago,Accion),
    puntajePorAccion(Accion,Puntaje),
    Puntaje < 0.

% b)
accionRecurrente(Accion):-
    accion(Mago,Accion),
    accion(OtroMago,Accion),
    Mago \= OtroMago.

% 2)
puntajeTotalCasa(Casa,PuntajeTotal):-
    casa(Casa),
    findall(Puntaje,puntajePorAccionDeCasa(Casa,Puntaje),Puntajes),
    sumlist(Puntajes, PuntajeTotal).

puntajePorAccionDeCasa(Casa,Puntaje):-
    esDe(Mago,Casa),
    accion(Mago,Accion),
    puntajePorAccion(Accion,Puntaje).

% 3)
casaGanadora(Casa):-
    puntajeTotalCasa(Casa,PuntajeTotal),
    forall(puntajeTotalCasa(_,OtroPuntajeTotal),
            PuntajeTotal >= OtroPuntajeTotal).

% 4) -> Modificamos la parte de accion! (1-a)



    


































% V2 -> De tito
%accion(harry,andarFueraCama).
%accion(hermione,tercerPiso).
%accion(hermione,bibliotecaRes).
%accion(harry,tercerPiso).
%accion(harry,bosque).
%accion(draco,mazmorras).
%accion(ron,ajedrez).
%accion(hermione,salvarAmigos).
%accion(harry,ganarAVoldemort).
%accion(hermione,respondio(dondeSeEncuentraBezoar,20,snape)).
%accion(hermione,respondio(comoLevitaUnaPluma,25,flitwick)).


% puntosAccion/2(Alumno,Puntaje).
%puntosAccion(andarFueraCama,-50):- accion(_,andarFueraCama).
%puntosAccion(bosque,-50):- accion(_,bosque).
%puntosAccion(tercerPiso,-75):- accion(_,tercerPiso).
%puntosAccion(bibliotecaRes,-10):- accion(_,bibliotecaRes).
%puntosAccion(mazmorras,0):- accion(_,mazmorras).
%puntosAccion(ajedrez,+50).
%puntosAccion(salvarAmigos,+50).
%puntosAccion(vencerAVoldemort,+60).


% 1)
% a)
%buenAlumno(Mago):-
%    puntosAccion(Mago,_,_),
%    forall(accion(Mago,Accion),
%        buenaAccion(Accion)).

%buenaAccion(Accion):-
%    puntosAccion(Accion,PuntosVariados),
%    PuntosVariados >= 0.

% b)
%accionRecurrente(Accion):-
%    accion(UnMago,Accion),
%    accion(OtroMago,Accion),
%    UnMago \= OtroMago.

% 2)
%puntajeTotalCasa(Casa,PuntajeTotal):-
%    casa(Casa,_),
%    findall(Puntaje,puntajeSingular(Casa,Puntaje),ListaPuntajes),
%    sumlist(ListaPuntajes, PuntajeTotal).

%puntajeSingular(Casa,Puntaje):-
%    esDe(Mago,Casa),
%    puntosAccion(Mago,_,Puntaje).

% 3)
%casaGanadora(Casa):-
%    casa(Casa,_),
%    puntajeTotalCasa(Casa,PuntajeCasa),
%    forall(puntajeTotalCasa(_,PuntajeOtraCasa),
%        PuntajeCasa >= PuntajeOtraCasa).

% 4) -> Dentro del predicado "accion" una de las posibles acciones 
%       nuevas es responder, la cual es un functor de la forma;
%       respondio(Pregunta,Dificultad,Profesor)





