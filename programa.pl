% PARTE 1 --------------------------------------

% Modelando magos -> mago/4(Nombre,TipoSangre,[Caracteristicas]).
mago(harry,mestiza,[corajudo,amistoso,orgulloso,inteligente]).
mago(draco,pura,[inteligente,orgulloso]).
mago(hermione,impura,[inteligente,orgulloso,responsable]).

% Odiaria quedar en que casa -> odiariaQuesar/2(Nombre,CasaOdiada).
odiariaQuedar(harry,slytherin).
odiariaQuedar(draco,hufflepuff).

% Modelando casas -> casa/2(NombreCasa,Interes)
casa(gryffindor,coraje).
casa(slytherin,orgullo).
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
    caracterApropiadoParaEntrar(Mago,Casa),
    not(odiariaQuedar(Mago,Casa)).

%4)
cadenaDeAmistades(ListaMagos):-
    casa(Casa,_),
    findall(Mago,cadenaDeAmistad(Mago,Casa),ListaMagosFULL),
    list_to_set(ListaMagosFULL,ListaMagos).

cadenaDeAmistad(Mago,Casa):-
    tieneCaracteristica(Mago,amistoso),
    puedeQuedarEn(Mago,Casa).


% PARTE 2 --------------------------------------

esDe(hermione,gryffindor).
esDe(ron,gryffindor).
esDe(harry,gryffindor).
esDe(draco,slytherin).
esDe(luna,ravenclaw).

accion(harry,andarFueraCama,-50).
accion(hermione,tercerPiso,-75).
accion(hermione,bibliotecaRes,-10).
accion(harry,tercerPiso,-75).
accion(harry,bosque,-50).
accion(draco,mazmorras,0).
accion(ron,ajedrez,+50).
accion(hermione,salvarAmigos,+50).
accion(harry,ganarAVoldemort,+60).
accion(hermione,respondio(dondeSeEncuentraBezoar,20,snape),10).
accion(hermione,respondio(comoLevitaUnaPluma,25,flitwick),25).
%accion(_,respondio(_,Dificultad,snape),Puntaje):- Puntaje is Dificultad/2.
%accion(_,respondio(_,Dificultad,_),Dificultad).

%realizoAccion(Alumno,-50):-
%    accion(Alumno,andarFueraCama)

% 1)
% a)
buenAlumno(Mago):-
    accion(Mago,_,_),
    forall(accion(Mago,Accion,_),
        buenaAccion(Accion)).

buenaAccion(Accion):-
    accion(_,Accion,PuntosVariados),
    PuntosVariados >= 0.

% b)
accionRecurrente(Accion):-
    accion(UnMago,Accion,_),
    accion(OtroMago,Accion,_),
    UnMago \= OtroMago.

% 2)
puntajeTotalCasa(Casa,PuntajeTotal):-
    casa(Casa,_),
    findall(Puntaje,puntajeSingular(Casa,Puntaje),ListaPuntajes),
    sumlist(ListaPuntajes, PuntajeTotal).

puntajeSingular(Casa,Puntaje):-
    esDe(Mago,Casa),
    accion(Mago,_,Puntaje).

% 3)
casaGanadora(Casa):-
    casa(Casa,_),
    puntajeTotalCasa(Casa,PuntajeCasa),
    forall(puntajeTotalCasa(_,PuntajeOtraCasa),
        PuntajeCasa >= PuntajeOtraCasa).

% 4) -> Dentro del predicado "accion" una de las posibles acciones 
%       nuevas es responder, la cual es un functor de la forma;
%       respondio(Pregunta,Dificultad,Profesor)





