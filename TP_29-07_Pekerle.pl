
% Tía Agatha, el carnicero y Charles son las únicas personas que viven en la mansión Dreadbury.
viveEnMansion(agatha).
viveEnMansion(carnicero).
viveEnMansion(charles).

% Agatha odia a todos los que viven en la mansión, excepto al carnicero.
odiaA(agatha,Persona):-
    viveEnMansion(Persona),
    Persona\=carnicero.

% Charles odia a todas las personas de la mansión que no son odiadas por la tía Agatha.
odiaA(charles,Persona):-
    viveEnMansion(Persona),
    not(odiaA(agatha,Persona)).

% El carnicero odia a las mismas personas que odia tía Agatha.
odiaA(carnicero,Persona):-
    odiaA(agatha,Persona).

% Quien no es odiado por el carnicero y vive en la mansión, es más rico que tía Agatha
masRicoQue(Persona,agatha):-
    not(odiaA(carnicero,Persona)),
    viveEnMansion(Persona).

% Quien mata es porque odia a su víctima y no es más rico que ella. Además, quien mata debe vivir en la mansión Dreadbury.
asesinoDe(Asesino,Victima):-
    viveEnMansion(Asesino),
    not(masRicoQue(Asesino,Victima)),
    odiaA(Asesino,Victima).

% 1.a- El programa debe resolver el problema de quién mató a la tía Agatha. 
% 1.b- Mostrar la consulta utilizada y la respuesta obtenida.

% asesinoDe(Asesino,agatha)
% ?- asesinoDe(Asesino,agatha).  
% Asesino = agatha.

% 2.a- Agregar los mínimos hechos y reglas necesarios para poder consultar:
% 2.b- Mostrar las consultas utilizadas para conseguir lo anterior, junto con las respuestas obtenidas.

% - Si existe alguien que odie a milhouse.
% ?- odiaA(Persona,milhouse). 
% false. <- ???? Pero nadie quiere a milhouse

% - A quién odia charles.
% ?- odiaA(charles,Persona).
% Persona = carnicero ;
% false.

% - El nombre de quien odia a agatha.
% ?- odiaA(Persona,agatha).
% Persona = agatha ;
% Persona = carnicero.

% - Todos los odiadores y sus odiados.
% Odiador = Odiado, Odiado = agatha ;
% Odiador = agatha,
% Odiado = charles ;
% Odiador = charles,
% Odiado = carnicero ;
% Odiador = carnicero,
% Odiado = agatha ;
% Odiador = carnicero,
% Odiado = charles.

% - Si es cierto que el carnicero odia a alguien.
% Persona = agatha ;
% Persona = charles.

