% ============================================================
% malla.pl - Malla completa corregida (para INF-253 Tarea 5)
% Generado a partir de la malla proporcionada por el usuario.
% Contiene: hechos asignatura/5, prerrequisitos/2 y predicados:
% habilitada/2, es_prerrequisito/2, permite_dar/3, siguiente_semestre/4, titulacion_optima/4
% ============================================================

% -----------------------------
% HECHOS: asignatura(codigo, nombre, creditos, SemestrePlan, orden)
% -----------------------------
asignatura('IWI-131','programacion',5,1,1).
asignatura('MAT-021','matematicas I',8,1,2).
asignatura('FIS-100','introduccion a la fisica',6,1,3).
asignatura('HRW-132','humanistico I',3,1,4).
asignatura('DEW-100','educacion fisisca I',2,1,5).
asignatura('QUI-010','quimica y sociedad',5,2,6).
asignatura('MAT-022','matematicas II',7,2,7).
asignatura('FIS-110','fisica general I',8,2,8).
asignatura('IWG-101','introduccion a la ingenieria',3,2,9).
asignatura('HRW-133','humanistico II',3,2,10).
asignatura('DEW-101','educacion fisica II',2,2,11).
asignatura('INF-134','estructuras de datos',5,3,12).
asignatura('MAT-023','matematicas III',7,3,13).
asignatura('FIS-130','fisica General III',8,3,14).
asignatura('INF-152','estructuras discretas',5,3,15).
asignatura('INF-260','teoria de sistemas',5,3,16).
asignatura('INF-1','libre I',2,3,17).
asignatura('INF-253','lenguajes de programacion',5,4,18).
asignatura('MAT-024','matematicas IV',6,4,19).
asignatura('FIS-120','fisica general II',8,4,20).
asignatura('INF-155','informatica teorica',5,4,21).
asignatura('IWN-170','economia IA',5,4,22).
asignatura('INF-2','libre II',2,4,23).
asignatura('INF-239','bases de datos',5,5,24).
asignatura('INF-245','arquitectura y organizacion de computadores',5,5,25).
asignatura('FIS-140','fisica general IV',8,5,26).
asignatura('INF-280','estadastica computacional',5,5,27).
asignatura('INF-270','organizaciones y sistemas de informacion',5,5,28).
asignatura('INF-3','libre III',2,5,29).
asignatura('INF-236','analisis y dise�o de software',5,6,30).
asignatura('INF-246','sistemas operativos',5,6,31).
asignatura('INF-276','ingenieria, informatica y sociedad',5,6,32).
asignatura('INF-221','algoritmos y complejidad',5,6,33).
asignatura('INF-292','optimizacion',5,6,34).
asignatura('INF-4','libre IV',2,6,35).
asignatura('INF-225','ingenieria de software',5,7,36).
asignatura('INF-256','redes de computadores',5,7,37).
asignatura('ICN-270','informacion y matematicas financieras',5,7,38).
asignatura('INF-285','computacion cientifica',5,7,39).
asignatura('INF-293','investigacion de operaciones',6,7,40).
asignatura('INF-5','libre V',2,7,41).
asignatura('INF-322','dise�o interfaces usuarias',5,8,42).
asignatura('INF-343','sistemas distribuidos',5,8,43).
asignatura('INF-305','electivo informatica I',5,8,44).
asignatura('INF-295','inteligencia artificial',5,8,45).
asignatura('INF-266','sistemas de gestion',5,8,46).
asignatura('INF-6','libre VI',2,8,47).
asignatura('INF-306','electivo informatica II',5,9,48).
asignatura('INF-307','electivo informatica III',5,9,49).
asignatura('INF-311','electivo I',5,9,50).
asignatura('INF-312','electivo II',5,9,51).
asignatura('INF-360','gestion de proyectos de informatica',5,9,52).
asignatura('INF-7','libre VII',2,9,53).
asignatura('INF-308','electivo informatica IV',5,10,54).
asignatura('INF-313','electivo III',5,10,55).
asignatura('INF-314','electivo IV',5,10,56).
asignatura('INF-228','taller desarrollo de proyecto de informatica',10,10,57).
asignatura('INF-309','trabajo de titulo I',2,10,58).
asignatura('INF-310','trabajo de titulo II',20,11,59).

% -----------------------------
% HECHOS: prerrequisitos(Asignatura, Requisitos)
% -----------------------------
prerrequisitos('IWI-131',[]).
prerrequisitos('MAT-021',[]).
prerrequisitos('FIS-100',[]).
prerrequisitos('HRW-132',[]).
prerrequisitos('DEW-100',[]).
prerrequisitos('QUI-010',[]).
prerrequisitos('MAT-022',['MAT-021']).
prerrequisitos('FIS-110',['MAT-021','FIS-100']).
prerrequisitos('IWG-101',[]).
prerrequisitos('HRW-133',[]).
prerrequisitos('DEW-101',[]).
prerrequisitos('INF-134',['IWI-131']).
prerrequisitos('MAT-023',['MAT-022']).
prerrequisitos('FIS-130',['MAT-022','FIS-110']).
prerrequisitos('INF-152',['IWI-131','MAT-021']).
prerrequisitos('INF-260',['IWG-101']).
prerrequisitos('INF-1',[]).
prerrequisitos('INF-253',['INF-134']).
prerrequisitos('MAT-024',['MAT-023']).
prerrequisitos('FIS-120',['MAT-023','FIS-110']).
prerrequisitos('INF-155',['INF-134','INF-152']).
prerrequisitos('IWN-170',['MAT-023']).
prerrequisitos('INF-2',[]).
prerrequisitos('INF-239',['INF-134']).
prerrequisitos('INF-245',['INF-134']).
prerrequisitos('FIS-140',['FIS-130','FIS-120']).
prerrequisitos('INF-280',['IWI-131','MAT-023']).
prerrequisitos('INF-270',['INF-260']).
prerrequisitos('INF-3',[]).
prerrequisitos('INF-236',['INF-239','INF-253']).
prerrequisitos('INF-246',['INF-245']).
prerrequisitos('INF-276',['INF-280']).
prerrequisitos('INF-221',['INF-152','INF-253']).
prerrequisitos('INF-292',['MAT-023']).
prerrequisitos('INF-4',[]).
prerrequisitos('INF-225',['INF-236']).
prerrequisitos('INF-256',['INF-246']).
prerrequisitos('ICN-270',['IWN-170']).
prerrequisitos('INF-285',['MAT-024','INF-221']).
prerrequisitos('INF-293',['INF-280','INF-292']).
prerrequisitos('INF-5',[]).
prerrequisitos('INF-322',['INF-225']).
prerrequisitos('INF-343',['INF-256']).
prerrequisitos('INF-305',[]).
prerrequisitos('INF-295',['INF-292','INF-221']).
prerrequisitos('INF-266',['INF-276']).
prerrequisitos('INF-6',[]).
prerrequisitos('INF-306',[]).
prerrequisitos('INF-307',[]).
prerrequisitos('INF-311',[]).
prerrequisitos('INF-312',[]).
prerrequisitos('INF-360',['INF-322','INF-266']).
prerrequisitos('INF-7',[]).
prerrequisitos('INF-308',[]).
prerrequisitos('INF-313',[]).
prerrequisitos('INF-314',[]).
prerrequisitos('INF-228',['INF-360']).
prerrequisitos('INF-309',['INF-360']).
prerrequisitos('INF-310',['INF-228','INF-309']).

% -----------------------------
% PREDICADOS CORREGIDOS
% -----------------------------

% habilitada(+Asignatura, +Aprobados)
% Verdadero si Asignatura NO está en Aprobados y todos sus prerrequisitos
% están aprobados.
habilitada(Asignatura, Aprobados) :-
    \+ member(Asignatura, Aprobados),
    prerrequisitos(Asignatura, Reqs),
    subset(Reqs, Aprobados).

% es_prerrequisito(+Pre, +Asignatura)
% Verdadero si Pre es prerrequisito directo o indirecto (transitivo)
es_prerrequisito(Pre, Asig) :-
    prerrequisitos(Asig, Lista),
    member(Pre, Lista).

es_prerrequisito(Pre, Asig) :-
    prerrequisitos(Asig, Lista),
    member(X, Lista),
    es_prerrequisito(Pre, X).

% permite_dar(+AprobadosPrev, +AsignaturaRecienAprobada, -NuevosHabilitados)
permite_dar(AprobPrev, AsigNueva, NuevosHabilitados) :-
    append([AsigNueva], AprobPrev, AprobDespues0),
    sort(AprobDespues0, AprobDespues),
    findall(A, (asignatura(A,_,_,_,_), habilitada(A, AprobPrev)), Antes0),
    sort(Antes0, Antes),
    findall(A, (asignatura(A,_,_,_,_), habilitada(A, AprobDespues)), Despues0),
    sort(Despues0, Despues),
    subtract(Despues, Antes, Dif),
    sort(Dif, NuevosHabilitados).

% siguiente_semestre(+Aprobados, +SemActual, +MaxCredSem, -AsignaturasSugeridas)
% Prioriza: (a) semestres anteriores pendientes (1..SemActual-1), (b) SemActual,
% (c) semestres posteriores. Dentro de cada grupo orden estable por SemestrePlan asc y Orden asc.
siguiente_semestre(Aprobados, SemActual, MaxCred, Sugeridas) :-
    % candidatas: asignaturas habilitadas y no aprobadas
    findall(Cod, (asignatura(Cod,_,_,_,_), \+ member(Cod,Aprobados), habilitada(Cod,Aprobados)), Cands0),
    sort(Cands0, Cands),

    % agrupar y ordenar por prioridad
    map_list_to_pairs(prioridad_para(SemActual), Cands, Pares),
    keysort(Pares, OrdenadosPares),
    pairs_values(OrdenadosPares, Ordenadas),

    % seleccionar respetando orden y max créditos (greedy estable)
    seleccionar_creditos(Ordenadas, MaxCred, Sugeridas).

% prioridad_para(+SemActual, +Codigo, -Clave)
% clave: par(Grupo, Peso) -> se usa como número: Grupo*1000 + Sem*10 + Orden
prioridad_para(SemActual, Cod, Clave) :-
    asignatura(Cod,_,_,Sem,Orden),
    ( Sem < SemActual -> Grupo = 0
    ; Sem == SemActual -> Grupo = 1
    ; Grupo = 2 ),
    Clave is Grupo*100000 + Sem*100 + Orden.

% seleccionar_creditos: recorre en orden y agrega si cabe
seleccionar_creditos([], _, []).
seleccionar_creditos([A|Resto], Max, [A|Sel]) :-
    asignatura(A,_,Cred,_,_),
    Cred =< Max,
    NuevoMax is Max - Cred,
    seleccionar_creditos(Resto, NuevoMax, Sel), !.
seleccionar_creditos([_|Resto], Max, Sel) :-
    seleccionar_creditos(Resto, Max, Sel).

% titulacion_optima(+Aprobados, +SemActual, +MaxCredSem, -Plan)
titulacion_optima(Aprobados, SemActual, MaxCred, Plan) :-
    findall(Cod, asignatura(Cod,_,_,_,_), Todas),
    sort(Todas, TodasOrdenadas),
    sort(Aprobados, AprobadosUnicos),
    subtract(TodasOrdenadas, AprobadosUnicos, Pendientes),
    ( Pendientes == [] -> Plan = []
    ; siguiente_semestre(AprobadosUnicos, SemActual, MaxCred, Sugeridas),
      append(Sugeridas, AprobadosUnicos, NuevosAprob0),
      sort(NuevosAprob0, NuevosAprob),
      Siguiente is SemActual + 1,
      titulacion_optima(NuevosAprob, Siguiente, MaxCred, Resto),
      Plan = [sem(SemActual, Sugeridas) | Resto]
    ).

% fin del archivo
