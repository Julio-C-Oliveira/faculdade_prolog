% Estados dinâmicos:
:- dynamic currentRoom/1.
:- dynamic currentState/1.
:- dynamic currentGold/1.
:- dynamic currentAttack/1.
:- dynamic remainingMoves/1.

:- dynamic monster/2.
:- dynamic reward/2.

:- dynamic storedPath/1.



% Estrutura da Dungeon:


% Caminhos que Avançam:
path(entrance, right, extraRoom).
path(entrance, left, room_01).
path(room_01, left, room_02).
path(room_01, right, room_04).
path(room_02, left, room_03).
path(room_03, right, room_04). 
path(room_04, left, room_05). 
path(room_05, right, room_06).
path(room_05, right, extraRoom).
path(room_06, right, finalRoom).

% Caminhos que são loops:
path(room_03, left, room_03).
path(room_04, right, room_04).

% Caminhos que retrocedem:
path(room_02, right, entrance).
path(extraRoom, left, room_05).


% Monstros:
monster(room_01, slime).
monster(room_02, goblin).
monster(room_03, wolf).
monster(room_04, zombie).
monster(room_05, ghost).
monster(room_06, majin).
monster(finalRoom, maou).
monster(extraRoom, overlord).


% Recompensa:
reward(room_01, gold).
reward(room_02, attackBuff).
reward(room_03, gold).
reward(room_04, attackBuff).
reward(room_05, gold).
reward(room_06, attackBuff).
reward(finalRoom, gold).
reward(extraRoom, invincibility).



% Lógica do Jogo:


% Valores Inciais:
currentRoom(entrance).
currentState(alive).
currentGold(0).
currentAttack(1).
remainingMoves(7).


% Ações:
walk(Side) :-
    currentRoom(FromRoom),
    (
        monster(FromRoom, Monster) ->
        format('O caminho está bloqueado pelo ~w! Derrote-o antes de prosseguir.\n', [Monster]), !, fail;
        path(FromRoom, Side, ToRoom) ->
            retract(currentRoom(FromRoom)),
            assertz(currentRoom(ToRoom)),
            format('O herói saiu da ~w para a ~w.\n', [FromRoom, ToRoom]),
            decrementMoves,
            explore;
            format('Não existe caminho nesa direção!\n'),
            fail
    ).

decrementMoves :-
    remainingMoves(Moves),
    Moves > 0,
    NewMoves is Moves - 1,
    retract(remainingMoves(Moves)),
    assertz(remainingMoves(NewMoves)),
    format('Movimentos restantes: ~w\n', [NewMoves]),
    (NewMoves =:= 0 -> gameOver; true).

gameOver :-
    retract(currentState(alive)),
    assertz(currentState(dead)),
    format('O herói ficou sem movimentos e morreu.\n').

explore :-
    currentRoom(Room),
    (
        monster(Room, Monster) ->
            format('O monstro ~w foi encontrado.\n', [Monster]);
            format('Nenhum monstro foi encontrado na sala.\n')
    ),
    (
        reward(Room, Reward) ->
            format('A recompensa ~w foi encontrada.\n', [Reward]);
            format('Nenhuma recompensa foi encontrada na sala.\n')
    ).

fight :-
    currentRoom(Room),
    monster(Room, Monster) ->
        (
            retract(monster(Room, Monster)),
            format('O herói derrotou o monstro ~w.\n', [Monster])
        );
        format('A sala não possui monstros').

loot :-
    currentRoom(Room),
    monster(Room, Monster) ->
        format('Derrote o ~w antes.', [Monster]);
        (
            reward(Room, Reward) -> 
            (
                retract(reward(Room, Reward)),
                format('Você obteve ~w.', [Reward])
            );
            format('A sala já foi saqueada')
        ).



% Implementação do Agente:


% Função de Busca: 
searchPath :-
    remainingMoves(Max),
    findall((Room, M), monster(Room, M), Monsters),
    findall(
        Path,
        dfsPlay(entrance, finalRoom, Max, [entrance], Monsters, Path),
        AllPaths
    ),
    uniquePaths(AllPaths, UniquePaths),
    predsort(compareLength, UniquePaths, SortedPaths),
    reverseMultiple(SortedPaths, SortedPathsReverse),
    format('Caminhos possíveis até a finalRoom:\n'),
    printPaths(SortedPathsReverse),
    head(SortedPathsReverse, BestPath),
    format('\nMelhor Caminho: ~w\n', [BestPath]),
    pathToActions(BestPath, Actions),
    retractall(storedPath(_)),
    assertz(storedPath(Actions)),
    format('\nAções do Melhor Caminho: ~w\n', [Actions]).



dfsPlay(finalRoom, finalRoom, _, Path, _, Path).

dfsPlay(Current, Goal, MovesIn, Visited, Monsters, Path) :-
    member((Current, Monster), Monsters),
    MovesIn > 0,
    MovesAfterFight is MovesIn - 1,
    select((Current, Monster), Monsters, NewMonsters),
    dfsPlay(Current, Goal, MovesAfterFight, Visited, NewMonsters, Path).

dfsPlay(Current, Goal, MovesIn, Visited, Monsters, Path) :-
    path(Current, _, Next),
    \+ member(Next, Visited),
    MovesIn > 0,
    MovesOut is MovesIn -1,
    dfsPlay(Next, Goal, MovesOut, [Next|Visited], Monsters, Path).

uniquePaths(Paths, UniquePaths) :-
    list_to_set(Paths, UniquePaths).

compareLength(Order, A, B) :-
    length(A, LA),
    length(B, LB),
    compare(Order, LA, LB).

reverseMultiple([], []).  
reverseMultiple([Head|Tail], [ReversedHead|ReversedTail]) :-
    reverseList(Head, ReversedHead), 
    reverseMultiple(Tail, ReversedTail).

reverseList([], []). 
reverseList([Head|Tail], ReversedList) :-
    reverseList(Tail, ReversedTail),
    concatenate(ReversedTail, [Head], X),
    ReversedList = X.

concatenate([], SecondaryList, SecondaryList). 
concatenate([Head|Tail], SecondaryList, [Head|TailConcatenateList]) :- 
    concatenate(Tail, SecondaryList, TailConcatenateList).

printPaths([]).
printPaths([P|Ps]) :-
    format('~w\n', [P]),
    printPaths(Ps).

head([Head|_], Head).

pathToActions([_], []).
pathToActions([Current, Next | Rest], [Action | ActionsRest]) :-
    determineDirection(Current, Next, Action),  
    pathToActions([Next | Rest], ActionsRest).

determineDirection(Room1, Room2, Direction) :-
    path(Room1, Direction, Room2).



% Deploy do Agente:

runBestPath :-
    storedPath(Actions) ->
        (
            format('====> Executando o melhor caminho: ~w <====\n', [Actions]),
            followActions(Actions)
        );
        format('Ainda não existe um caminho armazenado.\n').

followActions([]) :-
    format('\nChegou ao destino!\n').

followActions([Action | Rest]) :-
    walk(Action),
    fight,
    followActions(Rest).