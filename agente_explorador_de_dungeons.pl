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
% Caminhos que avançam.
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

% Caminhos que são loops.
% path(room_01, right, room_01).
path(room_03, left, room_03).
path(room_04, right, room_04).

% Caminhos que retrocedem.
path(room_02, right, entrance).
path(extraRoom, left, room_05).

% Monstros.
monster(room_01, slime).
monster(room_02, goblin).
monster(room_03, wolf).
monster(room_04, zombie).
monster(room_05, ghost).
monster(room_06, majin).
monster(finalRoom, maou).
monster(extraRoom, overlord).

% Recompensas.
reward(room_01, gold).
reward(room_02, attackBuff).
reward(room_03, gold).
reward(room_04, attackBuff).
reward(room_05, gold).
reward(room_06, attackBuff).
reward(finalRoom, gold).
reward(extraRoom, invincibility).

% Lógica do Jogo:
% Valores Iniciais.
currentRoom(entrance).
currentState(alive).
currentGold(0).
currentAttack(1).
remainingMoves(7).

% Busca no Ambiente.
explore :- 
    currentRoom(Room),
    (   
    	monster(Room, Monster) ->  
    		format('O monstro ~w foi encontrado.\n', [Monster]);
    		format('Nenhum monstro encontrado na sala.\n')
    ),
    (   
    	reward(Room, Reward) ->
    		format('A recompensa ~w foi encontrado.\n', [Reward]);
    		format('Nenhuma recompensa foi encontrada na sala.')
    ).

% Ações.
walk(Side) :-
    currentRoom(FromRoom),
    (   monster(FromRoom, Monster) ->
        format('O caminho está bloqueado pelo ~w! Derrote-o antes de prosseguir.\n', [Monster]),
        !, fail;   
        path(FromRoom, Side, ToRoom) ->
        retract(currentRoom(FromRoom)),
        assertz(currentRoom(ToRoom)),
        format('O herói saiu da ~w para a ~w.\n', [FromRoom, ToRoom]),
        decrementMoves,
        explore;   
        format('Não há caminho nessa direção!\n'),
        fail
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
            reward(Room, Reward),
            retract(reward(Room, Reward)),
            format('Você obteve ~w.', [Reward])
        ).



searchPath :-
    remainingMoves(Max),
    % pegamos todos os monstros iniciais
    findall((Room, M), monster(Room, M), Monsters),
    findall(Path,
            dfs_play(entrance, finalRoom, Max, [entrance], Monsters, Path),
            AllPaths),
    unique_paths(AllPaths, UniquePaths),
    predsort(compare_length, UniquePaths, SortedPaths),
    reverseMultiple(SortedPaths, SortedPathsReverse),
    format('Caminhos possíveis até a finalRoom:\n'),
    printPaths(SortedPathsReverse),
    head(SortedPathsReverse, BestPath),
    format('\nMelhor Caminho: ~w\n', [BestPath]),
    pathToActions(BestPath, Actions),
    retractall(storedPath(_)),
    assertz(storedPath(Actions)),
    format('\nAções do Melhor Caminho: ~w\n', [Actions]).

followActions([]) :-
    format('\nChegou ao destino!\n').

followActions([Action | Rest]) :-
    walk(Action),         % executa a ação (walk_left, walk_right, etc.)
    fight,
    followActions(Rest).

runBestPath :-
     storedPath(Actions)  ->  
        (
            format('====> Executando o melhor caminho: ~w <====\n', [Actions]),
            followActions(Actions)
        );
        format('Ainda não existe um caminho armazenado.\n').



% Funções auxiliares.
decrementMoves :-
    remainingMoves(Moves),
    Moves > 0,
    NewMoves is Moves - 1,
    retract(remainingMoves(Moves)),
    assertz(remainingMoves(NewMoves)),
    format('Movimentos restantes: ~w\n', [NewMoves]),
    (NewMoves =:= 0 -> gameOver ; true).

gameOver :-
    retract(currentState(alive)),
    assertz(currentState(dead)),
    format('O herói ficou sem movimentos e morreu.\n').

% Caso base: chegou no destino
dfs_play(finalRoom, finalRoom, _, Path, _, Path).

% Se houver monstro na sala atual, precisa derrotar
dfs_play(Current, Goal, MovesIn, Visited, Monsters, Path) :-
    member((Current, Monster), Monsters),      % há monstro aqui
    MovesIn > 0,
    MovesAfterFight is MovesIn - 1,
    % format('Simulação: o herói derrotou o ~w em ~w.\n', [Monster, Current]),
    % remove o monstro derrotado da lista
    select((Current, Monster), Monsters, NewMonsters),
    dfs_play(Current, Goal, MovesAfterFight, Visited, NewMonsters, Path).

% Caso contrário, anda
dfs_play(Current, Goal, MovesIn, Visited, Monsters, Path) :-
    path(Current, _, Next),
    \+ member(Next, Visited),
    MovesIn > 0,
    MovesOut is MovesIn - 1,
    % format('Simulação: andou de ~w para ~w.\n', [Current, Next]),
    dfs_play(Next, Goal, MovesOut, [Next|Visited], Monsters, Path).

printPaths([]).
printPaths([P|Ps]) :-
    format('~w\n', [P]),
    printPaths(Ps).

simulate_fight(Monster, MovesIn, MovesOut) :-
    MovesIn > 0,
    MovesOut is MovesIn - 1,
    format('Simulação: o herói derrotou o ~w.\n', [Monster]).

simulate_walk(FromRoom, Side, ToRoom, MovesIn, MovesOut) :-
    path(FromRoom, Side, ToRoom),
    MovesIn > 0,
    MovesOut is MovesIn - 1,
    format('Simulação: andou de ~w para ~w.\n', [FromRoom, ToRoom]).

unique_paths(Paths, UniquePaths) :-
    list_to_set(Paths, UniquePaths).

compare_length(Order, A, B) :-
    length(A, LA),
    length(B, LB),
    compare(Order, LA, LB).

head([Head|_], Head).

concatenate([], SecondaryList, SecondaryList).  % Caso a primeira lista esteja vazia, retorna a segunda lista
concatenate([Head|Tail], SecondaryList, [Head|TailConcatenateList]) :- % Pega o primeiro elemento da primeira lista e passa pra lista final.
    concatenate(Tail, SecondaryList, TailConcatenateList). % Reduz o tamanho da lista primária e pega a lista parcialmente concatenada.

reverseList([], []). % Se a lista for vazia não tem o que inverter
reverseList([Head|Tail], ReversedList) :-
    reverseList(Tail, ReversedTail),
    concatenate(ReversedTail, [Head], X), % Concatena a lista, mas jogando a cabeça pro final.
    ReversedList = X. % Retorna o resultado.

reverseMultiple([], []).  
reverseMultiple([Head|Tail], [ReversedHead|ReversedTail]) :-
    reverseList(Head, ReversedHead), 
    reverseMultiple(Tail, ReversedTail).

% Converte lista de salas em lista de ações
pathToActions([_], []).  % última sala, sem ação
pathToActions([Current, Next | Rest], [Action | ActionsRest]) :-
    determineDirection(Current, Next, Action),  % já retorna a ação (walk_left, walk_right...)
    pathToActions([Next | Rest], ActionsRest).

% Determina a direção necessária para ir de Room1 para Room2
determineDirection(Room1, Room2, Direction) :-
    path(Room1, Direction, Room2).
