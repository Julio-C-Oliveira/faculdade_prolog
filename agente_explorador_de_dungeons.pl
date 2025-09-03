% Estados dinâmicos:
:- dynamic currentRoom/1.
:- dynamic currentState/1.
:- dynamic currentGold/1.
:- dynamic currentAttack/1.
:- dynamic currentDefense/1.

:- dynamic monster/2. 
:- dynamic reward/2. 

% Estrutura da Dungeon:
% Caminhos que avançam.
path(entrance, right, room_01).
path(entrance, left, room_01).
path(room_01, left, room_02).
path(room_02, left, room_03).
path(room_03, right, room_04). 
path(room_04, left, room_05). 
path(room_05, right, room_06).
path(room_05, left, extraRoom).
path(room_06, right, finalRoom).

% Caminhos que são loops.
path(room_01, right, room_01).
path(room_03, left, room_03).
path(room_04, right, room_04).

% Caminhos que retrocedem.
path(room_02, right, entrance).
path(extraRoom, left, room_03).
path(extraRoom, right, room_05).

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
reward(room_03, defenseBuff).
reward(room_04, attackBuff).
reward(room_05, gold).
reward(room_06, magicPower).
reward(finalRoom, gold).
reward(extraRoom, invincibility).

% Lógica do Jogo:
% Valores Iniciais.
currentRoom(entrance).
currentState(alive).
currentGold(0).
currentAttack(1).

% Busca no Ambiente.
explore :- 
    currentRoom(Room),
    (   
    	monster(Room, Monster) ->  
    		format('O monstro ~w foi encontrado na ~w.\n', [Monster, Room]);
    		format('Nenhum monstro encontrado na sala.\n')
    ),
    (   
    	reward(Room, Reward) ->
    		format('A recompensa ~w foi encontrado na ~w.\n', [Reward, Room]);
    		format('Nenhuma recompensa foi encontrada na sala.')
    ).

% Ações.
walk(Side) :-
    currentRoom(FromRoom),
    path(FromRoom, Side, ToRoom),
    retract(currentRoom(FromRoom)),
    assertz(currentRoom(ToRoom)),
    format('O herói saiu da ~w para a ~w.\n', [FromRoom, ToRoom]),
    explore.

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
