% Autômato finito - Ex 01:

start(q1). % Estado inicial
end(q2). % Estado final

% As transições que avançam.
transition(q1, '1', q2).
transition(q2, '0', q3).

% Os self loops.
transition(q1, '0', q1).
transition(q2, '1', q2).

% As transições que retroagem.
transition(q3, '0', q2).
transition(q3, '1', q2).

test(X) :- 
    string_chars(X, Fita),
    start(Node),
    observer(Node, Fita), !.

observer(Node, []) :-
    end(Node), !.

observer(FromState, Fita) :-
    transition(FromState, X, ToState),
    walk(X, Fita, New_Fita),
    observer(ToState, New_Fita).

walk(Head, [Head|Tail], Tail).

% ?- test("101").
% Esse autômato aceita palavras que possuem a substring "10" + "0" ou "1".



% Autômato finito - Ex 02:
% Q = {q0, q1, q2, q3, q4}
% A = [a, b]
% F = [q1, q2]

% Estado inicial e final.
start(q0).
end(q1).
end(q2).

% As transições que avançam.
transition(q0, 'a', q1).
transition(q0, 'b', q2).
transition(q1, 'b', q3).
transition(q2, 'a', q4).

% As transições que retroagem.
transition(q3, 'a', q1).
transition(q4, 'b', q2).

% Os self loops.
transition(q1, 'a', q1).
transition(q2, 'b', q2).
transition(q3, 'b', q3).
transition(q4, 'a', q4).

test(X) :- 
    string_chars(X, Fita),
    start(Node),
    observer(Node, Fita), !.

observer(Node, []) :-
    end(Node), !.

observer(FromState, Fita) :-
    transition(FromState, X, ToState),
    walk(X, Fita, New_Fita),
    observer(ToState, New_Fita).

walk(Head, [Head|Tail], Tail).

% As menores palavras reconhecidas são: 
% - "a"
% - "b"

% A linguagem reconhecida por esse autômato é:
% - aaaaaaaaaa
% - bbbbbbbbbb
% - bab
% - aba


% Autômato com Movimento Vazio - Ex 01:
start(q0).
end(q2).

transition(q0, ε, q1).
transition(q1, ε, q2).

transition(q0, a, q0).
transition(q1, b, q1).
transition(q2, a, q2).

test(X) :- 
    string_chars(X, Fita),
    start(Node),
    observer(Node, Fita), !.

observer(Node, []) :-
    end(Node), !.

observer(FromState, []) :-
    transition(FromState, ε, ToState),
    observer(ToState, []).

observer(FromState, Fita) :-
    (   
    	transition(FromState, X, ToState);
    	transition(FromState, ε, ToState)
    ),
    walk(X, Fita, New_Fita),
    observer(ToState, New_Fita).

walk(Head, [Head|Tail], Tail).

% Qual a 5-upla desse autômato?
% Qual a menor palavra reconhecida? É "" vazio, já que existem transições do q0 até o q2 vazias.
% Qual a linguagem reconhecida? Ele aceita vazio, dois caracteres aleatórios isolados ou dois caracteres aleatórios seguidos de a's 

% Autômato com pilha - Ex 01:
q(1, 'e', 'e', '$', 2).
q(2, '0', 'e', '0', 2).
q(2, '1', '0', 'e', 3).
q(3, '1', '0', 'e', 3).
q(3, 'e', '$', 'e', 4).

inicio(1).
final(4).

teste(X) :- 
    string_chars(X, Fita),
    inicio(No),
    reconhecedor(No, Fita, []), !.


reconhecedor(No, [], []) :- final(No), !.
reconhecedor(De,[], Pilha) :- 
    q(De, e, L, E, Para),
    atualiza_pilha(Pilha, L, E, Nova_Pilha),
    reconhecedor(Para, [], Nova_Pilha).

reconhecedor(De,Fita, Pilha) :- 
    q(De, X, L, E, Para),
    X \== e,
    caminha(X, Fita, Nova_Fita),
    atualiza_pilha(Pilha, L, E, Nova_Pilha),
    reconhecedor(Para, Nova_Fita, Nova_Pilha).

reconhecedor(De,Fita, Pilha) :- 
    q(De, e, L, E, Para),
    atualiza_pilha(Pilha, L, E, Nova_Pilha),
    reconhecedor(Para, Fita, Nova_Pilha).

caminha(H,[H | T],T).

atualiza_pilha(Pilha, L, D, Nova_Pilha) :- 
    atualiza_leitura(Pilha, L, P1),
    atualiza_escrita(P1, D, Nova_Pilha).

atualiza_leitura([L | Pilha], L, Pilha).
atualiza_leitura(Pilha, e, Pilha).

atualiza_escrita(Pilha, E, [E | Pilha]) :- 
    E \== e, !.
atualiza_escrita(Pilha, e, Pilha).