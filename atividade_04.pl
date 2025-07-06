% Funções Auxiliares:
sum([], 0). % Caso base
sum([Head|Tail], FinalSum) :- % Define o que recebe e o que retorna.
    sum(Tail, TailSum), % Define a chamada recursiva.
    FinalSum is TailSum + Head. % Define o que é o FinalSum e adiciona a lógica da recursão. soma é igual a cabeça + a soma da cauda.

% Q - 01: Regra para verificar a frequência de um elemento na lista.
searchNumberOfRepetitions([], _, 0). % Se a lista está vazio o elemento não existe, ou seja o número de repetições é 0.
searchNumberOfRepetitions([Head|Tail], Element, NumberOfRepetitions) :- 
    searchNumberOfRepetitions(Tail, Element, NumberOfRepetitionsOnTail),
    (
        Element == Head -> NumberOfRepetitions is NumberOfRepetitionsOnTail + 1; % Soma 1 se a cabeça for igual a cabeça.
    	NumberOfRepetitions = NumberOfRepetitionsOnTail % Só passa adiante se for diferente.
    ).

% ?. searchNumberOfRepetitions([], 1, X) % Retorna 0 
% ?. searchNumberOfRepetitions([1,2,3,4,5,1], 1, X) % Retorna 2 
% ?. searchNumberOfRepetitions([1,2,3,4,5], 6, X) % Retorna 0 

% Q - 02: Regra para concatenar duas listas.
concatenate([], SecondaryList, SecondaryList).  % Caso a primeira lista esteja vazia, retorna a segunda lista
concatenate([Head|Tail], SecondaryList, [Head|TailConcatenateList]) :- % Pega o primeiro elemento da primeira lista e passa pra lista final.
    concatenate(Tail, SecondaryList, TailConcatenateList).  % Reduz o tamanho da lista primária e pega a lista parcialmente concatenada.

% Q - 03: Regra para concatenar duas listas dois a dois, só funciona pra listas com número par de itens.
concatenateTwoByTwo([], SecondaryList, SecondaryList).  % Caso a primeira lista esteja vazia, retorna a segunda lista
concatenateTwoByTwo([Head1, Head2|Tail], SecondaryList, [Head1, Head2|PartialConcatenateList]) :- % Pega o primeiro elemento da primeira lista e passa pra lista final.
    concatenateTwoByTwo(Tail, SecondaryList, PartialConcatenateList).  % Reduz o tamanho da lista primária e pega a lista parcialmente concatenada.

% Q - 04: Regra para somar as listas que estão dentro de uma lista. Exemplo: [[1,2,3], [1,1,1]], gera a saída [6, 3].
sumListOfListSeparately([], []). % Retorna uma lista vazia para poder concatenar com as outras.
sumListOfListSeparately([HeadList|TailLists], FinalListOfSums) :-
	sum(HeadList, HeadSum), % Faz a soma da cabeça.
    sumListOfListSeparately(TailLists, TailListOfSums), % Faz as somas da cauda.
    FinalListOfSums = [HeadSum|TailListOfSums]. % Concatena o resultado da cabeça com os da cauda.


% Q - 05: Regra para encontrar o último elemento na lista.
getLastElement([], none). % Retorna none caso a lista seja vazia.
getLastElement([Head|Tail], LastElement) :- 
    (
        Tail == [] -> LastElement = Head; % Se a cauda está vazia, então o último elemento está na cabeça.
        getLastElement(Tail, LastElement) % Se a primeira condição é falsa, então ainda não estamos no fim da lista.
    ).

% Q - 06: Regra para contar elementos contíguos 2 a 2 em uma lista.
sumAdjacentsTwoByTwo([], []). % Retorna lista vazia pra poder concatenar
sumAdjacentsTwoByTwo([FirstElement, SecondElement|Tail], SumList) :-
    sum([FirstElement, SecondElement], HeadSum), % Pega a soma dos adjacentes.
    sumAdjacentsTwoByTwo(Tail, TailSumList), % Pega a soma de adjacentes da cauda.
    SumList = [HeadSum|TailSumList]. % Concatena a lista de somas da cauda com a soma da cabeça. 

% Q - 07: Regra para construir a lista reversa.
reverseList([], []). % Se a lista for vazia não tem o que inverter
reverseList([Head|Tail], ReversedList) :-
    reverseList(Tail, ReversedTail),
    concatenate(ReversedTail, [Head], ReversedList). % Concatena a lista, mas jogando a cabeça pro final e retorna o resultado.

% Q - 08: Regra para informar se o elemento está na lista.
containsElement([], false). % Em caso de lista vazia, retorna false.
containsElement([Head|_], Head). % Compara o primeiro elemento da lista com o elemento que está sendo buscado, o nome é o mesmo para fazer a comparação.
containsElement([_|Tail], TailResult) :- 
    containsElement(Tail, TailResult).

% Q - 09: Regra para informar os elementos duplicados, considerando a ordem em que eles aparecem na lista.
getDuplicateElements([], none).
getDuplicateElements([Head|Tail], DuplicateElementsList). % Ainda não está pronto.

% Q - 10: Regra para eliminar os elementos duplicados.

