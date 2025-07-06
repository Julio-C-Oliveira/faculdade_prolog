% Q 01: Função que remova os itens duplicados de uma lista. Aridade 2.
containsElement([], _) :- 
    false. % Em caso de lista vazia, retorna false.
containsElement([Head|Tail], Element) :- 
    (
    	Head == Element -> true; % Se eles são iguais é verdade.
    	containsElement(Tail, Element) % Senão chama denovo até chegar no falso.
    ).

removeDuplicates([], []).
removeDuplicates([Head|Tail], ListWithoutDuplicates) :-
    removeDuplicates(Tail, TailWithoutDuplicates),
    (
        containsElement(TailWithoutDuplicates, Head) -> % Verifica se o elemento já existe no resultado. 
    		ListWithoutDuplicates = TailWithoutDuplicates; % Se existe só copia.
    		ListWithoutDuplicates = [Head|TailWithoutDuplicates] % Se não existe adiciona o elemento.
    ).



% Q 02:  Ordenar uma lista de forma crescente. Aridade 2.
minElement([], none).
minElement([Head|Tail], MinElement) :-
    minElement(Tail, MinTailElement),
    (
    	MinTailElement == none -> MinElement = Head; % ; é o senão.
        (
        	Head =< MinTailElement -> MinElement = Head;
        	MinElement = MinTailElement
        )
    ).

deleteFirstElementOccurrence(Element, [Head|Tail], ListWithoutFirstOcurrence) :-
    (
    	Element == Head -> 
    		ListWithoutFirstOcurrence = Tail;
    		deleteFirstElementOccurrence(Element, Tail, Rest),
    		ListWithoutFirstOcurrence = [Head|Rest]
    ).

putMinAtFirst([], []).  
putMinAtFirst([Head|Tail], [Min|Rest]) :-
    minElement([Head|Tail], Min), 
    deleteFirstElementOccurrence(Min, [Head|Tail], Rest). 

tail([_|Tail], Tail). % Função para pegar todos os elementos exceto o primeiro.
head([Head|_], Head). % Função pra pegar o primeiro elemento.

crescentSort([], []).
crescentSort([Head|Tail], OrderedList) :-
    putMinAtFirst([Head|Tail], MinAtFirstOrderedList), % Ordena o primeiro.
    tail(MinAtFirstOrderedList, TailMinAtFirstOrderedList), % Pega a cauda que ainda está desordenada.
    crescentSort(TailMinAtFirstOrderedList, PartialOrderedList), % Organiza a cauda.
    head(MinAtFirstOrderedList, Min), % Pega a cabeça que já está ordenada.
    OrderedList = [Min|PartialOrderedList]. % Junta a cabeça ordenada com a cauda ordenada.



% Q 03: Receba uma lista de inteiros e informe a quantidade de números primos adjacents entre si. 
hasDivisor(Number, K) :-
    K*K =< Number, % Se o número for menor ele possui divisor porque eu só preciso testar os valores até a raiz do valor.
    ( 
    	Number mod K =:= 0; % Se o resto com k for 0 tem divisor.
    	K2 is K + 2, % Mais dois que é pra verificar somente os números impares, os pares já foram descartados.
        hasDivisor(Number, K2)
    ).

isPrime(2) :- !. % Único número primo par.
isPrime(3) :- !. % Vou usar como base então tenho que retirar do caso.
isPrime(Number) :-
    Number > 1, % Tem que ser maior que 1.
    Number mod 2 =\= 0, % Se o resto for 0, é par e não é 2, ou seja tem divisor.
    \+ hasDivisor(Number, 3). % Verifica se existe algum divisor impar a partir do 3.

