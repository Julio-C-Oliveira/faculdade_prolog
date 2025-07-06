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