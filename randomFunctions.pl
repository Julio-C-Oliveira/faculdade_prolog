/*
    - funcao(Args*).
    - [Head|Tail] O pipe define a separação entre o que vai pra cabeça e o que vai pra cauda, no exemplo só um elemento está sendo extraido. 
    - ?. funcao(Args*) -> Retorna a veracidade da afirmação.
    - ?. funcao(Args*, X) -> Retorna o valor de X encontrado quando a função é verdadeira
    - -> essa setá representa então, se algo for verdade, faça isso.
    - ; esse ponto e virgula representa o senão.
    - , essa virgula representa o and.
    - :- significa é verdadeiro se, serve para definir clausulas.
    - Prolog retorna verdadeiro sempre que uma clausula se encaixa em uma busca, se nenhum for verdadeira retorna falso.
*/

head([Head|_], Head). % Função pra pegar o primeiro elemento.
% ?- head([0,1,2,3,4,5], X). % Teste, retorna 0.

tail([_|Tail], Tail). % Função para pegar todos os elementos exceto o primeiro.
% ?- tail([0,1,2,3,4,5], X). % Retorna [1,2,3,4,5]

sum([], 0). % Caso base
sum([Head|Tail], FinalSum) :- % Define o que recebe e o que retorna.
    sum(Tail, TailSum), % Define a chamada recursiva.
    FinalSum is TailSum + Head. % Define o que é o FinalSum e adiciona a lógica da recursão. soma é igual a cabeça + a soma da cauda.

% ?. sum([], X). % Retorna 0
% ?. sum([1,2,3,4,5], X). % Retorna 15

getLenght([], 0). % Caso base, não deu pra usar o nome lenght porque é reservado.
getLenght([_|Tail], FinalLenght) :- % O Head é uam váriavel anônima, pois seu valor não é necessário.
    getLenght(Tail, TailLenght), % Define como é a chamada recursiva.
    FinalLenght is 1 + TailLenght. % Define a lógica da recursão. Tamanho é igual a 1 + o tamanho da cauda.

% ?. getLenght([], X). % Retorna 0
% ?. getLenght([1,2,3,4,5], X). % Retorna 5

containsElement([], false). % Em caso de lista vazia, retorna false.
containsElement([Head|_], Head). % Compara o primeiro elemento da lista com o elemento que está sendo buscado, o nome é o mesmo para fazer a comparação.
containsElement([_|Tail], TailResult) :- 
    containsElement(Tail, TailResult).

% ?. containsElement([1,2,3,4,5], 0). % Retorna false
% ?. containsElement([1,2,3,4,5], 1). % Retorna true
% ?. containsElement([1,2,3,4,5], 2). % Retorna true

getElementIndex([], _, -1). % Retorna -1 caso a lista seja vazia.
getElementIndex([Head|_], Head, 0). % Retorna 0 caso o primeiro elemento seja igual a cabeça da lista.
getElementIndex([_|Tail], Element, FinalIndex) :- % Chamada recursiva.
    getElementIndex(Tail, Element, TailIndex), % Passo recursivo.
    (
    	TailIndex == -1 ->  FinalIndex = -1; % Caso o valor seja -1, para propagar a inexistência do item.
    	TailIndex >= 0 -> FinalIndex is TailIndex + 1 % Caso seja maior ou igual a zero para indicar que existe e retornar o valor correto.
    ).

% ?. getElementIndex([1,2,3,4,5], 0, X). % Retorna -1 
% ?. getElementIndex([1,2,3,4,5], 1, X). % Retorna 0
% ?. getElementIndex([1,2,3,4,5], 4, X). % Retorna 3

maxElement([], none).
maxElement([Head|Tail], MaxElement) :-
    maxElement(Tail, MaxTailElement),
    (
    	MaxTailElement == none -> MaxElement = Head; % ; é o senão.
        (
        	Head >= MaxTailElement -> MaxElement = Head;
        	MaxElement = MaxTailElement
        )
    ).

% ?. maxElement([], X). % Retorna none 
% ?. maxElement([1,2,3,4,5], X). % Retorna 5
% ?. maxElement([1,2,7,4,5], X). % Retorna 7

concatenate([], SecondaryList, SecondaryList).  % Caso a primeira lista esteja vazia, retorna a segunda lista
concatenate([Head|Tail], SecondaryList, [Head|TailConcatenateList]) :- % Pega o primeiro elemento da primeira lista e passa pra lista final.
    concatenate(Tail, SecondaryList, TailConcatenateList).  % Reduz o tamanho da lista primária e pega a lista parcialmente concatenada.

% ?. concatenate([], [1, 2, 3, 4], X). % Retorna [1, 2, 3, 4]
% ?. concatenate([1, 2, 3, 4], [], X). % Retorna [1, 2, 3, 4]
% ?. concatenate([1, 2, 3, 4], [7, 8, 9, 10], X). % Retorna [1, 2, 3, 4, 7, 8, 9, 10]


reverseList([], []). % Se a lista for vazia não tem o que inverter
reverseList([Head|Tail], ReversedList) :-
    reverseList(Tail, ReversedTail),
    concatenate(ReversedTail, [Head], X), % Concatena a lista, mas jogando a cabeça pro final.
    ReversedList = X. % Retorna o resultado.

% ?. reverseList([], X). % Retorna []
% ?. reverseList([1], X). % Retorna [1]
% ?. reverseList([1, 2, 3, 4, 5], X). % Retorna [5, 4, 3, 2, 1]

sumListOfLists([], 0). % Se for vazia retorna 0.
sumListOfLists([HeadList|TailLists], FinalSum) :- 
    sum(HeadList, SumHeadList),  % Soma a lista que está mna cabeça.
    sumListOfLists(TailLists, TailListSum),  % Obtém a soma da cauda.
    FinalSum is SumHeadList + TailListSum. % Junta a soma da cabeça com a da cauda.

% ?. sumListOfLists([[1, 2, 3, 4, 5],[5]], X). % Retorna 20