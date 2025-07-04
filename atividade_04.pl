% Testes Aleatorios:

/*
    - funcao(input, output).
    - [Head|Tail] O pipe define a separação entre o que vai pra cabeça e o que vai pra cauda, no exemplo só um elemento está sendo extraido. 
    -
*/
head([Head|_], Head). % Função pra pegar o primeiro elemento.
?- head([0,1,2,3,4,5], X) % Teste, retorna 0.

tail([_|Tail], Tail). % Função para pegar todos os elementos exceto o primeiro.
?- tail([0,1,2,3,4,5], X) % Retorna [1,2,3,4,5]

sum([], 0). % Caso base
sum([Head|Tail], FinalSum) :- % Define o que recebe e o que retorna.
    sum(Tail, PartialSum), % Define a chamada recursiva.
    FinalSum is PartialSum + Head. % Define o que é o FinalSum e adiciona a lógica da recursão. soma é igual a cabeça + a soma da cauda.

?. sum([1,2,3,4,5], X). % Retorna 15

getLenght([], 0). % Caso base, não deu pra usar o nome lenght porque é reservado.
getLenght([_|Tail], FinalLenght) :- % O Head é uam váriavel anônima, pois seu valor não é necessário.
    getLenght(Tail, PartialLenght), % Define como é a chamada recursiva.
    FinalLenght is 1 + PartialLenght. % Define a lógica da recursão. Tamanho é igual a 1 + o tamanho da cauda.

?. getLenght([1,2,3,4,5], X). % Retorna 5

containsElement([], false). % Em caso de lista vazia, retorna false.
containsElement([Head|_], Head). % Compara o primeiro elemento da lista com o elemento que está sendo buscado, o nome é o mesmo para fazer a comparação.
containsElement([_|Tail], FinalResult) :- 
    containsElement(Tail, FinalResult).

?. containsElement([1,2,3,4,5], 2). % Retorna true

% Funções Gerais:

% Q - 01: Regra para verificar a frequência de um elemento na lista.

% Q - 02: Regra para concatenar duas listas.

% Q - 03: Regra para concatenar duas listas dois a dois.

% Q - 04: Regra para somar as listas que estão dentro de uma lista. Exemplo: [[1,2,3], [1,1,1]], gera a saída [6, 3].

% Q - 05: Regra para encontrar o último elemento na lista.

% Q - 06: Regra para contar elementos contíguos 2 a 2 em uma lista.

% Q - 07: Regra para construir a lista reversa.

% Q - 08: Regra para informar se o elemento está na lista.

% Q - 09: Regra para informar os elementos duplicados, considerando a ordem em que eles aparecem na lista.

% Q - 10: Regra para eliminar os elementos duplicados.

/*
    Buscas Simples de Cabeça e Cauda:
    [Head | Tail] = [mia, vicent, yolanda, bob] 
        - Head = mia, 
        - Tail = [vicent, yolanda, bob]
    [Head1, Head2 | Tail] = [mia, vicent, yolanda, bob] 
        - Head1 = mia, 
        - Head2 = vicent, 
        - Tail = [yolanda, bob]
    [_, Head2 | Tail] = [mia, vicent, yolanda, bob]
        - Head2 = vicent,
        - Tail = [yolanda, bob]

    Soma Recursiva:
    
*/
