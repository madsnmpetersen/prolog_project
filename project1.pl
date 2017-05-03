:- use_module(library(lists)).

c(I,J) :-  %The definition of a comparator.
	integer(I), integer(J),
	I \= J.
	%I < J. Uncomment this line, and comment the one above to only consider standard networks as solutions, this is considerably faster.
	
sc(I,J) :-  %The definition of a comparator.
	integer(I), integer(J),
	I < J.

is_standard(C) :- %Checks that C is a network in the chosen model.
	C = [H|T], 
	isStandardComparator(H),
	(is_standard(T) ; T = []).

isStandardComparator(Comparator) :- %Tests that the element Comparator is a comparator in the chosen model.
	Comparator = c(I,J), sc(I,J).
	
comparatorInRange(c(I,J),N) :- %Can be used to generate possible comparators for a N channel network, or to check if a comparator is possible in an N channel network.
	(
	var(N)
	->
		N = J, c(I,J)
	;
		between(1,N,I), between(1,N,J), c(I,J)
	).
	
isComparator(Comparator) :- %Tests that the element Comparator is a comparator in the chosen model.
	Comparator = c(I,J), c(I,J).	

possibleFirsts(C,L) :- %Finds all comparators that can execute first in a network.
	notFirsts(1,C,[],Result), %finds all comparators that can't execute first on the input.
	sort(Result,ProperResult), %The index list must be sorted for selectchkIndexList/3 to work correctly.
	selectchkIndexList(ProperResult,C,L). %these comparators are then removed from the network, the remaining is those that can execute first.
	
notFirsts(_,[_|[]],_,[]) :- !. %Finds out what comparators can't be the first ones by finding out what comparators that are NOT specifically after others
notFirsts(Index,C,Temp,Result) :-
	after(Index,C,R1), %Calls after/3 to find out what comparators must execute directly after the index passed as Index.
	union(R1,Temp,TempNew), %The comparators are then joined to the ones already found, initially that is an empty list.
	IndexNew is Index + 1, %The next index is prepared to be supplied to the next recursive call.
	length(C,LenC), %Length of the network C is found so it can be compared.
	(
		LenC > IndexNew, %indicates that there are more elements to go through.
		notFirsts(IndexNew,C,TempNew,Result) %makes the next recursive call with the next Index, and the accumulation of the indicies found previously.
	;
		LenC = IndexNew, %indicates that all indices have been tested.
		union(R1,Temp,Result) %the result is the elements found in the latest call as well as all elements found in previous calls.
	).

after(I,C,Exlist) :- %Finds out what comparators in a network that HAS to execute AFTER index I.
	I1 is I+1, %Finds the index of the element after the one passed as parameter.
	directlyAfter(I,I1,C,Exlist). %makes the call to directlyAfter with the element passed as parameter as well as the one after it, and the network C.

directlyAfter(_,_,[],[]) :- !. %Used to find which comparators MUST execute after others for a given network.
directlyAfter(_,_,[_|[]],[]) :- !. %Base cases.
directlyAfter(E1Index,E2Index,C,Exlist) :- 
	nth1(E1Index,C,E1),%Gets the elements that are represented by the indices passed as parameters.
	nth1(E2Index,C,E2),
	length(C,L),
	(
		sharesChannel(E1,E2), !, %If the 2 elements share a channel, cut.
		(			
			E2Index = L, %The end of the network C is reached.
			Exlist = [E2Index|[]], ! %The result is obtained so cut.
		;
			E3Index is E2Index + 1, %If the end of network C is not reached, next element is found.
			Exlist = [E2Index|T], %Element is added to the exclusion list, since they share a channel and therefore Element 2 has to execute after Element 1.
			directlyAfter(E2Index,E3Index,C,T1), %Make a recursive call checking what elements must execute after the element just added.
			directlyAfter(E1Index,E3Index,C,T2), %Make a recursive call and check if Element 3 must execute after element 1.
			union(T1,T2,T) %The complete exclusion list is the combined lists found above.
		)
	;
		(
			E2Index = L, ! %If the elemts do not share a channel and the end of the network C is reached we are done.
		;
			E3Index is E2Index + 1, %Next element is found.
			directlyAfter(E1Index,E3Index,C,Exlist) %The next recursive call is made without adding any elements to the exclusion list.
		)
	).

sharesChannel(c(E1I,E1J),c(E2I,E2J)) :- %Determines if to comparators share an input channel or not.
	c(E1I,E1J), c(E2I,E2J), %Both must be legal comparators within the chosen model.
	(
	E1I = E2I, !;  %Each statement proves they share a channel, so we don't have to keep going if just 1 is proven true.
	E1J = E2J, !;
	E1I = E2J, !;
	E1J = E2I, !
	).
	
selectchkList([],List2,List2):-!.
selectchkList([H|T],List2,Result) :- %Removes every element in List1 from List2 with selectchk/3
	(
		member(H,List2)
	->
		selectchk(H,List2,Result1)
	;
		Result1 = List2
	),
	selectchkList(T,Result1,Result).
	
selectchkIndexList([],List2,List2):-!. %Removes the indices given in the first list from the second list and the result is the third list.
selectchkIndexList(_,[],[]):-!.
selectchkIndexList([H|T],List2,Result) :- %Index list has to be sorted.
	(
	length(List2,L2),
	H =< L2, H > 0
	->
		nth1(H,List2,_,Result1)
	;
		Result1 = List2
	),
	(
		T \= [],
		subtractFromList(1,T,T1),
		selectchkIndexList(T1,Result1,Result)
	;
		T = [],
		Result = Result1,!
	).

subtractFromList(_,[],[]):-!. %Subtracts an integer from every element in a list.
subtractFromList(Int,[H|T],[ResH|ResT]) :-
	ResH is H - Int,
	subtractFromList(Int,T,ResT).

is_network(C) :- %Checks that C is a network in the chosen model.
	C = [H|T], 
	isComparator(H), 
	(T = [] ; is_network(T)).

channels(C,N) :- %Assignment defined predicate.
	( var(N)
	->
		sort(C,Csorted),
		Csorted = [H|T],
		(T = [] ; channels(T,N)),
		comparatorInRange(H,N)
	;
		C = [H|T],
		(T = [] ; channels(T,N)),
		comparatorInRange(H,N)
	).

channelsOfLengthL(N,L,[H|T]) :- %Generates all possible networks of length L, over N channels.
	length([H|T],L),
	(T = [] ; channels(T,N)),
	comparatorInRange(H,N).
	
partialSolutionsOfLengthL(N,L,C):- %Even more limited solution space.
	length(C,L),
	findall(List,(between(1,N,X),possibleComparatorsForChannelX(X,N,List)),PList),
	Rem is N,
	possibleSolutionsOfLengthL(C,N,L,Rem,PList).
	
possibleSolutionsOfLengthL(N,L,C):- %Generates a more limited solution space that should contain a solution to a network on any number of channels.
	length(C,L),
	findall(List,(between(1,N,X),possibleComparatorsForChannelX(X,N,List)),List),
	permutation(List,PList),
	Rem is N,
	possibleSolutionsOfLengthL(C,N,L,Rem,PList).
possibleSolutionsOfLengthL(C,N,L,0,_):-
	Lnew is L - N,
	channelsOfLengthL(N,Lnew,C).
possibleSolutionsOfLengthL([H|T],N,L,Rem,[PListH|PListT]) :-
	member(H,PListH),
	RemNew is Rem - 1,
	(T = [] ; possibleSolutionsOfLengthL(T,N,L,RemNew,PListT)).

possibleComparatorsForChannelX(X,N,PList) :- %Generates a list PList that contains all comparators that contain channel X possible over N channels.
	findall(C,(comparatorInRange(C,N),(C = c(_,X); C = c(X,_))),PList).

run(C,Input,Output) :- %Doesn't work for input not using all channels.
	length(Input,L),
	channels(C,L),
	same_length(Input,Output),
	runWork(C,Input,Output).
	
runWork(C,Input,Output) :-
	C = [c(I,J)|T],
	nth1(I,Input,Xi),
	nth1(J,Input,Xj),
	(Xi > Xj, swap(Input,I,J,InputNew), (T = [], Output = InputNew ; runWork(T,InputNew,Output)); 
	Xi =< Xj, (T = [], Output = Input ; runWork(T,Input,Output))).

swap(As,I,J,Cs) :- %Swaps the the elements I and J, in the lists As and Cs.
   same_length(As,Cs),
   append(BeforeI,[AtI|PastI],As),
   append(BeforeI,[AtJ|PastI],Bs),
   append(BeforeJ,[AtJ|PastJ],Bs),
   append(BeforeJ,[AtI|PastJ],Cs),
   Inew is I - 1, % This is done to use the same indices as nth1/3.
   Jnew is J - 1,
   length(BeforeI,Inew),
   length(BeforeJ,Jnew).
 
binaryList(List,Len) :- %Generates binary number lists for use in testing if a comparator network is a sorting network.
	List = [H|T],
	between(0,1,H),
	(Len > 1, LenNew is Len - 1, binaryList(T,LenNew) ; Len = 1, T = []). 

isSorted([_|[]]):- !. %Tests wheter an input is sorted correctly, in ascending order.
isSorted([F,S|T]) :- 
	F =< S,
	isSorted([S|T]).

is_SN(S,N) :- %Tests if network S is a sorting network over N channels.
	is_network(S),
	findall(X, binaryList(X,N), [_|TestBatch]),
	comprehensiveTest(S,N,TestBatch).

comprehensiveTest(S,N,[H|T]) :- %Used in testing for sorting networks.
	run(S,H,Sorted),
	isSorted(Sorted),
	(T = [] ; comprehensiveTest(S,N,T)).
	
find_SN(N,K,S) :- %Finds a sorting network S of length K over N channels, Takes VERY long for networks of more than 4 channels.
	channelsOfLengthL(N,K,S),
	is_SN(S,N).

find_SN1(N,K,S) :- %Finds a sorting network S of length K over N channels, is faster than find_SN. But doesn't find all possible solutions.
	possibleSolutionsOfLengthL(N,K,S),
	is_SN(S,N).

find_SN2(N,K,S) :- %Finds a sorting network S of length K over N channels, is faster than find_SN1. But finds fewer possible solutions.
	partialSolutionsOfLengthL(N,K,S),
	is_SN(S,N).
	
network_to_layered([],[]). %Converts a network C into a layered network representation L of C.
network_to_layered(C,[HLayer|TLayers]) :-
	possibleFirsts(C,Firsts), %Returns the indices of the comparators in C that possibly can execute on the input first.
	permutation(Firsts,HLayer), %Any permutation of the comparators that can execute first is a possible layer, since within a layer the order of execution is irrelevant.
	selectchkList(HLayer,C,CNew), %The comparators that are layered together are then removed from C.
	network_to_layered(CNew,TLayers). %The recursive call is made with the new network that consists of C minus the comparators that are now a layer.
	
layered_to_network(L,C) :- %Gives all possible networks that the layered network L can be turned into.
	flatten(L,FlatL), %Flattens the layered network.
	length(FlatL,Len), %Length of the flatten network is found.
	length(C,Len), %The length of the network it translated to must be the same length as the flattened network.
	layered_to_networkWorker(FlatL,C). %Calls the worker predicate to generate the actual possible translations.
	
layered_to_networkWorker([],[]). %base case.
layered_to_networkWorker(L,C) :-
	possibleFirsts(L,FirstEs), %Find out which elements are candidates for the first element in the translated network.
	member(FirstE,FirstEs), %one of these candidates is then chosen, with another possible solution being another choice if there are more than 1.
	[FirstE|T] = C, %The first/next element of network C is the element chosen from among the possibilites.
	selectchk(FirstE,L,LNew), %the chosen element is removed from the available elements.
	layered_to_networkWorker(LNew,T). %the next recursive call is made until we hit the base case, which is that there are no more elements left to choose from.
	
layer(C,L) :- %Combines layered_to_network, network_to_layered and layer, must be called with a network or variable as first parameter and variable of layered network as second.
	(var(C) %If C is a variable
	->
		layered_to_network(L,C) %Then use layered_to_network/2
	;
		network_to_layered(C,L) %Else use network_to_layered/2
	).