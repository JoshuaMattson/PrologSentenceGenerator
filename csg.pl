% :- consult('small_dictionary.pl').

% Pattern 1
articlesp1(A) :- random_member(A, ['my', 'this']).
nounsp1a(N) :- random_member(N, ['computer', 'macbook', 'laptop']).
verbsp1(V) :- random_member(V, ['is running', 'runs', 'operates', 'computes']).
nounsp1b(AD) :- random_member(AD, ['prolog', 'macOS Sierra', 'our prolog project']).

% Pattern 2
articlesp2(A) :- random_member(A, ['the', 'a', 'this', 'one']).
adjectiveOnep2(AD) :- random_member(AD, ['green', 'brown', 'large', 'small']).
nounsp2(N) :- random_member(N, ['frog', 'tree', 'bush', 'animal', 'plant']).
verbsp2(V) :- random_member(V, ['is']).
adverbsp2(ADV) :- random_member(ADV, ['natural', 'biological', 'living', 'alive']).

% Pattern 3
adjectiveOnep3(A) :- random_member(A, ['colourless', 'transparent', 'silent', 'difficult']).
adjectiveTwop3(AD) :- random_member(AD, ['green', 'metallic', 'red', 'blue']).
nounsp3(N) :- random_member(N, ['ideas', 'memories', 'dreams', 'cities']).
verbsp3(V) :- random_member(V, ['sleep', 'walk', 'jump', 'sing']).
adverbsp3(ADV) :- random_member(ADV, ['furiously', 'calmly', 'softly', 'strictly']).


% DCG Grammar rules

% Pattern 1
articlesp1 --> [Word], {articlesp1(Word)}.
nounsp1a --> [Word], {nounsp1a(Word)}.
verbsp1 --> [Word], {verbsp1(Word)}.
nounsp1b --> [Word], {nounsp1b(Word)}.

% Pattern 2
articlesp2 --> [Word], {articlesp2(Word)}.
adjectiveOnep2 --> [Word], {adjectiveOnep2(Word)}.
nounsp2 --> [Word], {nounsp2(Word)}.
verbsp2 --> [Word], {verbsp2(Word)}.
adverbsp2 --> [Word], {adverbsp2(Word)}.

% Pattern 3
adjectiveOnep3 --> [Word], {adjectiveOnep3(Word)}.
adjectiveTwop3 --> [Word], {adjectiveTwop3(Word)}.
nounsp3 --> [Word], {nounsp3(Word)}.
verbsp3 --> [Word], {verbsp3(Word)}.
adverbsp3 --> [Word], {adverbsp3(Word)}.


ssp_phrase --> articlesp1, nounsp1a, verbsp1, nounsp1b.
ss_phrase --> articlesp2, adjectiveOnep2, nounsp2, verbsp2, adverbsp2.
s_phrase --> adjectiveOnep3, adjectiveTwop3, nounsp3, verbsp3, adverbsp3.
nonsense_phrase --> adverbsp3, verbsp3, nounsp3, adjectiveTwop3, adjectiveOnep3.

pattern1 --> ssp_phrase.
pattern2 --> ss_phrase.
pattern3 --> s_phrase.
pattern4 --> nonsense_phrase.

generate_sentence(Dcg) :-
	foreach(member(Word, Dcg), (string_codes(Word, X))), writeln(X).

generate_s(PatLists) :-
	member(Pat1, PatLists), generateP(Pat1).

generateP(Pat) :-
	call(Pat, Line, []), print_line(Line).	

generate_line(Dcg) :-
	(foreach((Word, Dcg), write(Word), write(' '))).

print_line(Line) :-
	foreach(
	member(Word, Line),
	(write(Word), write(' '))).



start :- 
	write("\nWhat kind of sentence would you like to generate? Choose a number"),
	write("\n1. Syntax, Semantic, and Pragmatic"),
	write("\n2. Syntax and Semantic"),
	write("\n3. Syntax"),
	write("\n4. NONSENSE\n"),
	read(Number),
	query(Number),
	start.

query(Number) :-
	isOne(Number, 1);
	isTwo(Number, 2);
	isThree(Number, 3);
	isFour(Number, 4).


% put random output in this check here
isOne(A, B) :-
	A == B,
	write("\n"),
	generate_s([pattern1]),
	write("\n").
isTwo(A, B) :-
	A == B,
	write("\n"),
	generate_s([pattern2]),
	write("\n").
isThree(A, B) :-
	A == B,
	write("\n"),
	generate_s([pattern3]),
	write("\n").
isFour(A, B) :-
	A == B,
	write("\n"),
	generate_s([pattern4]),
	write("\n").



:- start.

/* print_line(Line) :-
	foreach(member(Word, Line), write(Word)),
	write(' ').	
*/


