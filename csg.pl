% Sentence Generator
% Owen & Joshua 
% CPSC 312 2019


% Pattern 1
articlesp1(A) :- random_member(A, ['my', 'this', 'our']).
nounsp1a(N) :- random_member(N, ['computer', 'macbook', 'laptop']).
verbsp1(V) :- random_member(V, ['is running', 'runs', 'operates', 'computes']).
nounsp1b(AD) :- random_member(AD, ['prolog', 'macOS Sierra', 'our prolog project']).

% Pattern 2
articlesp2(A) :- random_member(A, ['the', 'a', 'this', 'one']).
adjectiveOnep2(AD) :- random_member(AD, ['green', 'brown', 'large', 'small', 'tiny', 'huge']).
nounsp2(N) :- random_member(N, ['frog', 'tree', 'bush', 'animal', 'plant']).
verbsp2(V) :- random_member(V, ['is']).
adjectiveTwop2(ADV) :- random_member(ADV, ['natural', 'biological', 'living', 'alive', 'beautiful', 'cute', 'strange']).

% Pattern 3
adjectiveOnep3(A) :- random_member(A, ['colourless', 'transparent', 'silent', 'difficult', 'strange', 'saturated', 'limitless', 'orthoganal']).
adjectiveTwop3(AD) :- random_member(AD, ['green', 'metallic', 'red', 'blue', 'squishy', 'tough', 'cold', 'distant', 'near', 'unfamiliar']).
nounsp3(N) :- random_member(N, ['ideas', 'memories', 'dreams', 'cities', 'novels', 'audiences', 'trees', 'sunsets']).
verbsp3(V) :- random_member(V, ['sleep', 'walk', 'jump', 'sing', 'dance', 'scream', 'talk', 'pounce', 'cry', 'listen']).
adverbsp3(ADV) :- random_member(ADV, ['furiously', 'calmly', 'softly', 'strictly', 'slowly', 'intently', 'menacingly', 'respectfully']).


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
adjectiveTwop2 --> [Word], {adjectiveTwop2(Word)}.

% Pattern 3
adjectiveOnep3 --> [Word], {adjectiveOnep3(Word)}.
adjectiveTwop3 --> [Word], {adjectiveTwop3(Word)}.
nounsp3 --> [Word], {nounsp3(Word)}.
verbsp3 --> [Word], {verbsp3(Word)}.
adverbsp3 --> [Word], {adverbsp3(Word)}.


ssp_phrase --> articlesp1, nounsp1a, verbsp1, nounsp1b.
ss_phrase --> articlesp2, adjectiveOnep2, nounsp2, verbsp2, adjectiveTwop2.
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


% User Input
start :- 
	write("\nWhat kind of sentence would you like to generate? Choose a corresponding number below"),
	write("\n1. Syntax, Semantics, and Pragmatics"),
	write("\n2. Syntax and Semantics"),
	write("\n3. Syntax (Chomsky Sentence)"),
	write("\n4. NONSENSE\n"),
	read(Number),
	query(Number),
	start.

query(Number) :-
	isOne(Number, 1);
	isTwo(Number, 2);
	isThree(Number, 3);
	isFour(Number, 4).


% Handles user input
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


