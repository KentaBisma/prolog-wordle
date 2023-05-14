:- dynamic exist_letter/1, green/1, yellow/1, word/1, answer/1.
:- dynamic session/2.

wordle:-
    \+ has_session,
    open('words.txt',read,Str),
    read_line(Str,L_answer),
    close(Str),
    random_member(Answer,L_answer),

    open('potential-words.txt',read,Str1),
    read_line(Str1,L_word),
    close(Str1),

    teardown,
    gen_words(L_word),
    gen_answers(L_answer),
    gen_existing_letters(Answer),

    gen_hint_knowledge,

    assertz(session(Answer, 4)), !.

wordle:-
    quit, wordle.

read_line(Stream,[]):-
    at_end_of_stream(Stream), !.

read_line(Stream,[X|L]):-
    read(Stream,X),
    read_line(Stream,L).

has_session:-
    has_session(_,_).

has_session(Answer, Tries):-
    bagof((X, Y), session(X,Y), [(Answer, Tries)|[]]).

update_session(Answer, Tries):-
    retract(session(_,_)),
    assertz(session(Answer, Tries)).

quit:-
    has_session,
    teardown,
    retract(session(_,_)).

guess(Guess, GuessResult):-
    has_session(Answer, Tries),
    check_exist(word(Guess)),
    atom_chars(Answer,L_answer),
    atom_chars(Guess,L_guess),
    check_word(L_guess, L_answer, [], Status),
    check_yellows(L_guess, Status, [], FinalStatus),
    update_hint_knowledge(L_guess, FinalStatus),
    check_status(Answer, Tries, FinalStatus, GuessResult),
    !.

guess(_, message(nonexist)).

gen_words([]).

gen_words([H|Tail]):-
    assertz(word(H)),
    gen_words(Tail).

gen_answers([]).

gen_answers([H|Tail]):-
    assertz(answer(H)),
    gen_answers(Tail).

gen_existing_letters(Answer):-
    atom_chars(Answer,L_answer),
    gen_existing_letters_(L_answer).

gen_existing_letters_([]):- !.

gen_existing_letters_([H|Tail]):-
    assertz(exist_letter(H)),
    gen_existing_letters_(Tail).

teardown:-
    teardown_hint_knowledge,
    retractall(word(_)),
    retractall(answer(_)),
    retractall(exist_letter(_)),
    retractall(green(_)),
    retractall(yellow(_)).    

check_status(Answer, _, [green,green,green,green,green], message(win, Answer)):-
    quit, !.

check_status(Answer, 0, _, message(lose, Answer)):-
    quit, !.

check_status(Answer, Tries, GuessResult, GuessResult):-
    retractall(green(_)),
    retractall(yellow(_)),
    Tries_ is Tries-1,
    update_session(Answer, Tries_).

check_word([], [], Status, Status).

check_word([G|Gs],[W|Ws], Acc, Status):-
    G = W,
    append(Acc,[green],NewAcc),
    assertz(green(G)),
    check_word(Gs,Ws,NewAcc,Status), !.

check_word([G|Gs],[W|Ws],Acc,Status):-
    G \= W,
    \+ check_exist(exist_letter(G)),
    append(Acc,[gray],NewAcc),
    check_word(Gs,Ws,NewAcc,Status), !.

check_word([_|Gs],[_|Ws],Acc,Status):-
    append(Acc,[noop],NewAcc),
    check_word(Gs,Ws,NewAcc,Status).

check_yellows([], [], Status, Status).

check_yellows([_|Gs], [S|Ss], Acc, FinalStatus):-
    S = green,
    append(Acc,[green],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([_|Gs], [S|Ss], Acc, FinalStatus):-
    S = gray,
    append(Acc,[gray],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([G|Gs], [S|Ss], Acc, FinalStatus):-
    S = noop,
    check_guessed(G),
    append(Acc,[yellow],NewAcc),
    assertz(yellow(G)),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([_|Gs], [_|Ss], Acc, FinalStatus):-
    append(Acc,[gray_],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_exist(Term):-
    bagof(_, Term, _).

check_guessed(Letter):-
    findall(_, green(Letter), GreenBag),
    length(GreenBag, GreenLength),
    findall(_, yellow(Letter), YellowBag),
    length(YellowBag, YellowLength),
    findall(_, exist_letter(Letter), ExistBag),
    length(ExistBag, ExistLength),
    PartiallyGuessed is YellowLength + GreenLength,
    ExistLength > PartiallyGuessed.

:- dynamic probable_solution/1, probable_valid_word/1.

teardown_hint_knowledge:-
    retractall(probable_valid_word(_)),
    retractall(probable_solution(_)).

gen_hint_knowledge:-
    findall(X, word(X), Xs),
    gen_hint_valid_words(Xs), !,
    findall(Y, answer(Y), Ys),
    gen_hint_solutions(Ys), !.

gen_hint_valid_words([]):- !.

gen_hint_valid_words([X|Xs]):-
    assertz(probable_valid_word(X)),
    gen_hint_valid_words(Xs).

gen_hint_solutions([]):- !.

gen_hint_solutions([X|Xs]):-
    assertz(probable_solution(X)),
    gen_hint_solutions(Xs).

update_hint_knowledge([], []):- !.

update_hint_knowledge([G1, G2, G3, G4, G5|[]], [S1, S2, S3, S4, S5|[]]):-
    process_hint_info(G1,S1,1),
    process_hint_info(G2,S2,2),
    process_hint_info(G3,S3,3),
    process_hint_info(G4,S4,4),
    process_hint_info(G5,S5,5).

process_hint_info(_, S, _):-
    S = gray_, !.

process_hint_info(G, S, _):-
    S = yellow,
    findall_set(X, subgoal_word_with_no_letter(X, G), L),
    retract_improbable(L), !.

process_hint_info(G, S, Idx):-
    S = green,
    findall_set(X, subgoal_word_with_no_letter_at(X, G, Idx), L),
    retract_improbable(L), !.

process_hint_info(G, S, _):-
    S = gray,
    findall_set(X, subgoal_word_with_letter(X, G), L),
    retract_improbable(L), !.

subgoal_word_with_no_letter_at(Word, ToLookFor, Idx):-
    probable_valid_word(Word),
    Idx_ is Idx - 1,
    \+ sub_atom(Word, Idx_, 1, _, ToLookFor).

subgoal_word_with_letter(Word, ToLookFor):-
    probable_valid_word(Word),
    sub_atom(Word, _, _, _, ToLookFor).

subgoal_word_with_no_letter(Word, ToLookFor):-
    probable_valid_word(Word),
    \+ sub_atom(Word, _, _, _, ToLookFor).

get_all_probable_solutions(L):-
    findall(X, probable_solution(X), L).

get_all_probable_valid_words(L):-
    findall(X, probable_valid_word(X), L).
    

findall_set(Template, Goal, Set) :-
    findall(Template, Goal, List),
    sort(List, Set).

apply_template([], _, Acc, Acc).
apply_template([Term|Terms], Template, Acc, Res) :-
    Result =.. [Template, Term],
    append(Acc, [Result], NewAcc),
    apply_template(Terms, Template, NewAcc, Res).

retract_improbable([]).
retract_improbable([Fact|Rest]) :-
    retractall(probable_valid_word(Fact)),
    retractall(probable_solution(Fact)),
    retract_improbable(Rest).