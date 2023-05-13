:- dynamic exist_letter/1, green/1, yellow/1, word/1.
:- dynamic session/2.

wordle:-
    \+ has_session,
    open('words.txt',read,Str),
    read_line(Str,L_word),
    close(Str),
    random_member(Answer,L_word),

    teardown,
    gen_words(L_word),
    gen_existing_letters(Answer),

    assertz(session(Answer, 5)), !.

wordle:-
    quit.

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
    check_status(Answer, Tries, FinalStatus, GuessResult),
    !.

guess(_, message(nonexist)).

gen_words([]).

gen_words([H|Tail]):-
    assertz(word(H)),
    gen_words(Tail).

gen_existing_letters(Answer):-
    atom_chars(Answer,L_answer),
    gen_existing_letters_(L_answer).

gen_existing_letters_([]):- !.

gen_existing_letters_([H|Tail]):-
    assertz(exist_letter(H)),
    gen_existing_letters_(Tail).

teardown:-
    retractall(word(_)),
    retractall(exist_letter(_)),
    retractall(green(_)),
    retractall(yellow(_)).    

check_status(_, _, [green,green,green,green,green], message(win)):-
    quit, !.

check_status(_, 0, _, message(lose)):-
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
    append(Acc,[gray],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_exist(Term):-
    findall(_, Term, Bag),
    length(Bag, Length),
    Length > 0.

check_guessed(Letter):-
    findall(_, green(Letter), GreenBag),
    length(GreenBag, GreenLength),
    findall(_, yellow(Letter), YellowBag),
    length(YellowBag, YellowLength),
    findall(_, exist_letter(Letter), ExistBag),
    length(ExistBag, ExistLength),
    PartiallyGuessed is YellowLength + GreenLength,
    ExistLength > PartiallyGuessed.