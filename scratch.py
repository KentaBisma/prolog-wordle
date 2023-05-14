from pyswip import Prolog

prolog = Prolog()
prolog.consult("wordle-sessioned.pl")
[*prolog.query('wordle')]
print(*prolog.query('has_session(A, _)'))
print(*prolog.query('guess(crave, R)'))
print(*prolog.query('get_all_probable_valid_words(L)'))
print(*prolog.query('guess(stump, R)'))
print(*prolog.query('get_all_probable_valid_words(L)'))
print(*prolog.query('guess(blink, R)'))
print(*prolog.query('get_all_probable_valid_words(L)'))
