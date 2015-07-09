:- encoding(utf8).

:- module(cedict, [
    cedict_read_file/2]).

:- use_module(library(dcg/basics), [
    nonblanks//1,
    string//1,
    string_without//2]).
:- use_module(library(pinyin), [
    num_dia/2]).

cedict_read_file(File, Dict) :-
  open(File, read, Stream),
  cedict_read(Stream, Dict),
  close(Stream).

cedict_read(Stream, Dict) :-
  read_line_to_codes(Stream, LineCodes),
  cedict_read(LineCodes, Stream, Dict).

cedict_read(end_of_file, _, []) :-
  !.
cedict_read(Codes, Stream, Dict) :-
  phrase(cedict_comment, Codes),
  !,
  cedict_read(Stream, Dict).
cedict_read(Codes, Stream, [Entry|Dict]) :-
  phrase(cedict_entry(Entry), Codes),
  !,
  cedict_read(Stream, Dict).
cedict_read(Codes, _, _) :-
  atom_codes(Atom, Codes),
  raise_exception(invalid_line(Atom)).

cedict_comment -->
  "#",
  string(_).

cedict_entry(json([traditional=Traditional, simplified=Simplified, pinyin=Pinyin, explanation=Explanation])) -->
  nonblanks(TraditionalCodes),
  { atom_codes(Traditional, TraditionalCodes) },
  " ",
  nonblanks(SimplifiedCodes),
  { atom_codes(Simplified, SimplifiedCodes) },
  " [",
  string_without("]", PinyinCodes0),
  { decode_ü(PinyinCodes0, PinyinCodes1),
    remove_fives(PinyinCodes1, PinyinCodes2),
    remove_spaces(PinyinCodes2, PinyinCodes3),
    num_dia(PinyinCodes3, PinyinCodes),
    atom_codes(Pinyin, PinyinCodes) },
  "] /",
  string(ExplanationCodes),
  { atom_codes(Explanation, ExplanationCodes) },
  % TODO Some explanations contain pinyin sections in square brackets, process
  % these as the pinyin above.
  "/".

decode_ü([], []) :-
  !.
decode_ü([117, 58|Rest0], [252|Rest]) :-
  !,
  decode_ü(Rest0, Rest).
decode_ü([First|Rest0], [First|Rest]) :-
  decode_ü(Rest0, Rest).

remove_fives(In, Out) :-
  exclude('=='(53), In, Out). % 53 = ASCII code of digit 5

remove_spaces([], []) :-
  !.
remove_spaces([32, Next|Rest0], [Next|Rest]) :-
  unicode_property(Next, category('Ll')), % lower-case letter
  !,
  remove_spaces(Rest0, Rest).
remove_spaces([First|Rest0], [First|Rest]) :-
  remove_spaces(Rest0, Rest).
