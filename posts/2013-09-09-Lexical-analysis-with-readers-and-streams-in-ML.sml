
signature LEXER =
sig
   datatype token = LParen | RParen | Atom of string | Dot
   val tokenize : (char, 'a) StringCvt.reader -> (token, 'a) StringCvt.reader
end
structure Lexer : LEXER =
struct
   datatype token = LParen | RParen | Atom of string | Dot
   (*
    * Given a char reader and stream, try to extract a Scheme atom
    * (string) from the stream, and return it with the rest of the stream
    *)
   fun getAtom rdr s =
       let
          fun return [] _ = NONE
            | return acc s = SOME (String.implode (rev acc), s)
   
          fun getAtom' acc s =
              case rdr s of
                   NONE => return acc s
                 | SOME (#"(", rest) => return acc s
                 | SOME (#")", rest) => return acc s
                 | SOME (x, rest) => if Char.isSpace x then
                                        return acc s
                                     else getAtom' (x :: acc) rest
       in
          getAtom' [] s
       end
   (*
    * Given a char reader, produce a token reader
    *)
   fun tokenize rdr =
       fn s =>
          case rdr (StringCvt.skipWS rdr s) of
              NONE => NONE
            | SOME (#".", s') => SOME (Dot, s')
            | SOME (#"(", s') => SOME (LParen, s')
            | SOME (#")", s') => SOME (RParen, s')
            | SOME (_, s') =>
              case getAtom rdr (StringCvt.skipWS rdr s) of
                  NONE => NONE
                | SOME (atom, s') => SOME (Atom atom, s')
end

structure Reader =
struct
   val list : ('a, 'a list) StringCvt.reader =
       fn [] => NONE
     | (x::xs) => SOME (x, xs)
   local
      open String
   in
      val string : (char, string) StringCvt.reader =
       fn "" => NONE
        | s => SOME (sub (s, 0), substring (s, 1, size s - 1))
   end
   local
      open Substring
   in
      val substring : (char, substring) StringCvt.reader = getc
   end
   
   val streamIO : (char, TextIO.StreamIO.instream) StringCvt.reader =
       TextIO.StreamIO.input1
   (*
    * Given a reader and a stream, consume the entire stream and return a
    * list of the resulting elements
    *)
   fun consume (rdr : ('a, 'b) StringCvt.reader) s =
       let
          fun consume' acc s =
              case rdr s of
                  NONE => rev acc
                | SOME (x, s') => consume' (x::acc) s'
       in
          consume' [] s
       end
end

local
   open String
   open Lexer
   open Reader
in
   val [LParen, Atom "foo", RParen] = consume (tokenize string) "(foo)"
   val [LParen, Atom "foo", Atom "bar", RParen] = consume (tokenize string) "(foo bar)"
   val [LParen, Atom "foo", Dot, Atom "bar", RParen] = consume (tokenize string) "(foo . bar)"
end
