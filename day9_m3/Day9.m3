(* -*- modula-3 -*- *)

MODULE Day9 EXPORTS Main;
IMPORT Fmt, Lex, Params, Rd, Stdio, Text, Wr;

TYPE
  Repetition = RECORD
    length: INTEGER;
    count: INTEGER;
  END;

EXCEPTION Bad_Match;


PROCEDURE Expect_Char(in: Rd.T; ch: CHAR) RAISES {Bad_Match, Rd.EndOfFile} =
  BEGIN
    IF Rd.GetChar(in) # ch THEN Rd.UnGetChar(in); RAISE Bad_Match END
  END Expect_Char;
  

PROCEDURE Expect_Int(in: Rd.T): INTEGER RAISES {Bad_Match} =
  BEGIN
    TRY RETURN Lex.Int(in) EXCEPT | Lex.Error => RAISE Bad_Match END;
  END Expect_Int;


PROCEDURE Expect_Repetition(in: Rd.T): Repetition RAISES {Bad_Match, Rd.EndOfFile} =
  VAR rep: Repetition;
  BEGIN
    Expect_Char(in, '(');
    rep.length := Expect_Int(in);
    Expect_Char(in, 'x');
    rep.count := Expect_Int(in);
    Expect_Char(in, ')');
    RETURN rep
  END Expect_Repetition;


PROCEDURE Consume_Nonblank(in: Rd.T; VAR count: INTEGER) RAISES {Rd.EndOfFile} =
  VAR ch: CHAR;
  BEGIN
    ch := Rd.GetChar(in); IF NOT ch IN Lex.Blanks THEN INC(count) END
  END Consume_Nonblank;


PROCEDURE Simple_Count(in: Rd.T): INTEGER =
  VAR count := 0;
      rep : Repetition;
  BEGIN
    LOOP TRY
      TRY
        rep := Expect_Repetition(in);
        INC(count, rep.length*rep.count);
        EVAL Rd.GetText(in, rep.length)    (* wasteful *)
      EXCEPT
      | Bad_Match => Consume_Nonblank(in, count)
      END
    EXCEPT
    | Rd.EndOfFile => RETURN count;
    END END;
  END Simple_Count;

PROCEDURE Recursive_Count(in: Rd.T; limit: REF INTEGER): INTEGER =
  VAR count := 0;
      initial: INTEGER;
      rep: Repetition;
      p: REF INTEGER;
  BEGIN
    initial := Rd.Index(in);
    REPEAT TRY
      TRY
        rep := Expect_Repetition(in);
        p := NEW(REF INTEGER); p^ := rep.length;
        INC(count, rep.count * Recursive_Count(in, p));
      EXCEPT
      | Bad_Match => Consume_Nonblank(in, count)
      END
    EXCEPT
    | Rd.EndOfFile => RETURN count;
    END
    UNTIL limit # NIL AND Rd.Index(in)-initial >= limit^;
    <* ASSERT(Rd.Index(in)-initial = limit^) *>
    RETURN count
  END Recursive_Count;

VAR is_simple: BOOLEAN;
    count := 0;
BEGIN
  is_simple := Params.Count > 1 AND Text.Equal("--initial", Params.Get(1));
  WITH in = Stdio.stdin, out = Stdio.stdout DO
    IF is_simple THEN
      count := Simple_Count(in)
    ELSE
      count := Recursive_Count(in, NIL)
    END;
    Wr.PutText(out, Fmt.Int(count) & "\n")
  END
END Day9.
