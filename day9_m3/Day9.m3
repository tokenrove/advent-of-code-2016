(* -*- modula-3 -*- *)

MODULE Day9 EXPORTS Main;
IMPORT Fmt, Lex, Params, Rd, Stdio, Text, Wr;

TYPE Repetition = RECORD length: INTEGER; count: INTEGER; END;

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

PROCEDURE Count(in: Rd.T; limit: REF INTEGER := NIL): INTEGER =
  VAR count := 0;
      initial: INTEGER;
      rep: Repetition;
  BEGIN
    initial := Rd.Index(in);
    REPEAT TRY
      TRY
        rep := Expect_Repetition(in);
        IF is_simple THEN
          INC(count, rep.length*rep.count);
          EVAL Rd.GetText(in, rep.length)    (* wasteful *)
        ELSE
          VAR p: REF INTEGER := NEW(REF INTEGER);
          BEGIN
            p^ := rep.length;
            INC(count, rep.count * Count(in, p));
          END
        END
      EXCEPT
      | Bad_Match =>
        IF NOT Rd.GetChar(in) IN Lex.Blanks THEN INC(count) END
      END
    EXCEPT
    | Rd.EndOfFile => RETURN count
    END
    UNTIL limit # NIL AND Rd.Index(in)-initial >= limit^;
    <* ASSERT(Rd.Index(in)-initial = limit^) *>
    RETURN count
  END Count;

VAR is_simple: BOOLEAN;
BEGIN
  is_simple := Params.Count > 1 AND Text.Equal("--initial", Params.Get(1));
  WITH in = Stdio.stdin, out = Stdio.stdout DO
    Wr.PutText(out, Fmt.Int(Count(in)) & "\n")
  END
END Day9.
