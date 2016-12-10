#!/usr/bin/apl --script --
rotate ← ~(⊂'--initial')∊⎕ARG
in ← ⍎(⎕FIO[26] ↑⌽⎕ARG)         ⍝ super dangerous but convenient
in ← (⍉⍣rotate⍤2) in ⍴⍨ (9÷⍨⍴in),3 3
⍞ ← +/ (,2×⌈/ in) < (,+/⍤1 in)
)OFF
