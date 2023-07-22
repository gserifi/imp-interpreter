# IMP Language Specification

## Grammar

### Characters

```
Letter = 'A' | ... | 'Z' | 'a' | ... | 'z'
Digit  = '0' | '1' | ... | '9'
```

### Tokens

```
Ident   = Letter { Letter | Digit }*
Numeral = Digit | Numeral Digit
Var     = Ident
```

### Arithmetic Expressions

```
Aexp  = Aexp1
Aexp1 = Aexp2 { Op1 Aexp2 }
Aexp2 = Aexp3 { Op2 Aexp3 }
Aexp3 = '(' Aexp ')'
      | Var
      | Numeral

Op1 = '+' | '-'
Op2 = '*'
```

### Boolean Expressions

```
Bexp = Bexp1
Bexp1 = Bexp2 { 'or' Bexp2 }
Bexp2 = Bexp3 { 'and' Bexp3 }
Bexp3 = 'not' Bexp3
      | '(' Bexp ')'
      | Aexp Rop Aexp

Rop  = '=' | '#' | '<' | '<=' | '>' | '>='
```

### Statements

```
Stm = 'skip'
    | Var ':=' Aexp
    | 'if' Bexp 'then' Stm 'else' Stm 'end'
    | 'while' Bexp 'do' Stm 'end'
    | '(' StmList ')'

StmList = Stm { ';' Stm }
```
