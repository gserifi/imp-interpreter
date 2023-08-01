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
Aexp  = '(' Aexp Op Aexp ')'
      | Var
      | Numeral

Op = '+' | '-' | '*'
```

### Boolean Expressions

```
Bexp = '(' Bexp Bop Bexp ')'
     | '!' Bexp
     | Aexp Rop Aexp

Bop  = '&&' | '||'
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
