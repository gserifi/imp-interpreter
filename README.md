# IMP Interpreter

IMP Language Interpreter written in Haskell

## Installation

Clone this repository and run `make`. _(Optional)_ If you want to run the binary from anywhere on your system, you can run `sudo make install`. That should work on Linux/Mac, if you're on Windows you have to do it manually. You can always uninstall by running `sudo make uninstall`

## Usage

_In the following I assume that you ran `make install`, otherwise instead of `imp` you have to use `./imp`._

### Run IMP program

```bash
imp program.imp
```

## Examples

Some examples of valid IMP programs, this should also show what features of the language are already implemented. For the syntax check out [LANGUAGE.md](./LANGUAGE.md).

```
x := 5;
y := 1;
while x > 1 do
  y := y * x;
  x := x - 1
end
```

Output:

```
x = 1
y = 120
```
