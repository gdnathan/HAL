# 🟣 FUN - HAL

![Stack CI workflow](https://github.com/EpitechPromo2024/B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard/actions/workflows/main.yml/badge.svg)

# Description 📖

This is a 3rd year EPITECH project which is an interpreter for a minimalist dialect of [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)) in Haskell.

The program can take files to be interpreted by arguments but can also interpret by using its [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

***

# Usage ⚙️

```
./hal [files | flags] ...
```

| Argument | Description                                            |
| -------- | ------------------------------------------------------ |
| `None`   | Launches the REPL.                                     |
| `file`   | A file that will be interpreted, several can be given. |

| Short flag | Long flag | Description                                  |
| ---------- | --------- | -------------------------------------------- |
| `-h`       | `--help`  | Displays the program's usage.                |
| `-i`       |           | Launches the REPL alongside other arguments. |

***

# How to build the project ? 🔥

This project has been setup with [Stack](https://docs.haskellstack.org/en/stable/README/) and works with a Makefile that wraps this framework with different rules.

| Command          | Result                                         |
| ---------------- | ---------------------------------------------- |
| `make`           | Builds a `hal` executable.                     |
| `make tests_run` | Runs tests.                                    |
| `make clean`     | Cleans build dumps, keeping the executable.    |
| `make fclean`    | Removes all of the files created by the build. |
| `make re`        | Respectively calls `make fclean` and `make`.   |

***

# Documentation 📚

***

# Builtins 🏠

## cons

* Takes 2 arguments and creates a new list cell from them.

The first one is called the <code>car</code> and the second one is called the <code>cdr</code>.

## car

* Takes 1 argument, a <code>cons</code>.

Returns the first argument of the <code>cons</code>.

## cdr

* Takes 1 argument, a <code>cons</code>.

Returns the second argument of the <code>cons</code>.

## eq?

* Takes 2 arguments.

Returns "#t" if the two arguments are equal.
Otherwise, returns "#f".

#### Notes

* Lists are only equal if they are empty.
* Symbols are only equal to themselves.
* Integers behave by the usual value comparison.

## atom?

Returns "#t" if its argument is atomic.
Otherwise, returns "#f".

#### Notes

* An atomic is anything besides an empty list.

***

# Arithmetic builtins 👨‍🔬

## +

* Takes a variable amount of arguments.

Returns the sum of the given arguments.

## -

* Takes a variable amount of arguments.

Returns the difference of the given arguments.

## *

* Takes a variable amount of arguments.

Returns the product of the given arguments.

## div

* Takes 2 arguments.

Returns the quotient of the first argument by the second.

#### Notes

* Division by zero will result in an error.

## mod

* Takes 2 arguments.

Returns the modulo of the first argument by the second.

#### Notes

* Modulo by zero will result in an error.

## <

* Takes 2 arguments.

Returns "#t" if the first argument is smaller than the second argument.
Otherwise, returns "#f".

***

# Special forms 🙃

## quote

* Takes 1 argument.

Returns the argument without evaluating it.

#### Notes

* As syntactic sugar, using <code>'</code> works similarly.

## lambda

* Takes a list of arguments as first argument, and an expression to evaluate as second argument.

Returns a lambda procedure that can be subsequently called.

#### Notes

* The first argument cannot contain arguments with the same names.

## define

* Takes a variable amount of arguments.

If its first argument is a symbol, associates the symbol to its second argument, and returns its name.
If its first argument is a list, defines a function which name is the first elemnt of the list, the rest of the list
its parameters, and the second argument the function’s body.

#### Notes

* A new function name cannot have a keyword's name.

## let

* Takes a list of key/values as first argument, and an expression as a second argument, evaluates the second argument within an environement where the key / value pairs are bound.

Returns the result of the expression.

## cond

* It takes a variable number of arguments.

Allows to conditionally evaluate expressions.
Each argument is a list.
<code>cond</code> successively evaluates the first element of each list. If its return value is "#t", it evaluates the second element of the list and returns it's value. Otherwise, it tries the next expression.

***

# REPL 🔵

The program can receive files in entry and give a result, but it can also interpret instructions via a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (such as [Python](https://www.python.org/)).

It reads an expression from the standard input, evaluates it, potentially prints a result and loops.

***

## Thanks for reading about the project ! 😊
