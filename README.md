## TinyLISP

Build using Intellij Idea Scala plugin or sbt from the command line.

The project currently only supports translating to SECD instructions.

### Supported functions

Binary operations: `+, -, *, /, <, >, =, cons`.

Unary operations: `car, cdr`.

Built-ins: `define, let, lambda, if, cons dot (.)`.

```
(= (cons a b) (a . b))

(define (funName arg1 arg2 ...)
    body)

(let (var1 var2) (val1 val2)
    body)

(lambda (arg1 arg2 ...)
    body)

(if cond
    trueBranch
    falseBrach)
```
