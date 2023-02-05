## TinyLISP

Build using Intellij Idea Scala plugin or sbt from the command line.

### API Usage

```scala
// construct parser
val result: List[Item] = rep(whitespace(item()))(code) match
  case Accept(value, rem) => // parser accepted
    if (rem.forall(_.isWhitespace)) { // all input was read
      val instructions = CompilationManager().compile(value)
      val runtime = Runtime(
        mutable.Stack.from(instructions), // code stack
      )
      runtime.run()
      runtime.getResult // return stack as a List
    }
    else throw IllegalArgumentException(s"Unexpected input at the end: '$rem'")
  case Reject(_) => throw IllegalArgumentException()
```

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
