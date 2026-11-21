# Assignment 8, CS3250

This repository contains my implementation of CatML!

## How to interact with this program

To compile and run the program, run `dune exec bin/main.exe <filepath>`, where the filepath is a `.catml` file. For
information about divergences from the spec, see step 3 of the reflection questions.

The `examples/` directory contains several `.catml` files that can be run.

```sh
$ dune exec bin/main.exe examples/factorial.catml
Evaluation result:
120
```

# Reflection Questions

## If you worked as a group, list all the group members.

I worked independently.

## Did you use generative AI to assist you in implementing this project? If so, briefly explain how you used it and for which components.

Not purposefully - I may have accidentally glanced at the AI Overview while googling questions, but generally I try not
to use LLMs.

## Did you make any design decisions that we should know about?

I made several slight variations from the spec in terms of language syntax. For demos of these changes, see the
examples. These include:

1. The syntax for functions is changed slightly, to use `=` instead of `->`. This disambiguates the syntax from that
   used for type annotations.
2. `Fix` is extended to support defining a recursive definition of any value, rather than only recursive functions.
   `examples/first.catml` demoes how this can be used to create a recursive pair. `Fix` should be called on a function
   that expects itself, but it can still be used to create simple recursive functions (see `examples/factorial.ml`).

In terms of implementation:
1. I rewrote the typecheck function to support basic error messages. It now returns a `Result`, where the error case
   represents the expression that went wrong.
2. I took care to ensure that niche parser cases are handled properly. `a -> b -> c` is parsed right-associatively as
`a -> (b -> c)`, and operator precedence is handled such that the parsed result is never ambiguous.
3. I implemented a small testing module, with a helper function to print a message when a test fails. I would have
   written more tests, but I ended up transitioning to using the `examples/` directory for writing programs.

## Did you implement any of the extra credit features? Briefly summarize your work.

Yes - see above. I'd say I added:
- A test suite (albeit a basic one, so maybe only a few points)
- Useful error messages for type errors
- A new language feature by extending fixpoints to work for any expression

## Any feedback on the structure of this assignment?

I think this was a really fun opportunity to build up a real program over time, and I wish more assignments like this
were given. However, I think a few things could've made it more fun:

1. I wish it had been slightly more built up as a singular project. Throughout the homeworks, we ended up writing
   implementations of each of the components, but then we just got starter code for the whole thing, so it felt like our
   individual implementations didn't "count". Instead, it would've been fun if we'd worked in a single Git repo the
   entire time, slowly building up a project. Starter code could still be provided, but it'd be downloaded separately by
   anyone who missed an assignment.
2. Rather than doing new material this week, it would've been nice to have some work-time in class. I recognize that
   we're probably on a tight schedule, but due dates right before Thanksgiving are always hard, and I think project time
   in class is always really helpful for getting oriented. I ended up starting some of this later than I wished, and if
   I had some days in class, I would've had a better sense of how much needed to be done.
3. Having some more links on the intricacies of Menhir parsing would be nice, since it's easy to get stuck with an
   unclear error. Personally, I found these links helpful:
   - https://twolodzko.github.io/posts/ocaml-parser.html
   - https://discuss.ocaml.org/t/juxtaposition-in-menhir/9782
   - https://discuss.ocaml.org/t/how-to-resolve-parser-conflicts/9314
   - https://cs.stackexchange.com/questions/145443/how-to-write-a-non-ambiguous-grammar-for-the-syntax-of-lambda-calculus
   - https://ocaml.org/docs/cli-arguments
   - https://batsov.com/articles/2022/11/27/reading-files-in-ocaml/

Thanks so much :​)
