# Vormbaar
Custom VM project. Should evolve into a full programming environment some day.

# DO NOT USE ANYWHERE NEAR PRODUCTION (yet)

## The Idea
After watching [this Talk](https://www.youtube.com/watch?v=8Ab3ArE8W3s),
I started looking through some of the languages mentioned in the talk and realized,
that I do not like the syntax of those languages. I wanted something more "traditional" to play around
with the ideas in the talk. Also, some of the languages have really "heavy" runtimes you have to use.

So I decided to start building my own dynamic programing environment. I want it to have some of the nice things
that smalltalk and lisp have, with their image based approach, but with enough flexibility to do further research
on such topics.

I also thought about syntax quite a bit and decided to try a completely different approach. I want to merge
the REPL/image-based approach from lisp, with [structural/projectional editing](https://en.wikipedia.org/wiki/Structure_editor).

For that I want to define the basic structures of a program just in code and build a UI
to edit this structure directly.

It's also planned to build that UI dynamically based on some sort of templating or ui language, tailored to building
projectional editors.

## What works right now?
Currently I have the necessary data structures to define functions, variables, constants, signed integers
and a few binary operators.

I have enough functionality to implement a recursive version of factorial, which can be seen in serialized
format [here](./factorial.ron).

To try it out, you can do this:
```bash
cargo run -- run factorial.ron "init=5&n=10"
```

Which should print out the following:
```
Scalar(I32(120))
```

## Usage
```
Usage: vorm <COMMAND>

Commands:
  create   Create an example ron file
  run      Run a ron or vmstate file
  debug    Deserialize the vm and print the whole vm on stdout
  compile  Compile a ron to a vmstate file
  pretty   Prettify a ron file
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

## How fast is it?

Pretty fast actually. The first benchmark (done with commit 4a87d9fdc3ae6f5cd310460c453af09eb13e7178) shows,
we are already in a similar ballpark as python, for a simple benchmark.

I'll probably not benchmark too much now, as there are more important features right now. If someone knows how to
improve performance without restricting whats possible too much, feel free to submit a PR.

The benchmark result can be found at [bench/README.md](./bench/README.md).

The python code that I used for the benchmark can be found at [bench/py/fac.py](./bench/py/fac.py).

Here is also a small flamegraph:
![flamegraph](./flamegraph.png)

## Whats up with the name?
`Vormbaar` means moldable in dutch, which captures the spirit of the project.
