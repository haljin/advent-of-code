This repo contains solutions for the advent of code (http://adventofcode.com) written in Erlang.

## Solutions
Code files contain solutions for the problems in various days. If you would like to know more about a particular solution, the more complicated ones have short explanation in the module comments.

## Running the examples
To run the examples simply use rebar to build it. Do not forget to fetch dependencies. One of the examples (day12) utilised the [jsx](https://github.com/talentdeficit/jsx) library to manipulate JSONs.

```
rebar get-deps
rebar compile
```

Some examples use excessive amounts of processes, just because it is more fun that way. Erlang VM has a limit on the max process count, that defaults to around 200000. You may want to start your Erlang VM with the +P flag:
```
erl +P 2000000
```

About 2 million process limit should do it, although it never really goes much about one million. :) 

## Lessons learned
* Day 6 - The VM has a limit on how many processes can be spawned. This is configurable, but must be remembered.
* Day 6 - Large ETS tables can be quite slow when performing selects. This may imply gproc may struggle when there's a LOT of processes running, although normally you probably wouldn't store ALL PIDs in one place.
* Day 20 - Every math problem can be brute-forced with enough processes!
* Day 20 - The catch expression seems to work in a strange way and not very well when there's a LOT of processes. Perhaps more investigation is required.
* Many days - Erlang and string operations to not add up nicely. But we all knew that. Same applies to large arrays.
