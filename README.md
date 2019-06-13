# Bachelor's thesis supplementary binary

Thesis title: Parsing algorithms for context-free languages

## How to run

```sh
cargo install
```

Running

```sh
thesis_bin -g <your/grammar/file>
```

will create a REPL for the given grammar file.
Words can be entered and must be seperated by whitespace.

```
+-----------+-----------------------------------------------------+
| Parameter | Description                                         |
+===========+=====================================================+
| -p        | Selects the parsing algorithm to use                |
|           | Possible options: 'earley', 'harrison', 'll1', 'lr1'|
|           | Will default to 'earley' if not specified otherwise |
+-----------+-----------------------------------------------------+
| -g        | Selects the grammar file to use                     |
+-----------+-----------------------------------------------------+
| -i        | Input word to use                                   |
|           | Default to REPL if not specified                    |
+-----------+-----------------------------------------------------+
| -o        | Path to output SPPF Dot file                        |
|           | Only works for the option 'earley'                  |
+-----------+-----------------------------------------------------+
| -f        | Outputs the FIRST and FOLLOW sets                   |
+-----------+-----------------------------------------------------+
```

## Licence

Copyright (c) Alexander Koch 2019