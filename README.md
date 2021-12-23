# library thingy

This finds the latest versions of the libraries I work on and their dependency relationships, if any, and renders the results as a `dot` file. To change this list, edit `main.scala`.

- To run it, do `scala-cli run . | dot -Tsvg` for instance.
- To make an executable do `scala-cli package -o thingy .` and then `./thingy`.

