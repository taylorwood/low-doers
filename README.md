# low-doers

Clojure reproduction of the agent-based model simulation from Proietti & Franco (2018) *Social Norms and the Dominance of Low-Doers*[^1],
which itself formalizes Gambetta & Origgi's "L-worlds."[^2]

Reproduced using the original NetLogo source (CoMSES 5120, AFL-3.0)[^3].

## Run

Requires a JDK and Clojure.

```sh
clojure -M:test     # run the test suite
clojure             # REPL; see the (comment ...) blocks in each namespace
```

## Figures

Reproductions of figures from the paper live in `notebooks/figures.clj`, rendered as a [Clerk](https://github.com/nextjournal/clerk) notebook.

Run simulation to generate `data.edn` first:
```sh
clojure -M:compute
```

Start the notebook:
```sh
clojure -M:clerk
```

[^1]: Proietti, C. & Franco, A. (2018). Social Norms and the Dominance of Low-Doers.
[^2]: Gambetta, D. & Origgi, G. (2013). The L-game: honesty and relational capital.
[^3]: NetLogo source model. <https://www.comses.net/codebases/5120/>
