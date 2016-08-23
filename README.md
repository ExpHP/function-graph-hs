# function-graph
Graph algorithms without a graph data structure.

---

This factors out a pattern that I use all of the time in Python, Rust, Haskell,
and other languages with first-class functions, which is to solve a problem
by reducing it to a graph problem, and then to solve that by writing two things:

* An edge function; some `v -> [v]`
* A generalized version of the graph algorithm, which takes the edge function as its first argument,
  and is usually written to be lazy (if possible) to support infinite graphs.

But since I've used this pattern in a lot of separate projects,
I have an awful lot of copies of some of those "generalized graph algorithms",
and to be quite frank I'm tired of writing them and searching for them!

This is experimental, and the current pickings are extremely slim.
The API will likely get an overhaul a fair several dozen times as I
add more to it and continue to use it in other work.
(That or it will die a slow, horrible death from criminal negligence)

### Installation

    git clone yadda yadda
    cabal sandbox add-source yadda yadda

Nothing on hackage yet; both because the thought of maintaining a project seems to spark in me some incomprehensible, *primal* sort of fear, and also because I'm not entirely wed to the name yet. `:V`

### Examples
You can find a terribly contrived example of possible usage
[in this stackoverflow post](http://stackoverflow.com/questions/39059037)
wherein I present the world's worst "edit distance" algorithm
based on the world's most naive "shortest path length" algorithm.

Real examples will have to wait until I adapt real code.
