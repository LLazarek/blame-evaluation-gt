# Gradual Typing
## Literature Review
### Background
- [Migratory Typing: Ten Years Later](https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf)
- [An Analysis and Survey of the Development of Mutation Testing](https://citeseerx.ist.psu.edu/document?doi=d7c38286734419b52de4262c9802ebdfcf4b9447)
- [How to Evaluate Blame for Gradual Types](https://llazarek.github.io/doc/icfp-2021-corrected.pdf)
- [Typed-Untyped Interactions: A Comparative Analysis](https://cs.brown.edu/people/bgreenma/publications/apples-to-apples/gdf-toplas-2023.pdf)
- Syntax, Semantics, Implementation—and Pragmatics
- [How to Evaluate Blame for Gradual Types, Part 2

### General Test Minimization
- [An Evaluation of Test Suite Minimization Techniques](https://www.cqse.eu/fileadmin/content/news/publications/2020-test-suite-minimization-swqd.pdf)
  - Adequate vs. Inadequate
  - Greedy
  - HGS
- [Regression testing minimization, selection and prioritization: a survey](https://coinse.github.io/publications/pdfs/Yoo2010fk.pdf)
  - GE
  - GRE
- [An Empirical Study of JUnit Test-Suite Reduction](https://personal.utdallas.edu/~lxz144130/publications/issre2011.pdf)
  - Greedy
  - HGS
  - GRE
  - ILP
- How Profilers Can Help Navigate Type Migration

### Mutation-Based Test Minimization
- [Procedures for Reducing the Size of Coverage-based Test Sets](https://cs.gmu.edu/~offutt/rsrch/papers/regress.pdf)
  - The ping-pong procedure is described, and MAR is compared with SAR.
- [An Empirical Study of Greedy Test Suite Minimization Techniques Using Mutation Coverage](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=10160008)
  - In the methodology section (3B), the mutation matrix and its uses are described.
- [Reduction of Test Suites Using Mutation](https://link.springer.com/content/pdf/10.1007/978-3-642-28872-2_29.pdf)
  - Section 3.1 describes the killing matrix.
- [Balancing Trade-Offs in Test-Suite Reduction](https://users.ece.utexas.edu/~gligoric/papers/ShiETAL14EvolRed.pdf)
  - Section 2.2 describes the *Statement Adequate Reduction* (SAR) technique. It also mentions how the Greedy algorithm is the most widely used, and it lists the other commonly used algorithms:
    - GE
    - GRE
    - HGS
    - ILP
  - Section 4.1 describes the use of a kill\[ing] matrix to compute test minimization algorithms.
