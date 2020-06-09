1. Does Blame Assignment Matter?

   - GT research has developed different choices for the run-time
     meaning of types.

   - The choices have implications for what these systems report when
     run-time type checks fail.

   - We present a **method** for systematically evaluating these
     choices and how affect their error message pragmatics.

   - The method directly builds on the blame trails idea of Lukas et al [POPL '2]0

   - We apply the method to implementations of the Natural and Transient semantics. 

2. Why and How Blame Assigment Algorithms Differ

   By example; adjusted version of the one in OOPSLA sec 2

   1. Why Blame Assignments Differ

   2. How Blame Assignments Differ

   3. A space of GT system design

      - In terms of error reporting, we identify two axes of GT system
        design: where checking occurs and whether blame is tracked.

3. A general method for analyzing the utility of blame

   1. How to analyze this space?
   
      - We want a quantative measure of error msg utility that allows
        us to evaluate each point in the above space.

   2. Blame trails
   
      - We want an empirical study to understand how the behavior of blame
        in real systems.

      - Blame trails provide a quantitative measure of a a system's
        error message utility, and they can be applied to every point
        in the above space.

      - We generalize the blame trails idea of Lukas et al [POPL '20] to make it
        more systematic, scalable, and general, by:
        1. Adding a null hypothesis baseline for comparison between multiple systems
        2. Analyzing multiple blame following strategies
        3. Mutating interfaces as well as programs
        4. Incorporating domain knowledge in the selection of blame following
           strategies and mutation
        5. Evaluating systems on the same platform for apples-to-apples comparisons

   3. Null hypothesis

      - We should have a baseline to tell how all of the points in the
        design space compare to having nothing at all.

4. Experimental design

   1. Summary of the blame trail experiment

      - We sample and follow blame trails through configuration
        lattices.

   2. Evaluating each point in the design space

      - We run the blame trail experiment for every point in the
        previously discussed design space.

   3. Selection of programs

      - We are using a set of representative programs

      - Argue the programs are representative of the range of programs
        written by TR developers; point to the JFP paper.

   4. Sampling of mutants

      - We have too many mutants to run them all, so we sample them
        with an aim to represent equally all of the mistakes captured
        by our operators.

5. Results 

6. Discussion

   1. Interesting discoveries...?

   2. Comparison of existing GT systems represented in our study

      - Natural is here, transient is here, erasure is here, and they
        compare as...

   3. Threats to validity

      - Racket stack traces
      - Mutant sampling
      - Blame trails do not necessarily capture all aspects of error reporting
      - Transient blame adaptations
      - More?

7. Related work

   - GT: systems we don't represent
   - Mutation testing
   - Fault localization

8. Conclusions

