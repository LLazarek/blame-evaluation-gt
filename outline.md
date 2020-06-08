1. Does Blame Assignment Matter?

   - GT research has developed different choices for the run-time
     meaning of types.

   - The choices have implications for what these systems report when
     run-time type checks fail.

   - We present a **method** for systematically evaluating these
     choices and how affect their error message pragmatics.

   - We apply the method to implementations of the Natural and Transient semantics. 

2. Why and How Blame Assigment Algorithms Differ

   - Why Blame Assignments Differ

   - How Blame Assignments Differ 

3. Analyzing these differences

   1. A space of GT system design

      - In terms of error reporting, we identify two axes of GT system
        design: where checking occurs and whether blame is tracked.

   2. How to analyze this space?
   
      - We want a quantative measure of error msg utility that allows
        us to evaluate each point in the above space.

   3. Blame trails
   
      - Blame trails provide a quantitative measure of a a system's
        error message utility, and they can be applied to every point
        in the above space.

   4. Null hypothesis

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

      - We are using a diverse set of programs from prior work on GT,
        and we insert bugs using mutation techniques.

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
      - More?

7. Related work

   - Inspired by POPL'20
   - GT
   - Contracts
   - Mutation testing
   - Fault localization

8. Conclusions

