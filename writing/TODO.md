
### Remaining Questions, Comments, and TODO Items

1. The latex run suggests that our paper uses the wrong citation format for
ICFP. A stickler of a PC or reviewer might kill the paper on “type-checking
grounds”. ~~~ **Lukas?**

2. Will you re-do your blame-map implementation with weak hash tables and
ephemerons? ETA? ~~~ **Ben?**

3. Will you re-run the experiment after fixing the bug/s? ~~~ **Lukas**, with
help from **Ben**.

4. @Ben: I somehow can’t reconcile the timing/out-of-memory reports in the two
parallel submissions. ~~~ **Ben?**

5. Section 7 states that we get 72,192 interesting scenarios. I multiplied the
two factors in my head `(* 756 96) = 72576` and then in Racket: **Done and pushed.**



6. Figure 6 could benefit from an in-figure explanation of what we’re really
looking at here. (Remember the lazy reviewer.) ~~~ **Lukas?**

7. I do not understand the following sentence (bottom of page 19): "Finally, there is no clear winner between Natural exceptions and Transient
exceptions despite the theoretically advantageous additional checks of Natural.”  
In what sense does the theory predict an advantage for Natural exception? ~~~ Christos replied with: ``Ben’s error approximation theorem is about checks
really not blame.'' This reply doesn't make any sense to me. I don't even know
how it connects to my question. ~~~ **Christos?**

8. Do we need to explain why the following is the case (top of page 21): "these proportions do not generalize to a representation of the full population.” **Christos?**

9. I am still struggling with our development of mutators. Shouldn’t
we have developed mutators that mess up the type ascribed to a module’s code
rather than the code? If we had done so, would we be able to tackle the
“complete monitoring” bugs in GFD’20? ~~~ What I specifically mean here is that _our prose_ implies the types are wrong
but _our experiment_ breaks the code. ~~~ **Christos?**

10. In section 9.1, we write "
Relatedly, the experimental setup hides how a rational programmer ascribes types
to extend a trail. When the run-time checks signal an impedance mismatch in the
real world, a real-world programmer does not have a typed module ready to swap
in. Instead, the programmer must come up with the next set of types, which means
making choices. It is usually possible to consistently assign types to variables
in a module in different ways. The maintenance of the benchmarks over many years
has driven home this lesson but, fortunately, it has also shown that the types
are in somewhat canonical.  The authors therefore conjecture that different
real-world programmers would often come up with equivalent type assignments
during debugging sessions. " ~~~ Could we point to quadU and quadT here? Is it worth checking whether the results
for the two benchmarks differ wrt to our investigation? Should we create
additional benchmarks like it? ~~~ **Christos?** (I did not understand your response to this one.) 

11.  The “editorial voice” with respect to Transient for Typed Racket, we should
inspect all phrases and go with Shallow Racket where possible. Sadly we used the
phrase “Transient Racket” in this submission and “Shallow Racket” in the other
one. I have changed two or three basic occurrences (those that emacs/grep can
find) but there should be more (and it would improve the paper for those readers
who look at both papers).  


12. I have not yet inspected the bibliography. ~~~ **Matthias**

-----------------------------------------------------------------------------

I looked at the TODOs and here is my initial answers: 

7. What I meant here is that Ben’s error preorder theorem says that Natural’s checks find more errors than those of Transient. A corollary of that is that a program may lead to a contract violation in Natural but in Transient it may lead to a failure of the underlying safety checks of the language. This is the result of natural performing checks ``earlier’’ than Transient which translate to  Natural performs more checks. The theorem is about models with blame but it holds independently of whether dynamic type checks issue blame or stack traces. Given the above, the comparable effectiveness of Natural exceptions and Transient exceptions comes as a bit of an upset of the theoretical promises. 

8. I am not sure. The issue is that with the statistics we know questions of degree do not generalize. The sentence is more of an admission that we didn’t do the generalization rather than that it is not possible — it should be and we should learn how to do that, so maybe this is a good place to start digging deeper into statistics.

9. My take is that it is unclear what is the difference between taking a function from Integers to Integers and mutating its type so that it claims it is a function form Integers to Booleans VS mutating a function from Integers to Boleans so that it returns an Integer. In other words I don’t think we can connect the way we introduce an impedance mismatch, as in where we did the mutation, with how effective it is in causing certain behavior. I do think that mutations of types may give us different results than mutations of code but this is not because one is fundamentally different than the other but because it is much more probable to get certain impedance mismatches with type mutations rather than code mutations given a set of benchmarks. 


10. My answer is yes. In fact we should do a proper analysis of all benchmarks to understand why the rational programmer behaves this or that way on this or that program. But I don’t know how to do that beyond a hand wavy qualitative analysis, which doesn’t scale even to all the mutants of quadT and quadU. So what I am thinking is that we should reverse engineer the problem: (i) we should construct programs with specific charactersitics in a controlled way; (ii) mutate them in a controlled manner and (iii) unleash the rational programmer on them and see what we get. If we hit something interesting we know why it is interesting. Then from that we can try to build metrics to identify the characteristics of the synthetic programs, locate matching real programs and confirm explicable patterns of behavior from the real data. 

I think this is where I find the rational programmer idea a bit restrictive. Simulating a programmer is only part of this. A much better analogy is to think about wet lab work or the geneticist to Shriram’s zoologist. The rational programmer is an experimental process to get a readout from a model organism, the three systems are three strains of the organism engineered to have certain properties, the mutations are drugs that we throw in the mix and that we show interact with the three strains in some interesting way. The experiment asks how stable the readouts are as we change strains and drugs. I also think that theory, as apects of experience or expectation made precise, can help here guide experiments, just like we did with the rational programmer.
