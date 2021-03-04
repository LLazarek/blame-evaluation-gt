
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
two factors in my head and then in Racket:

```
(* 756 96)
72576
```

**Done and pushed.**

6. Figure 6 could benefit from an in-figure explanation of what we’re really
looking at here. (Remember the lazy reviewer.) ~~~ **Lukas?**

7. I do not understand the following sentence (bottom of page 19): 

"Finally, there is no clear winner between Natural exceptions and Transient
exceptions despite the theoretically advantageous additional checks of Natural.”  

In what sense does the theory predict an advantage for Natural exception?

Christos replied with: ``Ben’s error approximation theorem is about checks
really not blame.'' This reply doesn't make any sense to me. I don't even know
how it connects to my question. ~~~ **Christos?**

8. Do we need to explain why the following is the case (top of page 21): 

"these proportions do not generalize to a representation of the full
population.”

Christos? 

9. I am still struggling with our development of mutators. Shouldn’t
we have developed mutators that mess up the type ascribed to a module’s code
rather than the code? If we had done so, would we be able to tackle the
“complete monitoring” bugs in GFD’20?

What I specifically mean here is that _our prose_ implies the types are wrong
but _our experiment_ breaks the code. ~~~ **Christos?**

10. In section 9.1, we write 

```
Relatedly, the experimental setup hides how a rational programmer ascribes types
to extend a trail. When the run-time checks signal an impedance mismatch in the
real world, a real-world programmer does not have a typed module ready to swap
in. Instead, the programmer must come up with the next set of types, which means
making choices. It is usually possible to consistently assign types to variables
in a module in different ways. The maintenance of the benchmarks over many years
has driven home this lesson but, fortunately, it has also shown that the types
are in somewhat canonical.  The authors therefore conjecture that different
real-world programmers would often come up with equivalent type assignments
during debugging sessions. 
```

Could we point to quadU and quadT here? Is it worth checking whether the results
for the two benchmarks differ wrt to our investigation? Should we create
additional benchmarks like it? ~~~ **Christos?**

(I did not understand your response to this one.) 

11.  The “editorial voice” with respect to Transient for Typed Racket, we should
inspect all phrases and go with Shallow Racket where possible. Sadly we used the
phrase “Transient Racket” in this submission and “Shallow Racket” in the other
one. I have changed two or three basic occurrences (those that emacs/grep can
find) but there should be more (and it would improve the paper for those readers
who look at both papers).  


12. I have not yet inspected the bibliography. ~~~ **Matthias**

(What I mean by this is the formatting of the bib entries.) 
