The first part of our response addresses one major comment from each review.

What is the origin of the rational programmer
================================================================================

Review A says:
> the paper talks in the introduction of its top-level innovation being the
> idea of the rational programmer. But then they go on to say that the idea
> is actually taken from Lazarek. Which is it then?

Lazarek et al. (POPL'20) provide the kernel insight that one can simulate
an idealized programmer on a large corpus of programs to test the behavior
of a language feature. This paper hones this kernel into an experimental
framework. With the framework we can now:

1. isolate confounding factors;
2. compare different semantics for gradual type boundaries in the form of 
   experimental modes.  

The paper christens this experimental framework the Rational Programmer.
We have already revised the prose accordingly. 

What the experimental results say about the theory vs the practice of blame
================================================================================

Review B says:
> The conclusions of the second contribution described in the introduction
>  (around lines 41-103) are confusing for me.

Theoretical work that compares different checking and blame strategies for
gradual types (Greenman et al. ICFP 2018 and OOPSLA 2019) declares a clear
winner: Natural.

- _Natural_ is the only semantics that is type sound in the conventional sense,
  completely monitors boundaries, and, most importantly here, blames all and
  only those components that are relevant to a type impedance mismatch (sound
  and complete blame).

- By contrast, _Transient_ may fail to blame some relevant components and may
  blame irrelevant ones.

- Erasure doesn't even assign checks to type boundaries. 

Hence, theory predicts that Natural is superior to Transient and Transient is
superior to Erasure.

Our experiments only appear to validate these predictions:

1. When Natural issues blame it is helpful in almost all debugging scenarios.

2. Natural blame looks better that Transient blame and both look better than
   Erasure's stacktraces.

However, a close inspection of the empirical data reveals that 
 a gray-shaded picture not the black and white print of theory:
   
1. One problem is that many debugging scenarios result in runtime errors with
   unhelpful stacktraces from the underlying checks of Racket rather than Typed
   Racket boundary checks.

1. Additionally, for some debugging scenarios Natural blame is not helpful but
   Transient blame is. The theory cannot explain these scenarios at this point.

2. Finally, while the theory claims that language designers should select
   Natural and its blame for maximum benefits, the data indicates that both
   Transient blame information and Erasure stacktraces often suffice.  Given the
   large cost of Natural (Takikawa et al. POPL 2016), theory has has shown
   itself to be of questionable value -- at this time. 

In sum our experiments reveal a mismatch between theory and practice where blame
may or may not matter for real programs running on real implementations. This is
in contrast to theoretical papers, which symbol-push small terms thru a reduction 
semantics in a tiny model of PLs (even if these models exhibit all essential 
attributes). 

As a result, we expect to invest in two intertwined research efforts going forward:

1. Revisiting the theory of blame to match the practice of blame.

2. Empirically analyzing the practice of blame beyond the scenarios this
   paper considers. Section 11 points to another space of scenarios that
   deserves analysis: scenarios where mismatches arise due to mistakes in
   type annotations rather than (or as well as) code.

   Indeed, the review astutely points out two more directions worth exploring:

    - programs where some components _cannot be typed_;
    - non-deterministic programs. 

Replies to individual points
==================================================================

Review #45A
===========================================================================

> p5. one thing I wonder about is how this all hinges on the rational programmer not only taking the right action, but also taking it right? Is that exactly what the rational programmer idea presupposes? (I guess this reflects on your remark in Sec. 11 that one problem is with errors in the ascribed types themselves.).

Yes, you hit the nail on the head.  One of the central parts of the
rational programmer is that it fixes the decision of how to respond to a
given scenario; so evaluating different ways of taking action boils down
to defining and testing the corresponding rational programmers.


> When the rational programmer "fixes" a module and reruns, the implicit assumption seems to be that he runs the program in the exact same way. That would make the most sense to me, but you do not seem to come out and say so.

Yes you're right. Thanks, we will clarify this in the prose.


> It would help if you could explain how a mutator like negate-cond actually leads to a program
> that has more type-level mistakes than before. It does not seem to me this part of the code
> itself will now fail, but that changing the conditions makes certain previously excluded paths in the code feasible.

Thanks for pointing this out, we will expand the explanation and examples
of mutators in the final paper.  In the specific cases of `negate-cond`
and `force-cond`, these mutators lead to type-level mistakes when the
program uses occurrence typing.  For example, they could both lead to a
mistake in a program like this:

```
(: use-int  (-> Integer Void))
(: use-bool (-> Boolean Void))
(: x        (U Integer Boolean))

(if (integer? x) (use-int x) (use-bool x))
```

> How do you arrive at the number 72192 in Section 7? It is not equal to 756 times 96.

Indeed, 756 is a typo: it should be 752.



Review #45B
===========================================================================

>  - Which blame assignment strategy is "good" and which is not "good"?

In this context, good means Natural and Transient. We will rephrase. 

>  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
>    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
>    be nicer to address them in a clearer manner.

Thanks for the feedback, we will rephrase the conclusion.


> - The paper often mixes gradual typing and migratory typing.  It is unclear how
>   it distinguishes them.

The paper uses the two terms interchangeably after the introduction,
because the distinction between the two does not change the evaluation
method.  Specifically, in the gradual typing setting the components are
smaller than whole modules, and there are more possible intermediate types
(incorporating Dyn) between untyped and fully-typed, both of which just
end up making the configuration lattice much much larger.


> - In principle, Natural blame should always point out the faulty components
>   by the Wadler-Findler slogan.  However, the experiment shows it is not the
>   case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
>   to a gap between theory and practice, but exposing a reason is crucial.)

(Combining this question with the related one from below next.)

> - Are failing Natural blame trails produced even for programs with impedance
>   mismatch?  (This question is related to the above issue with Natural blame.)

We agree that the paper needs a bit more explanation to clarify how
Natural blame can have failing scenarios, we will update it in the final
paper.  In short, Natural blame trails can by stymied by unhelpful
run-time errors from Racket. Because debugging scenarios are only
partially typed, some scenarios produce an error from the runtime (e.g.
`+` receiving a non-number) before any of Natural's run-time type checks
can discover a problem.  Runtime errors do not carry blame, only
stacktraces.


> - It is not fully explained how the given mutators change programs.  For
>   example, I cannot completely predict changes by the mutators deletion and
>   class:super.

Thanks for the comment, we will expand the explanation and examples of
mutators in the final paper.  In the specific case of `deletion`, the most
common place that this mutator applies (beyond the literal `begin`
expressions the example illustrates) are so-called "implicit begins":
places where sequences are allowed implicitly by enclosing forms like
`define` or `cond`.  Here are two more examples of `deletion` mutations
for these situations: 

```
(define (do-something) (step-1!) (step-2!))
~>
(define (do-something) (step-1!))

(cond [(do-something?) (step-1!) (step-2!)]
      [else (void)])
~>
(cond [(do-something?) (step-1!)]
      [else (void)])
```

In the case of `class:super`, the mutator simply removes calls to
`super-new`, causing the class to no longer instantiate the superclass.


> - The paper says that the experiment uses 72,192 sampled scenarios (line 802).
>   However, the total number of mutants (i.e., configuration lattices) is 756
>   and, from each lattice, 96 scenarios are sampled.  Thus, I guess 756*96 =
>   72576 scenarios are used.  Why does this mismatch happen?

Indeed, 756 is a typo: it should be 752.


> - The experiment samples multiple scenarios from a configuration lattice (line
>   800).  Does this mean the debugging process may start with a mix-typed program
>   and may not with a fully untyped one?  If so, why does the paper take such an
>   approach?

The debugging process can start with the fully untyped program but it
doesn't have to. The lattice contains the completely untyped program (bottom
configuration) and we sample from all the configurations where
the buggy module is untyped (including the fully untyped one). 

> - The paper often says that a blame system is (un)sound, but it is difficult for
>   me to identify what it precisely means.

The term refers to blame soundness from Greenman et al. (OOPSLA'19), but
we agree that for clarity the paper should define it in addition to the
reference.  Intuitively, a blame system is sound if it can only blame
components that have had control of the value witnessing a violation.  In
other words, the system cannot blame arbitrary components unrelated to the
witness of a contract violation.


> - The paragraph starting at line 1122 is quite difficult to understand for me.
>   [...]
>   - What does the paper mean by "incomplete population of the blame map"?
>   - To understand the idea on the improvements of Transit, more explanations
>     on the usage of blame maps in Transient would be needed.


 We will improve the prose in this paper to clarify. Thank you for
 pointing this out.  The paper is referring here to the incomplete
 population of the blame map described by Greenman et al. ([11]:
 OOPSLA'19). 

 For the details of how we have implemented the blame map and its
 population, there is a parallel experience report submission by the same
 set of authors describing the implementation of Transient in depth. 

>   - What "sophisticated typing features" are considered (line 589)?  It would be
>     crucial to confirm whether the proposed mutators are enough.

The features we have in mind appear in line 641. We will add a breakdown
of features per benchmark to the analysis of section 6.3. Our mutators do
target all of these. 


>   - The benchmarks are selected (line 647), but why?  The GPT benchmark suite of
>     Racket provides more examples.

The benchmarks in figure 5 are the ten with the largest dependency graphs
of the GTP suite. We selected those because small benchmarks have
short blame trails. We will rephrase L646-648 to make this clearer.


> - It seems that the experiment implicitly supposes a few assumptions on
>   benchmarks.  First, the original benchmarks must be fully typed.  Thus more
>   experiments on mix-typed programs where some components cannot be typed may be
>   needed.  Second, the benchmarks are supposed to be deterministic, which I find
>   from the definitions of trails.  If it is the case, while I do not think these
>   restrictions have to be lifted in the paper, it would be nice to expose them.

You are absolutely right, we will clarify these assumptions about the
benchmarks in the paper. They are inherited from the several
publications on the gradual benchmark suite. 


> - As an alternative of Transient first and last blame, perhaps determining
>   blamed modules by voting (i.e., blaming a module that has been most often
>   added to the blame map) might be promising (if the blame map has a large
>   population).  Have the authors considered it or another approach for Transient
>   blame?

We have since thought of a few alternative strategies, but not that
one: good idea!  Ultimately, Transient does not offer any
interpretation of its blame, so the paper chooses first and last as
two obvious reasonable choices.


> - Perhaps it is valuable for followers to share the experience on developing
>   mutators that are not interesting, .e.g., in the supplementary material.

We agree and would be happy to do so.


> - Why doesn't the paper adopt the author-year citation style?

This was a mistake typesetting the paper, we have fixed it. It didn't
affect the length of the paper


> - Page 7: How is the question (5) answered?

Thanks for pointing out that this is confusing.  As it is, the rest of
the paper is the implicit answer to (5), but we will clarify the
prose.


> - L356 "despite advertisements for the opposite":  I cannot find what this means.

This is meant to convey that although Lazarek et al. aim to evaluate
blame, they fail to adequately account for confounding factors.  We
will reword for clarity.


> - L370 "the latter must represent":  What does the "latter" specify?

It refers to impedance mismatches from the end of the preceding
sentence; we will clarify the wording here.


> - L424 "Let a configuration s of P":  Are configurations the same as scenarios?

Scenarios are a specific kind of configuration (that doesn't type the
buggy module).  We will adjust these two paragraphs to make this more
clear.


> - L431 "type-level mistake":  Is it the same as a impedance mismatch?

A type-level mistake is a mistake that causes an impedance mismatch at
a boundary between typed and untyped code.  We will clarify this point
in the prose.


> - L556: It would be helpful to describe how to extend trails and how to
>   determine if there is no scenario to be added.  (This comment is also related
>   to the issue with Natural blame).

Thanks, we will expand on this in the prose.


> - L557: "if it does" --> "if it does not"?

You're right, thanks, we will fix it.


> - L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
>   "does not reliably lead to type errors"?

This is a refinement of the arithmetic mutator that does lead to type
errors, because given two integers `*` always produces an integer, but `/`
may produce a rational, which have different types in Typed Racket's
sophisticated numeric tower (St-Amour et al. PADL 2011).  We will clarify
this comment in the footnote.


> - L785 "the interesting standard guided countless iterations":  I fail to find
>   what this intends.

The measure of interesting mutants provided a standard by which we
could judge potential mutators and iteratively develop an effective
set of them.  We will make this comment clearer.


> - What is the total number of failing trails in Transient?

3594 out of 72192 trails fail for transient last blame, and 3591 for
first.


> - L1099 "as behavioral economics has shown more recently":  Is there a reference
>   to be cited?

The best reference we might use here is Kahnemann's popular book entitled
"Thinking Fast and Slow", which explains the problem well.  Notably with
respect to this work, the book criticizes classical (mathematical)
economic theory for assuming that people act rationally all of the time,
which demonstrates how our work is complementary to human studies and
theory. All are necessary.


> - L1104 "deviating is a mistake":  Why?

Our long experience of ignoring blame as opposed to heeding it
suggests that ignoring it is usually unwise.


> - L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?

The slowdowns the paper quotes here are from Python benchmarks.


> - L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
>   does it intend?

This paragraph refers to a Python translation of the Racket `Sieve`
benchmark; it is confusingly worded, we will adjust the phrasing.


> - L118: "(at the mid-level on the left)[] imports" (the comma is removed)
> - L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
>   consider clarifying.
> - L347 "the rational programmer's effort": This was unclear at the first
>   reading.  It would be nice to briefly explain it here.
> - L451: "denotes the module [of P] that s blames"
> - L970: "the experiment provides [an] evidence that"
> - L1198 "But just because...":  It is difficult for me to find what this
>   sentence wants to say.  Please consider rephrasing.

Thanks for these comments, we will incorporate them to improve the prose.
