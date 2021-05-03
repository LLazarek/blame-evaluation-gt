The first part of our response addresses one major comment from each review.

The origin of the rational programmer
================================================================================

Review A says:
> the paper talks in the introduction of its top-level innovation being the
> idea of the rational programmer. But then they go on to say that the idea
> is actually taken from Lazarek. Which is it then?

Lazarek et al. (POPL'20) provide the kernel insight that one can simulate
an idealized programmer on a large corpus of programs to test the behavior
of a language feature. This paper hones this kernel into an experimental
framework. With the framework we can now:

1. isolate confounding factors, such as whether the utility of blame is due
   to blame tracking or just due the checks;
2. compare different semantics for gradual type boundaries in the form of
   experimental modes.

The paper christens this experimental framework the Rational Programmer.
We have already revised the prose accordingly.


What the results say about the theory vs practice of blame
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

- _Erasure_ doesn't even assign checks to type boundaries.

Hence, theory predicts that Natural is superior to Transient and Transient is
superior to Erasure.

Our experiments only appear to validate these predictions:

1. When Natural issues blame it is helpful in almost all debugging scenarios.

2. Natural blame looks better that Transient blame and both look better than
   Erasure's stacktraces.

However, a close inspection of the empirical data reveals a gray-shaded picture
instead of the black and white print of theory:

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

In sum, our experiments reveal a mismatch between the theory and the
practice of blame, where practice refers to blame's actual utility in real
programs that run in a real implementation rather than artificial examples
in an idealized model.

As a result, we expect to invest in two intertwined research efforts going forward:

1. Revisiting the theory of blame to match the practice of blame.

2. Empirically analyzing the practice of blame beyond the scenarios this
   paper considers. Section 11 points to another space of scenarios that
   deserves analysis: scenarios where mismatches arise due to mistakes in
   type annotations rather than (or as well as) code.

   Indeed, the review astutely points out two more directions worth exploring:

    - programs where some components _cannot be typed_;
    - non-deterministic programs.


Are the debugging scenarios difficult?
================================================================================

Review C says:
> The only thing that I am concerned about at this point is that, later in the
> paper, it is shown that "most cases the programmer need to type a single
> module to debug a scenario," which makes me wonder whether the authors did not
> generate difficult enough scenarios. If that is the case, what is the problem?

The bugs that we inject are non-trivial. To provide some evidence, we sampled a
few (~24,000) scenarios from three middle-of-the-road benchmarks (acquire,
take5, kcfa) and the median stack has 7 program modules, with the sizes ranging
from 3 to 10. This indicates that these bugs involve significant interaction
among different parts of the program.

One could argue that the benchmark programs are too small, because they consist
of about 10-14 modules. This is a decent size but not a truly large one, so
perhaps even larger benchmark programs are called for, though we conjecture that
results will remain roughly the same because the programming style is
representative (it includes a good number of higher-order idioms, for both
functions and classes; it includes a fair number of call-back scenarios across
module boundaries and storage of objects to be called later -- after the storage
call returns -- but again not too many).




Replies to individual points
==================================================================

Review #45A
===========================================================================

> The table in section 2 looks rather ugly

We will find another way to present the information.


> p5. one thing I wonder about is how this all hinges on the rational programmer
> not only taking the right action, but also taking it right? Is that exactly
> what the rational programmer idea presupposes? (I guess this reflects on your
> remark in Sec. 11 that one problem is with errors in the ascribed types
> themselves.).

Yes, you hit the nail on the head.  One of the central parts of the
rational programmer is that it fixes the decision of how to respond to a
given scenario; so evaluating different ways of taking action boils down
to defining and testing the corresponding rational programmers.


> When the rational programmer "fixes" a module and reruns, the implicit
> assumption seems to be that he runs the program in the exact same way. That
> would make the most sense to me, but you do not seem to come out and say so.

Yes you're right. Thanks, we will clarify this in the prose.


> It would help if you could explain how a mutator like negate-cond actually
> leads to a program that has more type-level mistakes than before. It does not
> seem to me this part of the code itself will now fail, but that changing the
> conditions makes certain previously excluded paths in the code feasible.

Thanks for pointing this out, we will expand the explanation and examples of
mutators in the final paper. In the specific cases of `negate-cond` and
`force-cond`, these mutators lead to type-level mistakes when the program uses
occurrence typing. For example, they could both lead to a mistake in a program
like this:

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


>  - Why does the paper conclude that "the existing theory does not predict
>    practice properly"?  Figure 8 shows that the theory (Natural) works well.
>  - What does the paper mean by "The existing practice may need additional
>    experiments"?  I have not found any evidence that confirms this claim in the
>    paper.
>  - What does "practice" means here?

We hope the start of the response addresses these questions adequately.


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
end up making the configuration lattice much larger.


> - In principle, Natural blame should always point out the faulty components
>   by the Wadler-Findler slogan.  However, the experiment shows it is not the
>   case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
>   to a gap between theory and practice, but exposing a reason is crucial.)

(Combining this question with the related one from below next.)

> - Are failing Natural blame trails produced even for programs with impedance
>   mismatch?  (This question is related to the above issue with Natural blame.)

We agree that the paper needs a bit more explanation to clarify how Natural
blame can have failing scenarios, we will update it in the final paper. In
short, Natural blame trails can by stymied by unhelpful errors from Racket's
runtime. Because debugging scenarios are only partially typed, some scenarios
produce an error from the runtime (e.g. `+` receiving a non-number) before any
of Natural's run-time type checks can discover a problem. Errors from the
runtime do not carry blame, only stacktraces.


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
configuration) and we sample trail roots from all the configurations where
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

We will improve the prose in this paper to clarify. Thank you for pointing this
out. The paper is referring here to the incomplete population of the blame map
described by Greenman et al. (OOPSLA'19).

For the details of how we have implemented the blame map and its population,
there is a parallel experience report submission by the same set of authors
describing the implementation of Transient in depth.


>   - What "sophisticated typing features" are considered (line 589)?  It would be
>     crucial to confirm whether the proposed mutators are enough.

The features we have in mind appear in line 641. We will add a breakdown of
features per benchmark to the analysis of section 6.3 to clarify that our
mutators target all of these.


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

You are absolutely right, we will clarify these assumptions about the benchmarks
in the paper. They are inherited from the other publications on the gradual
benchmark suite.


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



Review #45C
===========================================================================

> My only concern at this point is that we need to believe that the authors have
> implemented the Transiet semantics correctly in Racket, though no details are
> offered. The paper refers to another submission, so perhaps some details can
> be found in there.

Our implementation of Transient is faithful to Reticulated Python; the parallel
experience report submission by the same set of authors describes the
implementation of Transient in depth.


> I am wondering whether the approach that the paper calls Transient First could
> be regarded as Transient Early, since it points to an earlier part of the
> code. Similarly Transient Last could be Transient Late, but you would have to
> see whether this terminology really fits.

Thanks, we will consider these alternative names for the final paper.


> My only issue at this point is that the reader is left with numnbers only,
> while I think a reader would like to see selected examples, for example about
> which blame labels have been provided by Natural and which by Transient, so to
> see that one approach took a longer path, as well as similar examples the
> reader can learn from. Numbers do not seem to teach the whole story in this
> part of the paper.

This is a good idea, thanks. In the final paper we will try to incorporate one
or more examples that illustrate the difference in trails between the three
mechanisms.



Review #45D [R1]
===========================================================================

> First, the performance figures cited from [38] (worst case 5.4x
> slowdown) are used incorrectly: those numbers are about transient
> /without/ blame (see Section 6.2 of [38]), and given that "with the
> blame map turned off, the Transient semantics also runs these programs
> in a short amount of time and well within the memory limits" there
> appears to be no contradiction here. When blame /is/ enabled, [38]
> reports a worst-case 18x slowdown, which may be more in line with the
> results reported in this work, and as a result it's not actually clear
> that the earlier results are skewed in a way that demands explanation.

Yes you are right, this is our mistake: we meant to use the number with blame
here. After looking at the plot in Section 6.2 of [38] we believe the right
numbers are 6.2x and 17.2x (the 18x is an upper bound).

These numbers, however, are still orders-of-magnitude better than Shallow. Hence, 
we believe this gap needs an explanation.


> Claim (2), that local variables in Reticulated Python are dynamically
> typed because no types are ascribed to them, is false; Reticulated
> Python uses a local type inference system to give types to local
> variables (see Section 3.1.3 of [36]).

You are right and we should have been more precise here. Reticulated
does attempt to infer types for local variables. However, our
experience indicates Reticulated's inference often picks type Dynamic
for local variables. Section 5.4.4 of Greenman's dissertation presents
a tiny example that explains how Reticulated can infer `Dyn` for a
local variable.

This behavior came as a surprise to us too. We manually inspected the
output of `retic --mgd-transient --print file.py` for several benchmarks to
convince ourselves.

> Claim (4) that "on large programs, Reticulated suffers from high
> overhead" is unsubstantiated here or in cited prior work, and is also
> unclear -- overhead compared to what? The text compares it (vaguely) to
> Typed Racket (without specifying which semantics), but this isn't a
> useful comparison given the different baseline performance of Racket and
> CPython. It's not clear if the point is that performance of this
> benchmark in blame-free Reticulated is seriously slow compared to
> regular Python, or if the program itself is just slow in Python (in
> which case, what's the relevance to this paper?).

Thank you for pointing this out. The prose here does not explain clearly our
evidence for this claim.

The goal was to run Reticulated (with and without blame) on one
reasonably large benchmark to see if it ever had a huge
"timeout-level" slowdown, similar to the ones we observe in our
``rational programmer'' experiment.

To this end, we converted our fully-typed `sieve` benchmark from TR to
Reticulated. Then we ran this fully-typed Reticulated program twice:
with and without blame. The `with blame` variant experienced a huge
slowdown.

This little experiment suggests that Reticulated does not employ any
crucial implementation techniques that can mitigate the slowdown we
observe with Shallow Racket. Of course, all we have is a single data
point, and this must come thru more clearly in our prose. We would
hope that this submission and the parallel submission on Shallow
Racket's construction inspires the Reticulaed team to report data on
larger benchmarks than the ones in their published papers.

> The characterization of the Monotonic semantics for references [22] as a
> variant of natural isn't correct--its exception-raising and blame
> behavior for references (though not for functions) is quite different
> from that of Natural. For example, a monotonic reference imported from
> untyped code into two incompatible typed contexts will error
> immediately, before being used.

Indeed this characterization is not right, when it comes to boxes
monotonic is more than a mere variant of Natural. We will fix this
description. 
