Action items are marked with
===> TODO
: (highlight-lines-matching-regexp "^===> TODO")

ICFP 2021 Paper #45 Reviews and Comments
===========================================================================
Paper #45 How to Evaluate Blame for Gradual Types


Review #45A
===========================================================================

Overall merit
-------------
A. I will champion accepting this paper.

Reviewer expertise
------------------
Y. Knowledgeable

Paper summary
-------------
Different methods exists to blame parts of a gradually typed program for a failing execution (due to a type inconsistency). The authors take the idea of the rational programmer: a programmer who bases his changes to the gradually typed program only on information provided by the run-time system (this information could be blame information, or information about the crash itself).

The paper then foremostly discusses how to evaluate which of the approaches (Transient, Natural and Erasure) works best at finding the problem, and the effort it takes to do so: how many modules do you need to add types to before you find the source of the mistake?
The outcome of the study aims to show: is blame information useful or shouldn't we bother. And if blame is useful, which semantics, Natural or Transient, is most effective?

An important ingredient of their work is to generate sufficiently interesting programs to apply the methods to.

The experiment takes place in the context of (Typed) Racket

Comments for author
-------------------
This is a really nice paper that not only provides a lot of value in terms of the work that has been invested in it, but it also raises and answers a number of conceptual issues that can be applied in other fields beyond blame and gradual types, and is for that reason thought-provoking.

The paper is well-written with good examples, not too complicated, not too simple,
and drives home the message that there is more to a study of this kind than may seem from the the start.

I do have some remarks/questions:
===> TODO, minor
the paper talks in the introduction of its top-level innovation being the idea of the rational programmer. But then they go on to say that the idea is actually taken from Lazarek. Which is it then?

===> TODO, minor
The table in section 2 looks rather ugly

===> TODO
p5. one thing I wonder about is how this all hinges on the rational programmer not only taking the right action, but also taking it right? Is that exactly what the rational programmer idea presupposes? (I guess this reflects on your remark in Sec. 11 that one problem is with errors in the ascribed types themselves.).

===> TODO, minor
When the rational programmer "fixes" a module and reruns, the implicit assumption seems to be that he runs the program in the exact same way. That would make the most sense to me, but you do not seem to come out and say so.

===> TODO (common with B: want more discussion of mutants)
It would help if you could explain how a mutator like negate-cond actually leads to a program
that has more type-level mistakes than before. It does not seem to me this part of the code
itself will now fail, but that changing the conditions makes certain previously excluded paths in the code feasible.

===> TODO, minor
How do you arrive at the number 72192 in Section 7? It is not equal to 756 times 96.



Review #45B
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
X. Expert

Paper summary
-------------
The paper proposes a systematic, algorithmic way for evaluating the usefulness
of blame assignment systems on locating components with impedance mismatches.
The main idea, which follows Lazarek et al., is to simulate the behavior of
working programmers, who are supposed to seek for impedance mismatches only by
seeing blame information.  The paper uses two metrics: one is whether the
simulation with a blame assignment strategy locates a bug; the other is the
numbers of components that have been examined in the successful simulations.
The experiment is conducted with Racket, which implements several blame
assignment strategies (Natural blame and exception, Transient first blame, last
blame, and exception, and Erasure) on the benchmarks mutated to include
impedance mismatches.  The experimental result shows: blame is more useful than
just raising exceptions; Natural blame assignment is more useful than Transient
(perhaps due to the heavy usage of computer resource in the latter); and the
debugging process with the exception strategy tends to be shorter than that with
the corresponding blame strategy.

Comments for author
-------------------
### Strengths

The paper addresses an important problem shared between gradual typing and
contract systems.  The evaluation of blame assignment strategies is challenging
but key to confirm the value of the research on blame.  Perhaps the best way is
to conduct empirical studies with actual users, but it is difficult to scale.
The paper instead adopts the rational programmer, which is a more lightweight,
more scalable algorithmic evaluation approach proposed by Lazarek et al.  The
contributions of the paper I identify are: applying the rational programmer to a
comparison of blame assignment; proposing mutators that effectively inject bugs
coming from impedance mismatches; experiments conducted on several benchmarks;
and discussions about the experimental results.  These contributions are
insightful for language designers of gradual typing and contract systems and
worth presenting at ICFP.

### Weaknesses

On the other hand, I am confused about the following points in the paper.
I would like the authors to respond to these concerns.

===> TODO
- The conclusions of the second contribution described in the introduction
  (around lines 41-103) are confusing for me.
===> TODO, minor
  - Which blame assignment strategy is "good" and which is not "good"?
===> TODO
  - Why does the paper conclude that "the existing theory does not predict
    practice properly"?  Figure 8 shows that the theory (Natural) works well.
===> TODO
  - What does the paper mean by "The existing practice may need additional
    experiments"?  I have not found any evidence that confirms this claim in the
    paper.
===> TODO
  - What does "practice" means here?
===> TODO, minor
  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
    be nicer to address them in a clearer manner.

===> TODO
- The paper often mixes gradual typing and migratory typing.  It is unclear how
  it distinguishes them.

===> TODO
- In principle, Natural blame should always point out the faulty components
  by the Wadler-Findler slogan.  However, the experiment shows it is not the
  case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
  to a gap between theory and practice, but exposing a reason is crucial.)

===> TODO (common with A: want more discussion of mutants)
- It is not fully explained how the given mutators change programs.  For
  example, I cannot completely predict changes by the mutators deletion and
  class:super.

===> TODO, minor (common with A)
- The paper says that the experiment uses 72,192 sampled scenarios (line 802).
  However, the total number of mutants (i.e., configuration lattices) is 756
  and, from each lattice, 96 scenarios are sampled.  Thus, I guess 756*96 =
  72576 scenarios are used.  Why does this mismatch happen?

===> TODO, minor
- The experiment samples multiple scenarios from a configuration lattice (line
  800).  Does this mean the debugging process may start with a mix-typed program
  and may not with a fully untyped one?  If so, why does the paper take such an
  approach?

===> TODO
- The paper often says that a blame system is (un)sound, but it is difficult for
  me to identify what it precisely means.

===> TODO
- The paragraph starting at line 1122 is quite difficult to understand for me.
  - What does the paper mean by soundness of blame assignment?
  - What does the paper mean by "incomplete population of the blame map"?
  - To understand the idea on the improvements of Transit, more explanations
    on the usage of blame maps in Transient would be needed.

===> TODO
- Some experimental settings are unclear.
  - What "sophisticated typing features" are considered (line 589)?  It would be
    crucial to confirm whether the proposed mutators are enough.
  - The benchmarks are selected (line 647), but why?  The GPT benchmark suite of
    Racket provides more examples.

### Other comments

===> TODO, minor
- It seems that the experiment implicitly supposes a few assumptions on
  benchmarks.  First, the original benchmarks must be fully typed.  Thus more
  experiments on mix-typed programs where some components cannot be typed may be
  needed.  Second, the benchmarks are supposed to be deterministic, which I find
  from the definitions of trails.  If it is the case, while I do not think these
  restrictions have to be lifted in the paper, it would be nice to expose them.

===> TODO, minor
- As an alternative of Transient first and last blame, perhaps determining
  blamed modules by voting (i.e., blaming a module that has been most often
  added to the blame map) might be promising (if the blame map has a large
  population).  Have the authors considered it or another approach for Transient
  blame?

===> TODO, minor
- Perhaps it is valuable for followers to share the experience on developing
  mutators that are not interesting, .e.g., in the supplementary material.

### Minor comments
===> TODO: all otherwise un-marked minor comments below

===> TODO, minor
- Why doesn't the paper adopt the author-year citation style?

- L118: "(at the mid-level on the left)[] imports" (the comma is removed)

===> TODO, minor
- Page 7: How is the question (5) answered?

- L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
  consider clarifying.

- L347 "the rational programmer's effort": This was unclear at the first
  reading.  It would be nice to briefly explain it here.

===> TODO, minor
- L356 "despite advertisements for the opposite":  I cannot find what this means.

===> TODO, minor
- L370 "the latter must represent":  What does the "latter" specify?

===> TODO, minor
- L424 "Let a configuration s of P":  Are configurations the same as scenarios?

===> TODO, minor
- L431 "type-level mistake":  Is it the same as a impedance mismatch?

- L451: "denotes the module [of P] that s blames"

===> TODO
- Are failing Natural blame trails produced even for programs with impedance
  mismatch?  (This question is related to the above issue with Natural blame.)

===> TODO
- L556: It would be helpful to describe how to extend trails and how to
  determine if there is no scenario to be added.  (This comment is also related
  to the issue with Natural blame).

===> TODO, minor
- L557: "if it does" --> "if it does not"?

===> TODO, minor
- L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
  "does not reliably lead to type errors"?

===> TODO, minor
- L785 "the interesting standard guided countless iterations":  I fail to find
  what this intends.

===> TODO, minor
- What is the total number of failing trails in Transient?

- L970: "the experiment provides [an] evidence that"

===> TODO, minor
- L1099 "as behavioral economics has shown more recently":  Is there a reference
  to be cited?

===> TODO, minor
- L1104 "deviating is a mistake":  Why?

===> TODO, minor
- L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?

===> TODO, minor
- L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
  does it intend?

- L1198 "But just because...":  It is difficult for me to find what this
  sentence wants to say.  Please consider rephrasing.
