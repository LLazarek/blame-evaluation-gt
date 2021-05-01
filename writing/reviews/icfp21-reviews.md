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
the paper talks in the introduction of its top-level innovation being the idea of the rational programmer. But then they go on to say that the idea is actually taken from Lazarek. Which is it then?

The table in section 2 looks rather ugly

p5. one thing I wonder about is how this all hinges on the rational programmer not only taking the right action, but also taking it right? Is that exactly what the rational programmer idea presupposes? (I guess this reflects on your remark in Sec. 11 that one problem is with errors in the ascribed types themselves.).

When the rational programmer "fixes" a module and reruns, the implicit assumption seems to be that he runs the program in the exact same way. That would make the most sense to me, but you do not seem to come out and say so.

It would help if you could explain how a mutator like negate-cond actually leads to a program
that has more type-level mistakes than before. It does not seem to me this part of the code
itself will now fail, but that changing the conditions makes certain previously excluded paths in the code feasible.

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

- The conclusions of the second contribution described in the introduction
  (around lines 41-103) are confusing for me.
  - Which blame assignment strategy is "good" and which is not "good"?
  - Why does the paper conclude that "the existing theory does not predict
    practice properly"?  Figure 8 shows that the theory (Natural) works well.
  - What does the paper mean by "The existing practice may need additional
    experiments"?  I have not found any evidence that confirms this claim in the
    paper.
  - What does "practice" means here?
  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
    be nicer to address them in a clearer manner.

- The paper often mixes gradual typing and migratory typing.  It is unclear how
  it distinguishes them.

- In principle, Natural blame should always point out the faulty components
  by the Wadler-Findler slogan.  However, the experiment shows it is not the
  case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
  to a gap between theory and practice, but exposing a reason is crucial.)

- It is not fully explained how the given mutators change programs.  For
  example, I cannot completely predict changes by the mutators deletion and
  class:super.

- The paper says that the experiment uses 72,192 sampled scenarios (line 802).
  However, the total number of mutants (i.e., configuration lattices) is 756
  and, from each lattice, 96 scenarios are sampled.  Thus, I guess 756*96 =
  72576 scenarios are used.  Why does this mismatch happen?

- The experiment samples multiple scenarios from a configuration lattice (line
  800).  Does this mean the debugging process may start with a mix-typed program
  and may not with a fully untyped one?  If so, why does the paper take such an
  approach?

- The paper often says that a blame system is (un)sound, but it is difficult for
  me to identify what it precisely means.

- The paragraph starting at line 1122 is quite difficult to understand for me.
  - What does the paper mean by soundness of blame assignment?
  - What does the paper mean by "incomplete population of the blame map"?
  - To understand the idea on the improvements of Transit, more explanations
    on the usage of blame maps in Transient would be needed.

- Some experimental settings are unclear.
  - What "sophisticated typing features" are considered (line 589)?  It would be
    crucial to confirm whether the proposed mutators are enough.
  - The benchmarks are selected (line 647), but why?  The GPT benchmark suite of
    Racket provides more examples.

### Other comments

- It seems that the experiment implicitly supposes a few assumptions on
  benchmarks.  First, the original benchmarks must be fully typed.  Thus more
  experiments on mix-typed programs where some components cannot be typed may be
  needed.  Second, the benchmarks are supposed to be deterministic, which I find
  from the definitions of trails.  If it is the case, while I do not think these
  restrictions have to be lifted in the paper, it would be nice to expose them.

- As an alternative of Transient first and last blame, perhaps determining
  blamed modules by voting (i.e., blaming a module that has been most often
  added to the blame map) might be promising (if the blame map has a large
  population).  Have the authors considered it or another approach for Transient
  blame?

- Perhaps it is valuable for followers to share the experience on developing
  mutators that are not interesting, .e.g., in the supplementary material.

### Minor comments

- Why doesn't the paper adopt the author-year citation style?

- L118: "(at the mid-level on the left)[] imports" (the comma is removed)

- Page 7: How is the question (5) answered?

- L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
  consider clarifying.

- L347 "the rational programmer's effort": This was unclear at the first
  reading.  It would be nice to briefly explain it here.

- L356 "despite advertisements for the opposite":  I cannot find what this means.

- L370 "the latter must represent":  What does the "latter" specify?

- L424 "Let a configuration s of P":  Are configurations the same as scenarios?

- L431 "type-level mistake":  Is it the same as a impedance mismatch?

- L451: "denotes the module [of P] that s blames"

- Are failing Natural blame trails produced even for programs with impedance
  mismatch?  (This question is related to the above issue with Natural blame.)

- L556: It would be helpful to describe how to extend trails and how to
  determine if there is no scenario to be added.  (This comment is also related
  to the issue with Natural blame).

- L557: "if it does" --> "if it does not"?

- L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
  "does not reliably lead to type errors"?

- L785 "the interesting standard guided countless iterations":  I fail to find
  what this intends.

- What is the total number of failing trails in Transient?

- L970: "the experiment provides [an] evidence that"

- L1099 "as behavioral economics has shown more recently":  Is there a reference
  to be cited?

- L1104 "deviating is a mistake":  Why?

- L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?

- L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
  does it intend?

- L1198 "But just because...":  It is difficult for me to find what this
  sentence wants to say.  Please consider rephrasing.



Review #45C
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
Y. Knowledgeable

Paper summary
-------------
This paper provides a methodology for sistematically analyze blame assignment strategies in gradual typing, and reports on applying this methodology to a benchmark.

Comments for author
-------------------
Thank you for submitting a very well-written paper. It reads well and made my job easier. 
I like the research questions that the paper addresses-- there are a lot of unsubstantiated claims about blame that we take for granted. 
I think it could be easy to misjudge this paper, as in "We kind of knew that blame was useful". To that I reply "Can you point out a scientific reference for that?". We should appreciate these types of studies. Also, when we write grant proposals we are glad to be able to point out a systematic study rather than say "Well.. everybody knows".   
The paper begins with a good recap on the three major blame approaches applied on a concrete example. The table at the end of section 2 is a helpful summary overall. Section 4 spells out the challenges for the reader, and also justfies the use of Racket for this study. I think that the authors are correct, in that Racket is currently the most appropriate system for conducting a study of the like: solid system, lots of programs, and has been used for similar studies such as that of the POPL 2016 paper on evaluating gradual typing performance. My only concern at this point is that we need to believe that the authors have implemented the Transiet semantics correctly in Racket, though no details are offered. The paper refers to another submission, so perhaps some details can be found in there. 
Section 5 proceeds to describe the technical part of the methodology. It is based on the lattice from Takikawa et al. [25] (all possible combinations of typed/untyped modules). Then the paper characterizes the actions of a programmer trying to find type mismatches. It does so with the notion of a "rational programmer", which is a very interesting approach. Its mathematical formalization is based on the points in this lattice that are hit during the search for a type mismatch. These mathematical characterizations are very simple (not in a negative way), I like that they are fitting so naturally. 
I am wondering whether the approach that the paper calls Transient First could be regarded as Transient Early, since it points to an earlier part of the code. Similarly Transient Last could be Transient Late, but you would have to see whether this terminology really fits.  
Section 6 describes the benchmark that has been used for the study, which I am satisfied with, as programs vary in size, features being used, and other aspects. One of the challenges is to generate realistic blame scenarios. To this aim, the paper applies the idea of mutations, previously adopted in other contexts. Fig 6 gives a summary of the mutations applied-- they seem reasonable, and a lot of the mutations are new. The only thing that I am concerned about at this point is that, later in the paper, it is shown that "most cases the programmer need to type a single module to debug a scenario," which makes me wonder whether the authors did not generate difficult enough scenarios. If that is the case, what is the problem? 
Sections 8 and 9 report on the finding of the paper. These include statements that blame is useful, that Natural often produces shorter trails than Transient, and others. My only issue at this point is that the reader is left with numnbers only, while I think a reader would like to see selected examples, for example about which blame labels have been provided by Natural and which by Transient, so to see that one approach took a longer path, as well as similar examples the reader can learn from. Numbers do not seem to teach the whole story in this part of the paper.



Review #45D [R1]
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
X. Expert

Paper summary
-------------
This paper describes a methodology for evaluating different blame
assignment strategies in gradually typed languages by using an automated
"rational programmer," which uses blame information or other error
outputs to attempt to locate the sources of runtime type errors in
gradually typed programs by sequentially replacing untyped modules by
their typed equivalents. This occurs in a framework of lattices of
different configurations of gradually typed programs, from typed to
untyped, which have been mutated to introduce an error in untyped code
that would be detectable statically if it were typed. The blame strategy
is evaluated based on whether the rational programmer can use its output
to find the source of the error, and in how many steps.

The paper also applies this methodology to evaluate and compare several
semantics for gradual typing: the natural, transient, and erasure
semantics, and within the first two, versions equipped with blame
tracking and those that simply error and produce stack traces. The
result is that both natural and transient, with and without blame, are
better than erasure at debugging programs; both systems with blame
tracking outperform the systems without, but the systems without blame
tracking still significantly assist the rational programmer in finding
the source of the error. The paper concludes by considering threats to
the validity of the results: issues in the behavior of the
transient+blame approach, the realism of the rational programmer, and
the realism of its assumption that correct types for modules can always
be produced.

Comments for author
-------------------
I really enjoyed this paper. The core results of a methodology for
evaluating blame assignment strategies, comparing them to each other,
and comparing their presence vs. absence, could become an important tool
in designing gradual type systems, and is a tool that has been sorely
needed in the past. The results it reports on are both important and
interesting: they validate the utility of blame tracking and they allow
a direct comparison between two very different blame tracking systems.
This work also characterizes the importance of /having/ blame tracking
in the first place, which is an under-explored and important question
for researchers and developers wanting to expand sound gradual typing
into domains where the overhead of blame tracking may be untenable.

The paper is well written and clear in its methodology, results, and
limitations. I only have a few concerns about its characterization of
some of the related work. Most importantly, Section 9.3 makes some
strong claims about the behavior of Reticulated Python and the transient
semantics that I'm not sure hold up.

First, the performance figures cited from [38] (worst case 5.4x
slowdown) are used incorrectly: those numbers are about transient
/without/ blame (see Section 6.2 of [38]), and given that "with the
blame map turned off, the Transient semantics also runs these programs
in a short amount of time and well within the memory limits" there
appears to be no contradiction here. When blame /is/ enabled, [38]
reports a worst-case 18x slowdown, which may be more in line with the
results reported in this work, and as a result it's not actually clear
that the earlier results are skewed in a way that demands explanation.

Claim (2), that local variables in Reticulated Python are dynamically
typed because no types are ascribed to them, is false; Reticulated
Python uses a local type inference system to give types to local
variables (see Section 3.1.3 of [36]).

Claim (4) that "on large programs, Reticulated suffers from high
overhead" is unsubstantiated here or in cited prior work, and is also
unclear -- overhead compared to what? The text compares it (vaguely) to
Typed Racket (without specifying which semantics), but this isn't a
useful comparison given the different baseline performance of Racket and
CPython. It's not clear if the point is that performance of this
benchmark in blame-free Reticulated is seriously slow compared to
regular Python, or if the program itself is just slow in Python (in
which case, what's the relevance to this paper?).

The characterization of the Monotonic semantics for references [22] as a
variant of natural isn't correct--its exception-raising and blame
behavior for references (though not for functions) is quite different
from that of Natural. For example, a monotonic reference imported from
untyped code into two incompatible typed contexts will error
immediately, before being used.

