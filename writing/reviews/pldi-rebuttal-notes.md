Response notes format: indented after ⟶ or TODO.
⟶ is a drafted response, TODO records notes about what to respond to or how.

chrdimo notes format: unindented after chrdimo.

addressed: marks todos that have answers in `pldi-rebuttal.md`



PLDI 2021 Paper #66 Reviews and Comments
===========================================================================
Paper #66 How to Evaluate Blame for Gradual Types


Review #66A
===========================================================================

Overall merit
-------------
A. I will champion accepting this paper.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper presents a methodology for evaluating blame assignment strategies
based on the notion of "the rational programmer". The paper develops the
methodology in detail and then then uses it to evaluate three different blame
strategies: "Transient", "Natural" and "Erasure". 

In gradual typing, a blame assignment is used to show where the program went
wrong. Importantly, at least on the theory side, a well-typed component cannot
be blamed. The authors argue that such blame assignment has received a lot of
academic attention, yet industrial implementations of programming languages with
gradual typing completely forego blame. Why is this? Is it because blame is not
useful in practice? The paper aims to answer this question by applying the
developed methodology to three strategies: The "Natural" blame strategy relies
on proxies to perform additional type checks at run-time. The "Transient" blame
strategy inlines these checks and does not perform "deep" type checks. The
"Erasure" blame "strategy" performs no checks: it only fails if execution of the
program fails. 

The contribution of the paper is the methodology for evaluating blame (and
secondary its application to the three strategies.) The idea is to design an
algorithm that behaves as a programmer would debug a gradually typed program
that crashes at run-time with a type error: by incrementally adding types to the
untyped parts of the program until the source of the type error is revealed. In
this way, the question is: Does the blame assignment (or lack thereof in the
case of "Erasure") help guide the programmer? How many "steps" must he take to
find the defect and can he find it at all? 

Trying to summarize the methodology:

- Take a collection of fully typed programs.
- Consider mix-typed versions of those programs.
- Develop mutators (like in mutation testing) that inject type errors.
- Emulate the rational programmer who follows the "trail" in the spirit of
  Figure 5.
  - At each step the programmer decides on an untyped component and then types
    it, reruns the program, and either sees a type error or decides to type
    another untyped component.
- Take into account how the "next untyped component" is selected based on the
  specific system at hand. 
- Experimentally evaluate whether the above method is able to find the fault and
  through how many steps.

Comments for author
-------------------
This paper is exceptionally well-written and was a joy to read. Every time I had
a concern the paper seemed to anticipate it and provide useful commentary. 

The paper does a very good job at describing the challenges of how to evaluate
blame assignment strategies and then sets out to develop an automated evaluation
methodology based on the notion of "the rational programmer". The paper
carefully lays out the foundation for the generation of buggy mix-type programs,
the actions and variants ("modes") of the rational programmer which can depend
on the specific blame system, and finally how to sample the "experimental
space". The paper also includes a clear section on threats to validity.

I think it is important to have a methodology to evaluate the usefulness of
blame, and in lieu of large-scale and costly user studies, I think this paper
provides a very well thought out automatic approach (with the natural
limitations that such an approach must inevitably have.)

Points for:

- Addresses a problem that has not been addressed before.
- Clear, concise, and meaningful methodology.
- Clear description of the threats to validity.
- Very well-written paper.

Points against:
- none

# Questions

- Could you comment a bit more on what makes a mutator "good"? (other than
  experimentally seeing that it is good.) Perhaps, what would be a really bad
  mutator for evaluating blame assignment?

  
  ⟶ Abstractly, a good mutator reliably generates bugs that are detectable by the type system and by all three semantics' runtime type checks; ideally the bugs also manifest during the interaction of multiple components.
    We rely on the experimental check of our mutators since this criteria is difficult or impossible to decide otherwise.
    That said, we can reason that some mutators are clearly unlikely or impossible to be good.
	...
	We will extend the discussion at the start of section 4 to clarify this point.

  addressed
  TODO: Rewrite response along these lines...
    A common mutator in the literature is swapping constants of the same type, but this is not a good mutator for us because type system can't catch it.
	Example of an obviously bad mutator: replacing any string with some pre-defined non-empty string (e.g. "foobar") would be a bad mutator.
	Examples of mutators we cut: `wrap-cond` (tricks occurrence typing by hiding the result of conditionals to produce a type error, but never a dynamic error), `add-extra-method`, swapping `car` and `cdr`, `set!`ing an ID to itself (only causes a type error), swapping mutable and immutable hashes+vectors.
	# This answer should serve to justify our mutators and the challenge of making good mutators more broadly, to answer reviewer D's concerns about the delta of making mutators.


chrdimo: A bad mutator results in a type-level bug that is trivial to
locate. For instance a bug that leads to a type-level error during the
evaluation of the component that contains it is trivial to locate. In
contrast, a bug that is non-trivial to locate is one  that causes the
component that mutates it to evaluate without an error but produce instead
a value of the wrong type.  An interesting bug is one where the value with
the wrong type manages also to cross undetected to at least another
component than the one that produces it until a type-level checks flags
it. These latter bugs are those that good mutators should be able to
produce. 

- I don't understand where the 16,800 number comes from. Is it an exhaustive
  enumeration of all possible mutations? (Or the number of mutants constructed
  in X amount of time?)
  ⟶  The former, we will clarify in the text.

chrdimo: It is an exhaustive enumeration. We will clarify the prose. 

- How large are the programs in the benchmark suite?
  ⟶ The program sizes without type annotations, as reported by Racket's `syntax-sloc` tool, are below. We will add this information to the table in Figure 2.
    benchmark | LOC | components
	----------|-----|-----------
	acquire | 1941 | 9
	gregor | 2336 | 13
	kcfa | 328 | 7
	quadT | 7396 | 14
	quadU | 7282 | 14
	snake | 182 | 8
	suffixtree | 1500 | 6
	synth | 871 | 10
	take5 | 465 | 8
	tetris | 280 | 9


chrdimo: Please find below the loc and number of components for each
benchmark. We will add the information to figure 2.

   benchmark | LOC | components
	----------|-----|-----------
	acquire | 1941 | 9
	gregor | 2336 | 13
	kcfa | 328 | 7
	quadT | 7396 | 14
	quadU | 7282 | 14
	snake | 182 | 8
	suffixtree | 1500 | 6
	synth | 871 | 10
	take5 | 465 | 8
	tetris | 280 | 9


# Minor Comments

- L38: Should there have been a colon? Is the text "This contrast... " and
  challenge quote from [3]? Who is saying what here...
  ⟶ Thanks, we will reword the text from "This contrast" to clarify that this our research question, it is not coming from [3].


chrdimo: No, the quote is not from [3]. We will rephrase to clarify that
it is our statement of the research question. 

- L104: I was confused by the word "Next". Maybe instead: We now ..."
  ⟶ Thanks, we will change the wording.

chrdimo: Thank you. We will reword.


- L268: Grammar issue.
  ⟶ Thanks, we will fix this.

- L314: Floating period.
  ⟶ Thanks, we will fix this.

- L441: Add line numbers (or other metric of size).
  ⟶ See above, we will add this information.

- L980: Figure 5.4??
  ⟶ Thanks, this is a bad reference; it should be figure 6. We will fix it.
  
chrdimo: We will fix these. Thanks.  


- L956: 30,000 hours of compute time is 1,250 days. I assume that the CPU has
  many cores. Could you put that number?
  ⟶ Each CPU has 28 cores and 56 threads; when we last ran the experiment, it took about two weeks of real time from start to end. 
  We will add this information at the start of section 7.

chrdimo: Sure. Each CPU has 28 double cores.




- L991: Figure 6: It took me some time to understand the figure. I wonder if
  there is a better way to represent the same data?
  addressed
  TODO: come up with some thoughts for this figure
  ⟶ Thanks, we can see how it's difficult to read. We will consider some different presentations, such as...
    - a bunch of two-sided bar charts? Where there's one bar pair per pair of modes. Above the x-axis is % A > B, and below the axis is % B > A?
	- a bunch of stacked histograms? Where there's one stack per pair of modes. A bar has three parts: A > B, A = B, and B > A.

chrdimo: We have reached the same conclusion and we are considering
alternatives such as a series of two-sided bar charts that make it easier
to spot how two modes compare to each other. 

Review #66B
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
Y. **Knowledgeable** = "I follow the literature on this paper's topic(s)
   but may have missed some relevant developments"

Paper summary
-------------
The paper proposes an evaluation method for blame assignment
strategies and evaluates three different strategies with the
method. While many theoretical gradual typing systems have adopted
blame, blame has been neither shown to be practically useful nor
applied to real-world languages because the systematic evaluation of blame
is difficult. The authors introduce a notion of rational programmers
and simulate their behaviors to evaluate blame. A rational programmer
is assumed to iteratively add type annotations to an untyped portion
of his or her program according to the feedback given by a run-time
system. This behavior can be simulated even without real human
programmers. If a run-time system leads a programmer to the correct
locations of a bug, its blame strategy can be considered more useful
than a strategy that cannot do so. The paper compares the Natural,
Transient, and Erasure strategies. The result shows that both Natural
and Transient outperform Erasure, which does not assign any blame,
and, therefore, implies usefulness of blame assignments.

  TODO: This seems to imply that we are proposing or assuming the way that real "rational programmers" would behave. Should we try to answer or clarify about that?

Comments for author
-------------------
This paper tackles a long-standing difficult problem, which is the
problem of evaluating blame. Many existing gradual typing papers
provide blame systems, but because the papers do not evaluate blame
systems, it is not obvious whether blame itself is indeed helpful for
debugging. Among many theoretical gradual type systems adopting blame
assignments, no paper discussed the evaluation of blame systems. It is an
important problem because, without any evaluation, theoretical aspects
like gradual guarantee and the blame theorem may not be applicable to
practical systems combining static and dynamic typing.

This paper shows that blame assignments do help programmers, and it
may serve as a first step to close the gap between the theory and
the practice of gradual typing.

The experiment results show that there are debugging scenarios where
Transient is more useful than Natural. Does it happen simply because
of luck? Or, does it imply that Transient is more suitable than
Natural for a particular kind of bugs for some reason? Also, can there
be a new blame strategy that outperforms both Transient and Natural
all the time?

  addressed
  TODO: Response idea: This is an example where the experiment seems to contradict the theory.
	The models don't include all of the features in the benchmarks. <enumerate them here> It's possible that the Natural, Transient theories don't scale as expected to rich PL features.
	Make analogy to physics: theory makes predictions, we have found failures of the predictions. We have two courses of action: report the inconsistency to re-eval the theory, and do more experiments.
	It's also possible we have a bug in TR, or in Transient implementation.


chrdimo: We were also surprised to discover that transient outperforms
natural in some scenarios. After all, the theory of blame says otherwise. Even thought
we haven't identified specific patterns, we believe that the reason behind 
transient's unexpected success is due to the advanced features of Typed
Racket's type system that haven't appeared in models that compare Natural
and Transient: polymorphism, row types, mixins etc. Such features require 
custom loopholes to be compatible with Racket idioms. In turn these
loopholes can result in subpar checks in Natural. Our experimentation with 
transient indicates that the same problem doesn't occur in transient. 
We do not have any clues about a strategy that outperforms both Natural
and Erasure. But our current conjecture is that a simple strategy that
just points somewhere in the blame trail may be good enough and enable
downstream optimization that are keeping track of blame in Typed Racket
inhibits. The good thing is is that now we have a method to compare
different strategies. 

chrdimo? We need more details from Ben here.


Theoretical gradual type systems usually allow programmers to control
the precision of type annotations in a fine-grained way with the Dyn
type rather than distinguishing typed and untyped modules. Would the
same evaluation method be able to directly appliable to blame in such systems?

  addressed
  TODO: The answer is yes, but this needs more thought to frame the answer the right way.
  ⟶ Our evaluation framework should be directly applicable to blame in gradual typing systems using Dyn.
    The details of how it applies depends on the granularity of the blame information offered by the system -- in other words, what the system considers components.
	If components are modules, as in Typed Racket, then nothing changes.
	Systems with Dyn more likely treat top-level definitions as components, in which case definitions are individually typed (the whole definition is annotated) rather than modules.

chrdimo: Yes. Incorporating type Dyn boils down to considering a hierarchy
of types for each component rather than a single type. At the same time,
our method is parametric to the ``size'' of a component and adpats in a
straightforward manner to a setting where each definition can be
considered a component instead of a whole module. Of course both of these
adjustments lead to significantly larger scenario lattices than the ones we
investigate and they have to be sampled aggresively and carefully.


Please explain what impedance mismatches are in the paper. Is it
simply type mismatches?

  ⟶ Impedance mismatches are mismatches between a piece of code's true type and its annotation.
    For instance, the code `(λ (x) (+ x 42))` has type `Number -> Number` but might be annotated as `Number -> String`, causing an impedance mismatch.
    We will adjust the text to clarify or remove this terminology.


chrdimo: An impednace mismathc denotes a type annotation that doesn't
match the actual type of the annotated code. We will adjust the prose to
clarify. 

# Strengths

* The paper is well-organized.
* It proposes a novel solution to a long-standing difficult problem.
* Threats to validity are discussed in detail.

# Minor comments

The paper states that the effort distribution of the random mode
follows a normal distribution, as expected. Please explain why a
normal distribution is a reasonable expectation. When there are $n$
untyped components, the random mode randomly selects one of them, and
only one of them is a buggy component. Therefore, in the first
iteration, the random mode reaches the end of the trail with the
probability of $1/n$. In the second iteration, there are $(n-1)$
untyped components and the probability of a failure at the first
iteration is $(n-1)/n$, so the probability of effort $2$ is $1/n$
again. In this way, it seems to be that the probability of effort $i$
is $1/n$ where $1<=i<=n$ and $n$ is the number of initially untyped
components. Each trail has a different number of untyped components,
so the resulting distribution would be the average of such
distributions. Based on this reasoning, it is unclear why the
distribution is a normal distribution. This point does not affect the
contribution of the paper, but it is not clear from the paper.

  addressed
  TODO: response sketch: you're right, we misspoke, this is not the normal distribution; our reasoning for why the normal-ish shape of distribution we see makes sense is that given a fixed trail the probability at each step of the trail ending is 1/n, but we randomly select trails so we're multiplying the random distribution of trail lengths by that 1/n one? Not clear on the details of this reasoning.
  ⟶ 

chrdimo: You are right that for a given trail of length n the probability
that the random rational programmer discovers the buggy component after k 
attempts is always 1/n and independent of k. However we sample randomly
from the set of sceharios and as a result the set of trails. So the curve 
of the random programmer matches our sampling distribution. That said, as
you observe, what is the distribution of the random programmer is an
irrelevant point to our results and we will remove the characterization.


* page 9 line 980: Figure 5.4 -> Figure 6
  ⟶ Thanks, we will fix this.
* page 10 Figure 7: Null -> Random
  ⟶ Thanks, we will fix this.
* page 11 line 1103: ( 770 -> (770
  ⟶ Thanks, we will fix this.

chrdimo: Thank you. We will fix all these. 


Review #66C
===========================================================================

Overall merit
-------------
C. I would not accept this paper but will not argue strongly against
   accepting it.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper develops an approach for empirically evaluating the effectiveness of blame assignment in gradually typed languages. Roughly speaking, the idea is to model the steps taken by a "rational programmer" who systematically debugs errors in a gradually typed programming. The question is whether different blame assignment strategies are able to help this rational programmer find the "true" source of errors faster, requiring fewer steps.

To evaluate this approach, the paper also presents empirical results. Starting with a small corpus of programs, the authors use mutations to systematically (but synthetically) introduce type errors -- e.g., one mutation replaces a constants with another of a different type, and another mutation swaps method identifiers. They then conduct experiments to quantify the number of steps required by a rational programmer for several blame assignment schemes: Natural (used in Typed Racket, which tracks at most one location for blame), Transient (used in Reticulated Python, which tracks a set of locations), and Erasure (which does not actually assign blame, and is widely used in industry).

  TODO: do we need to clarify that effort is a secondary aspect of the evaluation, and more important is whether blame leads to the bug at all?

Unsurprisingly, the evaluation shows that blame is effective, when used by a rational programmer. The results are inconclusive, but some trends are clear. Most significant, all of the schemes that assign blame are more useful than Erasure. And Natural blame more often more useful than Transient blame.


Comments for author
-------------------
This is a neat paper that develops an approach for evaluating gradual typing and blame assignment. Thanks for submitting it to PLDI '21! The results are presented in an extremely clear way that draws out the key insights without compromising on technical detail. The notion of a rational programmer -- loosely modeled on rational agents from economic theory -- is a neat conceptual device. And the empirical approach does seem like the right way to address the question of the usefulness of blame. 

But while there is a lot to like about the paper, it also has some shortcomings. The suite of programs used for the evaluation is fairly small (c.f., the SIGPLAN checklist).
  
  addressed
  TODO: respond to this ^, saying that we use the standard suite of benchmarks for evaluating GT -- point to Ben's POPL paper and others, and we use all of the largest benchmarks in the suite

chrdimo: Coupld you please clarify what you mean by fairly small with
respect to the SIGPLAN checklist? We are the first to conduct an empirical
study for blame and gradual typing so we repurposed the standard
performance benchmark suite for Typed Racket and, in particular, its
largest and most complex benchmarks. Our inspection of the benchmarks
indicates that they are representative set of Racket programs in terms of
features and structure. 

The faults are introduced using synthetic mutations that may or may not correspond to the kinds of errors that arise in practice. This can be seen from the results in Figure 7 -- the lengths of the "trails" is quite small.

  addressed
  TODO: unclear that syntheticness relates to trail length at all, and synthetic bugs are the only option here -- there is no corpus of documented GT bugs in the wild anywhere. Indeed, most type errors never even make it into code repositories. Note that we are the first to evaluate blame in this setting.

chrdimo: There is no exisitng catalogue of bugs in gradually typed
programs. Furthermore, most type-level bugs do not survive after decent tsting and 
thus do not make it to code repos. There are a few anecdotes from the literature but they are mostly
artificial. Hence, since we need a large number of buggy scenarions for a
meaningful study, a synthetic approach is the only way forward. In
addtion, with our mutation we can tune the set of scenarios to get a
diverse set of bugs. 

While the study is mostly well done, it only considers two blame assignment strategies.
For example, it would be interesting to consider the "omniscient programmer" to get a bound on the value of blame. But this is not done.

  addressed
  TODO: We don't understand what is meant by "omniscient programmer", but our best understanding is the ideal oracle strategy that always picks the right module right away and thus always finds the bug in one step. We didn't show this because it's trivial. We do have other baselines to understand blame's performance: the random mode, and the exceptions modes for each of the semantics.

chrdimo: Could you please explain what you mean by "omniscient
programmer"? If this is the programmer that always makes the best choice,
i.e., finds the bug at the first try then this programmer is trivial. It
has 100% success rate and a constant blame trail of 1. Thus comparing it
with any strategy amounts to the number of scenarios where a strategy
fails and those where it succeeds but the blame trail has length greater
than one. This information is in the paper byt If you think it would be
useful, we point it out. Moreover, the experiment considers 10 different
modes of the rational programmer spread across 3 different gradaul typing
systems. 


And unfortunately the results are inconclusive -- Section 8 says "our results call for a deeper understanding of the two models for blame".

  addressed
  TODO: This is what makes our results surprising: they challenge the theoretical conclusions that Natural is superior to Transient, suggesting that the theory is not good enough to understand the pragmatics of blame in reality.


chrdimo: The quote from section 8 refers to the theory of blame rather
than our results. Our results show in clear terms that, at least for our
benchmarks, a blame strategy is superior that exceptions and that natural
and transient have comparable efficacy. The latter is surprisng given that
theoretical models (and our intiail expectations) point to the opposite.
Thus a side-effect of our experiment is a call to theoriticians for models
that are atuned with practice. 

Also, the published Transient scheme seems to have a minor bug, which might affect the outcomes when fixed (Section 8.2).
  addressed
  TODO: 8.2 does not indicate a bug in Transient, but rather a defect in the design of the system. ... something about a gap between Reticulated Python and 

chrdimo: Section 8.2 does not discuss a bug in Transient but rather a
limitation of its current design that we discovered exactly becasue of the
experiement.

Most if not all of these shortcomings are discussed in the paper, which is much appreciated. But they do risk undermine the value of the contributions to some degree.

chrdimo: We would like to point out that the resutls of the comparison
between the tree systems are only one part of the contribution of the
paper.  The other part is the method itself. Benchmarks, source of
interesting bugs and rational programmer modes are all parameters that can
be instantiated differently without affecting the recipe we discuss at the
end of section 2 nor the analytical framework from section 5 that
underlies our experiment.  

To explain one comment above: I was surprised not to see a comparison against an "omniscient programmer" to establish a baseline (c.f., the SIGPLAN checklist). That is, model a blame assignment scheme in which blame could be placed on any component. And always pick the component that minimizes the length of the trail. Of course, this would be infeasible to implement in practice, but it would place an upper bound on the utility of blame. 


chrdimo: Our experiemnt does have a baseline. The random mode of the
rational programmer serves as the ``control'' of our experiment. 
Furthermore the exception modes serve as further ``controls'' to
distinguish the effect fo checks from that of blame, 

Similarly, I wondered why the modes of the transient programmer only consider picking the first/last blame, and not other choices. Section 5.2 just says "our answer" is that there are at least "two reasonable options." Why are other choices not reasonable?
  addressed
  TODO: Answer suggested by Ben: it's unspecified how one should interpret Transient blame, and there isn't time to evaluate all possible choices, so we pick the most reasonable interpretations.


chrdimo: The transient semantics provide no interpratation for the blame
set. We picked the first and last because they match the intuitive
interpretation of the blame set as a list. We didn't mean to imply that
other options are unreasonable. We just needed to make a reasonable choice 
and keep the duration of the experiemtn within reasonbale limits.


Review #66D
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
Y. **Knowledgeable** = "I follow the literature on this paper's topic(s)
   but may have missed some relevant developments"

Paper summary
-------------
Several contract and gradual type systems provide mechanisms for
assigning blame: When encountering a run-time error signifying the
violation of a contract or type signature, which pieces of code should
be scrutinized by the programmer in seeking a fix?

In a recent POPL 2020 paper, Lazarek et al. provide the first attempt
to evaluate if and how blame assignment may help in actual debugging.
In that paper, bugs are systematically injected (via syntactic
mutation) into a suite of Racket programs, and the corresponding blame
trails are analyzed to see whether they, by writing more precise
contracts to "shift" blame from the assigned component, eventually
lead to the actual bug.

The current paper extends the POPL 2020 methodology from contracts in
Racket to gradual (or migratory) typing in Typed Racket, in several
steps:

First, to complement the "Natural" higher-order contract system of
(Typed) Racket, this work implements a "Transient" blame assignment
strategy, which unlike Natural inserts only shallow run-time checks.
(To date, Transient has appeared only in the separate design and
implementation of Reticulated Python, which prevents a head-to-head
comparison as desired in this work.)

Second, compared to POPL 2020, new mutators are defined to inject the
kinds of type-mismatch bugs that may be caught by the gradual type
system and corresponding run-time checks and blame assignment.

Third, the paper runs a large experiment on 10 benchmark programs,
inserting mutations and analyzing whether the blame trails produced by
each debugging mode --- following (a) the component blamed by Natural,
(b/c) the first or last component, respectively, blamed by Transient,
or (d) an exception raised by the underyling language --- conclude
with the actual bug. Results show that Natural, Transient-First, and
Transient-Last all produce short successful blame trails (typically of
length one or two) on most debugging tasks, and that they are all
similarly effective with no clear winner for all tasks.

Comments for author
-------------------
This work advances a compelling approach to evaluating a usability
question through a systematic exploration of synthetic bugs and fixes.
The large-scale experiment is impressive, and I find both of the main
results surprising:

1. That Natural is not much more effective than Transient. I thought
the shallow checks of Transient would certainly be less effective than
the full checks of Natural. It will be interesting to see if and how
this holds up in subsequent usability studies, both with synthetic and
real users and tasks.

2. That Transient --- as implemented in Typed Racket --- is much
slower than Natural. This also goes against intuition, as well as
previous results reported about Reticulated Python (but apparently
there were flaws in those experiments, as noted in this paper). Given
that the experiments are developed and run in the context of the
large, real-world Racket ecosystem, I wonder whether about the chance
of incidental implementation choices or bugs affecting this result.

  TODO: We should clarify that checks alone is faster, but adding blame is slower (bc blame map is linear).


Overall, I think this is a useful reference for further usability work
on contracts and gradual type systems, and am generally in favor of
acceptance.

At the same time, however, I believe the paper overstates the novelty
and challenges to "bring [the POPL 2020 approach] to the world of
gradual typing" (L1252). Comments about each of the three main
components of the methodology in turn below.


*Natural and Transient Under the Same Roof (Section 3)*

This seems to be "only" an engineering challenge.

And the presentation in this section is rather informal. As someone
with passable knowledge of Natural and Transient, I would have liked
to see a concise formalism with clear "knobs" for choosing between
blame assignment strategies. Trying to imagine readers with less
familiarity, I wonder how well this discussion serves as a primer.

  TODO: this is a minor part of the paper (not a result), there is a large gap between the model and what's needed to scale to a full lang -- it's not clear from the simple model how to design check insertion and blame tracking for new features. This required research, will be published elsewhere


*Custom Mutators (Section 4)*

The paper suggests that the mutators required here are significantly
different and more subtle, but this does not seem to be the case when
comparing Figure 3 and Table 3 of the POPL 2020 paper.

Of the 12 (of 16) gradual-typing related mutators listed in Figure 3:

* `constant`: Similar to previous, but now with a type error
* `deletion`: Similar to previous
* `public`: Similar to previous "hide-method"
* Four others involving swapping identifiers: Similar in spirit to
"swap argument" in previous

Of the 4 (of 16) Typed Racket related mutators:

* `arithmetic`: Simpler version of previous
* `boolean`: Same as in previous
* `negate-cond`: Same as in previous

So there is quite a bit of similarity. And on the flip side, why are
not all (8) mutators from the POPL 2020 experiment included here?

These questions are not so important in an absolute sense; _some_
specific mutators need to be chosen to generate some bugs, and these
seem to work fine. But they are salient given how much the paper
emphasizes the new mutators as a contribution.


*Debugging Strategies (Section 5)*

These also seem to be overemphasized. The main design choice seems to
be in how to use Transient blame, which reports multiple components,
and picking the first and last are pretty intuitive.

  TODO: the analytical part is important: we described all of these things in a common framework, allowing for comparison

I'm also not sure why the exception mode is needed in addition to
Erasure (I was confused in Section 2 L204-215 and Section 5.1 L732).
Indeed, Section 5.3 defines the Erasure mode the follow the Natural
exceptional mode.

  TODO: the exception modes are a key part of the analysis framework, enabling understanding of the value of blame separately from checking


*Additional Comments and Typos*

L69: I wish here, and elsewhere, the comparison to [13] had been made
more explicit. As discussed above, there seem to be more similarities
than suggested.

L92: "use of _a_ higher-order contract system"

L155: "authors extensive"

L253: "exports _it_ as"

L280: Missing `untyped-`

L314: " . "

L382: "Dyn,"

Fig 2: Presumably authors were included to indicate the variety of
sources, but I'm not sure that was necessary.

L597: "make us_e_ of"

L664: "described in _Section_ 4"

L980: "Figure 5.4" ==> "Figure 6"

L986: "2.8%" ==> "2.18%" ?

L1017: "33%" ==> "26%" ?

L1304: "Problem is ...."



Review #66E
===========================================================================

Overall merit
-------------
C. I would not accept this paper but will not argue strongly against
   accepting it.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper seeks to establish (or refute) the benefit of blame tracking in gradually typed programming environments. It takes several Typed Racket benchmarks and considers multiple approaches to blame tracking. The central idea is to use a fault injection method to create variant programs along two dimensions: type errors inserted according to some crafted heuristics, and static typing selectively removed on a component-by-component granularity. This creates a large number of variant programs which may produce run-time type errors with blame; this is then iterated to simulate a "rational programmer", i.e. such that blame is used to select where more static checking will be added next; at any given iteration this may or may not catch the injected error, and the number of iterations is counted as a proxy for effort. A further contribution is implementing the different forms of blame-tracking in the same framework so that they can be compared. The finding is that blame is found to offer some positive advantage in effort (over simply being guided by the stack trace of a run-time exception) in 9--13% of the initial sample of variants, with some minor differences between the forms of blame tracking tried.

  TODO: misunderstanding: we propose a methodology
  ⟶ The primary intended contribution of this paper is the methodology, and the experimental results serve as secondary contributions that validate the methodology's utility.
    We will rephrase the introduction to further emphasize that focus.
	Furthermore, we would like to emphasize that the two key findings in the experimental results are that 1) both Natural and Transient's dynamic type enforcement with blame tracking improve significantly over Erasure, and 2) Natural's blame tracking is not significantly better than Transient's, despite the theoretical indications to the contrary.

Comments for author
-------------------
I commend the authors for asking the question. Their method is clearly the product of much careful thought. I understand the difficulty in doing experiments about errors that tend not to hit 'real' source code repositories, and I appreciate the effort taken to re-create the different approaches to blame in a comparable setting.

However, I don't find the results to be as conclusive as the authors state. Aside from the Erasure case, whose relevance I found unclear (see below), conclusions rest on some small-valued "more useful than" percentages. These are pretty hard to interpret, because the "usefulness" metrics necessarily build in a lot of simplifying assumptions. E.g. in 5.4, the "percentage of scenarios where ... is more effective" doesn't account for *extent* of difference, and the trail-length "programmer effort" metric in 5.5 is also unlikely to be a great proxy. These limitations are understandable, method-wise, but overall the results are fairly null... they let us continue believing "what we'd expect" but not with any great sense of added confidence. That doesn't entirely diminish the work, of course, but I'd say the current write-up overplays its hand a little. I appreciate the discussion of threats to validity.

  TODO: misunderstanding: the results are not at all "what we'd expect" based on the theory
  ⟶ The status quo of what we would expect based on the current theory of Natural and Transient is that 1) either kind of blame tracking should be better than none, and 2) Natural should provide significantly better blame information than Transient and Erasure.
    Our results validate the first expectation that blame provides benefits over plain exceptions.
    Surprisingly, our results contradict the second expectation and suggest that in fact Natural and Transient offer comparable blame information.


The presentation of the results is very much around aggregates and summaries. Indeed the whole method is about having run a huge compute job over a large number of variants. Perhaps it's paranoia but I'd be interested to see some specific examples walked through by hand (in the paper) and some smaller sample manually classified (as results). That would add an extra sanity check that the metrics do correspond to some meaningful reality.

About Erasure: if I'm reading this correctly, the key thing here is that in Typed Racket, the set of static types is more expressive than the set of dynamic types. For example, there is a static notion of non-negative integer that is distinct from plain integer. This sort of design isn't universal -- in some languages/systems static types closely mirror the classification of objects in the language's dynamic semantics. Writing these stricter contracts into a program brings its own benefits, separate from blame or indeed from static checking. It feels like the paper doesn't take enough care to distinguish the two effects: the effect of systematically applying more refined contracts over program values, and the effect of gradually enabling static checking of those contracts (iteratively, guided by blame). The authors do mention this around line 204, and the comparisons in Figure 6 between "_ exceptions" and "Erasure" seem to be measuring this -- the gain from checking these extra annotations, with the more precise and/or more timely checks that they imply, relative to the erased case where only the language baseline contracts are checked. Indeed that's the point of 'exception' experiments. Since the biggest effect sizes on display are these ones -- between Erasure and anything else -- this seems at best distracting. I'd be glad to hear from the authors if I'm misunderstanding anything here. In the detailed comments below, I have noted some places in the text where it would be useful to remind the reader that this design property of Typed Racket is at play.

  TODO: misunderstanding about gradual typing as a whole, and the point of the exception modes
  ⟶ The idea of the Erasure semantics is not related to the expressiveness of the type system or how the types relate to kinds of runtime values.
    Rather, the key idea is that Erasure does not enforce type annotations with dynamic checks at all.
	Running a gradual program using the Erasure semantics is the same as stripping all annotations and running the resulting (completely dynamic) program.
	Thus, Erasure programs can only raise exceptions from either the checks performed by the language's primitive operations (e.g. `+`), or from the program itself raising an exception.
	In contrast, the other two semantics do enforce type annotations dynamically; both Natural and Transient insert dynamic checks verifying that the shape of values at runtime match their static annotations.
	Thus, in addition to exceptions, gradual programs using these semantics may raise a second kind of error: a dynamic type check failure.
	These dynamic errors can include blame information if the dynamic type checks are designed to track blame, or they can forego blame and just offer a stack trace.
	In the case of Natural, these two options are captured by the "Natural" and "Natural exceptions" modes, respectively.
	Thus the "Erasure" mode represents a completely different kind of checking (and thus error reporting) compared to both the "Natural" and "Natural exceptions" modes (the two of which share the same kind checking, but different error reporting).


It's interesting to note that there seems to be no convincing analogous experimental or modelling-based justification for plain old static typing, i.e. showing that it somehow presents a net gain to the programmer. Rather, this has simply been posited/assumed by a very long line of work. I am not defending that state of affairs, but it points to the difficulty of showing conclusively that something truly helps programmers. I can see value in this sort of simulation-style approach, and it is no less convincing than user studies. So I'd be interested to hear from the author(s) if they have any more arguments that (1) I've underestimated the results' conclusiveness, or (2) this is a novel/interesting family of methods that might be pursued more widely, or (3) that deeper experiments building on these ideas might yield more compelling insights.

  ⟶ (Matthias) As posed, the question expresses a significant mis-characterization of
    the submission. It is not the benefits of a gradual type system that
	are in doubt. The experimental setup keeps the type system constant
	but allows to answer the question which of several run-time checking
	regimes (for enforcing type consistency) provides the best explanatory
	messages in case of violations.
	Hence the analogous question for purely static type systems would ask
	which of several reporting schemes provides the best explanatory
	message for type-errors. In the case of simply typed languages and
	even language with local type inference, this question is basically
	meaningless. In the case of languages with HM type inference, the
	question has been implicitly raised for four decades with the
	development of alternative ways of finding the source of inference
	conflicts.
	If the reviewer is indeed interested in the question of whether type
	systems help programmers---a question that this submission does _not_
	ask---the recent OOPSLA literature contains several user studies. A
	simple Google query will suggest a short list of these publications.


Detailed comments:

  TODO ll: added responses to anything below that seems to warrant more than "Thanks, we'll fix it".

In the title, "evaluating blame" reads oddly. It is ambiguous, because "evaluate" sometimes means to compute a result. A fuller phrase, like "evaluating the usefulness of blame tracking...", would probably be worth the words.

The abstract doesn't say much about the work. It would be better written for experts to quickly gather an overview what the paper contributes.

Conversely, the main body of the paper skimps a bit on background. It never explicitly covers what "blame" means and how it works in practice. Similarly, it repeatedly talks about "impedance mismatches" without defining them. Perhaps this phrase is now standard in the gradual typing literature, which I haven't kept up with (hence my Z expertise). But I did read the Wadler/Findler paper carefully at the time. It did not talk about impedance mismatches. In any case, it is a fuzzy metaphor... please say exactly what it means here.
  ⟶ We will adjust the prose to more explicitly describe blame and impedance mismatches.

line 34: "then their compilers remove types and rely on the built-in safety checks of the underlying language to catch any problems". This reads oddly in context. Didn't they just do a bunch of static checking? So they are *not* relying just on dynamic checks to catch problems? Maybe there is something more accurate to say here... e.g. no dynamic check is removed, or something like that.

  ⟶ The static checking performed by languages using the Erasure semantics (like Flow, Hack, and TypeScript) is done in a best-effort fashion; the fact that a program passes type checking has absolutely no effect on how the program runs.
    These languages literally erase the types to obtain a program of the underlying language (e.g. TypeScript or PHP) to run.
	Those underlying languages perform all of the dynamic checks that a typical dynamic language performs for primitive operations (e.g. that `+` is only used with numbers).

line 38: "explicit statement and challenge" -- what is it?
  ⟶ The challenge questions whether blame is useful at all in gradual typing.
    We will clarify this in the prose.

line 49: I agree they got the word wrong, but you should explain this

line 60: around this paragraph the writing started to grate. There's no need to generalise about what people do or think, and the "As a matter of fact... simply..." style is somewhere between laboured and patronising. It's better to more plainly state the gap in the literature that you're addressing. Throughout the paper, much space could be saved by writing in a more direct style.
  ⟶ Noted, we will simplify the language where possible.

line 69: be explicit that Lazarek et al were (as I later gathered) doing something about blame to do with higher-order contracts but not gradual types

line 70: what does it mean to "follow" the slogan? Was unclear to me.
  ⟶ The following sentences are meant to explain this, so we will adjust the prose to make that clear.

line 90: at first I wondered: what is a case? Maybe say "program variant" to foreshadow the idea of generating mutants etc?

line 91-ish: "Transient, "Natural", "Erasure" -- be explicit that these are names that *you* are introducing
  ⟶ These are names from the Gradual Typing literature (see for instance [9, 10, 37]).

line 94: "forego" => "forgo"

line 119: "homo economicus" needs glossing or a reference

line 125: was wondering whether "impedance mismatch" just means "feasible run-time type error".

line 148: last sentence appears to contradict the preceding paragraph, and is left hanging oddly. Instead, make it the start of a new paragraph.

line 160: "disparity" is a strange word here. "diversity"?

line 170: "Otherwise, results from..." -- the point was already clear

line 174: ... instead of just saying what you built on, first say what you did! It's really not clear at this point.

line 187: what is a "component"? Should be easy to define.

line 199: "blame set as another form of a stack trace" -- yes. I was hoping to see a clearer example of debugging with and without blame, i.e. something making explicit the similarities and differences between having blame info and having only a stack trace at the error site.

line 211: "languages exceptions" typo

line 268: this explanation of the workings of proxies seemed overwrought

line 277: what is "responsibility" of a "party", exactly? Are "party", "module" and "component" all the same things?
  ⟶ Components are parties, and in the case of Typed Racket modules are components because they are the units of migration. Other gradual type systems, like those that do not require annotated entire modules, may consider smaller units of code (e.g. definitions) as components.
    We will clarify this in the prose.

line 283: "rewrites typed modules to inline checks" -- so a module gets turned into a check? Clearly not, but that's how it reads

line 293: I was wondering what constitutes a boundary crossing. Clearly, passing by function call or return crosses from the caller's module to the callee's. What about values exchanged through reads/writes to shared state?
  ⟶ Yes, communicating a value via state also constitutes a boundary crossing.
    We will clarify what a boundary crossing is in the prose.

line 299: "crosses" => "crossings" (probably)

line 313: it threw me that a program might not fail but "produce a wrong result". If it could be caught by the gradual type system, why can't it be caught at run time? My best get at explaining this is by what I wrote above, i.e. it's a consequence of Typed Racket's more refined static notion of type. In certain other systems this wouldn't be possible, because the static checker would only catch (albeit earlier) errors that would be caught at run time, so there would be no basis to call the result "wrong".
  ⟶ The result can be wrong in that it doesn't agree with the annotations (which we assume express programmer intent), even if none of the primitive operations applied to the result raise an error.
    For example, imagine the the client saves the result to a file.
    The language primitives for writing values to a file (typically) can write any kind of value to a file, so they have no checks that could fail due to the result being of the wrong type.
    Instead, the resulting file just ends up with the incorrect contents and the program terminates normally.

lien 352: "three interpretations" -- what are they? I don't see them in the figure.

line 380: from this I inferred that "migratory typing" means "gradual typing applied at modulewise granularity". Assuming that's correct, it's worth saying directly.
  ⟶ The three choices of using Dyn, the granularity of components, and how compilation affects completely untyped components represent orthogonal aspects of the design of a Gradual Typing system.
    In the case of Typed Racket's migratory typing, those three choices amount to more than just applying the ideas of [21] at the granularity of modules.

line 421: "type mistake" -- does this mean "feasible run-time type error"?
  ⟶ Could you clarify what a makes a runtime type error "feasible"?

line 424: "fully typed correct programs" -- presumably your method could also work with not-yet-fully-typed correct programs, just not ranging over the entire lattice in those cases. I was wondering whether that might give different/interesting results.
  ⟶ Yes, the approach could be applied, but the results may be mostly inconclusive depending on how many trails reach the point where the blamed component lacks type informtion.
    All trails that reach that point provide no useful information.

line 433: "without loss of diversity" -- this is glib. Clearly diversity is lost; just claim that what remains is still diverse enough.

line 497: "truthiness" needs explaining

line 517: the difference between #1 and #2 here again relies on the surprising property (to the unfamiliar) that Typed Racket has a notion of "type-level mistake" that doesn't surface under erasure (as an exception or whatever) but also is not a false positive (i.e. you're not talking about conservativeness of static checking). Line 538's "unavoidable" claim is probably also true only in such a context.
  TODO: Don't respond to this in light of above explanation(s) of why they misunderstand erasure and TR?

line 519: "at least three" -- better to claim this is only two here, then explain later that the driver doesn't count. Mentioning "three" up-front just raises an unnecessary question in the reader's mind.

line 634: earlier I had inferred that the distinction between "migratory" and "gradual" was more than just preferring one word; this seems to contradict that

line 640: what makes it "concise"?
  TODO ll: Why does it? I'm not sure about the word here either. Just say we'll change it?

line 673: "programmers runs" typo

line 727: "dubbed" -- reads oddly. Maybe italicise the "location", but it seems overkill. The phrase is pretty self-suggesting as it is.

line 756: "checked the value's type.." -- and the check passed!?
  ⟶ Indeed, Transient checks only check the top-level type constructor of a value, so many checks can pass for a bad value before it is unwrapped to discover the problem.
    We will add a reminder of this fact to the prose here.

line 752: "added to the blame set first" -- should it be a blame list, then?

line 936: no need for hyphen after the adverb

line 966: "more useful C" -- missing "than"

line 982: don't think these percentages deserve 3 significant figures
  TODO: "deserve"?
  ⟶ We include the hundreths place to highlight in particular that Transient first and last blame are not exactly the same.
    If this information isn't useful, we can use just the tenths place.

line 1304: "Problem is" -- missing "The". Also, no need to italicise the next sentence... it is really not that deep or surprising.
