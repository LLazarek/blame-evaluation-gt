PLDI 2021 Paper #66 Author Response
===========================================================================

Thank you to all the reviewers for the helpful feedback. 

The first part of our response addresses a couple of common themes in the reviews; the second part addresses all remaining concerns individually.

Are the empirical results conclusive or surprising?
===========================================================================

Here are the two relevant major conclusions from our experiment:

1. Transient and Natural make rational programmers more effective than
Erasure;

2. According to the rational programmer, the effectiveness of Natural is (merely)
comparable to Transient.

As review `D` astutely points out, the second one is surprising: _While Theory predicts that Natural blame is more accurate than Transient blame, the experimental results do not support this prediction to a sufficient degree._ 

Thus our results point to two possible explanations: 

- a need for theoretical models with improved predictive power

The predictive power of gradual typing formal models fails when it comes to blame. In all likelihood, the current models are too small and do not include crucial
features. (It is posisble that the current models are inherently limited.) 

- a need for "blame data" from large systems 

The presented data is based on systems with up to 15 modules, totalling 25,000 loc. While the chosen benchmarks represent a wide spectrum of realistic code, Natural may prove its worth only when the dependency chains are even deeper than the ones in the existing benchmarks. 

In both cases, our new method provides the tools for validating new theoretical results and/or gathering new data. 

Additionally, the experiment suggests that implementors can experiment with checks and blame to find a sweet spot between type soundness, blame effectiveness and performance. Our method
provides an automatic tool to guide their exploration.


Is the method novel? 
===========================================================================

Our inspiration is Lazarek et al.'s POPL paper, which shows the possibility of large-scale PL desiggn evaluations that do _not_ involve human sw devs.

The challenge is how to adapt this work to different problems than contracts. Given the close relationship between Natural and contracts, it is _natural_ to pursue an adaptation of the Lazarek method to gradual typing. Adaptation means retaining some ideas and injecting entirely new ones: 

1. Lazarek et al.'s work can use off-the-shelf mutators because their programs are untyped. For typed prorgams, we must create new ones, for which we occasionally chose the same name as Lazarek --- a significant articulation mistake on our side. For the final version, we will use distinct names. 

2. Lazarek et al.'s method cannot really conclude that blame is the reason for the success of the debugging process because it fails to isolate the effect of blame from that of checks or mere luck. What they can assert is only that blame shifting works. Our work demands a proper `control', which we have developed. 

3. Lazarek et al. deal with a single contact system and examine a yes-or-no question, namely, whether  blame-shifting finds the bug for all debugging scenarios of a benchmark. Our experiment compares fundamentally different error-reporting schemes. 

As to the specific complaints in reviews `c` and `d`: 

* Natural and Transient under the same roof (section 3).

Implementing Transient Typed Racket is not mere engineering.  Neither
Reticulated nor the Transient model provide answers about what shallow
checks correspond to Typed Racket's sophisticated types (e.g.
polymorphism, row types for mixins, occurrence typing) and what
information is necessary to record in the blame map. Moreover in the
process of the implementation we discovered issues with the published
Transient model and Reticulated that we had to fix. We do not think that
this paper is the appropriate place to describe the implementation in
detail.

* Custom mutators (section 4).

The identification of good mutators for gradual typing is not
straightforward. A bad mutator results in a type-level bug that is trivial
to locate. For instance, a bug that leads to a type-level error during the
evaluation of the component that contains it is trivial to locate. In
contrast, a bug that is non-trivial to locate is one that causes the buggy
component to evaluate without an error but produce instead a value of the
wrong type. Furthermore an interesting bug is one where the value with the
wrong type manages also to cross undetected to at least another component
before a type-level check flags it. Good mutators should be able to
produce these interesting bugs in our benchmarks. Finally, they should
produce interesting bugs that are detectable by all three systems so that
we can try all systems on the same debugging scenarios.  In general, there
is no methodology for constructing a new set of mutators that produce
interesting bugs for any definition of interesting. So narrowing down the
ones in the paper required rounds of trials to end up with a set of
mutators that leads to a sufficiently large set of interesting and diverse
bugs.


* TODO phrasing of "original set we tried" seems off
The sixteen mutators we include in the paper are not the original set we
tried and others that seemed like good candidates proved to be
ineffective. For instance replacing car with cdr should lead to type-level
bugs however none of them are interesting --- they blow up immediately.
Similarly, swapping mutable vectors and hashes with immutable ones is an
interesting mutation in the traditional sense and should lead to a type
error in Typed Racket since its type system distinguishes the two, however
it turns out such mutations  are not interesting in our setting.



* Modes of the rational programmer (section 5).


A key step for realizing our experiment is the precise and unified
description of debugging in the three gradual typing system  as  the
different modes of the rational programmer. Because of their common
definitional substrate of blame trails, modes enable the comparison
between the different systems even when sampling is involved.  Furthermore
we use modes to introduce in the framework baselines that help us isolate
confounding factors from the actual effectiveness of blame. This is
exactly the role of the exception modes for Natural and Transient; the
exception modes allow us to determine what part of the success of Natural
and Transient is due to the checks or due to blame (and also whether blame
masks useful information from exceptions). For example, we see that
Natural without blame but with the same checks outperforms Erasure in ~25%
of the scenarios (figure 6).  Blame adds another ~11% on top of that. For
Transient there is a small percentage of scenarios where exceptions due
better than blame but still blame improves over exceptions by ~11%
compared to Erasure. Finally, we introduce yet another mode, the random
one, to serve as the baseline for the all modes and exclude that our
observations are due to mere luck. 


As a final note, even though each of the above points individually may not
have sufficient technical depth for a separate contribution (which we do
not claim), together they exhibit the creative steps that are necessary to
lift the scope and magnitude of the idea from Lazarek's POPL paper  from
evaluating blame in a single behavioral contract system to a framework
for evaluating and comparing blame in different gradual typing systems.


