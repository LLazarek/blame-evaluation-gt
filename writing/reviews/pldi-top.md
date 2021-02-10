PLDI 2021 Paper #66 Author Response
===========================================================================

Thank you to all the reviewers for the helpful feedback. 

The first part of our response addresses two common themes in the
reviews; the second part addresses all remaining concerns individually.  

Are the empirical results conclusive? Are they surprising?
===========================================================================

Here are the two relevant major conclusions from our experiment:

1. Transient and Natural's blame makes rational programmers more effective than Erasure.

2. The effectiveness of Natural is (merely) comparable to Transient.

As review `D` points out, the second one is surprising: _While Theory
predicts that Natural blames more accurately than Transient, our
experimental results do not support this prediction._

In analogy to physics research, this points to two possible explanations:

- a need for theoretical models with improved predictive power

In all likelihood, the current models are too small to include language features
critical to blame assignment. 

- a need for "blame data" from large systems 

The presented data is based on systems with up to 14 modules (see table
below).  Natural may prove its worth only when the dependency chains are
deeper than the ones in the existing benchmarks. If Natural works better
for programs with deeper dependency chains, this should guide future research
on mixing approaches.

Is the method novel? 
===========================================================================

Our inspiration is Lazarek et al.'s POPL paper, which shows the possibility
of large-scale design evaluations that do _not_ involve human sw devs.

The challenge is how to adapt this work to problems other than
contracts. Given the close relationship between Natural and contracts, it
is _natural_ to pursue an adaptation of the Lazarek method to gradual
typing. Adaptation means retaining some ideas and injecting entirely new
ones:

1. Lazarek et al.'s work can use off-the-shelf mutators because they are
only looking for bugs that contracts can discover in an untyped world.  In
particular, type-preserving mutators are acceptable. Our work must use
mutators whose effect the type system can discover---meaning almost all of
Lazarek's. Unfortunately, we re-used Lazarek's _names_ for our mutators ---
a significant articulation mistake on our side. For the final version, we
will use distinct names.

2. Lazarek et al. deal with a single contract system and examine a yes-or-no
question, namely, whether  blame-shifting finds the bug for all debugging
scenarios of a benchmark. Our experiment compares fundamentally different
error-reporting schemes and asks questions of degree. 

3. Lazarek et al.'s method cannot really conclude that blame is the reason
for the success of the debugging process because it fails to isolate the
effect of blame from that of checks or mere luck. It merely validates that
blame shifting works. Our work focuses on the value of blame and that kind
of work demands proper `control's, which are entirely new. 

Additionally our work also has to work with at least two different
baselines:

- the run-time checking of the underlying language

All gradual type systems are built atop languages that check the tags of
values at run time and, if the tags don't match the operation, raise an
exception that reports the current stack (trace). Many programmers use
stack traces for debugging, and our work must show that blame assignment is
at least as good as this traditional method.

- the "random" ("lucky") programmer

A less-than-rational programmer may pick random elements from the error
report and "luck out" all the time. Hence we use the "random mode" as a
second baseline.

- One additional point from review `D` stands out, namely, the remark that
  bringing Natural and Transient under the same roof (section 3) is "just
  engineering."

Neither Reticulated Python nor the model of Transient provide answers about
what shallow checks correspond to the sophisticated types of Typed Racket
(or TypeScript), e.g.  polymorphism, row types for mixins, occurrence
typing) and what information is necessary to record in the blame map.
Similarly, neither of these two guides an implementor on how to record
blame properly in principle, not to speak of an implementation. But, we do
not think that this paper is the proper place to describe this aspect of
the work. 

