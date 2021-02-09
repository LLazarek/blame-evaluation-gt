PLDI 2021 Paper #66 Author Response
===========================================================================

First of all, we thank all of the reviewers for reading our submission, asking
critical questions, and offering constructive feedback. Our responses to all
comments and questions follow, in which we quote each comment/question and then
respond below.


Review #66A
===========================================================================
> # Questions
> - Could you comment a bit more on what makes a mutator "good"? (other than
>   experimentally seeing that it is good.) Perhaps, what would be a really bad
>   mutator for evaluating blame assignment?

A bad mutator results in a type-level bug that is trivial to locate. For
instance a bug that leads to a type-level error during the evaluation of the
component that contains it is trivial to locate. In contrast, a bug that is
non-trivial to locate is one that causes the component that mutates it to
evaluate without an error but produce instead a value of the wrong type. An
interesting bug is one where the value with the wrong type manages also to cross
undetected to at least another component than the one that produces it until a
type-level checks flags it. These latter bugs are those that good mutators
should be able to produce.


> - I don't understand where the 16,800 number comes from. Is it an exhaustive
>   enumeration of all possible mutations? (Or the number of mutants constructed
>   in X amount of time?)

It is an exhaustive enumeration. We will clarify the prose.


> - How large are the programs in the benchmark suite?

Here are the LOC counts and number of components for each benchmark. We will add
the information to figure 2.

  benchmark    | LOC  | components
  -------------|------|-----------
  acquire      | 1941 | 9
  gregor       | 2336 | 13
  kcfa         | 328  | 7
  quadT        | 7396 | 14
  quadU        | 7282 | 14
  snake        | 182  | 8
  suffixtree   | 1500 | 6
  synth        | 871  | 10
  take5        | 465  | 8
  tetris       | 280  | 9


> # Minor Comments
> - L38: Should there have been a colon? Is the text "This contrast... " and
>   challenge quote from [3]? Who is saying what here...

No, the quote is not from [3]. We will rephrase to clarify that it is our
statement of the research question.

> - L104: I was confused by the word "Next". Maybe instead: We now ..."

Thank you. We will reword.

> - L268: Grammar issue.
> - L314: Floating period.
> - L441: Add line numbers (or other metric of size).
> - L980: Figure 5.4??

We will fix these. Thanks.

> - L956: 30,000 hours of compute time is 1,250 days. I assume that the CPU has
>   many cores. Could you put that number?

Sure. Each CPU has 28 double-threaded cores.

> - L991: Figure 6: It took me some time to understand the figure. I wonder if
>   there is a better way to represent the same data?

We have reached the same conclusion and we are considering alternatives such as
a series of two-sided bar charts that make it easier to spot how two modes
compare to each other.




Review #66B
===========================================================================
> The experiment results show that there are debugging scenarios where
> Transient is more useful than Natural. Does it happen simply because
> of luck? Or, does it imply that Transient is more suitable than
> Natural for a particular kind of bugs for some reason? Also, can there
> be a new blame strategy that outperforms both Transient and Natural
> all the time?

We were also surprised to discover that transient outperforms natural in some
scenarios. After all, the theory of blame says otherwise. Even thought we
haven't identified specific patterns, we believe that the reason behind
transient's unexpected success is due to the advanced features of Typed Racket's
type system that haven't appeared in models that compare Natural and Transient:
polymorphism, row types, mixins, and more. Such features require custom
loopholes to be compatible with Racket idioms. In turn these loopholes can
result in subpar checks in Natural. Our experimentation with transient indicates
that the same problem doesn't occur in transient. We do not have any clues about
a strategy that outperforms both Natural and Erasure. But our current conjecture
is that a simple strategy that just points somewhere in the blame trail may be
good enough and enable downstream optimization that are keeping track of blame
in Typed Racket inhibits. The good thing is is that now we have a method to
compare different strategies.

chrdimo: TODO We need more details from Ben here.


> Theoretical gradual type systems usually allow programmers to control
> the precision of type annotations in a fine-grained way with the Dyn
> type rather than distinguishing typed and untyped modules. Would the
> same evaluation method be able to directly appliable to blame in such systems?

Yes. Incorporating type Dyn boils down to considering a hierarchy of types for
each component rather than a single type. At the same time, our method is
parametric to the ``size'' of a component and adapts in a straightforward manner
to a setting where each definition can be considered a component instead of a
whole module. Of course both of these adjustments lead to significantly larger
scenario lattices than the ones we investigate and they have to be sampled
aggressively and carefully.


> Please explain what impedance mismatches are in the paper. Is it
> simply type mismatches?

An impedance mismatch denotes a type annotation that doesn't match the actual
type of the annotated code. We will adjust the prose to clarify.


> The paper states that the effort distribution of the random mode
> follows a normal distribution, as expected. Please explain why a
> normal distribution is a reasonable expectation. ...
> This point does not affect the contribution of the paper, but it
> is not clear from the paper.

You are right that for a given trail of length n the probability that the random
rational programmer discovers the buggy component after k attempts is always 1/n
and independent of k. However we sample randomly from the set of scenarios and
as a result the set of trails. So the curve of the random programmer matches our
sampling distribution. That said, as you observe, what is the distribution of
the random programmer is an irrelevant point to our results and we will remove
the characterization.


> * page 9 line 980: Figure 5.4 -> Figure 6
> * page 10 Figure 7: Null -> Random
> * page 11 line 1103: ( 770 -> (770

Thank you. We will fix all these.




Review #66C
===========================================================================
> But while there is a lot to like about the paper, it also has some shortcomings. The suite of programs used for the evaluation is fairly small (c.f., the SIGPLAN checklist).

Could you clarify what you mean by fairly small with respect to the SIGPLAN
checklist? We are the first to conduct an empirical study for blame and gradual
typing, so we repurposed the standard performance benchmark suite for Typed
Racket -- in particular, its largest and most complex benchmarks. Our inspection
of the benchmarks indicates that they are representative set of Racket programs
in terms of features and structure.


> The faults are introduced using synthetic mutations that may or may not correspond to the kinds of errors that arise in practice. This can be seen from the results in Figure 7 -- the lengths of the "trails" is quite small.

There is no existing catalog of bugs in gradually typed programs. Furthermore,
most type-level bugs do not survive after decent testing and thus do not make it
to code repos. There are a few anecdotes from the literature but they are mostly
artificial. Hence, since we need a large number of buggy scenarios for a
meaningful study, a synthetic approach is the only way forward. In addition,
mutation allows us to tune the set of scenarios to get a diverse set of bugs.


> While the study is mostly well done, it only considers two blame assignment strategies.
> For example, it would be interesting to consider the "omniscient programmer" to get a bound on the value of blame. But this is not done.

Could you elaborate what you mean by "omniscient programmer"? If this is the
programmer that always makes the best choice, i.e. finds the bug at the first
try, then this programmer is trivial. It has 100% success rate and a constant
blame trail length of 1. Thus, comparing its success to any other strategy
amounts to counting the number of scenarios where the other strategy fails;
comparing effort is similarly trivial. This information is in the paper
implicitly, but we can certainly add an explicit mention if it would be useful.

Secondly, we would like to point out that the experiment considers then
different modes of the rational programmer spread across three different gradual
typing systems.


> And unfortunately the results are inconclusive -- Section 8 says "our results call for a deeper understanding of the two models for blame".

The quote from section 8 refers to the theory of blame rather than our results.
Our results show in clear terms (at least for our benchmarks) that a blame
strategy is superior to exceptions, and that Natural and Transient have
comparable efficacy. The latter is surprising given that theoretical models
suggest the opposite conclusion. Thus, a side-effect of our experiment is a call
to theoreticians for models that are aligned with practice.


> Also, the published Transient scheme seems to have a minor bug, which might affect the outcomes when fixed (Section 8.2).

Section 8.2 does not discuss a bug in Transient but rather a limitation of its
current design that we discovered exactly because of the experiment.


> Most if not all of these shortcomings are discussed in the paper, which is much appreciated. But they do risk undermine the value of the contributions to some degree.

We would like to point out that the results of the comparison between the three
systems are only one part of the contribution of the paper. The other part is
the method itself. The benchmarks, source of interesting bugs, and rational
programmer modes are all parameters that can be instantiated differently without
affecting either the recipe we discuss at the end of section 2 or the analytical
framework from section 5 that underlies our experiment.


> To explain one comment above: I was surprised not to see a comparison against an "omniscient programmer" to establish a baseline (c.f., the SIGPLAN checklist). That is, model a blame assignment scheme in which blame could be placed on any component. And always pick the component that minimizes the length of the trail. Of course, this would be infeasible to implement in practice, but it would place an upper bound on the utility of blame.

Our experiment does have a baseline. The random mode of the rational programmer
serves as the "control" of our experiment. Furthermore the exception modes each
serve as further "controls" to distinguish the effect of checks from that of
blame within each semantics.


> Similarly, I wondered why the modes of the transient programmer only consider picking the first/last blame, and not other choices. Section 5.2 just says "our answer" is that there are at least "two reasonable options." Why are other choices not reasonable?

The transient semantics provide no interpretation for the blame set. We picked
the first and last because they match the intuitive interpretation of the blame
set as a list. We didn't mean to imply that other options are unreasonable. We
just needed to make a reasonable choice and keep the duration of the experiment
within reasonable limits. We will clarify this point in the prose.




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

Comments for author
-------------------
I commend the authors for asking the question. Their method is clearly the product of much careful thought. I understand the difficulty in doing experiments about errors that tend not to hit 'real' source code repositories, and I appreciate the effort taken to re-create the different approaches to blame in a comparable setting.

However, I don't find the results to be as conclusive as the authors state. Aside from the Erasure case, whose relevance I found unclear (see below), conclusions rest on some small-valued "more useful than" percentages. These are pretty hard to interpret, because the "usefulness" metrics necessarily build in a lot of simplifying assumptions. E.g. in 5.4, the "percentage of scenarios where ... is more effective" doesn't account for *extent* of difference, and the trail-length "programmer effort" metric in 5.5 is also unlikely to be a great proxy. These limitations are understandable, method-wise, but overall the results are fairly null... they let us continue believing "what we'd expect" but not with any great sense of added confidence. That doesn't entirely diminish the work, of course, but I'd say the current write-up overplays its hand a little. I appreciate the discussion of threats to validity.
  TODO: misunderstanding: the results are not at all "what we'd expect" based on the theory

The presentation of the results is very much around aggregates and summaries. Indeed the whole method is about having run a huge compute job over a large number of variants. Perhaps it's paranoia but I'd be interested to see some specific examples walked through by hand (in the paper) and some smaller sample manually classified (as results). That would add an extra sanity check that the metrics do correspond to some meaningful reality.

About Erasure: if I'm reading this correctly, the key thing here is that in Typed Racket, the set of static types is more expressive than the set of dynamic types. For example, there is a static notion of non-negative integer that is distinct from plain integer. This sort of design isn't universal -- in some languages/systems static types closely mirror the classification of objects in the language's dynamic semantics. Writing these stricter contracts into a program brings its own benefits, separate from blame or indeed from static checking. It feels like the paper doesn't take enough care to distinguish the two effects: the effect of systematically applying more refined contracts over program values, and the effect of gradually enabling static checking of those contracts (iteratively, guided by blame). The authors do mention this around line 204, and the comparisons in Figure 6 between "_ exceptions" and "Erasure" seem to be measuring this -- the gain from checking these extra annotations, with the more precise and/or more timely checks that they imply, relative to the erased case where only the language baseline contracts are checked. Indeed that's the point of 'exception' experiments. Since the biggest effect sizes on display are these ones -- between Erasure and anything else -- this seems at best distracting. I'd be glad to hear from the authors if I'm misunderstanding anything here. In the detailed comments below, I have noted some places in the text where it would be useful to remind the reader that this design property of Typed Racket is at play.

It's interesting to note that there seems to be no convincing analogous experimental or modelling-based justification for plain old static typing, i.e. showing that it somehow presents a net gain to the programmer. Rather, this has simply been posited/assumed by a very long line of work. I am not defending that state of affairs, but it points to the difficulty of showing conclusively that something truly helps programmers. I can see value in this sort of simulation-style approach, and it is no less convincing than user studies. So I'd be interested to hear from the author(s) if they have any more arguments that (1) I've underestimated the results' conclusiveness, or (2) this is a novel/interesting family of methods that might be pursued more widely, or (3) that deeper experiments building on these ideas might yield more compelling insights.

Detailed comments:

In the title, "evaluating blame" reads oddly. It is ambiguous, because "evaluate" sometimes means to compute a result. A fuller phrase, like "evaluating the usefulness of blame tracking...", would probably be worth the words.

The abstract doesn't say much about the work. It would be better written for experts to quickly gather an overview what the paper contributes.

Conversely, the main body of the paper skimps a bit on background. It never explicitly covers what "blame" means and how it works in practice. Similarly, it repeatedly talks about "impedance mismatches" without defining them. Perhaps this phrase is now standard in the gradual typing literature, which I haven't kept up with (hence my Z expertise). But I did read the Wadler/Findler paper carefully at the time. It did not talk about impedance mismatches. In any case, it is a fuzzy metaphor... please say exactly what it means here.

line 34: "then their compilers remove types and rely on the built-in safety checks of the underlying language to catch any problems". This reads oddly in context. Didn't they just do a bunch of static checking? So they are *not* relying just on dynamic checks to catch problems? Maybe there is something more accurate to say here... e.g. no dynamic check is removed, or something like that.

line 38: "explicit statement and challenge" -- what is it?

line 49: I agree they got the word wrong, but you should explain this

line 60: around this paragraph the writing started to grate. There's no need to generalise about what people do or think, and the "As a matter of fact... simply..." style is somewhere between laboured and patronising. It's better to more plainly state the gap in the literature that you're addressing. Throughout the paper, much space could be saved by writing in a more direct style.

line 69: be explicit that Lazarek et al were (as I later gathered) doing something about blame to do with higher-order contracts but not gradual types

line 70: what does it mean to "follow" the slogan? Was unclear to me.

line 90: at first I wondered: what is a case? Maybe say "program variant" to foreshadow the idea of generating mutants etc?

line 91-ish: "Transient, "Natural", "Erasure" -- be explicit that these are names that *you* are introducing

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

line 283: "rewrites typed modules to inline checks" -- so a module gets turned into a check? Clearly not, but that's how it reads

line 293: I was wondering what constitutes a boundary crossing. Clearly, passing by function call or return crosses from the caller's module to the callee's. What about values exchanged through reads/writes to shared state?

line 299: "crosses" => "crossings" (probably)

line 313: it threw me that a program might not fail but "produce a wrong result". If it could be caught by the gradual type system, why can't it be caught at run time? My best get at explaining this is by what I wrote above, i.e. it's a consequence of Typed Racket's more refined static notion of type. In certain other systems this wouldn't be possible, because the static checker would only catch (albeit earlier) errors that would be caught at run time, so there would be no basis to call the result "wrong".

lien 352: "three interpretations" -- what are they? I don't see them in the figure.

line 380: from this I inferred that "migratory typing" means "gradual typing applied at modulewise granularity". Assuming that's correct, it's worth saying directly.

line 421: "type mistake" -- does this mean "feasible run-time type error"?

line 424: "fully typed correct programs" -- presumably your method could also work with not-yet-fully-typed correct programs, just not ranging over the entire lattice in those cases. I was wondering whether that might give different/interesting results.

line 433: "without loss of diversity" -- this is glib. Clearly diversity is lost; just claim that what remains is still diverse enough.

line 497: "truthiness" needs explaining

line 517: the difference between #1 and #2 here again relies on the surprising property (to the unfamiliar) that Typed Racket has a notion of "type-level mistake" that doesn't surface under erasure (as an exception or whatever) but also is not a false positive (i.e. you're not talking about conservativeness of static checking). Line 538's "unavoidable" claim is probably also true only in such a context.

line 519: "at least three" -- better to claim this is only two here, then explain later that the driver doesn't count. Mentioning "three" up-front just raises an unnecessary question in the reader's mind.

line 634: earlier I had inferred that the distinction between "migratory" and "gradual" was more than just preferring one word; this seems to contradict that

line 640: what makes it "concise"?

line 673: "programmers runs" typo

line 727: "dubbed" -- reads oddly. Maybe italicise the "location", but it seems overkill. The phrase is pretty self-suggesting as it is.

line 756: "checked the value's type.." -- and the check passed!?

line 752: "added to the blame set first" -- should it be a blame list, then?

line 936: no need for hyphen after the adverb

line 966: "more useful C" -- missing "than"

line 982: don't think these percentages deserve 3 significant figures

line 1304: "Problem is" -- missing "The". Also, no need to italicise the next sentence... it is really not that deep or surprising.
