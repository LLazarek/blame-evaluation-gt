PLDI 2021 Paper #66 Author Response
===========================================================================

Thank you to all reviewers for the helpful feedback.
Our rebuttal starts with a high level response, then responds to each remaining comment/question in turn.

High level response
-------------------

As review `D` observes, the results of our experiment are surprising;
they provide clear evidence that Natural and Transient blame have comparable utility in practice.
This conclusion flies in the face of theoretical predictions of Natural's superiority.
The tools of gradual typing theory don't immediately explain why we would see this disparity, perhaps because many of the advanced features of Typed Racket's type system haven't appeared in models that compare Natural and Transient: polymorphism, row types, mixins, and more.
The disparity between the theory's predictions and our experiment's conclusions demands two responses:
we must re-evaluate and extend the theory to improve its predictive power,
and we must perform more extensive experimentation to better understand the extent and nature of the divergence.
The latter response can already be addressed by applying the methodology we propose, and this applicability is why the methodology is the paper's primary contribution.

The methodology is the main contribution because it provides novel insights for evaluating blame in gradual typing along several dimensions.
- bringing different semantics into the same implementation
- designing custom mutators
- designing the framework for understanding and comparing utility of different strategies in terms of modes, and with many baselines against which to interpret the results

Then, clarify and strengthen position of reviewer A, using those comments as hooks to argue further against other reviews' criticisms.
In particular,
- Novelty of designing mutators
- Size and standardness of the benchmarks
- ... I think that's all?

Points already drafted to incorporate into the high level response
==================================================================

Review #66B (for surprisingness that Natural is not >> Transient):
--------------------------------------------------
> The experiment results show that there are debugging scenarios where
> Transient is more useful than Natural. Does it happen simply because
> of luck? Or, does it imply that Transient is more suitable than
> Natural for a particular kind of bugs for some reason? Also, can there
> be a new blame strategy that outperforms both Transient and Natural
> all the time?

We were also surprised to discover that Transient outperforms Natural in some
scenarios. After all, the theory of blame says otherwise. Even though we haven't
identified specific patterns, we believe that the reason behind Transient's
unexpected success is due to the advanced features of Typed Racket's type system
that haven't appeared in models that compare Natural and Transient:
polymorphism, row types, mixins, and more.

We do not have any clues about a strategy that outperforms both Natural and
Erasure. That said, our current conjecture is that a simple strategy that just
points somewhere in the blame trail may be good enough. Such a strategy would
also enable optimizations that Typed Racket currently foregoes in order to track
blame. The good thing is that now we have a method to compare different
strategies.

chrdimo: TODO We need more details from Ben here.



Review #66C (for conclusiveness of results):
--------------------------------------------------
> And unfortunately the results are inconclusive -- Section 8 says "our results call for a deeper understanding of the two models for blame".

The quote from section 8 refers to the theory of blame rather than our results.
Our results show in clear terms (at least for our benchmarks) that a blame
strategy is superior to exceptions, and that Natural and Transient have
comparable efficacy. The latter is surprising given that theoretical models
suggest the opposite conclusion. Thus, a side-effect of our experiment is a call
to theoreticians for models that are aligned with practice.


Review #66E (for conclusiveness and surprisingness of results):
--------------------------------------------------
> However, I don't find the results to be as conclusive as the authors state. Aside from the Erasure case, whose relevance I found unclear (see below), conclusions rest on some small-valued "more useful than" percentages. These are pretty hard to interpret, because the "usefulness" metrics necessarily build in a lot of simplifying assumptions. E.g. in 5.4, the "percentage of scenarios where ... is more effective" doesn't account for *extent* of difference, and the trail-length "programmer effort" metric in 5.5 is also unlikely to be a great proxy. These limitations are understandable, method-wise, but overall the results are fairly null... they let us continue believing "what we'd expect" but not with any great sense of added confidence. That doesn't entirely diminish the work, of course, but I'd say the current write-up overplays its hand a little. I appreciate the discussion of threats to validity.

  TODO: misunderstanding: the results are not at all "what we'd expect" based on the theory

The status quo of what we would expect based on the current theory of Natural
and Transient is that 1) either kind of blame tracking should be better than
none, and 2) Natural should provide significantly better blame information than
Transient and Erasure. Our results validate the first expectation that blame
provides benefits over plain exceptions. Surprisingly, our results contradict
the second expectation and suggest that in fact Natural and Transient offer
comparable blame information.





Review #66E (for main contribution being method):
--------------------------------------------------
> This paper seeks to establish (or refute) the benefit of blame tracking in gradually typed programming environments. It takes several Typed Racket benchmarks and considers multiple approaches to blame tracking. The central idea is to use a fault injection method to create variant programs along two dimensions: type errors inserted according to some crafted heuristics, and static typing selectively removed on a component-by-component granularity. This creates a large number of variant programs which may produce run-time type errors with blame; this is then iterated to simulate a "rational programmer", i.e. such that blame is used to select where more static checking will be added next; at any given iteration this may or may not catch the injected error, and the number of iterations is counted as a proxy for effort. A further contribution is implementing the different forms of blame-tracking in the same framework so that they can be compared. The finding is that blame is found to offer some positive advantage in effort (over simply being guided by the stack trace of a run-time exception) in 9--13% of the initial sample of variants, with some minor differences between the forms of blame tracking tried.

  TODO: misunderstanding: we propose a methodology

The primary intended contribution of this paper is the methodology, and the
experimental results serve as secondary contributions that validate the
methodology's utility. We will rephrase the introduction to further emphasize
that focus. Furthermore, we would like to emphasize that the two key findings in
the experimental results are that 1) both Natural and Transient's dynamic type
enforcement with blame tracking improve significantly over Erasure, and 2)
Natural's blame tracking is not significantly better than Transient's, despite
the theoretical indications to the contrary.


Review #66D (for novelty of the method):
--------------------------------------------------
> *Natural and Transient Under the Same Roof (Section 3)*
>
> This seems to be "only" an engineering challenge.

We disagree that implementing Transient Typed Racket is mere engineering.
Typed Racket comes with a way more sophisticated type system than
Reticulated and there are a lot of points around type checks and blame
that neither Reticulated nor the Transient model provide answers for.  We
are happy to share with the chair a recent dissertation chapter that
describes all the ideas and work that went into Transient Typed Racket.

We believe that adding a formal model to the paper would distract
from the main contribution of the paper as there are already multiple
publications on the semantics of Transient, Natural and Erasure and at
least one that compares them formally [Greenman et al. OOPSLA 2019]. That said, we
agree that we should improve the presentation in section 3 to introduce
more context.


> *Custom Mutators (Section 4)*
> The paper suggests that the mutators required here are significantly
> different and more subtle, but this does not seem to be the case when
> comparing Figure 3 and Table 3 of the POPL 2020 paper.
> ...

While we agree that there is overlap between our mutators and those from
Lazarek's POPL work (and the common set of mutators where the latter come from),
we disagree that the construction of custom mutators is straightforward. The
sixteen we include in the paper are not the original set we tried and others
that seemed like good candidates have proven to be ineffective. For instance
replacing car with cdr should lead to type-level bugs however none of them are
interesting as we define interesting in the paper. Similarly, swapping mutable
vectors and hashes with immutable ones is an interesting mutation in the
traditional sense and should lead to a type error in Typed Racket as its type
system distinguishes the two, however we discovered that again is not
interesting in our setting. In general, there is no methodology for constructing
a new set of mutators, so narrowing down the ones in the paper required that we
first characterize precisely what makes a bug interesting and then rounds of
trials to end up with a set of mutators that leads to a sufficiently large set
of interesting diverse bugs.



> *Debugging Strategies (Section 5)*
>
> These also seem to be overemphasized. The main design choice seems to
> be in how to use Transient blame, which reports multiple components,
> and picking the first and last are pretty intuitive.
>
> I'm also not sure why the exception mode is needed in addition to
> Erasure (I was confused in Section 2 L204-215 and Section 5.1 L732).
> Indeed, Section 5.3 defines the Erasure mode the follow the Natural
> exceptional mode.

We disagree that section 5 boils down to picking the first and last modes for
Transient. The core of this section is the analysis of of the different systems
and their unification in a common precise framework as modes. Because of the
common substrate of blame trails modes enable the comparison between the
different systems even when sampling is involved. Furthermore we use modes to
introduce in a uniform manner in the framework baselines that help us isolate
confounding factors from the actual effectiveness of blame. This si exactly the
role of the exception modes for Natural and Transient. Those two perform checks
at runtime and the exception modes help us determine what part of the success of
Natural and Transient is due to the extra checks or due to blame (and also
whether blame masks useful information from exceptions). For example, we see
that Natural without blame but with the same checks outperforms Erasure in ~25%
of the scenarios (figure 6). Blame add another ~11% on top of that. For
Transient there is a small percentage of scenarios where exceptions due better
than blame but still blame improves over exceptions by ~11% compared to Erasure.


As a final note, even though each of the above points individually may not have
sufficient technical depth for a separate contribution (which we do not claim),
together they exhibit the creative steps that are necessary to lift the scope
and magnitude of the idea from Lazarek's POPL paper on evaluating blame from a
single behavioral contract system to a framework for evaluating and comparing
different gradual typing systems.


Review #66C (for baselines):
--------------------------------------------------
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

> To explain one comment above: I was surprised not to see a comparison against an "omniscient programmer" to establish a baseline (c.f., the SIGPLAN checklist). That is, model a blame assignment scheme in which blame could be placed on any component. And always pick the component that minimizes the length of the trail. Of course, this would be infeasible to implement in practice, but it would place an upper bound on the utility of blame.

Our experiment does have a baseline. The random mode of the rational programmer
serves as the "control" of our experiment. Furthermore the exception modes each
serve as further "controls" to distinguish the effect of checks from that of
blame within each semantics.













Review #66A
===========================================================================
> - Could you comment a bit more on what makes a mutator "good"? (other than
>   experimentally seeing that it is good.) Perhaps, what would be a really bad
>   mutator for evaluating blame assignment?

A bad mutator results in a type-level bug that is trivial to locate. For
instance, a bug that leads to a type-level error during the evaluation of the
component that contains it is trivial to locate. In contrast, a bug that is
non-trivial to locate is one that causes the buggy component to evaluate without
an error but produce instead a value of the wrong type. An interesting bug is
one where the value with the wrong type manages also to cross undetected to at
least another component before a type-level check flags it. Good mutators should
be able to produce these interesting bugs.


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

Sure. Each CPU has 28 doubly-threaded cores.

> - L991: Figure 6: It took me some time to understand the figure. I wonder if
>   there is a better way to represent the same data?

We have reached the same conclusion and we are considering alternatives that
will make it easier to see how two modes compare to each other, such as a series
of two-sided bar charts.




Review #66B
===========================================================================
> Theoretical gradual type systems usually allow programmers to control
> the precision of type annotations in a fine-grained way with the Dyn
> type rather than distinguishing typed and untyped modules. Would the
> same evaluation method be able to directly appliable to blame in such systems?

Yes. Incorporating type Dyn boils down to considering a hierarchy of types for
each component rather than a single type. At the same time, our method is
parametric to the "size" of a component and adapts in a straightforward manner
to a setting where each definition can be considered a component instead of a
whole module. Of course both of these adjustments lead to significantly larger
scenario lattices than the ones we investigate, so keeping the experiment
computationally feasible would require more aggressive sampling.


> Please explain what impedance mismatches are in the paper. Is it
> simply type mismatches?

An impedance mismatch denotes a type annotation that is detected to disagree
with the actual type of the annotated code. We will adjust the prose to clarify.


> The paper states that the effort distribution of the random mode
> follows a normal distribution, as expected. Please explain why a
> normal distribution is a reasonable expectation. ...
> This point does not affect the contribution of the paper, but it
> is not clear from the paper.

You are right that for a given trail of length $n$ the probability that the
random rational programmer discovers the buggy component on every step is $1/n$.
The missing piece is that we randomly sample from the set of scenarios -- and by
extension the set of trails. So the shape of the random programmer's effort
matches our sampling distribution. That said, as you observe, the distribution
of the random programmer's effort is not relevant to our results, so we will
remove the characterization.


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




> Similarly, I wondered why the modes of the transient programmer only consider picking the first/last blame, and not other choices. Section 5.2 just says "our answer" is that there are at least "two reasonable options." Why are other choices not reasonable?

The transient semantics provide no interpretation for the blame set. We picked
the first and last because they match the intuitive interpretation of the blame
set as a list. We didn't mean to imply that other options are unreasonable. We
just needed to make a reasonable choice and keep the duration of the experiment
within reasonable limits. We will clarify this point in the prose.




Review #66D
===========================================================================

> Given that the experiments are developed and run in the context of the
> large, real-world Racket ecosystem, I wonder whether about the chance
> of incidental implementation choices or bugs affecting this result.

A side note here: Transient Typed Racket is faster than Natural Typed Racket
when we turn off blame. Exactly as a sanity check that this is not an artifact
of our implementation we have coded up benchmarks in Python and we confirmed
that in Reticulated the performance of transient with blame is even worse than
what we have observed in Typed Racket.




> *Additional Comments and Typos*
>
> L69: I wish here, and elsewhere, the comparison to [13] had been made
> more explicit. As discussed above, there seem to be more similarities
> than suggested.
> L92: "use of _a_ higher-order contract system"
> L155: "authors extensive"
> L253: "exports _it_ as"
> L280: Missing `untyped-`
> L314: " . "
> L382: "Dyn,"
> Fig 2: Presumably authors were included to indicate the variety of
> sources, but I'm not sure that was necessary.
> L597: "make us_e_ of"
> L664: "described in _Section_ 4"
> L980: "Figure 5.4" ==> "Figure 6"
> L986: "2.8%" ==> "2.18%" ?
> L1017: "33%" ==> "26%" ?
> L1304: "Problem is ...."

Thank you. We will fix these.



Review #66E
===========================================================================




> About Erasure: if I'm reading this correctly, the key thing here is that in Typed Racket, the set of static types is more expressive than the set of dynamic types. For example, there is a static notion of non-negative integer that is distinct from plain integer. This sort of design isn't universal -- in some languages/systems static types closely mirror the classification of objects in the language's dynamic semantics. Writing these stricter contracts into a program brings its own benefits, separate from blame or indeed from static checking. It feels like the paper doesn't take enough care to distinguish the two effects: the effect of systematically applying more refined contracts over program values, and the effect of gradually enabling static checking of those contracts (iteratively, guided by blame). The authors do mention this around line 204, and the comparisons in Figure 6 between "_ exceptions" and "Erasure" seem to be measuring this -- the gain from checking these extra annotations, with the more precise and/or more timely checks that they imply, relative to the erased case where only the language baseline contracts are checked. Indeed that's the point of 'exception' experiments. Since the biggest effect sizes on display are these ones -- between Erasure and anything else -- this seems at best distracting. I'd be glad to hear from the authors if I'm misunderstanding anything here. In the detailed comments below, I have noted some places in the text where it would be useful to remind the reader that this design property of Typed Racket is at play.

TODO: misunderstanding about gradual typing as a whole, and the point of the exception modes

The idea of the Erasure semantics is not related to the expressiveness of the
type system or how the types relate to kinds of runtime values. Rather, the key
idea is that Erasure does not enforce type annotations with dynamic checks at
all. Running a gradual program using the Erasure semantics is the same as
stripping all annotations and running the resulting (completely dynamic)
program. Thus, Erasure programs can only raise exceptions from either the checks
performed by the language's primitive operations (e.g. `+`), or from the program
itself raising an exception. In contrast, the other two semantics do enforce
type annotations dynamically; both Natural and Transient insert dynamic checks
verifying that the shape of values at runtime match their static annotations.
Thus, in addition to exceptions, gradual programs using these semantics may
raise a second kind of error: a dynamic type check failure. These dynamic errors
can include blame information if the dynamic type checks are designed to track
blame, or they can forego blame and just offer a stack trace. In the case of
Natural, these two options are captured by the "Natural" and "Natural
exceptions" modes, respectively. Thus the "Erasure" mode represents a completely
different kind of checking (and thus error reporting) compared to both the
"Natural" and "Natural exceptions" modes (the two of which share the same kind
checking, but different error reporting).


> It's interesting to note that there seems to be no convincing analogous experimental or modelling-based justification for plain old static typing, i.e. showing that it somehow presents a net gain to the programmer. Rather, this has simply been posited/assumed by a very long line of work. I am not defending that state of affairs, but it points to the difficulty of showing conclusively that something truly helps programmers. I can see value in this sort of simulation-style approach, and it is no less convincing than user studies. So I'd be interested to hear from the author(s) if they have any more arguments that (1) I've underestimated the results' conclusiveness, or (2) this is a novel/interesting family of methods that might be pursued more widely, or (3) that deeper experiments building on these ideas might yield more compelling insights.

(Matthias) As posed, the question expresses a significant mis-characterization
of the submission. It is not the benefits of a gradual type system that are in
doubt. The experimental setup keeps the type system constant but allows to
answer the question which of several run-time checking regimes (for enforcing
type consistency) provides the best explanatory messages in case of violations.
Hence the analogous question for purely static type systems would ask which of
several reporting schemes provides the best explanatory message for type-errors.
In the case of simply typed languages and even language with local type
inference, this question is basically meaningless. In the case of languages with
HM type inference, the question has been implicitly raised for four decades with
the development of alternative ways of finding the source of inference
conflicts.

If the reviewer is indeed interested in the question of whether type systems
help programmers---a question that this submission does _not_ ask---the recent
OOPSLA literature contains several user studies. A simple Google query will
suggest a short list of these publications.


  TODO ll: I added responses (after ⟶) to anything below that seems to warrant more than "Thanks, we'll fix it".


Detailed comments:

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
