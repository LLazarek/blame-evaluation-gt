PLDI 2021 Paper #66 Author Response
===========================================================================

Thank you to all the reviewers for the helpful feedback. 

We start off with a couple of common themes. Then we reply to all points
individually.

The conclusions from our experiment
===========================================================================

There are three clear major conclusions from our experiment:

1) The Transient and Natural rational programmers are more effective than
the Erasure one;

2) Part of  the additional effectiveness of the former is due to runtime
checks (that Erasure foregoes) but another part is due to blame;

3) The effectiveness of the Natural and Transient rational programmers is
comparable.

The first two results confirm what the vast literature on gradual typing
claims without systematic evidence: blame is useful. However, as review
`D` astutely points out, the third conclusion is surprising in two
different ways: 

(i) An important motivation for Transient is performance but our
experiment provides evidence that performance issues, which stem from the
design of Transient, have a negative effect on the effectiveness of blame.

(ii) Theory claims that natural blame is more accurate than transient
blame but our experiment provides evidence for the opposite;

Thus our results point to two directions for further research in gradual
typing: 

(i) Implementors can experiment with checks and blame to find a sweet spot
between type soundness, blame effectiveness and performance. Our method
provides an automatic tool to guide their exploration.

(ii) The predictive power of gradual typing formal models (the reason
d'etre of scientific models) fails when it comes to blame. This may be
because the current models are too small and do not include crucial
features or because the current models are inherently limited. In any case
our results are a call to theoreticians to re-examine their assumptions
about blame.  Our method provides an automatic tool to validate their
models similar to the way the methods of experimental physicists help
prove or disprove the  math of theoretical physicists.


Challenges for our work compared to Lazarek's POPL paper
===========================================================================

Our work builds on the idea of Lazarek et al.'s paper that it is possible
to evaluate the effectiveness of blame by simulating a debugging process.
Both their work and our work is inspired by Greenman et al's work that
uses a lattice of configurations to systematically explore variants of the
same program with specifications of different precision (for performance
reasons in Greenman et al).  Both the ideas of ``following`` blame while
debugging and of gradually adding specifications to tweak the evaluation
of the program originate from the practice of contracts (see the story in
the intro of Lazarek's paper and similar anecdotes in Meyer's writings).

That said,

1) In the context of Lazarek et al.'s work interesting bugs  are those
detectable by behavioral contracts. Hence, they can use off-the-shelf
mutators. In fact, their method does not detect the overwhelming majority
of bugs except when contracts describe the full functional correctness of
components. 

2) Lazarek et al.'s method does not isolate the effect of blame from that
of checks or mere luck. Thus they cannot really conclude that blame is the
reason for the success of the debugging process --- only that blame
shifting works. In other words, their work is lacking proper `controls'. 

3) Lazarek et al. deal with a single contact system and examine a yes or
no question: does blame-shifting find the bug for all debugging scenarios
of a benchmark. They do not provide any guidance how to generalize to
multiple systems with different notions of checks and/or blame in a
uniform way that also allows comparisons.

This brings us to the three problems we had to solve in order to design
our experiment: 

1) Natural and Transient under the same roof (section 3).

Implementing Transient Typed Racket is not mere engineering.  Neither
Reticulated nor the Transient model provide answers about what shallow
checks correspond to Typed Racket's sophisticated types (e.g.
polymorphism, row types for mixins, occurrence typing) and what
information is necessary to record in the blame map. Moreover in the
process of the implementation we discovered issues with the published
Transient model and Reticulated that we had to fix. We do not think that
this paper is the appropriate place to describe the implementation in
detail.



2) Custom mutators (section 4).

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


The sixteen mutators we include in the paper are not the original set we
tried and others that seemed like good candidates proved to be
ineffective. For instance replacing car with cdr should lead to type-level
bugs however none of them are interesting --- they blow up immediately.
Similarly, swapping mutable vectors and hashes with immutable ones is an
interesting mutation in the traditional sense and should lead to a type
error in Typed Racket since its type system distinguishes the two, however
it turns out such mutations  are not interesting in our setting.



3) Modes of the rational programmer (section 5).


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
of the scenarios (figure 6).  Blame add another ~11% on top of that. For
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



Replies to individual points
==================================================================


Review #66A
===========================================================================
> - Could you comment a bit more on what makes a mutator "good"? (other
>   than experimentally seeing that it is good.) Perhaps, what would be a
>   really bad mutator for evaluating blame assignment?

Please see our reply to the second part of the discussion at the beginning
of the rebuttal.


> - I don't understand where the 16,800 number comes from. Is it an
>   exhaustive enumeration of all possible mutations? (Or the number of
>   mutants constructed in X amount of time?)

It is an exhaustive enumeration. We will clarify the prose.


> - How large are the programs in the benchmark suite?

Here are the LOC counts and number of components for each benchmark. We
will add the information to figure 2.

  benchmark    | LOC  | components -------------|------|-----------
  acquire      | 1941 | 9 gregor       | 2336 | 13 kcfa         | 328  | 7
  quadT        | 7396 | 14 quadU        | 7282 | 14 snake        | 182  |
  8 suffixtree   | 1500 | 6 synth        | 871  | 10 take5        | 465  |
  8 tetris       | 280  | 9


> # Minor Comments
> - L38: Should there have been a colon? Is the text "This contrast... "
>   and challenge quote from [3]? Who is saying what here...

No, the quote is not from [3]. We will rephrase to clarify that it is our
statement of the research question.

> - L104: I was confused by the word "Next". Maybe instead: We now ..."

Thank you. We will reword.

> - L268: Grammar issue.
> - L314: Floating period.
> - L441: Add line numbers (or other metric of size).
> - L980: Figure 5.4??

We will fix these. Thanks.

> - L956: 30,000 hours of compute time is 1,250 days. I assume that the
>   CPU has many cores. Could you put that number?

Sure. Each CPU has 28 doubly-threaded cores.

> - L991: Figure 6: It took me some time to understand the figure. I
>   wonder if there is a better way to represent the same data?

We have reached the same conclusion and we are considering alternatives
that will make it easier to see how two modes compare to each other, such
as a series of two-sided bar charts.




Review #66B
===========================================================================
> The experiment results show that there are debugging scenarios where
> Transient is more useful than Natural. Does it happen simply because of
> luck? Or, does it imply that Transient is more suitable than Natural for
> a particular kind of bugs for some reason? Also, can there be a new
> blame strategy that outperforms both Transient and Natural all the time?

We were also surprised to discover that Transient outperforms Natural in
some scenarios. After all, theory says otherwise. We haven't identified
any patterns for these scenarios. We conjecture that they point to
limitations of the theory but we can;t exclude that some of them aren't
because of some unknown Typed Racket bug. Please see the first part of the
discussion at the beginning of our response for more details and for our
take on your last question.



> Theoretical gradual type systems usually allow programmers to control
> the precision of type annotations in a fine-grained way with the Dyn
> type rather than distinguishing typed and untyped modules. Would the
> same evaluation method be able to directly appliable to blame in such
> systems?

Yes. Incorporating type Dyn boils down to considering a hierarchy of types
for each component rather than a single type. At the same time, our method
  is parametric to the "size" of a component and adapts in a
  straightforward manner to a setting where each definition can be
  considered a component instead of a whole module. Of course both of
  these adjustments lead to significantly larger scenario lattices than
  the ones we investigate, so keeping the experiment computationally
  feasible would require more aggressive sampling.


> Please explain what impedance mismatches are in the paper. Is it simply
> type mismatches?

An impedance mismatch denotes a type annotation that is detected to
disagree with the actual type of the annotated code. We will adjust the
prose to clarify.


> The paper states that the effort distribution of the random mode follows
> a normal distribution, as expected. Please explain why a normal
> distribution is a reasonable expectation. ...  This point does not
> affect the contribution of the paper, but it is not clear from the
> paper.

You are right that for a given trail of length $n$ the probability that
the random rational programmer discovers the buggy component on every step
is $1/n$.  The missing piece is that we randomly sample from the set of
scenarios -- and by extension the set of trails. So the shape of the
random programmer's effort matches our sampling distribution. That said,
as you observe, the distribution of the random programmer's effort is not
relevant to our results, so we will remove the characterization.


> * page 9 line 980: Figure 5.4 -> Figure 6
> * page 10 Figure 7: Null -> Random
> * page 11 line 1103: ( 770 -> (770

Thank you. We will fix all these.




Review #66C
===========================================================================
> But while there is a lot to like about the paper, it also has some
> shortcomings. The suite of programs used for the evaluation is fairly
> small (c.f., the SIGPLAN checklist).

Could you clarify what you mean by fairly small with respect to the
SIGPLAN checklist? We are the first to conduct an empirical study for
blame and gradual typing, so we repurposed the standard performance
benchmark suite for Typed Racket -- in particular, its largest and most
complex benchmarks. Our inspection of the benchmarks indicates that they
are representative set of Racket programs in terms of features and
structure.


> The faults are introduced using synthetic mutations that may or may not
> correspond to the kinds of errors that arise in practice. This can be
> seen from the results in Figure 7 -- the lengths of the "trails" is
> quite small.

There is no existing catalog of bugs in gradually typed programs.
Furthermore, most type-level bugs do not survive  decent testing and thus
do not make it to code repos. There are a few anecdotes from the
  literature but they are mostly artificial. Hence, since we need a large
  number of buggy scenarios for a meaningful study, a synthetic approach
  is the only way forward. In addition, mutation allows us to tune the set
  of scenarios to get a diverse set of interesting bugs.  We discuss in
  section 8 how our approach to bug injection is a threat to validity,


> While the study is mostly well done, it only considers two blame
> assignment strategies.

We would like to point out that the experiment considers ten different
modes of the rational programmer spread across three different gradual
typing systems.


> For example, it would be interesting to consider the "omniscient
> programmer" to get a bound on the value of blame. But this is not done.

If  an  "omniscient programmer"  is the programmer that always makes the
best choice, i.e. finds the bug at the first try, then this corresponds to
a trivial mode; It has 100% success rate and a constant blame trail length
of 1.  Thus, comparing its success to any other strategy amounts to
counting the number of scenarios where the other strategy fails; comparing
effort is similarly trivial. This information is in the paper implicitly,
but we can certainly add an explicit mention if it would be useful.



> And unfortunately the results are inconclusive -- Section 8 says "our
> results call for a deeper understanding of the two models for blame".

The quote from section 8 refers to the theory of blame rather than our
results.  Please see our discussion of the first theme above for more
details on this.

> Also, the published Transient scheme seems to have a minor bug, which
> might affect the outcomes when fixed (Section 8.2).

Section 8.2 does not discuss a bug in Transient but rather a limitation of
its current design that we discovered exactly because of the experiment.


> Most if not all of these shortcomings are discussed in the paper, which
> is much appreciated. But they do risk undermine the value of the
> contributions to some degree.

We would like to point out that the results of the comparison between the
three systems are only one part of the contribution of the paper. The
other part is the method itself. 

> To explain one comment above: I was surprised not to see a comparison
> against an "omniscient programmer" to establish a baseline (c.f., the
> SIGPLAN checklist). That is, model a blame assignment scheme in which
> blame could be placed on any component. And always pick the component
> that minimizes the length of the trail. Of course, this would be
> infeasible to implement in practice, but it would place an upper bound
> on the utility of blame.

Our experiment does have a baseline. The random mode of the rational
programmer serves as the "control" of our experiment. Furthermore the
exception modes each serve as further "controls" to distinguish the effect
of checks from that of blame within each semantics. Please also see our
discussion of modes in the second theme at the beginning of our reply,


> Similarly, I wondered why the modes of the transient programmer only
> consider picking the first/last blame, and not other choices. Section
> 5.2 just says "our answer" is that there are at least "two reasonable
> options." Why are other choices not reasonable?

The transient semantics provide no interpretation for the blame set. We
picked the first and last because they match the intuitive interpretation
of the blame set as a list. We didn't mean to imply that other options are
unreasonable. We just needed to make a reasonable choice to keep the
experiment feasible.




Review #66D
===========================================================================

> Given that the experiments are developed and run in the context of the
> large, real-world Racket ecosystem, I wonder whether about the chance of
> incidental implementation choices or bugs affecting this result.

A side note here: Transient Typed Racket is faster than Natural Typed
Racket when we turn off blame. Exactly as a sanity check that this is not
an artifact of our implementation we have coded up benchmarks in Python
and we confirmed that in Reticulated the performance of transient with
blame is even worse than what we have observed in Typed Racket.


> *Natural and Transient Under the Same Roof (Section 3)*
>
> This seems to be "only" an engineering challenge.


> *Custom Mutators (Section 4)* The paper suggests that the mutators
> required here are significantly different and more subtle, but this does
> not seem to be the case when comparing Figure 3 and Table 3 of the POPL
> 2020 paper.  ...

> *Debugging Strategies (Section 5)*
>
> These also seem to be overemphasized. The main design choice seems to be
> in how to use Transient blame, which reports multiple components, and
> picking the first and last are pretty intuitive.
>
> I'm also not sure why the exception mode is needed in addition to
> Erasure (I was confused in Section 2 L204-215 and Section 5.1 L732).
> Indeed, Section 5.3 defines the Erasure mode the follow the Natural
> exceptional mode.

Please see our discussion of the challenges for our method at the
beginning of our response. 

> *Additional Comments and Typos*
>
> L69: I wish here, and elsewhere, the comparison to [13] had been made
> more explicit. As discussed above, there seem to be more similarities
> than suggested.  L92: "use of _a_ higher-order contract system" L155:
> "authors extensive" L253: "exports _it_ as" L280: Missing `untyped-`
> L314: " . " L382: "Dyn," Fig 2: Presumably authors were included to
> indicate the variety of sources, but I'm not sure that was necessary.
> L597: "make us_e_ of" L664: "described in _Section_ 4" L980: "Figure
> 5.4" ==> "Figure 6" L986: "2.8%" ==> "2.18%" ?  L1017: "33%" ==> "26%" ?
> L1304: "Problem is ...."

Thank you. We will fix these.



Review #66E
===========================================================================



> However, I don't find the results to be as conclusive as the authors
> state. Aside from the Erasure case, whose relevance I found unclear (see
> below), conclusions rest on some small-valued "more useful than"
> percentages. These are pretty hard to interpret, because the
> "usefulness" metrics necessarily build in a lot of simplifying
> assumptions. E.g. in 5.4, the "percentage of scenarios where ... is more
> effective" doesn't account for *extent* of difference, and the
> trail-length "programmer effort" metric in 5.5 is also unlikely to be a
> great proxy. These limitations are understandable, method-wise, but
> overall the results are fairly null... they let us continue believing
> "what we'd expect" but not with any great sense of added confidence.
> That doesn't entirely diminish the work, of course, but I'd say the
> current write-up overplays its hand a little. I appreciate the
> discussion of threats to validity.

 We absolutely disagree with you. Please see our discussion of the
 conclusions form our experiment at the beginning of our reply. 

 chrdimo: Should we be more assertive here about the metrics? I think this
 is a hard to defend position because of the non-constructive manner of the
 criticism --- it is plain dismissive without explanation. 

> About Erasure: if I'm reading this correctly, the key thing here is that
> in Typed Racket, the set of static types is more expressive than the set
> of dynamic types. For example, there is a static notion of non-negative
> integer that is distinct from plain integer. This sort of design isn't
> universal -- in some languages/systems static types closely mirror the
> classification of objects in the language's dynamic semantics. Writing
> these stricter contracts into a program brings its own benefits,
> separate from blame or indeed from static checking. It feels like the
> paper doesn't take enough care to distinguish the two effects: the
> effect of systematically applying more refined contracts over program
> values, and the effect of gradually enabling static checking of those
> contracts (iteratively, guided by blame). The authors do mention this
> around line 204, and the comparisons in Figure 6 between "_ exceptions"
> and "Erasure" seem to be measuring this -- the gain from checking these
> extra annotations, with the more precise and/or more timely checks that
> they imply, relative to the erased case where only the language baseline
> contracts are checked. Indeed that's the point of 'exception'
> experiments. Since the biggest effect sizes on display are these ones --
> between Erasure and anything else -- this seems at best distracting. I'd
> be glad to hear from the authors if I'm misunderstanding anything here.
> In the detailed comments below, I have noted some places in the text
> where it would be useful to remind the reader that this design property
> of Typed Racket is at play.


We do think there is a misunderstanding here about what gradual typing is
in general. 

1) Erasure is the established name of the semantics implemented by
TypeScript, Flow etc. These languages come with an equally sophisticated
type system as Typed Racket. Programmers add annotations to their program
and the type checker does its best to type check the program in order to
discover mistakes and help the IDE. Then types are erased and the program
is run. Since the program is not fully typed, the run time type checks of
primitive operations may detect an incompatible argument and raise an
exception. 

2) The other two semantics introduce extra run time checks (in the form of
proxies for Natural and inlined checks for Transient) to enforce that
values untyped code provides to typed code have the expected type. when
one of these checks fails they also issue blame (with different
mechanisms). 

3) In Typed Racket programmers cannot add contracts. The precision of the
checks is determined by the types and matches those. In turn the types of
primitive operations much the checks that their Racket counterparts
perform. Finally our benchmarks come with the most precise types that
allow them to type check (where precision is based on subtyping). 

4) The Natural and Transient exception modes perform the same checks as
Natural and Transient correspondingly. The only difference is that they do
not produce blame but instead signal an exception with the stack trace of
the failed check. We use these modes to separate the effectiveness of
blame from that of the checks. 

chrdimo: I am not sure if I understand what this guy is trying to say
here. It seems a bit like rambling to me sprinkled with bold statements. 

> It's interesting to note that there seems to be no convincing analogous
> experimental or modelling-based justification for plain old static
> typing, i.e. showing that it somehow presents a net gain to the
> programmer. Rather, this has simply been posited/assumed by a very long
> line of work. I am not defending that state of affairs, but it points to
> the difficulty of showing conclusively that something truly helps
> programmers. I can see value in this sort of simulation-style approach,
> and it is no less convincing than user studies. So I'd be interested to
> hear from the author(s) if they have any more arguments that (1) I've
> underestimated the results' conclusiveness, or (2) this is a
> novel/interesting family of methods that might be pursued more widely,
> or (3) that deeper experiments building on these ideas might yield more
> compelling insights.

For point 1 and 3 please see our discusssion at the beginning of the
reply. For point 2, the idea of the rational programmer is a new one so
far applied to blame in contracts (Lazarek et al. POPL 2020) and blame in
gradual typing (our work). We don't know the future but we believe it can
have a similar effect in programming languages as homo economicus in
economics. 

That said, as posed, the comment expresses a significant
mis-characterization of the submission. It is not the benefits of a
gradual type system that are in doubt. The experimental setup keeps the
type system constant but allows to answer the question which of several
run-time checking regimes (for enforcing types) provides the best
explanatory messages in case of violations.  Hence the analogous question
for purely static type systems would ask which of several reporting
  schemes provides the best explanatory message for type errors.  In the
  case of simply typed languages and even languages with local type
  inference, this question is basically meaningless. In the case of
  languages with HM type inference, the question has been implicitly
  raised for four decades with the development of alternative ways of
  finding the source of inference conflicts.

If the reviewer is indeed interested in the question of whether type
systems help programmers---a question that this submission does _not_
ask---the recent OOPSLA literature contains several user studies. A simple
Google query will suggest a short list of these publications.



> line 34: "then their compilers remove types and rely on the built-in
> safety checks of the underlying language to catch any problems". This
> reads oddly in context. Didn't they just do a bunch of static checking?
> So they are *not* relying just on dynamic checks to catch problems?
> Maybe there is something more accurate to say here... e.g. no dynamic
> check is removed, or something like that.

In Erasure, a program can type check and still have type-level errors
because some type annotations may be missing. In contrast to Natural and
Transient, types do not result in run time checks so the only way to
detect type-level errors that the type checker couldn't find is the
built-in checks of the language's primitives.  

  
> line 38: "explicit statement and challenge" -- what is it?

The challenge questions whether blame is useful at all in gradual typing.
We will clarify this in the prose.

> line 49: I agree they got the word wrong, but you should explain this

> line 60: around this paragraph the writing started to grate. There's no
> need to generalise about what people do or think, and the "As a matter
> of fact... simply..." style is somewhere between laboured and
> patronising. It's better to more plainly state the gap in the literature
> that you're addressing. Throughout the paper, much space could be saved
> by writing in a more direct style.

Noted, we will simplify the language where possible.

> line 69: be explicit that Lazarek et al were (as I later gathered) doing
> something about blame to do with higher-order contracts but not gradual
> types

Thanks we will add a more detailed comparison along the lines of the
discussion at the beginning of our response.


> line 70: what does it mean to "follow" the slogan? Was unclear to me.
  
The following sentences are meant to explain this, so we will adjust the
prose to make that clear.

> line 90: at first I wondered: what is a case? Maybe say "program
> variant" to foreshadow the idea of generating mutants etc?

It means debugging scenario. We will clarify.

> line 91-ish: "Transient, "Natural", "Erasure" -- be explicit that these
> are names that *you* are introducing
 
These are standard names for these three systems [9, 10, 37]. We didn't
introduce them.


> line 187: what is a "component"? Should be easy to define.

In this context a component is a module. 

> line 199: "blame set as another form of a stack trace" -- yes. I was
> hoping to see a clearer example of debugging with and without blame,
> i.e. something making explicit the similarities and differences between
> having blame info and having only a stack trace at the error site.


> line 277: what is "responsibility" of a "party", exactly? Are "party",
> "module" and "component" all the same things?
 
Yes in this context. Blame refers to the unit of blame which in Typed
Racket is a module. 

> line 293: I was wondering what constitutes a boundary crossing. Clearly,
> passing by function call or return crosses from the caller's module to
> the callee's. What about values exchanged through reads/writes to shared
> state?

Yes, communicating a value via state also constitutes a boundary crossing.
We will clarify what a boundary crossing is in the prose.



> In the title, "evaluating blame" reads oddly. It is ambiguous, because
> "evaluate" sometimes means to compute a result. A fuller phrase, like
> "evaluating the usefulness of blame tracking...", would probably be
> worth the words.

> The abstract doesn't say much about the work. It would be better written
> for experts to quickly gather an overview what the paper contributes.

> Conversely, the main body of the paper skimps a bit on background. It
> never explicitly covers what "blame" means and how it works in practice.
> Similarly, it repeatedly talks about "impedance mismatches" without
> defining them. Perhaps this phrase is now standard in the gradual typing
> literature, which I haven't kept up with (hence my Z expertise). But I
> did read the Wadler/Findler paper carefully at the time. It did not talk
> about impedance mismatches. In any case, it is a fuzzy metaphor...
> please say exactly what it means here.



> line 313: it threw me that a program might not fail but "produce a wrong
> result". If it could be caught by the gradual type system, why can't it
> be caught at run time? My best get at explaining this is by what I wrote
> above, i.e. it's a consequence of Typed Racket's more refined static
> notion of type. In certain other systems this wouldn't be possible,
> because the static checker would only catch (albeit earlier) errors that
> would be caught at run time, so there would be no basis to call the
> result "wrong".

Consider a function with a type annotation that says its a function from
integers to string that formats its argument to a string and returns it.
If we import the function in an untyped module under Erasure there are no
checks to make sure that the function is going to be applied to integers.
So we can apply it to character or a string and still get back a result
except that it is not the string representation of a number.



> line 380: from this I inferred that "migratory typing" means "gradual
> typing applied at modulewise granularity". Assuming that's correct, it's
> worth saying directly.
  

There are two points that distinguish gradual typing from migratory
typing: the existence of Dyn and the granularity of components.  We will
clarify the prose. 

> line 424: "fully typed correct programs" -- presumably your method could
> also work with not-yet-fully-typed correct programs, just not ranging
> over the entire lattice in those cases. I was wondering whether that
> might give different/interesting results. 

Yes, the approach could be applied.

> line 517: the difference between #1 and #2 here again relies on the
> surprising property (to the unfamiliar) that Typed Racket has a notion
> of "type-level mistake" that doesn't surface under erasure (as an
> exception or whatever) but also is not a false positive (i.e. you're not
> talking about conservativeness of static checking). Line 538's
> "unavoidable" claim is probably also true only in such a context.

Yes please see our answer to a relevant comment above.

 
> line 756: "checked the value's type.." -- and the check passed!?
  
Indeed, Transient checks only check the top-level type constructor of a
value.  We will add a reminder of this fact to the prose here.



> line 119: "homo economicus" needs glossing or a reference

> line 94: "forego" => "forgo"

> line 125: was wondering whether "impedance mismatch" just means
> "feasible run-time type error".

> line 148: last sentence appears to contradict the preceding paragraph,
> and is left hanging oddly. Instead, make it the start of a new
> paragraph.

> line 160: "disparity" is a strange word here. "diversity"?

> line 170: "Otherwise, results from..." -- the point was already clear

> line 174: ... instead of just saying what you built on, first say what
> you did! It's really not clear at this point.


> line 211: "languages exceptions" typo

> line 268: this explanation of the workings of proxies seemed overwrought

> line 283: "rewrites typed modules to inline checks" -- so a module gets
> turned into a check? Clearly not, but that's how it reads

> line 299: "crosses" => "crossings" (probably)

> lien 352: "three interpretations" -- what are they? I don't see them in
> the figure.

> line 421: "type mistake" -- does this mean "feasible run-time type
> error"?
  
> line 433: "without loss of diversity" -- this is glib. Clearly diversity
> is lost; just claim that what remains is still diverse enough.

> line 497: "truthiness" needs explaining

> line 519: "at least three" -- better to claim this is only two here,
> then explain later that the driver doesn't count. Mentioning "three"
> up-front just raises an unnecessary question in the reader's mind.

> line 634: earlier I had inferred that the distinction between
> "migratory" and "gradual" was more than just preferring one word; this
> seems to contradict that

> line 640: what makes it "concise"?
  

> line 673: "programmers runs" typo

> line 727: "dubbed" -- reads oddly. Maybe italicise the "location", but
> it seems overkill. The phrase is pretty self-suggesting as it is.

> line 752: "added to the blame set first" -- should it be a blame list,
> then?

> line 936: no need for hyphen after the adverb

> line 966: "more useful C" -- missing "than"

> line 982: don't think these percentages deserve 3 significant figures


> line 1304: "Problem is" -- missing "The". Also, no need to italicise the
> next sentence... it is really not that deep or surprising.

Thanks for all these detailed feedback points. We will use them to improve
the prose. 

