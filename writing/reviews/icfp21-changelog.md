ICFP21 Changelog
================

0a27607: *Add a few words to Section 3 clarifying that the RP runs the program in the same way every time.*
Addresses review A:
> When the rational programmer "fixes" a module and reruns, the implicit
> assumption seems to be that he runs the program in the exact same way. That
> would make the most sense to me, but you do not seem to come out and say so.


904c836: *Add a note about what "practice" means at the end of the intro.*
Addresses review B:
>  - Why does the paper conclude that "the existing theory does not predict
>    practice properly"?  Figure 8 shows that the theory (Natural) works well.
>  - What does the paper mean by "The existing practice may need additional
>    experiments"?  I have not found any evidence that confirms this claim in the
>    paper.
>  - What does "practice" means here?


2e535fc: *Adjust Transient threats discussion to make clear when we talk about Python benchmarks vs Racket ones.*
Addresses review B:
> - L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?
> - L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
>   does it intend?


a53206e: *Answer question (5) explicitly.*
Addresses review B:
> - Page 7: How is the question (5) answered?


684ce1e: *Note assumptions that benchmark modules are typeable and deterministic.*
Addresses review B:
> - It seems that the experiment implicitly supposes a few assumptions on
>   benchmarks.  First, the original benchmarks must be fully typed.  Thus more
>   experiments on mix-typed programs where some components cannot be typed may be
>   needed.  Second, the benchmarks are supposed to be deterministic, which I find
>   from the definitions of trails.  If it is the case, while I do not think these
>   restrictions have to be lifted in the paper, it would be nice to expose them.


96309b9: *Reword conclusions of intro.*
Addresses review B:
>  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
>    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
>    be nicer to address them in a clearer manner.


4161560: *Use the term configuration consistently to clarify that a debugging scenario is a kind of configuration.*
Addresses review B:
> - L424 "Let a configuration s of P":  Are configurations the same as scenarios?


(Matthias did it): *Remove footnote about mutating `*` and `/`.*
Addresses review B:
> - L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
>   "does not reliably lead to type errors"?


beb0a22, long old commits: *Typo fixes.*
All reviews mentioning typos.
Including the 756/752 typo.


4978e79: *Rephrase start of conclusion.*
Addresses review B:
> - L1198 "But just because...":  It is difficult for me to find what this
>   sentence wants to say.  Please consider rephrasing.


453030a: *Rephrase use of "soundness mechanisms".*
Addresses review B:
> - L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
>   consider clarifying.


34b80a1: *Note what effort specifically means in challenges section.*
Addresses review B:
> - L347 "the rational programmer's effort": This was unclear at the first
>   reading.  It would be nice to briefly explain it here.


cd8b943: *Clean up tables in figures 4 and 5.*


c5c4dce: *Add an example of the kinds of programs occurrence-typing mutators target.*
Addresses review A:
> It would help if you could explain how a mutator like negate-cond actually
> leads to a program that has more type-level mistakes than before. It does not
> seem to me this part of the code itself will now fail, but that changing the
> conditions makes certain previously excluded paths in the code feasible.


698007d: *Extend definition of blame modes to use stack in absence of blame, and add paragraph explaining why it's necessary.*
Addresses review B:
> - Are failing Natural blame trails produced even for programs with impedance
>   mismatch?  (This question is related to the above issue with Natural blame.)


c93cef0: *Simplify the table layout at the end of section 2.*
Addresses review B:
> The table in section 2 looks rather ugly


8804f64: *Add definition of sound and complete blame.*
Addresses review B:
> - The paper often says that a blame system is (un)sound, but it is difficult for
>   me to identify what it precisely means.


9683ae2: *Add references for homo economicus and its problems.*
Address review B:
> - L1099 "as behavioral economics has shown more recently":  Is there a reference
>   to be cited?


511c90f: *Fix discussion of Monotonic in related work.*
Addresses review D:
> The characterization of the Monotonic semantics for references [22] as a
> variant of natural isn't correct--its exception-raising and blame
> behavior for references (though not for functions) is quite different
> from that of Natural. For example, a monotonic reference imported from
> untyped code into two incompatible typed contexts will error
> immediately, before being used.


8183046: *Fix descriptions of Reticulated type inference and performance in the discussion section.*
Addresses all of review D's other concerns.


141a371: *Add new prose + figure at beginning of discussion section walking through an example trail for every mode.*
Addresses review C:
> Sections 8 and 9 report on the finding of the paper. These include statements
> that blame is useful, that Natural often produces shorter trails than
> Transient, and others. My only issue at this point is that the reader is left
> with numnbers only, while I think a reader would like to see selected
> examples, for example about which blame labels have been provided by Natural
> and which by Transient, so to see that one approach took a longer path, as
> well as similar examples the reader can learn from. Numbers do not seem to
> teach the whole story in this part of the paper.


