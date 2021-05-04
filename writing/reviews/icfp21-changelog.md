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

