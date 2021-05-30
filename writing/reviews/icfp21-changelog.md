Change log
==========

Log format
----------
Review #:
> comment
*Changes to address.*


Stressed changes
----------------
Review C:
> Sections 8 and 9 report on the finding of the paper. These include statements
> that blame is useful, that Natural often produces shorter trails than
> Transient, and others. My only issue at this point is that the reader is left
> with numnbers only, while I think a reader would like to see selected
> examples, for example about which blame labels have been provided by Natural
> and which by Transient, so to see that one approach took a longer path, as
> well as similar examples the reader can learn from. Numbers do not seem to
> teach the whole story in this part of the paper.
*Add new prose + figure at beginning of discussion section (9, pages 22-23) walking through an example trail for every mode.*


Review D:
> The characterization of the Monotonic semantics for references [22] as a
> variant of natural isn't correct--its exception-raising and blame
> behavior for references (though not for functions) is quite different
> from that of Natural. For example, a monotonic reference imported from
> untyped code into two incompatible typed contexts will error
> immediately, before being used.
*Fix discussion of Monotonic in related work section (10, page 26).*


Review D's other concerns:
*Fix descriptions of Reticulated type inference and performance in the discussion section (9.6, pages 25-26).*
Specifically, we 
1. Fixed the mistaken slowdown multipliers (bottom of page 25), and
2. revised all of the bullets summarizing the factors that may explain the observed slowdown (top of page 26).


Other revisions based on reviews
--------------------------------
Review A:
> When the rational programmer "fixes" a module and reruns, the implicit
> assumption seems to be that he runs the program in the exact same way. That
> would make the most sense to me, but you do not seem to come out and say so.
*Note in section 3 (page 6) that the rational programmer runs the program in the same way every time.*


Review B:
>  - Why does the paper conclude that "the existing theory does not predict
>    practice properly"?  Figure 8 shows that the theory (Natural) works well.
>  - What does the paper mean by "The existing practice may need additional
>    experiments"?  I have not found any evidence that confirms this claim in the
>    paper.
>  - What does "practice" means here?
*Rephrase the end of the intro section (1, page 2) to make clear the meaning of "practice".*


Review B:
> - L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?
> - L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
>   does it intend?
*Adjust Transient threats discussion to make clear when we talk about Python benchmarks vs Racket ones (section 9.6, pages 25-26).*


Review B:
> - Page 7: How is the question (5) answered?
*Answer question 5 explicitly in section 3.1 (page 7).*


Review B:
> - It seems that the experiment implicitly supposes a few assumptions on
>   benchmarks.  First, the original benchmarks must be fully typed.  Thus more
>   experiments on mix-typed programs where some components cannot be typed may be
>   needed.  Second, the benchmarks are supposed to be deterministic, which I find
>   from the definitions of trails.  If it is the case, while I do not think these
>   restrictions have to be lifted in the paper, it would be nice to expose them.
*Note assumptions that benchmark modules are typeable and deterministic (in 6.1, pages 13-14).*


Review B:
>  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
>    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
>    be nicer to address them in a clearer manner.
and
>  - Which blame assignment strategy is "good" and which is not "good"?
*Reword conclusions of the intro section (1, page 2).*


Review B:
> - L424 "Let a configuration s of P":  Are configurations the same as scenarios?
*Use the term configuration consistently to clarify that a debugging scenario is a kind of configuration (section 5, especially 5.1 on page 9).*


Review B:
> - L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
>   "does not reliably lead to type errors"?
*Remove footnote about mutating `*` and `/`.*


All reviews mentioning typos.
*Typo fixes, including in particular the 756/752 typo.*


Review B:
> - L1198 "But just because...":  It is difficult for me to find what this
>   sentence wants to say.  Please consider rephrasing.
*Rephrase start of conclusion (11, page 27).*


Review B:
> - L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
>   consider clarifying.
*Rephrase to eliminate use of "soundness mechanisms" (2).*


Review A:
> It would help if you could explain how a mutator like negate-cond actually
> leads to a program that has more type-level mistakes than before. It does not
> seem to me this part of the code itself will now fail, but that changing the
> conditions makes certain previously excluded paths in the code feasible.
and B:
> - It is not fully explained how the given mutators change programs.
*Add an example and explanation of the kinds of programs occurrence-typing mutators target (6.2, page 15).*

Review B:
> - In principle, Natural blame should always point out the faulty components
>   by the Wadler-Findler slogan.  However, the experiment shows it is not the
>   case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
>   to a gap between theory and practice, but exposing a reason is crucial.)
and
> - Are failing Natural blame trails produced even for programs with impedance
>   mismatch?  (This question is related to the above issue with Natural blame.)
*Extend definition of blame modes to clarify that we use stack in absence of blame, and add paragraph explaining why it's necessary (5.2, 5.3; pages 10-11).*
*This now connects more clearly with the description on page 17, next to figure 10, describing trail failures we observed in the data.*


Review B:
> The table in section 2 looks rather ugly
*Simplify the table layout at the end of section 2 (page 4).*


Review B:
> - The paper often says that a blame system is (un)sound, but it is difficult for
>   me to identify what it precisely means.
*Ensured that sound and complete blame is adequately referenced throughout.*


Review B:
> - L1099 "as behavioral economics has shown more recently":  Is there a reference
>   to be cited?
*Add references for homo economicus and its problems (3, 9.3; pages 5, 24).*


Review B:
>   - The benchmarks are selected (line 647), but why?  The GPT benchmark suite of
>     Racket provides more examples.
*Added a paragraph to section 6.1 (page 14) clarifying how the benchmarks from GTP were filtered.*


Review B:
> - L356 "despite advertisements for the opposite":  I cannot find what this means.
*Clarified on page 8.*


Review B:
> - L370 "the latter must represent":  What does the "latter" specify?
*Rephrased on page 8.*


Other improvements
------------------
*Update data in results section (8) with fixed issue that made some modes look worse than they should.*
Fixing this issue does not change our analysis or conclusions.


*Clean up tables in figures 4 and 5 (pages 13, 14).*


*Add a new threat subsection (9.2, page 24) to make generalization caveats very clear.*


*Clarified notions of boundary and blame, and how they differ between Natural and Transient, in section 2.*


*Add a new threat subsection (9.4, page 25) to clarify the threat of erasure bias.*


*Minor prose revisions, touch-ups, and rewording throughout.*
