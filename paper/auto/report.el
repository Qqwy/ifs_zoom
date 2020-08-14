(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "float"
    "geometry"
    "algorithm2e"
    "listings"
    "xcolor"
    "pifont"
    "subcaption"
    "enumitem"
    "newfloat"
    "todonotes"
    "kantlipsum"
    "rugscriptie")
   (TeX-add-symbols
    "cmark"
    "xmark"
    "altasterism")
   (LaTeX-add-labels
    "sec:org3c937f9"
    "sec:org5ee3bdd"
    "sec:org28a8b3e"
    "sec:orgb9c5f45"
    "section:background"
    "sec:org1ea3452"
    "figure:sierpinsky_iterations"
    "sec:orgf792716"
    "sec:org2fa4da6"
    "sec:orgc306129"
    "subsection:viewport_transformation"
    "sec:org63bff09"
    "sec:orgf3b100a"
    "sec:org14a3cb9"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org01e11eb"
    "sec:org9976dc4"
    "subsection:chaos_game_gpu"
    "sec:org28f9daf"
    "subsection:deterministic_gpu"
    "sec:org468fe1c"
    "section:research_question"
    "sec:org96bc9cc"
    "section:approach"
    "sec:orgbdcca37"
    "sec:orgb6b9fff"
    "sec:org55cc09a"
    "subsection:point_cloud_optimizations"
    "sec:orgfea5511"
    "subsection:self_similarity"
    "sec:org639899b"
    "subsection:coloring"
    "sec:orgfce0c01"
    "figure:program_flow"
    "sec:org8da3235"
    "sec:org0d90f96"
    "sec:org6fbcfbe"
    "listing:barnsley_fern_ifs_file"
    "sec:orge405780"
    "sec:org5af5b4e"
    "sec:org49b20f3"
    "sec:orgb39c18c"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org3110526"
    "section:findings"
    "sec:org3adce89"
    "subsection:jumping_restrictions"
    "sec:org25a544f"
    "sec:org2f46562"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org8024ee5"
    "sec:org0dfd15a"
    "section:conclusion"
    "sec:org3e80da6"
    "section:further_work"
    "sec:org8fa2bc5"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

