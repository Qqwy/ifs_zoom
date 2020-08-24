(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "caption"
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
    "sec:org5bf88c3"
    "sec:org37188b9"
    "sec:orgc38a3e2"
    "sec:org5427fed"
    "section:background"
    "sec:orgf2dec53"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org98eff38"
    "sec:org7c5f687"
    "sec:org93776c4"
    "subsection:viewport_transformation"
    "sec:org70804c5"
    "sec:org816d2a2"
    "sec:orge55d88f"
    "sec:org6492fc1"
    "sec:orgf50515e"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orge96706e"
    "sec:orgdfbed32"
    "subsection:chaos_game_gpu"
    "sec:org4bae271"
    "subsection:deterministic_gpu"
    "sec:org4e0887c"
    "section:research_question"
    "sec:org0df2582"
    "section:approach"
    "sec:org3ec50a9"
    "sec:org8609329"
    "sec:orga5c59ef"
    "subsection:point_cloud_optimizations"
    "sec:orgf039e0d"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org8082244"
    "subsection:coloring"
    "sec:org6826848"
    "figure:program_flow"
    "sec:org4dac0fa"
    "sec:orge3a92eb"
    "sec:orgf353e4b"
    "listing:barnsley_fern_ifs_file"
    "sec:org101ebe0"
    "sec:org9b86566"
    "sec:orgeacf60c"
    "sec:orgb605ae7"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org9b27a15"
    "section:findings"
    "sec:orgcf1c8cc"
    "subsection:jumping_restrictions"
    "sec:org1d7e9e6"
    "figure:sierpinsky_transformation_borders"
    "sec:org72e722e"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org71c655a"
    "sec:orgc0b22a6"
    "section:conclusion"
    "sec:orgf575055"
    "section:further_work"
    "sec:org0657420"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

