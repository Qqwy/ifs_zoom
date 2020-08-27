(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "sec:org7240d30"
    "sec:orgd35ce86"
    "sec:org323fc8b"
    "sec:orge357989"
    "section:background"
    "sec:org21b369c"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org461458b"
    "subsection:formal_ifs_definition"
    "sec:orge0d050f"
    "sec:orgead3505"
    "subsection:viewport_transformation"
    "sec:org06f5d14"
    "sec:orga10df4c"
    "sec:org76146bb"
    "sec:orga95b4a5"
    "sec:org5055aec"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org78f22f0"
    "sec:org8a89bc9"
    "subsection:chaos_game_gpu"
    "sec:org0d3e9de"
    "subsection:deterministic_gpu"
    "sec:org6246573"
    "section:research_question"
    "sec:org7af0901"
    "section:approach"
    "sec:orgda9c035"
    "sec:org34eeb2f"
    "sec:org9cac667"
    "subsection:point_cloud_optimizations"
    "sec:org983262c"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org0362c3d"
    "subsection:coloring"
    "sec:orgdf85bb7"
    "figure:program_flow"
    "sec:orgcf38e4e"
    "sec:org23f3eb9"
    "sec:orge9049d5"
    "listing:barnsley_fern_ifs_file"
    "sec:org4743482"
    "sec:org64dc7d2"
    "sec:org10ae76e"
    "sec:org0710bba"
    "figure:barnsley_points"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides"
    "figure:barnsley_guides_vs_points"
    "sec:org009d0aa"
    "section:findings"
    "sec:orgb970861"
    "subsection:jumping_restrictions"
    "sec:org84143dd"
    "figure:sierpinsky_transformation_borders"
    "sec:org3a15828"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org86c8585"
    "sec:orgf2ca863"
    "section:conclusion"
    "sec:org6e0b6eb"
    "section:further_work"
    "sec:org5c3239b"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

