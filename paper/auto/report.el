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
    "sec:orgcbac8f9"
    "sec:org81a5d96"
    "sec:orgad41f31"
    "sec:org5bb4103"
    "section:background"
    "sec:org8ea7682"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org23cf2c1"
    "subsection:formal_ifs_definition"
    "sec:org3d29d19"
    "sec:org52338e9"
    "subsection:viewport_transformation"
    "sec:orgce9b24e"
    "sec:org401cd6a"
    "sec:orge6db19b"
    "sec:orga91d556"
    "sec:org033f677"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org6deb60e"
    "sec:orgf35d103"
    "subsection:chaos_game_gpu"
    "sec:orgedf7f19"
    "subsection:deterministic_gpu"
    "sec:orgc952029"
    "section:research_question"
    "sec:orgb6c8a1e"
    "section:approach"
    "sec:org97de704"
    "sec:orga07332c"
    "sec:org35380bf"
    "subsection:point_cloud_optimizations"
    "sec:org60c4c7b"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org4d787eb"
    "subsection:coloring"
    "sec:org7052480"
    "figure:program_flow"
    "sec:org09f105d"
    "sec:org76c040d"
    "sec:org47aa908"
    "listing:barnsley_fern_ifs_file"
    "sec:orgd092a71"
    "sec:org54d4ae4"
    "sec:orgd28cb11"
    "sec:org0c75402"
    "figure:barnsley_points"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides"
    "figure:barnsley_guides_vs_points"
    "sec:org4020d2a"
    "section:findings"
    "sec:orgb28c048"
    "subsection:jumping_restrictions"
    "sec:org37b3a8a"
    "figure:sierpinsky_transformation_borders"
    "sec:org498caf1"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgad0c160"
    "sec:org764ae14"
    "section:conclusion"
    "sec:orga088b2c"
    "section:further_work"
    "sec:org2990aae"
    "appendix:ifss"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

