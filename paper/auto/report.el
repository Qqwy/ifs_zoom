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
    "sec:orgd88de45"
    "sec:orgeaf20a7"
    "sec:orga5b2460"
    "sec:orgd84a6d8"
    "section:background"
    "sec:org4f51759"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgc0cbde3"
    "subsection:formal_ifs_definition"
    "sec:orgf7f5f53"
    "sec:orgc228070"
    "subsection:viewport_transformation"
    "sec:org89ea1ba"
    "sec:org24aebeb"
    "sec:orgf5cc6a6"
    "sec:orga8bb6ed"
    "sec:org3d53d73"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org83088d0"
    "sec:org7f3bd12"
    "subsection:chaos_game_gpu"
    "sec:org77ffd1c"
    "subsection:deterministic_gpu"
    "sec:orgdbf9234"
    "section:research_question"
    "sec:org26f3388"
    "section:approach"
    "sec:orgaab2a08"
    "sec:org95f2603"
    "sec:org7fee1f7"
    "subsection:point_cloud_optimizations"
    "sec:orge5954d6"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgf67bbda"
    "subsection:coloring"
    "sec:org2830548"
    "figure:program_flow"
    "sec:org69ad330"
    "sec:org41c7928"
    "sec:org9cc2959"
    "listing:barnsley_fern_ifs_file"
    "sec:org6f88d76"
    "sec:orgb80cc47"
    "sec:org087b415"
    "sec:org3f4c18e"
    "figure:barnsley_points"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides"
    "figure:barnsley_guides_vs_points"
    "sec:org398ff7e"
    "section:findings"
    "sec:org65f2efd"
    "subsection:jumping_restrictions"
    "sec:org8cd0fea"
    "figure:sierpinsky_transformation_borders"
    "sec:org46123c8"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org6a4f19c"
    "sec:orga792377"
    "section:conclusion"
    "sec:org6a08dfe"
    "section:further_work"
    "sec:org7ad83de"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

