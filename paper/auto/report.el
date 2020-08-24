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
    "sec:org289c79d"
    "sec:orgfd3e6ac"
    "sec:orgd3d349e"
    "sec:org0fcf850"
    "section:background"
    "sec:org7d9f357"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgfd99fc9"
    "sec:org88f1c90"
    "sec:org00b285f"
    "subsection:viewport_transformation"
    "sec:org0d31366"
    "sec:orgcb4c722"
    "sec:org194da06"
    "sec:org4508e30"
    "sec:org85972cc"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org22d48dd"
    "sec:org9b5fd9e"
    "subsection:chaos_game_gpu"
    "sec:org6c6859b"
    "subsection:deterministic_gpu"
    "sec:org3719c2f"
    "section:research_question"
    "sec:org5b0a7be"
    "section:approach"
    "sec:org5ddbd88"
    "sec:orga94cf5a"
    "sec:org21cdaa7"
    "subsection:point_cloud_optimizations"
    "sec:orgc8ac767"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orgd8f3352"
    "subsection:coloring"
    "sec:org835d214"
    "figure:program_flow"
    "sec:org27aa2a5"
    "sec:orgdef7346"
    "sec:orgb0685a0"
    "listing:barnsley_fern_ifs_file"
    "sec:org9169de1"
    "sec:orgde1489a"
    "sec:orgba2000b"
    "sec:orgc1d5c42"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org965cc06"
    "section:findings"
    "sec:orge17e675"
    "subsection:jumping_restrictions"
    "sec:orge9f735e"
    "figure:sierpinsky_transformation_borders"
    "sec:org7fdc09e"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgdaf96cd"
    "sec:org1e92545"
    "section:conclusion"
    "sec:org9d45636"
    "section:further_work"
    "sec:org10f51da"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

