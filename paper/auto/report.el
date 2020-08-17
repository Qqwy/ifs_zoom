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
    "sec:org18514b2"
    "sec:org8202a4a"
    "sec:orgf56d144"
    "sec:org90f54be"
    "section:background"
    "sec:orga3cd6e6"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgaa1ac28"
    "sec:org84e3816"
    "sec:orge930250"
    "subsection:viewport_transformation"
    "sec:orgd08db56"
    "sec:orge3aaf7b"
    "sec:org1701481"
    "sec:org333c158"
    "sec:orgc331e16"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org55fc3d2"
    "sec:org1f9fd6f"
    "subsection:chaos_game_gpu"
    "sec:org4b8bdf7"
    "subsection:deterministic_gpu"
    "sec:orgff29a2a"
    "section:research_question"
    "sec:orgd0af613"
    "section:approach"
    "sec:orgd1732a3"
    "sec:org80055b9"
    "sec:org6332cb2"
    "subsection:point_cloud_optimizations"
    "sec:org0def9f9"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orga72685f"
    "subsection:coloring"
    "sec:org108730b"
    "figure:program_flow"
    "sec:org5afec4a"
    "sec:org959e93b"
    "sec:org05d90d3"
    "listing:barnsley_fern_ifs_file"
    "sec:orgc3ca0f9"
    "sec:org003d8b8"
    "sec:org43b0f3e"
    "sec:orgc422743"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org2951ccb"
    "section:findings"
    "sec:orga2ac8fe"
    "subsection:jumping_restrictions"
    "sec:org170c7f9"
    "figure:sierpinsky_transformation_borders"
    "sec:orgf6d8f51"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org9304b27"
    "sec:org3c26a7d"
    "section:conclusion"
    "sec:orgcfc3350"
    "section:further_work"
    "sec:orgdd34aac"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

