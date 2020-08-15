(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "sec:orgf7374aa"
    "sec:org3924f43"
    "sec:org789bf3f"
    "sec:org397c3b1"
    "section:background"
    "sec:orge7cfc89"
    "figure:sierpinsky_iterations"
    "sec:org71231b6"
    "sec:org01c01a1"
    "sec:org1f6d18e"
    "subsection:viewport_transformation"
    "sec:org962f075"
    "sec:orga78fe74"
    "sec:org9fb8135"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgce8d387"
    "sec:orgbe11e2b"
    "subsection:chaos_game_gpu"
    "sec:org813caa0"
    "subsection:deterministic_gpu"
    "sec:orga129072"
    "section:research_question"
    "sec:org0e73e63"
    "section:approach"
    "sec:org551c1fd"
    "sec:orgdc7aa38"
    "sec:org0c48458"
    "subsection:point_cloud_optimizations"
    "sec:org65b6877"
    "subsection:self_similarity"
    "sec:orgd369463"
    "subsection:coloring"
    "sec:org2a3072d"
    "figure:program_flow"
    "sec:orgc030b8b"
    "sec:org05fc06e"
    "sec:orgaf15538"
    "listing:barnsley_fern_ifs_file"
    "sec:orga93c522"
    "sec:org07d9f47"
    "sec:orgdc52a0e"
    "sec:orgd043695"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org50785ff"
    "section:findings"
    "sec:org7e98813"
    "subsection:jumping_restrictions"
    "sec:org7871375"
    "sec:orge802b88"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org906a165"
    "sec:org24663af"
    "section:conclusion"
    "sec:org0124ead"
    "section:further_work"
    "sec:org6e763f9"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

