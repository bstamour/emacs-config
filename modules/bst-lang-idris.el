;; TODO: Integrate this somehow.

(deftheme idris-leuven-theme
  "Idris-mode colors compatible with Leuven")

(custom-theme-set-faces
 'idris-leuven-theme
 '(idris-equals-face ((t (:foreground "black" :weight semi-bold))))
 '(idris-colon-face ((t (:foreground "black" :weight semi-bold))))
 '(idris-metavariable-face ((t (:underline (:color "#006DAF" :style wave) :slant italic))))
 '(idris-operator-face ((t (:foreground "black" :weight semi-bold))))
 '(idris-repl-output-face ((t (:foreground "#606060"))))
 '(idris-semantic-function-face ((t (:foreground "darkgreen"))))
 '(idris-semantic-implicit-face ((t (:slant italic))))
 '(idris-keyword-face ((t (:foreground "black" :weight bold :underline t)))))

(provide-theme 'idris-leuven-theme)

(provide 'bst-lang-idris)
