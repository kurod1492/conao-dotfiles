
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; key-chord
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package use-package-chords
  :ensure t)

(use-package smartrep
  :ensure t)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))
