(use-package key-chord :ensure t
  :init ;; (el-get-bundle zk-phi/key-chord)
  :config
  (setq key-chord-two-keys-delay 0.15
        key-chord-safety-interval-backward 0.1
        key-chord-safety-interval-forward  0.25)
  (key-chord-mode 1))
(use-package use-package-chords :ensure t)
