(require 'origami)


(with-eval-after-load 'origami
    (defhydra origami-hydra (:color blue :hint none)
      "
      _:_: recursively toggle node       _a_: toggle all nodes    _t_: toggle node
      _o_: show only current node        _u_: undo                _r_: redo
      _R_: reset                         _c_: close node          _C_: close all
      "
      (":" origami-recursively-toggle-node)
      ("a" origami-toggle-all-nodes)
      ("t" origami-toggle-node)
      ("o" origami-show-only-node)
      ("u" origami-undo)
      ("r" origami-redo)
      ("R" origami-reset)
      ("c" origmai-close-node)
      ("C" origami-close-all-nodes))

    ;;(face-spec-reset-face 'origami-fold-header-face)

    (define-key origami-mode-map (kbd "C-c o") 'origami-hydra/body))


(add-hook 'prog-mode-hook 'origami-mode)


(provide 'init-origami)
