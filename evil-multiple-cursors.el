(require 'iedit-lib)

(defcustom emc-mode-line
  `(" emc:" (:eval (format ,(propertize "%d" 'face 'font-lock-warning-face)
                           (emc//cursors-count))))
  "Evil multiple cursors mode line."
  :group 'evil-multiple-cursors)
(put 'emc-mode-line 'risky-local-variable t)

(defvar emc--advices nil
  "List of all command advices.")
(setq emc--advices nil)

(defvar emc--run-for-all-cmds
  '(evil-forward-char
    evil-backward-char
    evil-next-line
    evil-previous-line)
  "List of all commands to run for all cursors.")

(define-minor-mode evil-multiple-cursors
  "A multiple cursors mode that will hopefully work with evil."
  nil emc-mode-line nil
  (if evil-multiple-cursors
      (progn
        (emp//advice-add-all))
    ;; disable mode
    (emp//advice-remove-all)
    ;; (setq emc--advices nil)
    (iedit-cleanup)))

(defun emc/add-cursor ()
  "Add a cursor under point."
  (interactive)
  (unless (emc//find-cursor-at-point)
    (push (iedit-make-occurrence-overlay (point) (1+ (point)))
          iedit-occurrences-overlays))
  (emc//maybe-multiple-cursors-mode))

(defun emc//cursors-count ()
  "Return the number of current overlays."
  (if (null iedit-occurrences-overlays)
      0
    (length iedit-occurrences-overlays)))

(defun emc//maybe-multiple-cursors-mode ()
  "Enable evil-multiple-cursors-mode if there is more than one active cursor."
  (if (> (emc//cursors-count) 0)
      (evil-multiple-cursors)
    (evil-multiple-cursors -1)))

(defun emc//find-cursor-at-point ()
  "Return non nil if point is at a cursor position."
  (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name))

(defun emp//advice-add-all ()
  "Enable all the command advices in `emc-advices'"
  (dolist (a emc--advices)
    (let ((cmd (car a))
          (advice (cdr a)))
      (advice-add cmd :around advice))))

(defun emp//advice-remove-all ()
  "Disable all the command advices in `emc-advices'"
  (dolist (a emc--advices)
    (let ((cmd (car a))
          (advice (cdr a)))
      (advice-remove cmd advice))))

(defun emp//execute-for-all-cursors (cmd)
  "Enable CMD to be replicated with all cursors."
  (let ((advice (intern (format "emp/adv-%S" cmd))))
    (eval
     `(progn
        (push ',(cons cmd advice) emc--advices)
        (defun ,advice (func &rest args)
          (advice-remove ',cmd ',advice)
          (save-excursion
            (dolist (o iedit-occurrences-overlays)
              (evil-goto-char (overlay-start o))
              (let ((goal-column (current-column)))
                (call-interactively func))
              (move-overlay o (point) (1+ (point)))))
          (call-interactively func)
          (advice-add ',cmd :around ',advice))))))

(dolist (cmd emc--run-for-all-cmds)
  (emp//execute-for-all-cursors cmd))

