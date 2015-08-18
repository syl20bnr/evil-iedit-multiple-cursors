(require 'eieio)
(require 'iedit-lib)

(defcustom imc-mode-line
  `(" emc:" (:eval (format ,(propertize "%d" 'face 'font-lock-warning-face)
                           (imc//cursors-count))))
  "iedit multiple cursors mode line."
  :group 'iedit-multiple-cursors)
(put 'imc-mode-line 'risky-local-variable t)

(defclass imc-cursor ()
  ((overlay :initarg :overlay
            :initform nil
            :documentation "Overlay associated to the cursor.")
   (dx :initarg :dx
       :initform 0
       :type number
       :documentation "Column delta relative to point.")
   (dy :initarg :dy
       :initform 0
       :type number
       :documentation "Row delta relative to point.")))

(defvar imc--overlay-lenght 1
  "Length of the cursors.")
(setq imc--overlay-lenght 1)

(defvar imc--advices nil
  "List of all command advices.")
(setq imc--advices nil)

(defvar imc--cursors nil
  "List of `imc-cursor'.")
(setq imc--cursors nil)
(make-variable-buffer-local 'imc--cursors)

(defvar imc--refresh-cursors-cmds
  '(evil-forward-char
    evil-backward-char
    evil-next-line
    evil-previous-line)
  "List of all commands that refresh the cursor positions.")
(setq imc--refresh-cursors-cmds
      '(evil-forward-char
        evil-backward-char
        evil-next-line
        evil-previous-line))

(defun imc//advice-command-refresh-cursors (cmd)
  "Advice CMD in order to refresh cursors positions."
  (let ((advice (intern (format "emp/adv-nav-%S" cmd))))
    (eval
     `(progn
        (push ',(list cmd advice :around) imc--advices)
        (defun ,advice (func &rest args)
          (advice-remove ',cmd ',advice)
          (ignore-errors
            (call-interactively func))
          (imc//refresh-cursors)
          (advice-add ',cmd :around ',advice))))))
(dolist (cmd imc--refresh-cursors-cmds)
  (imc//advice-command-refresh-cursors cmd))

(define-minor-mode iedit-multiple-cursors
  "A multiple cursors mode for iedit and Evil."
  nil imc-mode-line nil
  (if iedit-multiple-cursors
      (progn
        (imc//compute-all-cursor-delta)
        (imc/add-cursor (point))
        (imc//advice-add-all)
        (add-hook 'evil-insert-state-exit-hook 'imc//normalize-cursor-start nil t))
    (imc//advice-remove-all)
    (remove-hook 'evil-insert-state-exit-hook 'imc//normalize-cursor-start t)
    (imc//cleanup)))

(defun imc//cleanup ()
  "Remove all the cursors and clean data structures."
  (setq imc--cursors nil)
  (iedit-cleanup))

(defun imc/make-cursor (position)
  "Return an `imc-cursor' object from absolution buffer POSITION.
Return nil if there is already a cursor at POSITION."
  (unless (imc//find-cursor-at-position position)
    (let ((o (iedit-make-occurrence-overlay position (1+ position))))
      (push o iedit-occurrences-overlays)
      (imc-cursor nil :overlay o))))

(defun imc/add-cursor (&optional position)
  "Add a cursor under point."
  (interactive)
  (let ((c (imc/make-cursor (if position position (point)))))
    (when c (push c imc--cursors))))

(defun imc/grow-cursor ()
  "Grow cursors by 1."
  (interactive)
  (setq imc--overlay-lenght (1+ imc--overlay-lenght))
  (imc//refresh-cursors))

(defun imc//find-cursor-at-position (position)
  "Return non nil if POSITION is a cursor position."
  (iedit-find-overlay-at-point position 'iedit-occurrence-overlay-name))

(defun imc//compute-all-cursor-delta ()
  "Compute the column and row delta for all registered cursors."
  (let ((pcol (column-number-at-pos (point)))
        (prow (line-number-at-pos (point))))
    (dolist (c imc--cursors)
      (imc//compute-cursor-delta c pcol prow))))

(defun imc//compute-cursor-delta (c &optional rcol rrow)
  "Compute the column and row delta for passed Cursor.
RCOL and RROW are coordinates of the reference point."
  (let ((o (oref c :overlay))
        (rcol (if rcol rcol (column-number-at-pos (point))))
        (rrow (if rrow rrow (line-number-at-pos (point)))))
    (save-excursion
      (goto-char (overlay-start o))
      (oset c :dx (- (column-number-at-pos (point)) rcol))
      (oset c :dy (- (line-number-at-pos (point)) rrow)))))

(defun imc//refresh-cursors ()
  "Refresh all cursors"
  (dolist (c imc--cursors)
    (let ((px (column-number-at-pos (point)))
          (py (line-number-at-pos (point)))
          (mx (save-excursion
                (end-of-line)
                (column-number-at-pos (point))))
          (my (line-number-at-pos (point-max))))
      (dolist (c imc--cursors)
        (imc//move-cursor c px py mx my)))))

(defun imc//move-cursor (c &optional px py mx my)
  "Move cursor C given referential PX,PY bound to MX and MY."
  (let ((px (if px px (column-number-at-pos (point))))
        (py (if py py (line-number-at-pos (point))))
        (mx (if mx mx (save-excursion
                        (end-of-line)
                        (column-number-at-pos (point)))))
        (my (if my my (line-number-at-pos (point-max))))
        (o (oref c :overlay))
        (dx (oref c :dx))
        (dy (oref c :dy)))
    (let ((nx (+ px dx))
          (ny (+ py dy)))
      ;; clamp px
      (when (> 0 nx) (setq dx (- px)))
      (when (> nx mx) (setq dx (- mx px)))
      ;; clamp py
      (when (> 0 ny) (setq dy (- py)))
      (when (> ny my) (setq dy (- my py)))
      ;; update overlay
      (save-excursion
        (ignore-errors
          (unless (equal dx 0) (forward-char dx))
          (unless (equal dy 0) (forward-line dy)))
        (move-overlay o (point) (+ (point) imc--overlay-lenght))))))

(defun imc//normalize-cursor-start ()
  "Reduce all cursor to length 1."
  (setq imc--overlay-lenght 1)
  (dolist (c imc--cursors)
    (let* ((o (oref c :overlay))
           (npos (overlay-start o)))
      (when (eq o (iedit-find-current-occurrence-overlay))
        (evil-goto-char npos))
      (save-excursion
        (goto-char npos)
        (move-overlay o npos (+ npos imc--overlay-lenght)))))
  (imc//compute-all-cursor-delta))

(defun imc//cursors-count ()
  "Return the number of current overlays."
  (if (null imc--cursors) 0 (length imc--cursors)))

(defun imc//maybe-iedit-multiple-cursors-mode ()
  "Enable iedit-multiple-cursors-mode if there is more than one active cursor."
  (if (> (imc//cursors-count) 0)
      (iedit-multiple-cursors)
    (iedit-multiple-cursors -1)))

(defun imc//advice-add-all ()
  "Enable all the command advices in `imc--advices'"
  (dolist (a imc--advices)
    (let ((cmd (car a))
          (advice (cadr a))
          (step (caddr a)))
      (advice-add cmd step advice))))

(defun imc//advice-remove-all ()
  "Disable all the command advices in `imc--advices'"
  (dolist (a imc--advices)
    (let ((cmd (car a))
          (advice (cadr a)))
      (advice-remove cmd advice))))
