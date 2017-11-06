;; custom generic functions assembled from the internets

(defun my-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun my-shift-region-left (start end &optional count)
  "stolen form py-shift-region-left"
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))

  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (my-shift-region start end (- (prefix-numeric-value (or count 2))))
  t
)

(defun my-shift-region-right (start end &optional count)
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (my-shift-region start end (prefix-numeric-value (or count 2)))
  t
)



(defun find-thing-at-point (&optional always-ask)
  (interactive "P")
  (let* ((at-point (thing-at-point 'symbol))
         (s (and at-point (intern at-point)))
         (v (or (variable-at-point)
                (and s (boundp s) s)))
         (f (or (function-called-at-point)
                (and s (fboundp s) s))))
    (push-mark (point) t)
    (cond
     (always-ask (call-interactively 'find-function))
     ((and v (not (numberp v)))
      (find-variable v))
     ((and f (subrp (symbol-function f)))
      (let ((buf-pos (find-function-search-for-symbol
                      f nil (help-C-file-name (symbol-function f) 'subr))))
        (and (car buf-pos) (pop-to-buffer (car buf-pos)))))
     (f (find-function f))
     (t (call-interactively 'find-function)))))


(defun region-or-thing (thing)
  "Return a vector containing the region and its bounds if there is one
or the thing at the point and its bounds if there is no region"
  (if (use-region-p)
      (vector (buffer-substring-no-properties (region-beginning) (region-end))
              (region-beginning) (region-end))
    (let* ((bounds (bounds-of-thing-at-point thing))
           (beg (car bounds))
           (end (cdr bounds)))
      (if (and beg end)
          (vector (buffer-substring-no-properties beg end) beg end)
        (message "Nothing under point")))))
