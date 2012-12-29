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

