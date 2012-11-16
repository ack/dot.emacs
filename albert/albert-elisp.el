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



;; window utils
(defun window-swap-pair-vertical (win right vertical)
  (interactive)
  (let ((buff (window-buffer right)))
    (delete-window right)
    (if (eq vertical t)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer buff)))

(defun window-swap-orientation ()
  (interactive)
  ; nuke myself
  (walk-windows
   (lambda (me)
     ; check i'm still "live" - i have a buffer
     (if (window-buffer me)
         (let ((right (window-in-direction 'right me))
               (left (window-in-direction 'left me))
               (above (window-in-direction 'above me))
               (below (window-in-direction 'below me))
               (next (window-right me)))
           (if (and (> (count-windows) 1)
                    (not (eq next nil)))
               (cond
                ((eq next right) (window-swap-pair-vertical me next t))
                ((eq next below) (window-swap-pair-vertical me next nil)))))))
   nil nil))

(defun bury-window ()
  (interactive)
  (delete-window (get-buffer-window)))
