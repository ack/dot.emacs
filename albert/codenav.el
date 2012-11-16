(set-default 'imenu-auto-rescan t)

(defvar imenu--selection-buffer " *imenu-select*")
(defvar imenu--target-buffer nil)
(defun imenu-make-selection-buffer (&optional index-alist)
  (interactive)
  (require 'which-func)
  (setq index-alist (if index-alist index-alist (imenu--make-index-alist)))
  (let ((cur (which-function)))
    (when (listp cur)
      (setq cur (car cur)))
    (setq imenu--target-buffer (current-buffer))
    (pop-to-buffer imenu--selection-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (dolist (x index-alist)
      (insert (car x) "\n"))
    (if cur (search-backward (concat cur "\n") nil t))
    ;(beginning-of-buffer)
    (imenu-selection-mode)
    ))

; function/object/etc selected
(defun imenu-selection-select ()
  (interactive)
  (let ((sel (substring (thing-at-point 'line) 0 -1)))
    ;(bury-buffer)
    (pop-to-buffer imenu--target-buffer)
    (imenu sel)))

;(add-hook 'ruby-mode-hook 'imenu-add-menubar-index)
;(add-hook 'ruby-mode-hook 'imenu-make-selection-buffer)
                                     




(defun my-icompleting-read (prompt choices)
  (flet ((ido-make-buffer-list (default)
           (setq ido-temp-list choices)))
    (ido-read-buffer prompt)))


(defun my-imenu-jump-to-function ()
"Jump to a function found by Semantic within the current buffer
with ido-style completion."
  (interactive)
  (save-excursion
    (setq imenu--index-alist (funcall imenu-create-index-function)))
  (let ((thing (assoc
                   (my-icompleting-read "Go to: "
                                        (mapcar #'car imenu--index-alist))
                   imenu--index-alist)))
    (when thing
      (funcall imenu-default-goto-function (car thing) (cdr thing))
      (recenter))))



(defun my-imenu-next-browse ()
  (interactive)
  (next-line)
  (imenu-selection-select)
  (pop-to-buffer imenu--selection-buffer)
  )

(defun my-imenu-previous-browse ()
  (interactive)
  (previous-line)
  (imenu-selection-select)
  (pop-to-buffer imenu--selection-buffer)
  )

(defun my-imenu-select-collapse ()
  (interactive)
  (kill-buffer-and-window)
  (imenu-selection-select)
  )

(define-derived-mode imenu-selection-mode fundamental-mode "imenu"
  "Major mode for imenu selection."
  (suppress-keymap imenu-selection-mode-map)
  (define-key imenu-selection-mode-map "n" 'my-imenu-next-browse)
  (define-key imenu-selection-mode-map "\C-n" 'my-imenu-next-browse)
  (define-key imenu-selection-mode-map "p" 'my-imenu-previous-browse)
  (define-key imenu-selection-mode-map "\C-p" 'my-imenu-previous-browse)
  (define-key imenu-selection-mode-map "\C-m" 'my-imenu-select-collapse)
  (define-key imenu-selection-mode-map "k" 'kill-this-buffer)
  )

