
;;============================================================
;;
;;
;;   GLOBAL SHORTCUTS / KEYBINDINGS
;;   alt keycode (kbd "C-c k")
;;
;; \242 :: f12
;; \275 :: "M-="
;; ^C   :: "C-c"
;;
;;============================================================


(global-set-key (kbd "C-c f")       'grep-find)

; keep other-buffer working in dired
(global-unset-key "\C-o")
(define-key dired-mode-map "\C-o" 'other-window)

(global-set-key "\C-xg"       'goto-line)
(global-set-key "\C-x\C-w"    'what-line)
(global-set-key "\C-o"        'other-window)
;(global-set-key "\C-z"        'term)
(global-set-key "\C-xt"       'delete-trailing-whitespace)

(global-set-key "\M-\S-c" 'comment-or-uncomment-region)

;; browse recent cuts
(global-set-key "\C-ck"      'browse-kill-ring)

(global-unset-key "\M-k")
(global-set-key "\M-k" 'kill-buffer)

; vim style shift-j
(fset 'join-lines
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 134217848 106 111 105 110 45 108 105 110 101 return] 0 "%d")) arg)))
(global-set-key "\C-\M-j" 'join-lines)



;; apropos narrow-to-region
(global-unset-key "\C-xnn")
(global-unset-key "\C-xnp")
(global-unset-key "\C-xnd")

(global-set-key (kbd "<M-f10>") 'browse-url)

(defun google ()
  "Do a Google search of the region or symbol at the point"
  (interactive)
  (let ((phrase (elt (region-or-thing 'symbol) 0)))
    (browse-url (concat "http://www.google.com/search?q="
                        (replace-regexp-in-string " " "+" phrase)))))
(global-set-key "\M-?" 'google)


                                        ; repeat-last-command
(global-set-key [M-f7] 'repeat)

                                        ; doc-last-command
(defun doc-last-command()
  (interactive)
  (describe-function last-command))

(global-set-key [C-f7] 'doc-last-command)


(global-unset-key "\M-t")
(global-set-key "\M-t" 'projectile-jump-to-project-file)
(global-set-key "\M-p" 'projectile-jump-to-project-file)


;(define-key global-map (kbd "RET") 'newline-and-indent)
;(global-set-key  (kbd "RET") 'newline-and-indent)
;(global-unset-key "\C-m")
;(global-set-key "\C-m" 'newline-and-indent)


;(global-unset-key "\C-x\C-c") ;; keep closing emacs?



;; Nice quick buffer switching
;(global-set-key "\C-x\C-b" 'electric-buffer-list) ; reserved for C-F4





;----------
; <F1>

;----------
; <F2>
;(global-set-key [f2]              'other-window)
;(global-set-key [f2]            'highlight-phrase)
;(global-set-key [C-f2]          'unhighlight-regexp)


;----------
; <F3>
; 
; 'find-tag-at-point
; 'cscope-find-this-symbol
; 'cscope-index-files


;----------
; <F4>
;----------
; <F5>
;----------
; <F7>

;----------
; <F8>



;----------
; <F9>


;----------
; <F10>


;----------
; <F11>



;----------



