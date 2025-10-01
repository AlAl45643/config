;;;; PACKAGE management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(use-package use-package-core
  :custom
  (use-package-always-defer t))

;;;; keybinds

(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup)
  )


(general-create-definer global-evil-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix ","
  :non-normal-prefix "C-,")

;; may use someday
;; (general-create-definer global-leader
;;   :keymaps 'override
;;   :states '(insert normal hybrid motion visual operator)
;;   :prefix "SPC m"
;;   :non-normal-prefix "M-SPC m"
;;   "" '(:ignore t :which-key (lambda (arg) (cons (cadr (split-string (car arg) " ")) (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(defmacro +general-global-menu! (name infix-key &rest body)
  "Create a definer named +general-global-NAME.
 Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     ;; don't use :which-key it is less performant according to general.el author
     ;; also for some reason which-key duplicates keybindings
     (which-key-add-key-based-replacements (concat "SPC " ,infix-key) ,name)  
     (which-key-add-key-based-replacements (concat "M-SPC " ,infix-key) ,name)  
     (general-create-definer ,(intern (concat "+general-global-" name))
       :keymaps 'override
       :states '(insert emacs normal hybrid motion visual operator)
       :prefix "SPC"
       :non-normal-prefix "M-SPC"
       :infix ,infix-key
       )
     (,(intern (concat "+general-global-" name))
      ,@body)))


(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))




;; rules
;; binding rules
;; 1. All commands should be be classified as minibuffer, module, mode, and global.
;; 2. Any command that is to be bound should be prioritized based on its frequency of use against all other commands within their type.
;; 3. The commands should then be bound to the shortest semantically correct key binding left for that command in order.
;; 4. If multiple semantic keybindings for a command are the same length then the keybinding shall be chosen according to this order
;; 1) evil
;; 2) evil menu
;; 3) global menu
;;
;; global menu semantic rules
;; 1. Each menu should be named according to the most descriptive adjective that describes every command it contains. 
;; 2. Commands that have similar functionality and adjective but are in different menus should have the same key.
;; 3. The menu key should be either the first or last character of the descriptive adjective or the first character of each syllable in that adjective. The descriptive function keyword does not have necessarily have to be in the command
;; 4. Any command bound within a menu should be the smoothest key for the chord that is the first or last character of the most descriptive function keyword in the command or the first character in each syllable of the keyword.
;;
;; evil menu semantic rules
;; 1. The evil menu should not have any menus within it.
;; 2. Any command bound within the evil menu should be the smoothest key for the chord that is the first or last character of the most descriptive function keyword in the command or the first character in each syllable of the keyword.
;;
;; evil semantic rules
;; 1. All keys bound outside of the global menu and evil menu should follow evil semantic rules.
;;
;; general.el rules
;; + Use definers when , prefix or SPC prefix.
;; + Use mode-maps for mode-specific keybinds instead of :predicate or hooks unless mode-maps do not exist
;; + Use mode-map with #'evil-make-intercept-map and #'evil-normalize hook instead of 'override with :predicate unless mode keymap does not exist
;; 
;;
;; Active keymap hiearchy
;; overriding-terminal-local-map
;; overriding-local-map
;; (get-char-property (point) 'keymap)
;; emulation-mode-map-alists == general-override-mode-map (copying the ones with state) then evil-mode-maps
;; minor-mode-overriding-map-alist
;; minor-mode-map-alist == general-override-mode-map then minor-mode-maps
;; (get-char-property (point) 'local-map)
;; current-local-map
;; current-global-map == emacs insert keys


;; minibuffer specific command priority
;; C-j vertico-next
;; C-k vertico-previous
;; C-b evil-backward char
;; C-f evil-forward char
;; C-S-j scroll-down-command
;; C-S-k scroll-up-command

;;
;; module specific command priority
;; C-SPC corfu-insert-separator
;; C-h corfu-popupinfo-toggle
;; C-S-j corfu-popupinfo-scroll-up
;; C-S-k corfu-popupinfo-scroll-down
;; Enter ibuffer-visit-buffer quit-ibuffer
;; Enter visual org-remark-mark
;; Enter imenu-goto-node
;; C-j imenu evil-next-line
;; C-k imenu evil-previous-line
;; C-g imenu-list-quit-window
;; o org-remark-open
;; C-n dape-next
;; C-p dape-step-in  
;; C-t dape-continue  
;; C-I dape-info  
;; C-<escape> dape-quit  
;; C-P dape-step-out  
;; C-<tab> org-cycle
;; C-<iso-lefttab> org-shifttab
;; C-<tab> yas-keymap
;; C-<iso-lefttab> 'yas-prev-field



;; global key commands priority
;; M-x execute-extended command
;; TAB smart-tab (vertico / eshell/ corfu/ indent / hippie-expand)
;; , s switch-to-buffer
;; , r run-program
;; , t popper-toggle
;; , e save-some-buffers
;; g h description-at-point
;; , b dape-breakpoint 
;; SPC e t test-code 
;; == format-buffer
;; , f find-file
;; , l eshell
;; , d project-dired
;; , n eval-defun
;; , i imenu-list-smart-toggle
;; C-<tab> yas-expand / yas-next-field
;; SPC e d dape 
;; , g magit-status
;; , k kill-buffer
;; , v eval-expression / dape-eval-expression / edebug 
;; , p evaluate sexp 
;; SPC s t toggle-theme
;; , c toggle-truncate-lines 
;; , a org-agenda 
;; g c evil-commentary
;; , B dape-breakpoint-remove-all 
;; g d goto-definition-at-point // eglot // xref
;; SPC e a execute-code-action // eglot
;; C-<iso-lefttab> yas-prev-field
;; , m search-documentation
;; SPC e b build-program 
;; SPC c u evil-mc-undo-all-cursors 
;; [ x xref-go-back
;; ] x xref-go-forward
;; SPC c a evil-mc-make-all-cursors
;; SPC c o evil-mc-make-cursor-move-next-line
;; g r find-references // eglot
;; SPC p p completion-at-point
;; , o online-search
;; SPC f d delete-file
;; SPC c h evil-mc-make-cursor-here 
;; SPC c r evil-mc-resume-cursors 
;; SPC c p evil-mc-pause-cursors 
;; SPC e m sharper-main-transient
;; SPC o t org-sparse-tree-heading-tag
;; SPC j s project-switch-to-buffer
;; SPC j f project-find-file
;; ,, evil-snipe-repeat-reverse
;; g D xref-find-definitions-other-window
;; SPC j j project-switch-project
;; SPC j h project-search

;; needs to be added
;; shell-command with project root
;; evil close window
;; org-next-same-heading
;; org-stop-clock



;;; minibuffer
(general-def vertico-map
  "C-j" 'vertico-next
  "C-k" 'vertico-previous
  "C-S-j" 'scroll-up-command
  "C-b" 'evil-backward-char
  "C-f" 'evil-forward-char
  "C-S-k" 'scroll-down-command)

;;; module specific keybinds
(general-def corfu-map
  "C-SPC" 'corfu-insert-separator
  "RET" nil)


(after! corfu-popupinfo
  (general-def corfu-popupinfo-map
    "C-h" 'corfu-popupinfo-toggle
    "C-S-j" 'corfu-popupinfo-scroll-up
    "C-S-k" 'corfu-popupinfo-scroll-down))

(after! org-remark
  (general-def 'visual org-remark-mode-map 
    "<return>" 'org-remark-mark)
  (general-def 'normal org-remark-mode-map
    "o" 'org-remark-open
    "]m" 'org-remark-view-next
    "[m" 'org-remark-view-prev
    "r" 'org-remark-delete))

(after! imenu-list
  (general-def imenu-list-major-mode-map
    "<return>" (lambda (arg) (interactive "P")
                 (if arg
                     (progn (evil-goto-line arg) (imenu-list-goto-entry) (evil-scroll-line-to-top (line-number-at-pos)))
                   (imenu-list-goto-entry) (evil-scroll-line-to-top (line-number-at-pos))))
    "C-j" 'evil-next-line
    "C-k" 'evil-previous-line
    "C-g" 'imenu-list-quit-window))

(after! evil-org
  (general-def org-mode-map
    "C-<tab>" 'org-cycle
    "C-<iso-lefttab>" 'org-shifttab))

(after! yasnippet
  (general-def yas-keymap
    "<tab>" nil
    "TAB" nil
    "C-<tab>" 'yas-next-field
    "C-<iso-lefttab>" 'yas-prev-field))


(after! dape
  ;; 'override mode is an alias for general-override-mode-map 
  ;; '(insert normal) state is required for general.el to override evil keybindings
  (general-def '(insert normal) 'override
    ;; dape doesn't have keymaps
    :predicate 'dape-active-mode
    "C-n" 'dape-next
    "C-p" 'dape-step-in
    "C-<escape>" 'dape-quit
    "C-c" 'dape-continue
    "C-i" 'dape-info
    "C-S-p" 'dape-step-out)
  )

;;; miscaleanous global commands

(general-def 'insert
  "TAB" (lambda () (interactive)
          (cond ((minibufferp) (vertico-insert))
                ((derived-mode-p 'eshell-mode 'comint-mod) (completion-at-point))
                ((and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)) (corfu-insert))
                (mark-active (indent-region (region-beginning) (region-end)))
                ((looking-at "\\_>") (hippie-expand nil))
                (t (indent-for-tab-command)))))


(general-unbind iedit-mode-keymap
  "TAB"
  "<tab>"
  "<backtab")


(after! helpful
  (general-def
    "C-h f" (lambda () (interactive) (save-selected-window (call-interactively 'helpful-callable)))
    "C-h v" (lambda () (interactive) (save-selected-window (call-interactively 'helpful-variable)))
    "C-h k" (lambda () (interactive) (save-selected-window (call-interactively 'helpful-key)))
    "C-h x" (lambda () (interactive) (save-selected-window (call-interactively 'helpful-command)))
    "C-h F" (lambda () (interactive) (save-selected-window (call-interactively 'helpful-function)))))


;;; evil global commands
(general-def 'normal
  "g h" (lambda () (interactive) (save-selected-window (helpful-at-point)))
  "C-S-d" 'evil-scroll-up
  "g d" 'xref-find-definitions
  "g D" 'xref-find-definitions-other-window
  "] x" 'xref-go-forward
  "[ x" 'xref-go-back)

(after! org-remark-info
  (general-def 'normal Info-mode-map
    "s" 'evil-snipe-s
    "S" 'evil-snipe-S))

(general-nmap "=" (general-key-dispatch 'evil-indent
                    "=" (lambda () (interactive) (indent-region (point-min) (point-max)))))


(after! eglot
  (general-def 'normal eglot-mode-map
    "g h" 'eldoc-doc-buffer)
  ;;    "?" 'consult-eglot-symbols
  (general-nmap eglot-mode-map "=" (general-key-dispatch 'evil-indent
                                     "=" 'eglot-format-buffer)))

;;; evil , global key commands

(global-evil-definer
  "," 'evil-snipe-repeat-reverse
  "s" 'switch-to-buffer
  "e" '("save-all-buffers" . (lambda () (interactive) (save-some-buffers "!")))
  "t" 'popper-toggle
  "f" 'find-file
  "l" 'eshell
  "o" '("online-search" . (lambda (x) (interactive "sSearch: ") (browse-url (concat "https://duckduckgo.com/?q=" x))))
  "i" 'imenu-list-smart-toggle
  "k" 'kill-buffer
  "n" 'eval-defun
  "d" 'dired
  "b" 'dape-breakpoint-toggle
  "p" 'eval-last-sexp
  "c" 'toggle-truncate-lines
  "a" 'org-agenda
  "B" 'dape-breakpoint-remove-all
  )

(global-evil-definer emacs-lisp-mode-map
  "m" '("browse-documentation" . (lambda () (interactive) (info-other-window "elisp"))))
(after! csharp-mode
  (global-evil-definer csharp-ts-mode-map
    "m" '("browse-documentation" . (lambda (x) (interactive "sSearch: ") (browse-url (concat "https://duckduckgo.com/?q=" x "+site%3Alearn.microsoft.com"))))))
(after! php-ts-mode
  (global-evil-definer php-ts-mode-map
    "m" 'php-browse-manual))

(after! magit-autoloads
  (global-evil-definer
    "g" 'magit-status))

(after! csharp-mode
  (global-evil-definer csharp-ts-mode-map
    "r" 'sharper-transient-run))
(after! python
  (global-evil-definer python-ts-mode-map
    "r" '("python-run-script" . (lambda ()                             
                                  "Run python script"                  
                                  (interactive)                        
                                  (if (not (get-buffer "*Python*"))    
                                      (run-python "python3 -i" nil t)
                                    (let ((kill-buffer-query-functions nil))
                                      (kill-buffer "*Python*"))
                                    (run-python "python3 -i" nil t)
                                    )
                                  ;; Without this line python returns NameError: name '__PYTHON_EL_eval_file' is not defined
                                  (sit-for 1)
                                  (python-shell-send-buffer)           
                                  (pop-to-buffer "*Python*")           
                                  ))))                                 
(after! auctex
  (global-evil-definer LaTeX-mode-map
    "r" 'TeX-command-master))

(global-evil-definer emacs-lisp-mode-map
  "v" 'eval-expression)
(after! dape
  (global-evil-definer
    ;; dape doesn't have an active mode-map
    :predicate 'dape-active-mode
    "v" 'dape-evaluate-expression))
(after! edebug
  (global-evil-definer
    "v" 'edebug-eval))


;;; space global commands

(+general-global-menu! "cursor" "c")
(+general-global-cursor
  "u" 'evil-mc-undo-all-cursors
  "a" 'evil-mc-make-all-cursors
  "o" 'evil-mc-make-cursor-move-next-line
  "c" 'evil-mc-make-cursor-here
  "r" 'evil-mc-resume-cursors
  "p" 'evil-mc-pause-cursors)

(+general-global-menu! "code" "e")

(after! eglot
  (+general-global-code eglot-mode-map
    "a" 'eglot-code-actions))

(after! csharp-mode
  (+general-global-code csharp-ts-mode-map
    "t" 'sharper-transient-test
    "b" 'sharper-transient-build
    "m" 'sharper-main-transient))

(after! dape
  (+general-global-code
    "d" 'dape))

(+general-global-menu! "completion" "p")

(+general-global-completion
  "p" 'completion-at-point)

(+general-global-menu! "project" "j")

(after! project
  (+general-global-project
    "f" 'project-find-file
    "s" 'project-switch-to-buffer
    "j" 'project-switch-project
    "h" 'project-search))

(+general-global-menu! "file" "f")
(+general-global-file
  "d" 'delete-file)


(+general-global-menu! "miscellaneous" "s")
(+general-global-miscellaneous
  "t" '("toggle-theme" . (lambda ()
                           "Toggle theme"
                           (interactive)
                           (cond ((equal custom-enabled-themes '(modus-vivendi)) (disable-theme 'modus-vivendi) (load-theme 'tango-dark))
                                 ((equal custom-enabled-themes '(tango-dark)) (disable-theme 'tango-dark) (load-theme 'modus-operandi-tinted))
                                 ((equal custom-enabled-themes '(modus-operandi-tinted)) (disable-theme 'modus-operandi-tinted) (load-theme 'modus-vivendi-tinted))
                                 ((equal custom-enabled-themes '(modus-vivendi-tinted)) (disable-theme 'modus-vivendi-tinted) (load-theme 'modus-vivendi))))))

(+general-global-menu! "org" "o")
(+general-global-org
  "t" '("org-match-sparse-tree-heading" . (lambda ()
                                            "Check heading of each tag"
                                            (interactive)
                                            (org-match-sparse-tree)
                                            (org-shifttab 1))))

;;;; text editing

(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-undo-system 'undo-redo)
    ;; `evil-collection' assumes `evil-want-keybinding' is set to
    ;; `nil' before loading `evil' and `evil-collection'
    ;; @see https://github.com/emacs-evil/evil-collection#installation
    (setq evil-want-keybinding nil))
  (evil-mode 1)
  :custom
  (evil-want-minibuffer t)
  )


(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init)
  :diminish evil-collection-unimpaired-mode
  )


(use-package evil-snipe
  :ensure t
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  :diminish evil-snipe-local-mode
  )

(use-package evil-owl
  :ensure t
  :custom
  (evil-owl-display-method 'posframe)
  (evil-owl-extra-posframe-args '(:width 50 :height 20))
  (evil-owl-max-string-length 50)
  (evil-owl-idle-delay 0.5)
  :init
  (evil-owl-mode)
  :diminish evil-owl-mode
  )


(use-package evil-mc
  :ensure t
  :init
  (global-evil-mc-mode)
  :diminish evil-mc-mode
  )

(use-package evil-commentary
  :ensure t
  :hook (prog-mode . evil-commentary-mode)
  :diminish evil-commentary-mode
  )

(use-package evil-multiedit
  :ensure t
  :demand t
  :config
  (evil-multiedit-default-keybinds)
  )


;;;; org


(use-package org-remark
  :ensure t
  :hook ((after-init . org-remark-global-tracking-mode)
         (Info-mode . org-remark-info-mode))
  :diminish org-remark-global-tracking-mode
  :diminish org-remark-mode
  )

;;;###autoload
(defun my/org-agenda-to-appt ()
  " Erase all reminders and rebuilt reminders for today from the agenda"
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;;;###autoload
(defun my-org-inf-repeat ()
  (interactive)
  "Treat the TODO as a repeater by logging it"
  (if (org-element-property :REPEAT (org-element-at-point))
      (let ((note (cdr (assq 'state org-log-note-headings)))
            lines)
        (setq org-log-note-marker (set-marker (make-marker) (aref (plist-get (plist-get (org-element-at-point) 'headline) :standard-properties) 0)))
        (setq org-log-note-return-to (set-marker (make-marker) (aref (plist-get (plist-get (org-element-at-point) 'headline) :standard-properties) 0)))
        (setq note
              (org-replace-escapes
               note
               (list (cons "%u" (user-login-name))
		     (cons "%U" user-full-name)
		     (cons "%t" (format-time-string
			         (org-time-stamp-format 'long 'inactive)
			         (current-time)))
		     (cons "%T" (format-time-string
			         (org-time-stamp-format 'long nil)
			         (current-time)))
		     (cons "%d" (format-time-string
			         (org-time-stamp-format nil 'inactive)
			         (current-time)))
		     (cons "%D" (format-time-string
			         (org-time-stamp-format nil nil)
			         (current-time)))
		     (cons "%s" (cond
			         ((not org-log-note-state) "\"DONE\"")
			         ((string-match-p org-ts-regexp
						  org-log-note-state)
				  (format "\"[%s]\""
					  (substring org-log-note-state 1 -1)))
			         (t (format "\"%s\"" org-log-note-state))))
		     (cons "%S"
			   (cond
			    ((not org-log-note-previous-state) "\"TODO\"")
			    ((string-match-p org-ts-regexp
					     org-log-note-previous-state)
			     (format "\"[%s]\""
				     (substring
				      org-log-note-previous-state 1 -1)))
			    (t (format "\"%s\""
				       org-log-note-previous-state)))))))
        (push note lines)
        (org-fold-core-ignore-modifications
          (org-with-wide-buffer
           ;; Find location for the new note.
           (goto-char org-log-note-marker)
           (set-marker org-log-note-marker nil)
           ;; Note associated to a clock is to be located right after
           ;; the clock.  Do not move point.
           (unless (eq org-log-note-purpose 'clock-out)
             (goto-char (org-log-beginning t)))
           ;; Make sure point is at the beginning of an empty line.
           (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert-and-inherit "\n")))
	         ((looking-at "[ \t]*\\S-") (save-excursion (insert-and-inherit "\n"))))
           ;; In an existing list, add a new item at the top level.
           ;; Otherwise, indent line like a regular one.
           (let ((itemp (org-in-item-p)))
             (if itemp
	         (indent-line-to
	          (let ((struct (save-excursion
			          (goto-char itemp) (org-list-struct))))
	            (org-list-get-ind (org-list-get-top-point struct) struct)))
	       (org-indent-line)))
           (insert-and-inherit (org-list-bullet-string "-") (pop lines))
           (let ((ind (org-list-item-body-column (line-beginning-position))))
             (dolist (line lines)
	       (insert-and-inherit "\n")
               (unless (string-empty-p line)
	         (indent-line-to ind)
	         (insert-and-inherit line))))
           (run-hooks 'org-after-note-stored-hook)
           (message "Note stored")
           (org-back-to-heading t)))
        (with-current-buffer (marker-buffer org-log-note-return-to)
          (goto-char org-log-note-return-to)))))



(use-package org
  :hook ((org-agenda-finalize . my/org-agenda-to-appt)
         (org-agenda-finalize . append))
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-clock-sound (concat user-emacs-directory "bell.wav"))
  (org-agenda-timegrid-use-ampm t)
  ;; Activate appointments so we get notifications
  (appt-activate t)
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'my/org-agenda-to-appt)
  ;; keep track of when todo is finished when created
  (org-log-done 'time)
  ;; set agenda files
  (org-agenda-files nil)
  (org-log-into-drawer t)
  ;; file path for plantuml
  ;; ask before killing a pomodora timer
  (org-src-lang-modes
   '(("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("desktop" . conf-desktop)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("toml" . conf-toml)
     ("plantuml" . plantuml)))
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (dot . t)
                                                           (scheme . t)))
  (setq org-agenda-files '("~/org/TODO.org"))
  (add-to-list 'org-shiftright-hook #'my-org-inf-repeat)
  )

;;;###autoload 
(defun my-org-pomodoro-choose-break-time (arg)
  "Choose break time for pomodoro"
  (interactive "nBreak time: ")
  (setq org-pomodoro-short-break-length arg))

;;;###autoload
(defun my-org-pomodoro-around-finished (orig-fun &rest args)
  "Choose break time unless we've reached a long break for pomodoro"
  (if (zerop (mod (+ org-pomodoro-count 1) org-pomodoro-long-break-frequency))
      (apply orig-fun args)
    (org-pomodoro-maybe-play-sound :pomodoro)
    (call-interactively #'my-org-pomodoro-choose-break-time)
    (apply orig-fun args)
    ))


;;;###autoload
(defun my/org-pomodoro-resume-after-break ()
  "Resume pomodoro timer after running it"
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))

;;;###autoload
(defun my-org-pomodoro-clockout-before-kill ()
  "Clock out time before exiting `org-pomodoro' so time is accurately tracked"
  (if (org-clocking-p)
      (save-window-excursion
        (org-clock-out))))

(use-package org-pomodoro
  :ensure t
  :hook (org-pomodoro-break-finished . my/org-pomodoro-resume-after-break)
  :custom
  (org-pomodoro-ask-upon-killing t)
  ;; change finish sound to differentiate between starting and stopping
  (org-pomodoro-finished-sound (concat user-emacs-directory "bell.wav"))
  ;; change pomo length and pomo break length
  (org-pomodoro-length 30)
  (org-pomodoro-short-break-length 7)
  (org-pomodoro-long-break-length 15)
  :config
  (advice-add 'org-pomodoro-finished :around #'my-org-pomodoro-around-finished)
  (advice-add 'org-pomodoro-kill :before #'my-org-pomodoro-clockout-before-kill)
  )

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode .  evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar shift todo heading))
  :diminish evil-org-mode
  )

;;;; code

(use-package geiser-mit
  :ensure t)

(use-package pet
  :ensure t
  :after (python)
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  )
(use-package magit
  :ensure t)

(use-package dape
  :ensure t
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "debug-adapters/netcoredbg/"))
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  (dape-display-source . pulse-momentary-highlight-one-line)
  (dape-start . (lambda () (save-some-buffers t t)))
  (dape-start . (lambda () (repeat-mode 1)))
  (dape-stopped . (lambda () (repeat-mode -1)))
  (dape-compile . kill-buffer)
  :custom
  (dape-key-prefix nil)
  (dape-buffer-window-arrangement 'gud)
  (dape-breakpoint-global-mode)
  (dape-info-hide-mode-line nil)

  )


;;;###autoload
(defvar my/eglot-completion-functions (list #'cape-file #'yasnippet-capf #'eglot-completion-at-point))

;;;###autoload
(defun my/eglot-capf ()
  "Configure eglot corfu completion display with multiple backends such as yasnippet"
  (setq-local completion-at-point-functions
              (list (apply 'cape-capf-super my/eglot-completion-functions))))


(use-package eglot
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "langservers/csharp/omnisharp/"))
  (add-to-list 'exec-path (concat user-emacs-directory "langservers/LaTeX/"))
  :hook ((csharp-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :config
  (setq completion-category-defaults nil)

  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  ;; This line causes function to delete or add characters when exiting https://github.com/minad/cape/issues/81
                                        ;  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab"))))




(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :diminish eldoc-mode
  )

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-ts-mode)
  )

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))



(use-package elisp-mode
  :hook (emacs-lisp-mode . (lambda () (setq imenu-generic-expression (append (list  (list "Use Package" "^(use-package \\(.+\\)" 1)) imenu-generic-expression))))
  )


(use-package web-mode
  :ensure t
  :mode ((("\\.phtml\\'") . web-mode)
         (("\\page\\'") . web-mode))
  )


(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  )

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))

(use-package sharper
  :ensure t
  :demand t
  )

;;(use-package xref-union
;;  :ensure t
;;  :hook ((prog-mode . xref-union-mode)
;;         (prog-mode . (lambda () (remove-hook 'xref-backend-functions #'etags--xref-backend))))
;;  )
;;
;; install rip-grep
;;(use-package dumb-jump
;;  :ensure t
;;  :hook (prog-mode . (lambda () (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)))
;;  )

;;;; completion

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  ;; cycle when reaching end of popup
  (corfu-cycle t)
  (corfu-on-exact-match 'quit)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'nil)
  (corfu-preselect 'first)
  (corfu-auto t)
  ;; if it doesn't work it is probably because the lsp is overriding it with :company-prefix-length
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (global-corfu-minibuffer nil)
  (corfu-popupinfo-delay nil)
  )

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
  :config
  (defadvice hippie-expand (around hippie-expand-case-fold)
    (let ((case-fold-search nil))
      ad-do-it))
  (ad-activate 'hippie-expand)
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :custom
  (yas-also-auto-indent-first-line t)
  :diminish yas-minor-mode
  )

(use-package yasnippet-snippets
  :ensure t
  )

;;;###autoload
(defun my/yasnippet-add-completion-functions ()
  "This function adds yasnippet-capf to completion-at-point-functions."
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package yasnippet-capf
  :ensure t
  :init
  :hook ((prog-mode org-mode) . my/yasnippet-add-completion-functions)
  )


(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  ;; fixes C-k defaulting to adding a digraph in M-x
  (eval-after-load "evil-maps"
    (dolist (map '(evil-insert-state-map))
      (define-key (eval map) "\C-k" nil)
      ))
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )


(use-package consult
  :ensure t
  :custom
                                        ; turns on vertico for : in evil
  (completion-in-region-function 'consult-completion-in-region)
  )


(use-package cape
  :ensure t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  (add-hook 'completion-at-point-functions #'cape-file)
  )

;;;; ui

;; modus-operandi-tinted python
;; modus-viviendi-tinted C#
;; modus-viviendi Elisp

(use-package popper
  :ensure t ; or :straight t
  :init
  (setq popper-display-control 'user)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eshell\\*"
          "\\*dotnet"
          "events\\*"
          "\\*shell\\*"
          "\\*Python\\*"
          dired-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package window
  :custom
  (menu-bar-mode nil)
  (tab-bar-mode nil)
  (tool-bar-mode nil)
  (line-number-mode nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions nil)
  (window-sides-slots '(2 2 2 2))
  (display-buffer-alist
   '(("\\*info\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 0.33))
     ("\\*helpful\\|\\*Help\\*\\|\\*eldoc\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . -1)
      (window-width . 0.33))
     ((or "\\*dotnet\\|\\*Messages\\*\\|Output\\*\\|events\\*\\|\\*eshell\\*\\|\\*shell\\*\\|\\*Python*" (major-mode . compilation-mode) (major-mode . dired-mode))
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-height 0.30))
     ((derived-mode . magit-mode)
      (display-buffer-reuse-window
       display-buffer-in-direction)
      (mode magit-mode)
      (window . root)
      (window-width . 0.30)
      (direction . left))))
  )


(use-package font-core
  :config
  (set-frame-font "JetBrains Mono 10" nil t))


(use-package kind-icon
  :ensure t
  :after corfu
  :demand t
  :custom
                                        ; (kind-icon-blend-background t)
                                        ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  (kind-icon-mapping
   '((array "a" :icon "code-brackets" :face font-lock-type-face)
     (boolean "b" :icon "circle-half-full" :face
              font-lock-builtin-face)
     (class "c" :icon "view-grid-plus-outline" :face
            font-lock-type-face)
     (color "#" :icon "palette" :face success)
     (command "cm" :icon "code-greater-than" :face default)
     (constant "co" :icon "lock-remove-outline" :face
               font-lock-constant-face)
     (constructor "cn" :icon "table-column-plus-after" :face
                  font-lock-function-name-face)
     (enummember "em" :icon "order-bool-ascending-variant" :face
                 font-lock-builtin-face)
     (enum-member "em" :icon "order-bool-ascending-variant" :face
                  font-lock-builtin-face)
     (enum "e" :icon "format-list-bulleted-square" :face
           font-lock-builtin-face)
     (event "ev" :icon "lightning-bolt-outline" :face
            font-lock-warning-face)
     (field "fd" :icon "application-braces-outline" :face
            font-lock-variable-name-face)
     (file "f" :icon "file-document-outline" :face
           font-lock-string-face)
     (folder "d" :icon "folder" :face font-lock-doc-face)
     (interface "if" :icon "application-brackets-outline" :face
                font-lock-type-face)
     (keyword "kw" :icon "key-variant" :face font-lock-keyword-face)
     (macro "mc" :icon "lambda" :face font-lock-keyword-face)
     (magic "ma" :icon "auto-fix" :face font-lock-builtin-face)
     (method "m" :icon "function-variant" :face
             font-lock-function-name-face)
     (function "f" :icon "function" :face font-lock-function-name-face)
     (module "{" :icon "file-code-outline" :face
             font-lock-preprocessor-face)
     (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
     (operator "op" :icon "plus-minus" :face
               font-lock-comment-delimiter-face)
     (param "pa" :icon "cog" :face default)
     (property "pr" :icon "wrench" :face font-lock-variable-name-face)
     (reference "rf" :icon "library" :face
                font-lock-variable-name-face)
     (snippet "S" :icon "content-cut" :face font-lock-string-face)
     (string "s" :icon "sticker-text-outline" :face
             font-lock-string-face)
     (struct "%" :icon "code-braces" :face
             font-lock-variable-name-face)
     (text "tx" :icon "script-text-outline" :face font-lock-doc-face)
     (typeparameter "tp" :icon "format-list-bulleted-type" :face
                    font-lock-type-face)
     (type-parameter "tp" :icon "format-list-bulleted-type" :face
                     font-lock-type-face)
     (unit "u" :icon "ruler-square" :face font-lock-constant-face)
     (value "v" :icon "plus-circle-outline" :face
            font-lock-builtin-face)
     (variable "va" :icon "variable" :face
               font-lock-variable-name-face)
     (t "." :icon "crosshairs-question" :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(use-package imenu-list
  :ensure t
  :hook ((imenu-list-after-jump . (lambda () (imenu-list-smart-toggle))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'left))


;;Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.



  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  )


(use-package posframe
  :ensure t
  )



(use-package adaptive-wrap
  :ensure t
  :hook ((eshell-mode help-mode html-ts-mode prog-mode evil-org-mode dired-mode helpful-mode info-mode) . adaptive-wrap-prefix-mode)
  )



(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-max-description-length nil)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-separator ":")
  (which-key-allow-multiple-replacements t)
  (which-key-popup-type 'minibuffer)
  :init
  (which-key-mode)
  :diminish which-key-mode
  )

(use-package diminish
  :ensure t
  )


;;;; miscaleanous

(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  )


(use-package eat
  :ensure t
  )

(use-package savehist
  :init
  (savehist-mode))


(use-package eshell
  :hook ((eshell-first-time-mode . (lambda () (yas-minor-mode -1)))
         (((eshell-mode shell-mode) . (lambda () (corfu-mode -1)))))
  :config
  (require 'em-tramp)
  (setq password-cache-expiry 3600)
  (setq eshell-prefer-lisp-functions t)
  (setq eshell-prefer-lisp-variables t)
  (setq password-cache t) 
  (setq password-cache-expiry 3600) 
  )

(use-package helpful
  :ensure t
  :demand t
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elisp-demos
  :ensure t
  )


;;;; emacs default

(use-package emacs
  :mode ("\\.sql\\'" . sql-mode)
  :hook (((prog-mode evil-org-mode html-ts-mode ibuffer-mode imenu-list-minor-mode dired-mode LaTeX-mode) . display-line-numbers-mode)
         (((prog-mode html-ts-mode) . (lambda () (setq indent-tabs-mode nil)))))
  :config
  ;; ellipsis marker single character of three dots in org
  (with-eval-after-load 'mule-util
    (setq truncate-string-ellipsis "…"))
  ;; disable transparency
  (add-to-list 'default-frame-alist '(alpha-background . 1.0))
  ;; yes or no now y or n
  (if (version<= emacs-version "28")
      (defalias 'yes-or-no-p 'y-or-n-p)
    (setopt use-short-answers t))

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (add-to-list 'custom-enabled-themes 'tango-dark)
  (load-theme 'tango-dark)
  (blink-cursor-mode 0)

  :custom
  (undo-limit 400000)           ;; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ;; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)  ;; 48mb  (default is 24mb)


  ;; turn off comp warnings
  (native-comp-async-report-warnings-error nil)
  ;; get rid of menu bar, tab bar, and tool bar
  ;; ;; setup differnet directoy for backups and autosaves
  (backup-directory-alist '(("." . (concat user-emacs-directory "backups/"))))
  ;; tabs insert spaces
  (indent-tabs-mode nil)
  ;; cursor over actual space of character
  (x-stretch-cursor t)
  (window-combination-resize t) ;; take new window space from all other windows
  ;; buffer is same version as file when opened
  (global-auto-revert-mode 1)
  ;; end double space between sentences
  (sentence-end-double-space nil)

  (desktop-save-mode 1)

  (doc-view-resolution 200)
  ;; support opening new minibuffers from inside existing minibuffers for evil :
  (enable-recursive-minibuffers t)
  ;; hide commands in M-x which do not work in current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; do not allow cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))


  )

;;;; archive


;; (use-package iedit
;;   :ensure t)

;;(defun tree-sitter! ()
;;  "Dispatch to turn on tree sitter.
;;
;;Used as a hook function which turns on `tree-sitter-mode'
;;and selectively turn on `tree-sitter-hl-mode'.
;;according to `+tree-sitter-hl-enabled-modes'"
;;  (turn-on-tree-sitter-mode)
;;  ;; conditionally enable `tree-sitter-hl-mode'
;;  (let ((mode (bound-and-true-p tree-sitter-hl-mode)))
;;    (when-let (mode (if (pcase +tree-sitter-hl-enabled-modes
;;                          (`(not . ,modes) (not (memq major-mode modes)))
;;                          ((and `(,_ . ,_) modes) (memq major-mode modes))
;;                          (bool bool))
;;                        (unless mode +1)
;;                      (if mode -1)))
;;      (tree-sitter-hl-mode mode))))                             #'cape-file))
;;(use-package tree-sitter-langs
;;  :ensure t)
;;
;;(use-package tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (setq tree-sitter-debug-jump-buttons t
;;        tree-sitter-debug-highlight-jump-region))
;;
;;
;;(use-package evil-textobj-tree-sitter
;;  :ensure t
;;  :config
;;  (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
;;  (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
;;  (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
;;  (defvar +tree-sitter-goto-next-map (make-sparse-keymap))
;;
;;  (evil-define-key '(visual operator) 'tree-sitter-mode
;;    "i" +tree-sitter-inner-text-objects-map
;;    "a" +tree-sitter-outer-text-objects-map)
;;  (evil-define-key 'normal 'tree-sitter-mode
;;    "[g" +tree-sitter-goto-previous-map

;;    "]g" +tree-sitter-goto-next-map))

;;(use-package minuet
;;  :ensure t
;;  :bind
;;  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
;;   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
;;   ("C-c m" . #'minuet-configure-provider)
;;   :map minuet-active-mode-map
;;    ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
;;   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
;;   ("TAB" . #'minuet-accept-suggestion) ;; accept whole completion
;;    ;; Accept the first line of completion, or N lines with a numeric-prefix:
;;    ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
;;   ("M-a" . #'minuet-accept-suggestion-line)
;;   ("M-e" . #'minuet-dismiss-suggestion))
;;
;;  :init
;;;; if you want to enable auto suggestion.
;;;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
;;  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
;;  :config
;;  (setq minuet-provider 'openai-fim-compatible)
;;  (setq minuet-n-completions 1) ;; recommended for Local LLM for resource saving
;;;; I recommend beginning with a small context window size and incrementally
;;;; expanding it, depending on your local computing power. A context window
;;;; of 512, serves as an good starting point to estimate your computing
;;;; power. Once you have a reliable estimate of your local computing power,
;;;; you should adjust the context window to a larger value.
;;  (setq minuet-context-window 250)
;;  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8012/v1/completions")
;;;; an arbitrary non-null environment variable as placeholder
;;  (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
;;  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;;;; The model is set by the llama-cpp server and cannot be altered
;;;; post-launch.
;;  (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")
;;
;;;; Llama.cpp does not support the `suffix` option in FIM completion.
;;;; Therefore, we must disable it and manually populate the special
;;;; tokens required for FIM completion.
;;  (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)
;;  (minuet-set-optional-options
;;   minuet-openai-fim-compatible-options
;;   :prompt
;;   (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
;;     (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
;;             (plist-get ctx :language-and-tab)
;;             (plist-get ctx :before-cursor)
;;             (plist-get ctx :after-cursor)))
;;   :template)
;;
;;  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))
