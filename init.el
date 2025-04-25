;;; PACKAGE management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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


;;; keybinding core

(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup)
  )

(general-define-key
 :keymaps 'override
 :states  '(insert emacs normal hybrid motion visual operator)
 :prefix-map 'my/prefix-map
 :prefix-command 'my/prefix-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC")
(general-create-definer global-definer
  :wk-full-keys nil
  :keymaps 'my/prefix-map)



(general-define-key
 :keymaps 'override
 :states '(insert emacs normal hybrid motion visual operator)
 :prefix-map 'my/evil-prefix-map
 :prefix-command 'my/evil-prefix-map
 :prefix ","
 :non-normal-prefix "C-,")
(general-create-definer global-evil-definer
  :wk-full-keys nil
  :keymaps 'my/evil-prefix-map)


(general-create-definer global-leader
  :keymaps 'override
  :states '(insert normal hybrid motion visual operator)
  :prefix "SPC m"
  :non-normal-prefix "M-SPC m"
  "" '(:ignore t :which-key (lambda (arg) (cons (cadr (split-string (car arg) " ")) (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(defmacro +general-global-menu! (name infix-key &rest body)
  "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     ;; don't use :which-key it is less performant according to general.el author
     ;; also for some reason which-key duplicates keybindings
     (which-key-add-keymap-based-replacements my/prefix-map ,infix-key ,name)
     (general-create-definer ,(intern (concat "+general-global-" name))
       :wrapping global-definer
       :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
       :infix ,infix-key
       :wk-full-keys nil)
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




;;; rules
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
;; 1. Each menu should be named according to the most descriptive adjective that describes every command it contains. A comment description for each menu should also be created to describe exactly what commands are supposed to be put in this menu.
;; 2. The menu key should be either the first or last character of the descriptive adjective or the first character of each syllable in that adjective. The descriptive function keyword does not have necessarily have to be in the command
;; 3. Any command bound within a menu should be the smoothest key for the chord that is the first or last character of the most descriptive function keyword in the command or the first character in each syllable of the keyword.
;; 4. Commands that have similar functionality and adjective but are in different menus should have the same key.
;;
;; evil menu semantic rules
;; 1. The evil menu should not have any menus within it.
;; 2. Any command bound within the evil menu should be the smoothest key for the chord that is the first or last character of the most descriptive function keyword in the command or the first character in each syllable of the keyword.
;;
;; evil semantic rules
;; 1. All keys bound outside of the global menu and evil menu should follow evil semantic rules.

;; minibuffer specific command priority
;; C-j vertico-next
;; C-k vertico-previous
;; C-b evil-backward char
;; C-f evil-forward char
;; C-S-j scroll-down-command
;; C-S-k scroll-up-command

;;
;; module specific command priority
;; TAB corfu-insert
;; C-SPC corfu-insert-separator
;; C-h corfu-popupinfo-toggle
;; C-S-j corfu-popupinfo-scroll-up
;; C-S-k corfu-popupinfo-scroll-down
;;
;; mode specific commands
;; php-search-documentation // add to lookups
;; php-browse-manual // add to lookups
;; d sharper-main-transient
;;
;; global key commands priority
;; M-x execute-extended command
;; TAB smart-tab (vertico / eshell/ corfu/ indent / hippie-expand)
;; ,s switch-to-buffer
;; ,f find-file
;; ,e save-some-buffers
;; == format-buffer
;; ,l eshell
;; ,a evil-mc-make-all-cursors
;; C-<tab> yas-expand
;; C-<tab> yas-next-field
;; g h description-at-point
;; ,k kill-buffer
;; ,u evil-mc-undo-all-cursors
;; ,P evil-mc-skip-and-goto-prev-cursor
;; ,p evil-mc-skip-and-goto-next-cursor
;; SPC c p evil-mc-pause-cursors
;; SPC c r evil-mc-resume-cursors
;; ,v eval-expression
;; ,t toggle-truncate-lines
;; ,u search-documentation
;; ,g org-agenda
;; ,d project-dired
;; C-<iso-lefttab> yas-prev-field
;; SPC e a execute-code-action // eglot
;; g d goto-definition-at-point // eglot
;; g r find-references // eglot
;; g c evil-commentary
;; , u evil-mc-undo-last-added-cursor
;; , h evil-mc-make-cursor-here
;; SPC e asignature-activate
;; SPC p p completion-at-point
;; SPC j fproject-find-file
;; SPC j h project-search
;; SPC j s project-switch-to-buffer
;; SPC j j project-switch-project


;; minibuffer
(general-def vertico-map
  "C-j" 'vertico-next
  "C-k" 'vertico-previous
  "C-S-j" 'scroll-down-command
  "C-b" 'evil-backward-char
  "C-f" 'evil-forward-char
  "C-S-k" 'scroll-up-command)

;; module specific keybinds
(general-def corfu-map
  "C-SPC" 'corfu-insert-separator
  "RET" nil)


(after! corfu-popupinfo
  (general-def corfu-popupinfo-map
    "C-h" 'corfu-popupinfo-toggle
    "C-S-j" 'corfu-popupinfo-scroll-up
    "C-S-k" 'corfu-popupinfo-scroll-down))



;;; global commands

;; miscaleanous commands

(general-def 'insert
  "TAB" (lambda () (interactive)
          (cond ((minibufferp) (vertico-insert))
                ((derived-mode-p 'eshell-mode 'comint-mod) (completion-at-point))
                ((and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)) (corfu-insert))
                (mark-active (indent-region (region-beginning) (region-end)))
                ((looking-at "\\_>") (hippie-expand nil))
                (t (indent-for-tab-command)))))

(general-def
  "C-<tab>" 'yas-expand)

(after! yasnippet
  (general-def yas-keymap
    "<tab>" nil
    "TAB" nil
    "C-<tab>" 'yas-next-field
    "C-<iso-lefttab>" 'yas-prev-field))

;; evil commands

(general-def 'normal
  "g h" (lambda () (interactive) (let (sym)
                                   ;; sigh, function-at-point is too clever.  we want only the first half.
                                   (cond ((setq sym (ignore-errors
                                                      (with-syntax-table emacs-lisp-mode-syntax-table
                                                        (save-excursion
                                                          (or (not (zerop (skip-syntax-backward "_w")))
                                                              (eq (char-syntax (char-after (point))) ?w)
                                                              (eq (char-syntax (char-after (point))) ?_)
                                                              (forward-sexp -1))
                                                          (skip-chars-forward "`'")
                                                          (let ((obj (read (current-buffer))))
                                                            (and (symbolp obj) (fboundp obj) obj))))))
                                          (describe-function sym))
                                         ((setq sym (variable-at-point)) (describe-variable sym))
                                         ;; now let it operate fully -- i.e. also check the
                                         ;; surrounding sexp for a function call.
                                         ((setq sym (function-at-point)) (describe-function sym))))))

(general-nmap "=" (general-key-dispatch 'evil-indent
                    "=" (lambda () (interactive) (indent-region (point-min) (point-max)))))
(after! lsp-mode
  (general-def 'normal lsp-mode-map
    "g h" 'lsp-describe-thing-at-point
    "g r" 'lsp-find-references)
  (general-nmap lsp-mode-map "=" (general-key-dispatch 'evil-indent
                                   "=" 'lsp-format-buffer)))

(after! eglot
  (general-def 'normal eglot-mode-map
    "g h" 'eldoc-doc-buffer)
  (general-nmap eglot-mode-map "=" (general-key-dispatch 'evil-indent
                                     "=" 'eglot-format-buffer)))
;; evil , key commands

(global-evil-definer
  "s" 'switch-to-buffer
  "f" 'find-file
  "e" 'save-some-buffers
  "l" 'eshell
  "k" 'kill-buffer
  "a" '("make-all-cursors" . evil-mc-make-all-cursors)
  "q" '("quit-all-cursors" . evil-mc-undo-all-cursors)
  "p" '("pause-cursors" . evil-mc-pause-cursors)
  "r" '("resume-cursors" . evil-mc-resume-cursors)
  "v" 'eval-expression
  "t" 'toggle-truncate-lines
  "g" 'org-agenda
  "d" 'project-dired
  "u" 'evil-mc-undo-last-added-cursor
  "h" 'evil-mc-make-cursor-here
  )

(general-def 'normal emacs-lisp-mode-map
  ", m" '("browse-documentation" . (lambda () (interactive) (info-other-window "elisp") (call-interactively 'Info-index))))

(general-def 'insert emacs-lisp-mode-map
  "C-, m" '("browse-documentation" . (lambda () (interactive) (info-other-window "elisp") (call-interactively 'Info-index))))

(after! csharp-mode
  (global-leader csharp-ts-mode-map
    "d" '("dotnet-ui" . sharper-main-transient))
  (general-def 'normal csharp-ts-mode-map
    ", m" '("browse-documentation" . (lambda () (interactive) (browse-url "https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/"))))
  (general-def 'insert csharp-ts-mode-map
    "C-, m" '("browse-documentation" . (lambda () (interactive) (browse-url "https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/")))))

(after! php-ts-mode
  (general-def 'normal php-ts-mode-map
    ", m" 'php-browse-manual)
  (general-def 'insert php-ts-mode-map
    "C-, m" 'php-browse-manual))

;; space commands

(+general-global-menu! "cursor" "c")
(+general-global-cursor
  "n" '("skip-and-goto-next-cursor" . evil-mc-skip-and-goto-next-cursor)
  "p" '("skip-and-goto-prev-cursor" . evil-mc-skip-and-goto-prev-cursor))

(+general-global-menu! "code" "e")
(after! lsp-mode
  (general-def 'normal lsp-mode-map
    "SPC e a" 'lsp-execute-code-action
    "SPC e s" 'lsp-signature-activate)
  (general-def 'insert lsp-mode-map
    "M-SPC e a" 'lsp-execute-code-action
    "M-SPC e s" 'lsp-signature-activate))

(after! eglot
  (general-def 'normal eglot-mode-map
    "SPC e a" 'eglot-code-actions)
  (general-def 'insert eglot-mode-map
    "M-SPC e a" 'eglot-code-actions))

(+general-global-menu! "completion" "p")
(+general-global-completion
  "p" 'completion-at-point)

(+general-global-menu! "project" "j")
(+general-global-project
  "f" 'project-find-file
  "s" 'project-switch-to-buffer
  "j" 'project-switch-project
  "h" 'project-search)



   ;;; config for installed packages

(use-package use-package-core
  :custom
  (use-package-always-defer t))


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
  )


(use-package evil-snipe
  :ensure t
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
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
  )



(use-package evil-mc
  :ensure t
  :init
  (global-evil-mc-mode)


  )

(use-package evil-commentary
  :ensure t
  :hook (prog-mode . evil-commentary-mode)
  )


;;;###autoload
(defun my/org-agenda-to-appt ()
  " Erase all reminders and rebuilt reminders for today from the agenda"
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))


(use-package org
  :hook ((org-agenda-finalize . bh/org-agenda-to-appt)
         (org-agenda-finalize . append))
  :mode ("\\.org\\'" . org-mode)
  :custom
  (bh/org-agenda-to-appt)
  ;; Activate appointments so we get notifications
  (appt-activate t)
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'bh/org-agenda-to-appt)
  ;; keep track of when todo is finished when created
  (org-log-done 'time)
  ;; change org clock sound for
  (org-clock-sound (concat user-emacs-directory "bell.wav"))
  ;; set agenda files
  (org-agenda-files nil)
  (org-log-into-drawer t)
  ;; file path for plantuml
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
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
  )


;;;###autoload
(defun my/org-pomodoro-resume-after-break ()
  " resume pomodoro timer after running it"
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))

(use-package org-pomodoro
  :ensure t
  :hook (org-pomodoro-break-finished . my/org-pomodoro-resume-after-break)
  :custom
  (org-pomodoro-ask-upon-killing t)
  ;; change finish sound to differentiate between starting and stopping
  (org-pomodoro-finished-sound (concat user-emacs-directory "bell.wav"))
  ;; change pomo length and pomo break length
  (org-pomodoro-length 25)
  (org-pomodoro-long-break-length 15)
  )

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode .  evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar shift todo heading))
  )


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
  (corfu-auto-delay 0.5)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (global-corfu-minibuffer nil)
  (corfu-popupinfo-delay nil)
  )

(use-package hippie-exp
  :commands (hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
  )

;;;###autoload
(defvar my/eglot-completion-functions (list #'yasnippet-capf #'eglot-completion-at-point))

;;;###autoload
(defun my/eglot-capf ()
  "Configure eglot corfu completion display with multiple backends such as yasnippet"
  (setq-local completion-at-point-functions
              (list (apply 'cape-capf-super my/eglot-completion-functions))))

(use-package eglot
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "langservers/omnisharp/"))
                                        ; ; prog-mode causes a wrong type argument warning from eglot but you can just ignore it
  :config
                                        ;TODO ; turn off eglots completion categories so we can add our own
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))

  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  ;; This line causes function to delete or add characters when exiting https://github.com/minad/cape/issues/81
                                        ;  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  )


;;;###autoload
(defun my/lsp-mode-setup-completion ()
  " Configure orderless "
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(use-package lsp-mode
  :ensure t
  :init
  :hook (((csharp-ts-mode php-ts-mode) . lsp)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         ;;         ;; This code makes lsp-completion-at-point more likely to give way control to other completion functions
         ;;         (lsp-completion-mode . (lambda () (progn
         ;;                                            (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
         ;;                                            (setq completion-at-point-functions (delq #'lsp-completion-at-point completion-at-point-functions))
         ;;                                            (add-to-list 'completion-at-point-functions #'non-greedy-lsp)))))
         ;; This code makes lsp-completion-at-point only run after other completion functions cannot match.
         (lsp-completion-mode . (lambda () (progn
                                             (setq completion-at-point-functions (delq #'lsp-completion-at-point completion-at-point-functions))
                                             (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)))))
  :custom
  (lsp-completion-provider :none)
  (lsp-signature-cycle t)
  (lsp-enable-suggest-server-download nil)

  :config
  ;; enable which-key
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  ;; get rid of lsp warnings
  (add-to-list 'warning-suppress-log-types '(lsp-mode))
  (add-to-list 'warning-suppress-types '(lsp-mode)))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  )


(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

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
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  (add-hook 'completion-at-point-functions #'cape-file)
  )

(use-package kind-icon
  :ensure t
  :after corfu
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

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  )


(use-package yasnippet-snippets
  :ensure t
  )


(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-ts-mode)
  )

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))


(use-package rainbow-delimiters
  :ensure t
  :hook ((html-ts-mode prog-mode) . rainbow-delimiters-mode)
  )

(use-package adaptive-wrap
  :ensure t
  :hook ((eshell-mode help-mode html-ts-mode prog-mode evil-org-mode dired-mode) . adaptive-wrap-prefix-mode)
  )


(use-package sharper
  :ensure t
  :demand t
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
  :init
  (which-key-mode)
  )



(use-package web-mode
  :ensure t
  :mode ((("\\.phtml\\'") . web-mode)
         (("\\page\\'") . web-mode))
  )


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


;;; config for default emacs packages

;;;###autoload
(defun my/set-font ()
  (when (find-font (font-spec :name phundrak/default-font-name))
    (set-face-attribute 'default nil
                        :font phundrak/default-font-name
                        :height phundrak/default-font-size)))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :mode ("\\.sql\\'" . sql-mode)
  :hook (((help-mode prog-mode evil-org-mode html-ts-mode) . display-line-numbers-mode)
         (server-after-make-frame . my/set-font)
         (((prog-mode html-ts-mode) . (lambda () (setq indent-tabs-mode nil))))
         ((eshell-mode shell-mode) . (lambda () (corfu-mode -1)))
         (before-save . whitespace-cleanup)
         (prog-mode . electric-pair-mode))
  :config
  (defvar phundrak/default-font-size 90
    "Default font size.")

  (defvar phundrak/default-font-name "Cascadia Code"
    "Default font.")
  (my/set-font)
  ;; ellipsis marker single character of three dots in org
  (with-eval-after-load 'mule-util
    (setq truncate-string-ellipsis "…"))
  ;; disable transparency
  (add-to-list 'default-frame-alist '(alpha-background . 1.0))
  ;; yes or no now y or n
  (if (version<= emacs-version "28")
      (defalias 'yes-or-no-p 'y-or-n-p)
    (setopt use-short-answers t))
  ;; set theme to tango dark
  (add-to-list 'custom-enabled-themes 'tango-dark)
  (load-theme 'tango-dark)

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  :custom
  (undo-limit 400000)           ;; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ;; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)  ;; 48mb  (default is 24mb)


  ;; turn off comp warnings
  (native-comp-async-report-warnings-error nil)
  ;; get rid of menu bar, tab bar, and tool bar
  (menu-bar-mode nil)
  (tab-bar-mode nil)
  (tool-bar-mode nil)
  ;; setup differnet directoy for backups and autosaves
  (backup-directory-alist (concat user-emacs-directory "backups/"))
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

;;; archive

(use-package evil-multiedit
  :ensure t
  :demand t
  :config
  (evil-multiedit-default-keybinds)
  )

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
