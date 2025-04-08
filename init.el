;;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; functions

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; resume pomodoro timer after running it
(defun my/org-pomodoro-resume-after-break ()
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))

(defun my/set-font ()
  (when (find-font (font-spec :name phundrak/default-font-name))
    (set-face-attribute 'default nil
                        :font phundrak/default-font-name
                        :height phundrak/default-font-size)))
(defvar run-current-file-dispatch nil
  "A dispatch table used by `run-current-file' to call dedicated function to run code.
Value is a association list.
Each item is (EXT . FUNCTION).
EXT is filename extension (sans the dot), type string.
FUNCTION is a elisp function name to call, type symbol.
If file extension match, and FUNCTION is defined, call it, pass current buffer's filepath as arg.
Else, `run-current-file-map' is looked up." )

(setq run-current-file-dispatch
      '(("el" . load)
        ("elc" . load)
        ("java" . xah-java-compile-and-run)))

(defvar run-current-file-map
  "A association list that maps file extension to a command for running the file, used by `run-current-file'.
Each item is (EXT . PROGRAM).
EXT is filename extension (sans the dot), type string.
PROGRAM is program name or path, with command options to run a file, type string.
A filename is appended after the PROGRAM string as external command to call.")

(setq run-current-file-map
      '(
        ;; following are tested as of 2024-12-20

        ("fs" . "dotnet fsi")
        ("fsx" . "dotnet fsi")
        ("go" . "go run")
        ("js" . "deno run")
        ("php" . "php")
        ("pl" . "perl")
        ("ps1" . "pwsh")
        ("py" . "python")
        ("py2" . "python2")
        ("py3" . "python3")
        ("rb" . "ruby")
        ("ts" . "deno run")
        ("m" . "wolframscript -print all -file")
        ("wl" . "wolframscript -print all -file")
        ("wls" . "wolframscript -print all -file")))

(defun run-current-file (Filename)
  "Execute the current file.
Output is printed to buffer *xah-run output*.

File suffix is used to determine what external command to run, in the variable `run-current-file-map'.

If file is modified, it is auto saved before run.

The variable `run-current-file-dispatch' allows you to customize this command to call other function to run the current file.

URL `http://xahlee.info/emacs/emacs/elisp_run_current_file.html'
Created: 2020-09-24
Version: 2024-12-20"
  (interactive (if buffer-file-name (progn (when (buffer-modified-p) (save-buffer)) (list buffer-file-name)) (user-error "Buffer is not file. Save it first.")))
  (let ((xoutbuf (get-buffer-create "*xah-run output*" t))
        (xext (file-name-extension Filename))
        xdispatch)
    (setq xdispatch (assoc xext run-current-file-dispatch))
    (if xdispatch
        (if (fboundp (cdr xdispatch))
            (progn
              (message "calling %s" (cdr xdispatch))
              (funcall (cdr xdispatch) Filename))
          (warn "`run-current-file' found function %s in run-current-file-dispatch but it is unbound. Normal run continues using `run-current-file-map'." xdispatch))
      (let ((xappCmdStr (cdr (assoc xext run-current-file-map))))
        (when (not xappCmdStr) (error "%s: Unknown file extension: %s. check `run-current-file-map'" real-this-command xext))
        (cond
         (t
          (progn
            (with-current-buffer xoutbuf (erase-buffer))
            (apply 'start-process (append (list "Run" xoutbuf) (split-string xappCmdStr " +" t) (list Filename) nil))
            (display-buffer xoutbuf))))))))


;;; config for packages that I know listed in order they should be loaded

(use-package evil
  :defer t
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
  :defer t
  :ensure t
  :init
  (evil-collection-init)
  )


(use-package evil-snipe
  :defer t
  :ensure t
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  )

(use-package evil-owl
  :defer t
  :ensure t
  :custom
  (evil-owl-display-method 'posframe)
  (evil-owl-extra-posframe-args '(:width 50 :height 20))
  (evil-owl-max-string-length 50)
  (evil-owl-idle-delay 0.5)
  :init
  (evil-owl-mode)
  )



(use-package org
  :defer t
  :hook ((org-agenda-finalize . bh/org-agenda-to-appt)
         (org-agenda-finalize . append))
  :bind (("C-c l" . #'org-store-link)
         ("C-c a" . #'org-agenda)
         ("C-c c" . #'org-capture))
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
  ;; include diary for agenda
  (org-agenda-include-diary t)
  ;; restore agendas to how they previously were after quitting agenda view
  (org-agenda-restore-windows-after-quit t)
  ;; set default org-agenda span to a week
  (org-agenda-span 'week)
  ;; set time grid to ampm
  (org-agenda-timegrid-use-ampm t)
  ;; enable plantuml and emacs-lisp in #+BEGIN
  (org-babel-load-languages '((emacs-lisp . t) (plantuml . t)))
  ;; put logs into drawer
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

(use-package org-pomodoro
  :defer t
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
  :defer t
  :ensure t
  :after (evil org)
  :hook (org-mode .  evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar shift todo heading))
  )

;;(use-package evil-textobj-tree-sitter
;;  :ensure t
;;  :config
;;  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;;  )


(use-package corfu
  :defer t
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  ;; cycle when reaching end of popup
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'nil)
  (corfu-preselect 'first)
  (corfu-auto t)
  ;; if it doesn't work it is probably because the lsp is overriding it with :company-prefix-length
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (global-corfu-minibuffer nil)
  (corfu-popupinfo-delay 0.5)
  :bind (:map corfu-map
              ("TAB" . corfu-insert)
              ("RET" . nil)
              ("<return>" . nil)
              ("M-p" . nil))


  )

(use-package eglot
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "langservers/omnisharp/"))
;  (add-to-list 'exec-path (concat user-emacs-directory "langservers/csharp-language-server-main/src/CSharpLanguageServer/"))
  ;; combine yasnippet eglot and cape
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'yasnippet-capf
                       #'eglot-completion-at-point
                       #'cape-file))))
  :ensure t
  :defer t
  :hook (prog-mode . eglot-ensure)
  :config
  ;; turn off eglots completion categories so we can add our own
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))

  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  )

(use-package eldoc
  :defer t
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  )


(use-package vertico
  :defer t
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("TAB" . vertico-exit)
              ("<tab" . vertico-exit))
  :config
  ;; fixes C-k defaulting to adding a digraph in M-x
  (eval-after-load "evil-maps"
    (dolist (map '(evil-insert-state-map))
      (define-key (eval map) "\C-k" nil)
      ))
  )

(use-package orderless
  :defer t
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )

(use-package savehist
  :defer t
  :init
  (savehist-mode)
  )

(use-package consult
  :defer t
  :ensure t
  :custom
  ; turns on vertico for : in evil
  (completion-in-region-function 'consult-completion-in-region)
  )

(use-package cape
  :ensure t
  :defer t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )




;;Enable rich annotations using the Marginalia package
(use-package marginalia
  :defer t
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  )



(use-package posframe
  :defer t
  :ensure t
  )

(use-package yasnippet-capf
  :after cape
  :ensure t
  :init
  (defun my/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  :hook (org-mode . my/yasnippet-capf-h)
  )


(use-package yasnippet
  :defer t
  :ensure t
  :hook (corfu-mode . yas-minor-mode)
  :bind (:map yas-keymap
              ("C-TAB" . yas-next-field-or-maybe-expand)
              ("C-<tab>" . yas-next-field-or-maybe-expand)
              ("C-S-TAB" . yas-prev-field)
              ("C-<iso-lefttab>" . yas-prev-field))
  :init
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :defer t
  :ensure t
  )


(use-package php-mode
  :defer t
  :ensure t
  :mode ("\\.php\\'" . php-ts-mode)

  )

(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package adaptive-wrap
  :defer t
  :ensure t
  :hook ((help-mode prog-mode evil-org-mode) . adaptive-wrap-prefix-mode)
  )


; dotnet wrapper
(use-package sharper
  :defer t
  :ensure t
  :bind
  ("C-c n" . sharper-main-transient)
  )

(use-package which-key
  :defer t
  :ensure t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-separator ":")
  :init
  (which-key-mode)
  )


(use-package web-mode
  :defer t
  :ensure t
  :mode ((("\\.phtml\\'") . web-mode)
         (("\\page\\'") . web-mode))
  )

(use-package treesit-auto
  :ensure t
  :init
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)

  )

(use-package minuet
  :ensure t
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
    ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("TAB" . #'minuet-accept-suggestion) ;; accept whole completion
    ;; Accept the first line of completion, or N lines with a numeric-prefix:
    ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
;; if you want to enable auto suggestion.
;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ;; recommended for Local LLM for resource saving
;; I recommend beginning with a small context window size and incrementally
;; expanding it, depending on your local computing power. A context window
;; of 512, serves as an good starting point to estimate your computing
;; power. Once you have a reliable estimate of your local computing power,
;; you should adjust the context window to a larger value.
  (setq minuet-context-window 250)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8012/v1/completions")
;; an arbitrary non-null environment variable as placeholder
  (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;; The model is set by the llama-cpp server and cannot be altered
;; post-launch.
  (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")

;; Llama.cpp does not support the `suffix` option in FIM completion.
;; Therefore, we must disable it and manually populate the special
;; tokens required for FIM completion.
  (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)
  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :prompt
   (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
     (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
             (plist-get ctx :language-and-tab)
             (plist-get ctx :before-cursor)
             (plist-get ctx :after-cursor)))
   :template)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))


(use-package emacs
  :defer t
  :mode ("\\.sql\\'" . sql-mode)
  :hook (((prog-mode evil-org-mode) . display-line-numbers-mode)
         (server-after-make-frame . my/set-font)
         ;; spaces for indentation
         ((prog-mode . (lambda () (setq indent-tabs-mode nil))))
         ;; remove trailing spaces
         (before-save . whitespace-cleanup))
  :bind ("C-c p" . toggle-truncate-lines)

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

  (setq treesit-language-source-alist
        '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (php "https://github.com/tree-sitter/tree-sitter-php")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (elisp "https://github.com/emacs-tree-sitter/elisp-tree-sitter")))
  :custom
  (undo-limit 400000)           ;; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ;; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)  ;; 48mb  (default is 24mb)

  (indent-tabs-mode nil)

  ;; turn off comp warnings
  (native-comp-async-report-warnings-error nil)
  ;; get rid of menu bar, tab bar, and tool bar
  (menu-bar-mode nil)
  (tab-bar-mode nil)
  (tool-bar-mode nil)
  ;; setup differnet directoy for backups and autosaves
  (backup-directory-alist (concat user-emacs-directory "backups"))
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

;;(use-package lsp-mode
;;  :defer t
;;  :ensure t
;;  :init
;;  (defun my/update-completions-list ()
;;    (progn
;;        (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
;;        (setq completion-at-point-functions
;;              '(yasnippet-capf non-greedy-lsp cape-file cape-dabbrev
;;))))
;;  (setq lsp-keymap-prefix "C-c s")
;;  (defun my/lsp-mode-setup-completion ()
;;    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;          '(orderless))) ;; Configure orderless
;;  :hook (((web-mode php-mode css-mode sql-mode csharp-mode mhtml-mode js-mode) . lsp)
;;         (lsp-completion-mode . my/lsp-mode-setup-completion)
;;         (lsp-completion-mode . my/update-completions-list))
;;  :custom
;;  (lsp-completion-provider :none) ;; we use corfu!!
;;  (lsp-signature-cycle t)
;;  :config
;;                                        ; enable which-key
;;  (with-eval-after-load 'lsp-mode
;;    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;;                                        ; get rid of lsp warnings
;;  (add-to-list 'warning-suppress-log-types '(lsp-mode))
;;  (add-to-list 'warning-suppress-types '(lsp-mode))
;;                                        ; make lsp completer less greedy
;;)

;;(use-package lsp-ui
;;  :defer t
;;  :ensure t
;;  :hook (lsp-mode . lsp-ui-mode)
;;  :custom
;;  (gc-cons-threshold 100000000)
;;  (read-process-output-max (* 1024 1024)) ;; 1mb
;;  (lsp-ui-doc-enable t)
;;  (lsp-ui-doc-position 'top)
;;  (lsp-ui-doc-side 'right)
;;  (lsp-ui-doc-delay 0)
;;  (lsp-ui-doc-border "red")
;;  (lsp-ui-doc-max-height 100)
;;  (lsp-ui-doc-max-width 100)
;;  (lsp-ui-doc-show-with-mouse t)
;;  (lsp-ui-sideline-enable t)
;;  (lsp-ui-sideline-show-diagnostics t)
;;  (lsp-ui-sideline-delay 0)
;;)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
