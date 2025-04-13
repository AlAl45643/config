;;; package management
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

;;; functions and variables

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
         (hippie-expand nil)
        (indent-for-tab-command)))))

;; regexp count=\([0-9]+\)
;;;###autoload
(defvar my/eglot-completion-functions (list #'yasnippet-capf                              #'eglot-completion-at-point))

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

;;;###autoload
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (apply 'cape-capf-super my/eglot-completion-functions))))
;;;###autoload
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

;; Erase all reminders and rebuilt reminders for today from the agenda
;;;###autoload
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; resume pomodoro timer after running it
;;;###autoload
(defun my/org-pomodoro-resume-after-break ()
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))
;;;###autoload
(defun my/set-font ()
  (when (find-font (font-spec :name phundrak/default-font-name))
    (set-face-attribute 'default nil
                        :font phundrak/default-font-name
                        :height phundrak/default-font-size)))

;;;###autoload
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
;;;###autoload
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
;;;###autoload
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

(use-package evil-multiedit
  :ensure t
  :init
  (add-hook 'after-init-hook #'evil-multiedit-default-keybinds)
  )


(use-package iedit
  :ensure t
  :defer t)

(use-package evil-mc
  :ensure t
  :defer t
  :config
  (evil-define-key '(normal visual) evil-mc-key-map
    (kbd "g c RET") #'evil-mc-make-cursor-here
    (kbd "g c a") #'evil-mc-make-all-cursors
    (kbd "g c A") #'evil-mc-make-cursor-in-visual-selection-end
    (kbd "g c I") #'evil-mc-make-cursor-in-visual-selection-beg
    (kbd "g c o") #'evil-mc-make-cursor-move-next-line
    (kbd "g c O") #'evil-mc-make-cursor-move-prev-line
    (kbd "g c q") #'evil-mc-undo-all-cursors
    (kbd "g c u") #'evil-mc-undo-last-added-cursor
    (kbd "g c C-p") #'evil-mc-pause-cursors
    (kbd "g c C-r") #'evil-mc-resume-cursors
    (kbd "g c n") #'evil-mc-make-and-goto-next-cursor
    (kbd "g c N") #'evil-mc-make-and-goto-last-cursor
    (kbd "g c p") #'evil-mc-make-and-goto-prev-cursor
    (kbd "g c P") #'evil-mc-make-and-goto-first-cursor
    (kbd "g c d") #'evil-mc-make-and-goto-next-match
    (kbd "g c D") #'evil-mc-make-and-goto-prev-match
    (kbd "g c s") #'evil-mc-skip-and-goto-next-match
    (kbd "g c S") #'evil-mc-skip-and-goto-prev-match
    (kbd "g c c") #'evil-mc-skip-and-goto-next-cursor
    (kbd "g c C") #'evil-mc-skip-and-goto-prev-cursor))


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
  :bind ((:map corfu-map
              ("TAB" . corfu-insert)
              ("RET" . nil)
              ("<return>" . nil)
              ("M-p" . nil))
         (:map corfu-popupinfo-map
               ("C-h" . corfu-popupinfo-toggle)))
  :config
  )

(use-package hippie-exp
  :commands (hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-line
          try-expand-all-abbrevs
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name
          try-expand-list))
  :bind ("TAB" . smart-tab)
  :custom
  (keymap-set "M-/" #'smart-tab
              (lambda () (and (frame-live-p corfu--frame)
                              (frame-visible-p corfu--frame))))
)
(use-package eglot
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "langservers/omnisharp/"))
  :ensure t
  :defer t
 ; ; prog-mode causes a wrong type argument warning from eglot but you can just ignore it
  :hook ((html-ts-mode prog-mode) . eglot-ensure)
  :config
  ;; turn off eglots completion categories so we can add our own
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))

  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  ;; This line causes function to delete or add characters when exiting https://github.com/minad/cape/issues/81
;  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
 )

;;(use-package lsp-mode
;;  :defer t
;;  :ensure t
;;  :init
;;  (setq lsp-keymap-prefix "C-c s")
;;
;;  :hook ((prog-mode . lsp)
;;         (lsp-completion-mode . my/lsp-mode-setup-completion)
;;         ;; This code makes lsp-completion-at-point more likely to give way control to other completion functions
;;         (lsp-completion-mode . (lambda () (progn
;;                                            (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
;;                                            (setq completion-at-point-functions (delq #'lsp-completion-at-point completion-at-point-functions))
;;                                            (add-to-list 'completion-at-point-functions #'non-greedy-lsp)))))
;;       ;; This code makes lsp-completion-at-point only run after other completion functions cannot match.
;; ;;       (lsp-completion-mode . (lambda () (progn
;; ;;                                             (setq completion-at-point-functions (delq #'lsp-completion-at-point completion-at-point-functions))
;; ;;                                           (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)))))
;;  :custom
;;  (lsp-completion-provider :none) ;; we use corfu!!
;;  (lsp-signature-cycle t)
;;  (lsp-enable-suggest-server-download nil)
;;  :config
;;  ;; enable which-key
;;  (with-eval-after-load 'lsp-mode
;;    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;;  ;; get rid of lsp warnings
;;  (add-to-list 'warning-suppress-log-types '(lsp-mode))
;;  (add-to-list 'warning-suppress-types '(lsp-mode))
;;)


(use-package eldoc
  :defer t
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  )

;;(use-package tree-sitter-langs
;;  :ensure t)
;;
;;(use-package tree-sitter
;;  :defer t
;;  :config
;;  (require 'tree-sitter-langs)
;;  (setq tree-sitter-debug-jump-buttons t
;;        tree-sitter-debug-highlight-jump-region))
;;
;;
;;(use-package evil-textobj-tree-sitter
;;  :ensure t
;;  :defer t
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
              ("<tab>" . vertico-exit))
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
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

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
  :ensure t
  :init
  :hook ((prog-mode org-mode) . (lambda () (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
  )

(use-package yasnippet
  :defer t
  :ensure t
  :bind (:map yas-keymap
              ([(tab)] . nil)
              ("TAB" . nil)
              ("C-TAB" . yas-next-field-or-maybe-expand)
              ("C-<tab>" . yas-next-field-or-maybe-expand)
              ("C-S-TAB" . yas-prev-field)
              ("S-TAB" . nil)
              ("S-<tab>" . nil)
              ("C-<iso-lefttab>" . yas-prev-field))
  :bind (:map yas-minor-mode-map
              ([(tab)] . nil)
              ("TAB" . nil)
              ("C-TAB" . yas-expand)
              ("C-<tab>" . yas-expand))
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

(use-package js2-mode
  :defer t
  :ensure t
  :mode ("\\.js\\'" . js2-mode))


(use-package rainbow-delimiters
  :defer t
  :ensure t
  :hook ((html-ts-mode prog-mode) . rainbow-delimiters-mode)
  )

(use-package adaptive-wrap
  :defer t
  :ensure t
  :hook ((eshell-mode help-mode html-ts-mode prog-mode evil-org-mode) . adaptive-wrap-prefix-mode)
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
  :init
  (which-key-mode)
  )



(use-package web-mode
  :defer t
  :ensure t
  :mode ((("\\.phtml\\'") . web-mode)
         (("\\page\\'") . web-mode))
  )


;;(use-package minuet
;;  :defer t
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


(use-package emacs
  :defer t
  :mode ("\\.sql\\'" . sql-mode)
  :hook (((help-mode prog-mode evil-org-mode html-ts-mode) . display-line-numbers-mode)
         (server-after-make-frame . my/set-font)
         (((prog-mode html-ts-mode) . (lambda () (setq indent-tabs-mode nil))))
         ((eshell-mode shell-mode) . (lambda () (corfu-mode -1)))
         (before-save . whitespace-cleanup)
         (prog-mode . electric-pair-mode))
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(adaptive-wrap cape consult corfu evil-collection evil-org evil-owl
                   evil-snipe evil-textobj-tree-sitter f ht lv
                   marginalia markdown-mode orderless org-pomodoro
                   php-mode plz posframe rainbow-delimiters sharper
                   spinner treesit-auto tsc undo-fu-session vertico
                   web-mode yasnippet-capf yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
