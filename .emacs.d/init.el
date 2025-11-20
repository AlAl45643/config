;;; rules
;;;; package rules
;;;;; guiding values
;; 1. Modularity decreases development costs by limiting complexity.
;;;;; feature rules
;; + Every section except core should be modular.
;; + use package last parantheses should not be on its own line
;; + don't use custom but setopt instead since it messes with completion
;; + put setopts in :init as customization avoids loading as much as it can
;; + diminish should be on the last line
;; + :demand :mode :hook :general  :init :config :general-config
;; + prefer loading packages with hooks :after :init :config but with :hooks and :general
;; + one line for each :hook
;; + hooks should be placed in the package where the function was provided
;;;; keybind rules
;;;;; guiding values
;; + A keybind should be 1. useful 2. memorable 3. shorter than less used and longer than more used keybinds 4. easy to press
;; + Structure makes memorable keybinds.
;; + Keybinds can be separated into three semantic categories. The first being keybinds that are only useful within a context (mode), the second being keybinds that are useful in any context (mode), and the third being keybinds that are somewhere between. 
;; + Emacs prefixes should be incorporated into your keybind scheme so that you can discover more useful keybinds and that your keybind scheme if not optimal is most likely useful.
;;;;; binding rules
;; 1. Is the command always useful? If it is then bind it globally. If not, which mode and state is it useful in?
;; 2. What is the shortest memorable keybind you can think of?
;; 3. Is the keybind available? if it is, then bind the keybind to the command. If not, then is the command currently bound less used than our command? if it is, then replace the command and if command used redo step 7 for the command you replaced. if it is not, then think of the next shortest memorable keybind and redo step 3.

;; + SPC prefix commands should be commands with higher frequency of use than , prefix commands.
;; + The , prefix and the SPC prefix are restricted to global commands while the \ prefix is restricted to major mode.
;; + Inbetween keybinds shall be global through functions or local depending on whichever solution is cleaner.
;; + Incorporate emacs prefixes such as C-h and C-x.
;; + Prefer binding to prefix-maps when incorporating emacs prefixes.
;; + Only defer load for keybinds when keymap isn't available or commands aren't needed until package is loaded.
;; + Keybinds should be put in the package definition that provides them
;; + Use M-F instead of M-S-f

;;;;; keybind conventions
;; q ephermal quit
;; Z Q non epehermal quit
;; C-j C-k when j k isn't available
;; g j g k next same heading
;; ]] [[ next visible heading
;; C-J C-K next grouping or scroll
;;; core
;;;; packages
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

(straight-use-package 'use-package)

(use-package use-package-core
  :custom
  (use-package-always-defer t))
(use-package diminish
  :straight t
  )
(use-package general
  :straight t
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  )

(defmacro my-install-package (name &optional straight)
  (if straight
      `(use-package ,name
         :straight ,straight)
    `(use-package ,name
       :straight t)))

;;;; my-main-leader-evil-map
(defun my-run-program ()
  "Run program at point depending on mode."
  (interactive)
  (cond
   ((equal major-mode (or 'python-ts-mode 'python-mode))
    (if (not (get-buffer "*Python*"))    
        (run-python "python3 -i" nil t)
      (let ((kill-buffer-query-functions nil)
            (python (get-buffer-window "*Python*")))
        (cond
         (python (with-selected-window python
                   (kill-buffer-and-window)))
         ((get-buffer "*Python*") (kill-buffer "*Python*"))))
      (run-python "python3 -i" nil t)
      ;; Without this line python returns NameError: name '__PYTHON_EL_eval_file' is not defined
      (sit-for 1)
      (python-shell-send-buffer)           
      (pop-to-buffer "*Python*")           
      ))
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode)) (sharper-transient-run))
   ((equal major-mode 'LaTeX-mode) (call-interactively #'TeX-command-master))
   (t (message "No program to run"))))

(defun my-eval-expression ()
  "Eval expression depending on mode."
  (interactive)
  (cond
   ((and (featurep 'dape) dape-active-mode)
    (call-interactively #'dape-evaluate-expression))
   ((and (featurep 'edebug) edebug-mode)
    (call-interactively #'edebug-eval-expression))
   (t (call-interactively #'eval-expression)))
  )

(defun my-save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers "!"))

(defun my-browse-csharp-docs (x)
  "Browse csharp docs by searching duckduckgo with site: learn.microsoft.com."
  (interactive "sSearch: ")
  (browse-url (browse-url (concat "https://duckduckgo.com/?q=" x "+site%3Alearn.microsoft.com"))))

(defun my-browse-python-docs (x)
  "Browse python docs by search duckduckgo with site: docs.python.org"
  (interactive "sSearch: ")
  (browse-url (concat "https://duckduckgo.com/?q=" x "+site%3Adocs.python.org")))


(defun my-browse-documentation ()
  "Browse documentation for mode."
  (interactive)
  (cond
   ((equal major-mode 'emacs-lisp-mode)
    (info-other-window "elisp"))
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (call-interactively #'my-browse-csharp-docs))
   ((equal major-mode (or 'php-ts-mode php-mode))
    (php-browse-manual))
   ((equal major-mode (or 'python-ts-mode 'python-mode))
    (call-interactively #'my-browse-python-docs))))


(defun my-window-bookmark-home ()
  "Move to home bookmark."
  (interactive)
  (bookmark-maybe-load-default-file)
  ;; work around to get org-agenda buffer working in bookmarks
  (org-agenda-list)
  (bookmark-jump "Burly: home"))



(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator)
 :prefix-map 'my-main-leader-map
 :prefix "SPC"
 :non-normal-prefix "C-SPC")

(general-create-definer main-leader-definer
  :keymaps 'my-main-leader-map)

(main-leader-definer
  "s" 'switch-to-buffer
  "r" 'my-run-program
  "t" 'popper-toggle
  "e" 'my-save-all-buffers
  "p" 'org-pomodoro
  "f" 'find-file
  "l" 'vterm
  "d" 'dired
  "n" 'eval-defun
  "i" 'consult-imenu
  "g" 'magit-project-status
  "k" 'kill-buffer
  "v" 'my-eval-expression
  "a" 'org-agenda-list
  "u" 'my-browse-documentation
  "c" 'toggle-truncate-lines
  "m" 'make-frame-command
  "1" 'my-window-bookmark-home
  "o" 'my-online-search
  "w" 'eww
  "b" 'remember
  )

;;;; my-second-leader-evil-map
(defun my-online-search (x)
  "Search duckduckgo with X"
  (interactive "sSearch: ")
  (browse-url (concat "https://duckduckgo.com/?q=" x))
  )

(defun my-test-code ()
  "Run test cases for program."
  (interactive)
  (cond
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (sharper-transient-test))
   (t (message "%s" "No test cases to run."))))

(defun my-build-code ()
  "Build program."
  (interactive)
  (cond
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (sharper-transient-build))
   (t (message "%s" "No program to build."))))

(defun my-code-action ()
  "Perform code actions at point."
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively 'eglot-code-actions))
   (t (message "%s" "No code actions at point."))))


(defun my-cycle-theme ()
  "Cycle through preferred themes."
  (interactive)
  (cond ((equal custom-enabled-themes '(modus-vivendi)) (disable-theme 'modus-vivendi) (load-theme 'tango-dark))
        ((equal custom-enabled-themes '(tango-dark)) (disable-theme 'tango-dark) (load-theme 'modus-operandi-tinted))
        ((equal custom-enabled-themes '(modus-operandi-tinted)) (disable-theme 'modus-operandi-tinted) (load-theme 'modus-vivendi-tinted))
        ((equal custom-enabled-themes '(modus-vivendi-tinted)) (disable-theme 'modus-vivendi-tinted) (load-theme 'modus-vivendi))))

(defun my-persist-eldoc (interactive)
  (interactive (list t))
  (if (get-buffer "*persisted eldoc*")
      (kill-buffer "*persisted eldoc*"))
  (with-current-buffer eldoc--doc-buffer
    (let ((s (buffer-string)))
      (with-current-buffer (generate-new-buffer "*persisted eldoc*")
        (insert s)
        (display-buffer (current-buffer))
        (set-window-start (get-buffer-window "*persisted eldoc*") 0)))))

(defun my-code-rename ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively 'eglot-rename))
   (t (message "%s" "Nothing to rename at point."))))

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator)
 :prefix-map 'my-second-leader-map
 :prefix "\\"
 :non-normal-prefix "C-\\")

(defmacro +general-global-menu! (name prefix-key &rest body)
  "Create a definer named +general-global-NAME.
  Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     ;; find a way to replace this with keymap based replacement
     (which-key-add-key-based-replacements (concat "\\ " ,prefix-key) ,name)
     (which-key-add-key-based-replacements (concat "C-\\ " ,prefix-key) ,name)
     (general-define-key
      :keymaps 'my-second-leader-map
      :prefix-map ',(intern (concat "my-" name "-map"))
      :prefix ,prefix-key)
     (general-create-definer ,(intern (concat "+general-global-" name))
       :keymaps ',(intern (concat "my-" name "-map")))
     (,(intern (concat "+general-global-" name))
      ,@body)))

(+general-global-menu! "code" "e"
  "t" 'my-test-code
  "b" 'my-build-code
  "a" 'my-code-action
  "d" 'dape
  "n" 'my-code-rename
  "k" 'docker
  )

(+general-global-menu! "miscellaneous" "s"
  "t" 'my-cycle-theme
  "e" 'my-persist-eldoc
  "n" 'remember-notes)
(+general-global-menu! "eval" "v"
  "s" 'eval-last-sexp
  "b" 'eval-buffer
  "r" 'eval-region)

(+general-global-menu! "completion" "p"
  "p" 'completion-at-point)

(+general-global-menu! "project" "j"
  "f" 'project-find-file
  "s" 'project-switch-to-buffer
  "j" 'project-switch-project
  "h" 'project-search)

(+general-global-menu! "file" "f"
  "d" 'delete-file
  "c" 'copy-file)

(general-define-key
 :keymaps 'my-second-leader-map
 "x" `("extend" . ,ctl-x-map)
 )

;;; evil suite
;;;; packages
(my-install-package evil)
(my-install-package evil-collection)
(my-install-package evil-easymotion)
(my-install-package evil-owl)
(my-install-package evil-mc)
(my-install-package evil-commentary)
(my-install-package evil-multiedit)
(my-install-package posframe)
;;;; config
(defun smart-tab ()
  (interactive)
  (cond ((buffer-local-value 'vertico--input (current-buffer)) (vertico-insert))
        ((minibufferp) (let ((res (run-hook-wrapped 'completion-at-point-functions #'completion--capf-wrapper 'all)))
                         (if res
                             (completion-at-point)
                           (hippie-expand nil)))) 
        ((derived-mode-p 'eshell-mode) (let ((res (run-hook-wrapped 'completion-at-point-functions #'completion--capf-wrapper 'all)))
                                         (if res
                                             (completion-at-point)
                                           (hippie-expand nil))))
        ((and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)) (corfu-insert))
        (mark-active (indent-region (region-beginning) (region-end)))
        ((looking-at "\\_>") (hippie-expand nil))
        (t (indent-for-tab-command))))

(defun my-format-buffer ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively #'eglot-format-buffer))
   (t (indent-region (point-min) (point-max)))))

(use-package evil
  :demand t
  :init
  (setopt
   evil-undo-system 'undo-redo
   evil-want-keybinding nil
   evil-want-minibuffer t
   evil-emacs-state-modes nil
   evil-insert-state-modes nil
   evil-motion-state-modes nil
   evil-insert-state-message nil
   evil-emacs-state-message nil
   evil-replace-state-message nil
   evil-visual-state-message nil
   evil-mode-line-format nil
   evil-visual-char-message nil
   evil-visual-line-message nil
   evil-visual-block-message nil
   evil-visual-screen-line-message nil)
  (evil-mode 1)
  :config
  (evil-define-command my-evil-window-vsplit-left (&optional count file)
    "Split the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
left. If COUNT and `evil-auto-balance-windows'are both non-nil
then all children of the parent of the splitted window are
rebalanced."
    :repeat nil
    (interactive "<wc><f>")
    (split-window (selected-window) (when count (- count))
                  'left)
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (when file
      (evil-edit file)))
  :general-config
  ('normal
   "=" (general-key-dispatch 'evil-indent
         "=" 'my-format-buffer)
   "C-S-d" 'evil-scroll-up
   "[ x" 'xref-go-back
   "] x" 'xref-go-forward
   "g d" 'xref-find-definitions
   "C-w C-v" 'my-evil-window-vsplit-left)
  ('insert
   "TAB" 'smart-tab)
  ('(normal visual) 'override
   "," (general-simulate-key "C-c")
   "<menu>" (general-simulate-key "C-c")
   "C-c ," 'evil-repeat-find-char-reverse)
  ('insert 'override
           "C-," (general-simulate-key "C-c")
           "C-<menu>" (general-simulate-key "C-c"))
  ('(normal motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line))

(use-package easy-motion
  :general
  ('normal
   "M-f" 'evilem-motion-find-char
   "M-F" 'evilem-motion-find-char-backward
   ))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init)
  :init
  (setopt
   evil-collection-setup-minibuffer t)
  :diminish evil-collection-unimpaired-mode)


(use-package evil-mc
  :hook (evil-mode . global-evil-mc-mode)
  :diminish evil-mc-mode)

(use-package evil-owl
  :hook (evil-mode . evil-owl-mode)
  :init
  (setopt
   evil-owl-display-method 'posframe
   evil-owl-max-string-length 50
   evil-owl-idle-delay 0.5)
  :custom
  (evil-owl-extra-posframe-args '(:width 50 :height 20))
  :diminish evil-owl-mode)

(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode)
  :diminish evil-commentary-mode)

(defun my-evil-multiedit-maintain-visual-cursor-advice (func &rest args)
  (if evil-visual-state-minor-mode
      (set-window-point (selected-window) (- (point) 1))))
(use-package evil-multiedit
  :hook (evil-mode . evil-multiedit-mode)
  :config
  (evil-multiedit-default-keybinds)
  (advice-add 'evil-multiedit-match-and-next :before #'my-evil-multiedit-maintain-visual-cursor-advice)
  (advice-add 'evil-multiedit-match-and-prev :before #'my-evil-multiedit-maintain-visual-cursor-advice)
  :config
  (general-unbind iedit-mode-keymap
    "TAB"
    "<tab>"
    "<backtab"))
;;; org
;;;; packages
(my-install-package org)

;;;; config
(use-package org
  :mode ("\\.org\\'" . org-mode))

;;;; org tasks and notes
;;;;; packages
(my-install-package org-pomodoro)
(my-install-package evil)
(my-install-package evil-org)
;;;;; config
(defun my-org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun org-match-sparse-tree-heading ()
  "Check heading of each tag."
  (interactive)
  (org-save-outline-visibility nil
    (org-match-sparse-tree)
    ))

(use-package org
  :hook (org-agenda-finalize . my-org-agenda-to-appt)
  :general-config
  (org-mode-map
   "C-<tab>" 'org-cycle
   "C-<iso-lefttab>" 'org-shifttab
   "C-c t" 'org-match-sparse-tree-heading)
  :init
  (setopt
   org-clock-sound (concat user-emacs-directory "bell.wav")
   org-agenda-timegrid-use-ampm t
   appt-activate t
   org-log-done 'time
   org-agenda-files nil
   org-log-into-drawer t
   org-agenda-files '("~/org/TODO.org")
   )
  :config
  (run-at-time "24:01" nil 'my-org-agenda-to-appt)
  )

(defun my-org-pomodoro-choose-break-time (arg)
  "Choose break time for pomodoro."
  (interactive "nBreak time (0 if overtime): ")
  (setq org-pomodoro-short-break-length arg))

(defun my-org-pomodoro-finished-with-overtime-advice (orig-fun &rest args)
  "Advise around `org-pomodoro-finished' to choose break time"
  (org-pomodoro-play-sound :pomodoro)
  (call-interactively #'my-org-pomodoro-choose-break-time)
  (cond ((= org-pomodoro-short-break-length 0) (org-pomodoro-overtime))
        ((zerop (mod (+ org-pomodoro-count 1) org-pomodoro-long-break-frequency)) (apply orig-fun args))
        (t (apply orig-fun args))))


(defun my-org-pomodoro-resume-after-break ()
  "Resume pomodoro timer if pomodoro timer is not currently in overtime."
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))

(defun my-org-pomodoro-clockout-before-kill-advice ()
  "Clock out time before exiting `org-pomodoro' so time is accurately tracked."
  (if (org-clocking-p)
      (save-window-excursion
        (org-clock-out))))

(use-package org-pomodoro
  :hook (org-pomodoro-break-finished . my-org-pomodoro-resume-after-break)
  :init
  (setopt
   org-pomodoro-ask-upon-killing t
   org-pomodoro-finished-sound (concat user-emacs-directory "bell.wav")
   org-pomodoro-length 30
   org-pomodoro-short-break-length 7
   org-pomodoro-long-break-length 15)
  :config
  (advice-add 'org-pomodoro-finished :around #'my-org-pomodoro-finished-with-overtime-advice)
  (advice-add 'org-pomodoro-kill :before #'my-org-pomodoro-clockout-before-kill-advice))

(use-package evil
  :demand t)

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :init
  (evil-mode 1)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar shift todo heading))
  :diminish evil-org-mode)



;;;; org babel diagrams
;;;;; packages
(my-install-package plantuml-mode)
;;;;; config
(use-package org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (dot . t))))
(use-package plantuml-mode
  :init
  (setopt
   org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar")) 
  :config
  (if (not (file-exists-p org-plantuml-jar-path))
      (plantuml-download-jar)))
;;;; org babel racket
;;;;; packages
(my-install-package ob-racket (ob-racket :type git :host github :repo "hasu/emacs-ob-racket"
                                         :files ("*.el" "*.rkt")))
(my-install-package racket-mode)
;;;;; config
(use-package org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((racket . t))))

(use-package ob-racket
  :hook (ob-racket-pre-runtime-library-load-hook . ob-racket-raco-make-runtime-library))

;;;; org babel python
;;;;; config
(use-package org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t))))

;;; better help
;;;;; packages
(my-install-package helpful)
(my-install-package evil)
(my-install-package elisp-demos)
;;;;; config
(defun my-help-at-point ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) 
    (call-interactively 'eldoc-doc-buffer))
   ((equal major-mode 'inferior-python-mode)
    (my-python-eldoc-at-point))
   ((featurep 'helpful)
    (save-selected-window (helpful-at-point)))
   (t (message "No help at point provider"))))

(defun my-helpful-callable-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-callable)))

(defun my-helpful-variable-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-variable)))

(defun my-helpful-key-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-key)))

(defun my-helpful-command-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-command)))

(defun my-helpful-function-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-function)))

(use-package evil
  :demand t)

(use-package helpful
  :demand t
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :general-config
  (help-map
   "f" 'my-helpful-callable-save-window
   "v" 'my-helpful-variable-save-window
   "k" 'my-helpful-key-save-window
   "x" 'my-helpful-command-save-window
   "F" 'my-helpful-function-save-window)
  ('normal
   "g h" 'my-help-at-point))



;;; better info
;;;;; packages
(my-install-package org-remark)
(my-install-package org)
(my-install-package pdf-tools)
;;;;; config
(use-package pdf-tools
  :init
  (pdf-tools-install t))
(use-package org
  :demand t)

(use-package org-remark
  :hook
  (Info-mode . org-remark-info-mode)
  :config
  (org-remark-create "understand"
                     '(:background "#1d3c25"))
  (org-remark-create "keyword"
                     '(:strike-through "cyan"))
  (org-remark-create "sentence"
                     '(:underline "white"))
  (org-remark-create "argument"
                     '(:overline "red"))
  (org-remark-create "highlight"
                     '(:foreground "#fce94f"))
  :general-config
  ('visual org-remark-mode-map
           "<return>" 'org-remark-mark-highlight
           "C-c 1" 'org-remark-mark-understand
           "C-c 2" 'org-remark-mark-keyword
           "C-c 3" 'org-remark-mark-sentence
           "C-c 4" 'org-remark-mark-argument)
  ('normal org-remark-mode-map
           "o" 'org-remark-open
           "]m" 'org-remark-view-next
           "[m" 'org-remark-view-prev
           "r" 'org-remark-delete)
  :diminish org-remark-global-tracking-mode
  :diminish org-remark-mode
  )

(defun my-file-extension (filename)
  (if (string-match "\\(?:\\.[A-Za-z]+\\)+$" filename)
      (let* ((file-extension (match-string 0 filename))
             (file-extension-list (split-string file-extension "\\." t)))
        file-extension-list)
    nil))

(defun my-Info-find-node (filename nodename &optional no-going-back strict-case
                                   noerror)
  "Go to an Info node specified as separate FILENAME and NODENAME.
NO-GOING-BACK is non-nil if recovering from an error in this function;
it says do not attempt further (recursive) error recovery.

This function first looks for a case-sensitive match for NODENAME;
if none is found it then tries a case-insensitive match (unless
STRICT-CASE is non-nil).

If NOERROR, inhibit error messages when we can't find the node."
  (info-initialize)
  (setq nodename (info--node-canonicalize-whitespace nodename))
  (setq filename (Info-find-file filename noerror))
  ;; Go into Info buffer.
  (or (derived-mode-p 'Info-mode) (info-pop-to-buffer filename))
  ;; Record the node we are leaving, if we were in one.
  (and (not no-going-back)
       Info-current-file
       (push (list Info-current-file Info-current-node (point))
             Info-history))
  
  (if (and filename (my-file-extension filename))
      (let ((buffer (find-file-noselect filename)))
        (switch-to-buffer buffer)
        (require 'general)
        (general-def 'normal 'local
          "u" 'info))
    (Info-find-node-2 filename nodename no-going-back strict-case)
    ))

(use-package info
  :config
  (advice-add 'Info-find-node :override #'my-Info-find-node))

;;; git
;;;; packages
(my-install-package magit)
;;;; config
(use-package magit
  :general-config
  ('(visual normal) magit-mode-map
   "] ]" 'magit-section-forward
   "[ [" 'magit-section-backward)
  ('normal magit-section-mode-map
           "] ]" 'magit-section-forward
           "[ [" 'magit-section-backward))


;;; docker
;;;; packages
(my-install-package docker)
;;; php
;;;;; packages
(my-install-package php-mode)
;;;;; config
(use-package php-mode
  :mode ("\\.php\\'" . php-ts-mode)
  )
;;; python
;;;; packages
(my-install-package pet)
(my-install-package treesit-auto)
;;;; config
(defun my-python-repl ()
  "Go to Python REPL and create it if needed."
  (interactive)
  (if (python-shell-get-buffer)
      (python-shell-switch-to-shell)
    (run-python "python3 -i" nil t)))

(defun my-python-eldoc-at-point ()
  "Get python documentation in eldoc with `python-eldoc--get-doc-at-point'."
  (interactive)
  (call-interactively 'eldoc-doc-buffer)
  (eldoc-display-in-buffer `((,(python-eldoc-function))) nil)
  )

(use-package python
  :general-config
  ('normal python-ts-mode-map
           "g z" 'my-python-repl)
  (python-ts-mode-map
   "C-c C-n" 'python-shell-send-defun)
  (inferior-python-mode-map
   "C-c ?" 'my-python-eldoc-at-point))

(use-package pet
  :demand t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  :diminish pet-mode)

(use-package treesit-auto
  :demand t
  :init
  (setopt
   treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))
;;; javascript
;;;;; packages
(my-install-package js2-mode)
;;;;; config
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))
;;; emacs lisp
;;;;; config
(defun my-elisp-imenu ()
  "Set up imenu for elisp."
  (setq imenu-generic-expression (append (list  (list "Use Package" "^(use-package \\(.+\\)" 1)) imenu-generic-expression)))
(use-package elisp-mode
  :hook (emacs-lisp-mode . my-elisp-imenu))


;;; sql
(use-package sql
  :mode ("\\.sql\\'" . sql-mode))
;;; debugging
;;;;; packages
(my-install-package dape)
;;;;; config
(use-package dape
  :hook 
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  (dape-display-source . pulse-momentary-highlight-one-line)
  (dape-start . (lambda () (save-some-buffers t t)))
  (dape-compile . kill-buffer)
  (python-ts-mode . dape-breakpoint-global-mode)
  (csharp-ts-mode . dape-breakpoint-global-mode)
  :custom
  (dape-key-prefix nil)
  :init
  (setopt
   dape-buffer-window-arrangement 'gud
   dape-info-hide-mode-line nil)
  :config
  (push (cons '("Z Q" . nil)
              (lambda (kb)
                (cons (car kb)
                      (if dape-active-mode
                          "dape-quit"
                        "evil-quit"))))
        which-key-replacement-alist)
  :general-config
  ('normal 'override
           :predicate 'dape-active-mode
           "Z Q" '("dape-quit" . dape-quit))
  ('(normal insert) 'override
   :predicate 'dape-active-mode
   "<f5>" 'dape-continue
   "<f6>" 'dape-step-out
   "<f7>" 'dape-step-in
   "<f8>" 'dape-next)
  (python-ts-mode-map
   "C-c B" 'dape-breakpoint-remove-all
   "C-c D" 'dape-disconnect-quit
   "C-c M" 'dape-disassemble
   "C-c R" 'dape-repl
   "C-c S" 'dape-select-stack
   "C-c b" 'dape-breakpoint-toggle
   "C-c e" 'dape-breakpoint-expression
   "C-c f" 'dape-restart-frame
   "C-c h" 'dape-breakpoint-hits
   "C-c i" 'dape-info
   "C-c l" 'dape-breakpoint-log
   "C-c m" 'dape-memory
   "C-c p" 'dape-pause
   "C-c q" 'dape-quit
   "C-c r" 'dape-restart
   "C-c t" 'dape-select-thread
   "C-c u" 'dape-until
   "C-c w" 'dape-watch-dwim)
  (csharp-ts-mode-map
   "C-c B" 'dape-breakpoint-remove-all
   "C-c D" 'dape-disconnect-quit
   "C-c M" 'dape-disassemble
   "C-c R" 'dape-repl
   "C-c S" 'dape-select-stack
   "C-c b" 'dape-breakpoint-toggle
   "C-c e" 'dape-breakpoint-expression
   "C-c f" 'dape-restart-frame
   "C-c h" 'dape-breakpoint-hits
   "C-c i" 'dape-info
   "C-c l" 'dape-breakpoint-log
   "C-c m" 'dape-memory
   "C-c p" 'dape-pause
   "C-c q" 'dape-quit
   "C-c r" 'dape-restart
   "C-c t" 'dape-select-thread
   "C-c u" 'dape-until
   "C-c w" 'dape-watch-dwim))

(use-package edebug
  :config
  (setq edebug-mode-map (make-sparse-keymap))
  :general-config
  ('(insert normal) edebug-mode-map
   "<f8>" 'edebug-step-mode
   "<f5>" 'edebug-go-mode
   "<f6>" 'edebug-step-out
   "<f7>" 'edebug-step-in)
  ('normal edebug-mode-map
           "Z Q" 'top-level
           "q" 'top-level)
  (edebug-mode-map    "C-c n"       'edebug-next-mode
                      "C-c G"       'edebug-Go-nonstop-mode
                      "C-c t"       'edebug-trace-mode
                      "C-c T"       'edebug-Trace-fast-mode
                      "C-c c"       'edebug-continue-mode
                      "C-c C"       'edebug-Continue-fast-mode

                      ;;"f"       #'edebug-forward ; not implemented
                      "C-c f"       'edebug-forward-sexp
                      "C-c h"       'edebug-goto-here

                      "C-c I"       'edebug-instrument-callee

                      ;; quitting and stopping
                      "C-c q"       'top-level
                      "C-c Q"       'edebug-top-level-nonstop
                      "C-c a"       'abort-recursive-edit
                      "C-c S"       'edebug-stop

                      ;; breakpoints
                      "C-c b"       'edebug-set-breakpoint
                      "C-c u"       'edebug-unset-breakpoint
                      "C-c U"       'edebug-unset-breakpoints
                      "C-c B"       'edebug-next-breakpoint
                      "C-c x"       'edebug-set-conditional-breakpoint
                      "C-c X"       'edebug-set-global-break-condition
                      "C-c D"       'edebug-toggle-disable-breakpoint

                      ;; evaluation
                      "C-c r"       'edebug-previous-result
                      "C-c e"       'edebug-eval-expression
                      "C-c C-x C-e" 'edebug-eval-last-sexp
                      "C-c E"       'edebug-visit-eval-list

                      ;; views
                      "C-c w"       'edebug-where
                      "C-c v"       'edebug-view-outside        ; maybe obsolete??
                      "C-c p"       'edebug-bounce-point
                      "C-c P"       'edebug-view-outside        ; same as v
                      "C-c W"       'edebug-toggle-save-windows

                      ;; misc
                      "C-c ?"       'edebug-help
                      "C-c d"       'edebug-pop-to-backtrace

                      "C-c -"       'negative-argument

                      ;; statistics
                      "C-c ="       'edebug-temp-display-freq-count)
  ('(normal insert) edebug-eval-mode-map
   "RET" 'edebug-update-eval-list))


;;; language server completion backends documentation output
;;;;; packages
(my-install-package cape)
(my-install-package yasnippet)
(my-install-package yasnippet-snippets)
(my-install-package yasnippet-capf)
(my-install-package orderless)
;;;;; config
(defvar my-eglot-completion-functions (list #'yasnippet-capf #'eglot-completion-at-point)
  "The list of completion functions to combine to replace `eglot-completion-at-point'.")

(defun my-eglot-capf ()
  "Configure `completion-at-point-functions' to replace `eglot-completion-at-point' with completion results including all completions in `my-eglot-capf'."
  ;; Remember that local values in completion-at-point-functions take priority over global values."
  (setq-local completion-at-point-functions
              (list (apply 'cape-capf-super my-eglot-completion-functions))))

(defun my-file-completion-for-eglot ()
  "Give `cape-file' priority in `completion-at-point-functions'."
  ;; Remember that local values in completion-at-point-functions take priority over global values."
  (add-hook 'completion-at-point-functions #'cape-file -100 t))

(use-package eglot
  :hook
  (csharp-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  :config
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)
  (add-hook 'eglot-managed-mode-hook #'my-file-completion-for-eglot 100)
  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  ;; This line causes function to delete or add characters when exiting https://github.com/minad/cape/issues/81
  ;;  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab")))
  (setf (alist-get '(csharp-mode csharp-ts-mode) eglot-server-programs) '("csharp-language-server")))

(defvar eldoc-ratio 0.30)

(defun my-buffer-distance (string buffer)
  "Get Levenshtein distance of STRING and BUFFER."
  (string-distance string (tramp-get-buffer-string buffer)))

(defun my-buffer-length (buffer)
  "Get string length of BUFFER."
  (length (tramp-get-buffer-string buffer)))

(defun my-eldoc-docs-string (list)
  "Get all strings in eldoc LIST and concat them."
  (let (value)
    (dolist (elt list value)
      (setq value (concat value (substring-no-properties (car elt)))))))

(defun my-save-eldoc-point-advice (orig-fun docs interactive)
  "Advise `eldoc-display-in-buffer' to save eldoc window position if window is active and DOCS is similar."
  (if (and eldoc--doc-buffer
           docs
           (get-buffer-window (eldoc-doc-buffer))
           (< (/ (float (my-buffer-distance (my-eldoc-docs-string docs) (eldoc-doc-buffer))) (my-buffer-length (eldoc-doc-buffer))) eldoc-ratio))
      (let* ((eldoc (eldoc-doc-buffer))
             (window (get-buffer-window eldoc))
             (start (window-start window)))
        (funcall orig-fun docs interactive)
        (set-window-start window start))
    (funcall orig-fun docs interactive)
    ))

(use-package eldoc
  :init
  (setopt
   eldoc-echo-area-prefer-doc-buffer t
   eldoc-echo-area-use-multiline-p nil)
  :config
  (advice-add 'eldoc-display-in-buffer :around #'my-save-eldoc-point-advice)
  :diminish eldoc-mode)

(use-package yasnippet
  :init
  (setopt
   yas-also-auto-indent-first-line t)
  (yas-global-mode 1)
  :general-config
  (yas-keymap
   "<tab>" nil
   "TAB" nil
   "C-<tab>" 'yas-next-field
   "C-<iso-lefttab>" 'yas-prev-field)
  :diminish yas-minor-mode)

;;; default completion backends 
;;;; packages
(my-install-package cape)
(my-install-package yasnippet)
(my-install-package yasnippet-snippets)
(my-install-package yasnippet-capf)
(my-install-package orderless)
;;;; config
(defun my-hippie-expand-advice (orig-fun &rest args)
  (let ((case-fold-search nil))
    (apply orig-fun args)))

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
  :config
  (advice-add 'hippie-expand :around #'my-hippie-expand-advice)
  )

(use-package yasnippet
  :init
  (setopt
   yas-also-auto-indent-first-line t)
  (yas-global-mode 1)
  :general-config
  (yas-keymap
   "<tab>" nil
   "TAB" nil
   "C-<tab>" 'yas-next-field
   "C-<iso-lefttab>" 'yas-prev-field)
  :diminish yas-minor-mode)

(defun my-yasnippet-add-completion-functions ()
  "Add yasnippet-capf to `completion-at-point-functions'."
  ;; Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Add yasnippet-capf globally
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package yasnippet-capf
  :hook ((prog-mode org-mode) . my-yasnippet-add-completion-functions)
  )

;;; completion middle end
;;;; packages
(my-install-package orderless)
;;;; config
(use-package orderless
  :init
  (setopt
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))
;;; completion frontends
;;;; packages
(my-install-package consult)
(my-install-package vertico)
(my-install-package corfu)
(my-install-package kind-icon)
(my-install-package marginalia)
;;;; config
(use-package corfu
  :init
  (setopt
   corfu-cycle t
   corfu-on-exact-match 'quit
   corfu-quit-no-match 'separator
   corfu-preview-current 'nil
   corfu-preselect 'first
   corfu-auto t
   corfu-auto-prefix 2
   corfu-auto-delay 0.3
   corfu-min-width 80
   corfu-max-width corfu-min-width
   corfu-count 14
   corfu-scroll-margin 4
   global-corfu-minibuffer nil
   corfu-popupinfo-delay nil)
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :general-config
  (corfu-map
   "M-SPC" 'corfu-insert-separator
   "RET" nil
   )
  ('insert corfu-map
           "C-S-k" 'corfu-scroll-down
           "C-S-j" 'corfu-scroll-up)
  (corfu-popupinfo-map
   "C-h" 'corfu-popupinfo-toggle
   "C-S-j" 'corfu-popupinfo-scroll-up
   "C-S-k" 'corfu-popupinfo-scroll-down))

(use-package vertico
  :init
  (setopt
   vertico-cycle t)
  (vertico-mode)
  :config
  (eval-after-load "evil-maps"
    (dolist (map '(evil-insert-state-map))
      (define-key (eval map) "\C-k" nil)
      ))
  :general-config
  ('(insert normal) vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   "C-S-j" 'scroll-up-command
   "C-b" 'evil-backward-char
   "C-f" 'evil-forward-char))

(use-package consult
  :init
  (setopt completion-in-region-function 'consult-completion-in-region))

(use-package kind-icon
  :init
  (setopt
   kind-icon-mapping
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
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )
(use-package marginalia
  :init
  (marginalia-mode))
;;; windows
;;;; packages
(my-install-package popper)
(my-install-package burly)
;;;; config
(use-package popper
  :init
  (setopt
   popper-display-control 'user
   popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*eshell\\*"
     "\\*dotnet"
     "events\\*"
     "\\*shell\\*"
     "\\*dape-shell\\*" 
     "\\*vterm\\*"
     "^\\* docker.+ up"
     "^\\* docker.+ exec"
     "\\*Racket"
     (lambda (buf) (with-current-buffer buf
                     (derived-mode-p 'comint-mode)))
     debugger-mode
     compilation-mode
     ))
  (popper-mode 1)
  (popper-echo-mode 1))

(defun my-fit-window-to-buffer (&optional window max-height min-height max-width min-width preserve-size)
  (let* ((wind (if window window (selected-window)))
         (initial-width (window-width wind))
         (initial-height (window-height window)))
    (fit-window-to-buffer window max-height min-height max-width min-width preserve-size)
    (if (or (not (equal (window-width wind) initial-width)) (not (equal (window-height wind) (window-height wind))))
        t
      nil)))

(defun my-fit-window-to-right-side (window)
  "Use `fit-window-to-buffer' with right side window specifications."
  (let ((max-width (floor (* 0.35 (frame-width))))
        (max-height (floor (* 0.50 (frame-height)))))
    (if (my-fit-window-to-buffer window max-height window-min-height max-width)
        nil
      (window-resize window (- max-width (window-width window)) t)
      (window-resize window (- max-height (window-height window))))))


;; TODO
(defun my-fit-window-to-magit (window)
  "Use 'fit-window-to-buffer' but make it work for magit's behavior."
  (let ((max-width (floor (* 0.50 (frame-width))))
        (max-height (floor (* 1.00 (frame-height)))))
    (fit-window-to-buffer window max-height window-min-height max-width)))

(use-package window
  :init
  (setopt
   menu-bar-mode nil
   fit-window-to-buffer-horizontally t
   tab-bar-mode nil
   tool-bar-mode nil
   line-number-mode nil
   switch-to-buffer-in-dedicated-window 'pop
   switch-to-buffer-obey-display-actions nil
   window-sides-slots '(2 2 2 2))
  :config
  (setq display-buffer-alist
        '(((or "\\*info\\*" (major-mode . eww-mode))
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . my-fit-window-to-right-side))
          ("\\*helpful\\|\\*Help\\*\\|\\*eldoc\\*\\|\\*persisted eldoc\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . -1)
           (window-width . my-fit-window-to-right-side))
          ((or "\\*dotnet\\|\\*Messages\\*\\|Output\\*\\|events\\*\\|\\*eshell\\*\\|\\*shell\\*\\|\\*dape-shell\\*\\|\\*vterm\\*\\|^\\* docker.+ up\\|^\\* docker.+ exec\\|\\*Racket" (major-mode . compilation-mode)  (major-mode . debugger-mode) (derived-mode . comint-mode)) 
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 0.50))
          ((or "^\\*docker.+\\*$" (derived-mode . magit-mode) "\\*Remember\\*")
           (display-buffer-reuse-window display-buffer-in-direction)
           (window . root)
           (window-width . 0.50)
           (direction . left))
          ((major-mode . dired-mode)
           (display-buffer-reuse-mode-window display-buffer-in-direction)
           (window . root)
           (window-width . 0.50)
           (direction . left)))))

;;; visual non-functional changes
;;;; packages
(my-install-package adaptive-wrap)
;;;; config
(use-package font-core
  :config
  (set-frame-font "JetBrains Mono 10" nil t)
  (setopt line-spacing 1))

(use-package adaptive-wrap
  :hook ((eshell-mode help-mode html-ts-mode prog-mode evil-org-mode dired-mode helpful-mode info-mode) . adaptive-wrap-prefix-mode)
  )

;;; which key
(use-package which-key
  :init
  (setopt which-key-sort-order #'which-key-key-order-alpha
          which-key-sort-uppercase-first nil
          which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 6
          which-key-side-window-slot -10
          which-key-max-description-length nil
          which-key-idle-delay 0.5
          which-key-separator ":"
          which-key-allow-multiple-replacements t
          which-key-popup-type 'minibuffer
          )
  (which-key-mode)
  :diminish which-key-mode
  )
;;; file manager
(defun my-dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive nil dired-mode)
  (my-dired--find-possibly-alternative-file (dired-get-file-for-visit)))

(defun my-dired--find-possibly-alternative-file (file)
  "Find FILE, but respect `dired-kill-when-opening-new-dired-buffer'."
  (if (and dired-kill-when-opening-new-dired-buffer
           (file-directory-p file)
           (< (length (get-buffer-window-list)) 2))
      (progn
        (set-buffer-modified-p nil)
        (dired--find-file #'find-alternate-file file))
    (my-dired--find-file file)))

(defun my-dired--find-file (file)
  "Call FIND-FILE-FUNCTION on FILE, but bind some relevant variables."
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t)
        ;; This binding prevents problems with preserving point in
        ;; windows displaying Dired buffers, because reverting a Dired
        ;; buffer empties it, which changes the places where the
        ;; markers used by switch-to-buffer-preserve-window-point
        ;; point.
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (org-display-buffer-in-window (find-file-noselect file) `((window . ,(window-in-direction 'right))))
    (delete-window)
    ))

(defun my-dired-keybinds ()
  "Create keybinds for dired."
  (general-def 'normal dired-mode-map
    "q" 'evil-window-delete
    "RET" 'my-dired-find-file))

(use-package dired
  :general-config
  ('normal dired-mode-map
           "q" 'evil-window-delete
           "RET" 'my-dired-find-file))
;;; grammar
;;;; config
(use-package ispell
  :hook ((prog-mode org-mode) . ispell-minor-mode)
  :init
  (setopt
   ispell-program-name "hunspell"
   ispell-dictionary "en_US")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US")
  :general-config
  (ispell-minor-keymap
   "C-c d" 'ispell-word
   "RET" nil
   "SPC" nil)
  :diminish ispell-minor-mode)

(use-package flyspell
  :hook ((org-mode LaTeX-mode) . flyspell-mode)
  :diminish flyspell-mode)
;;; shells
;;;; packages
(my-install-package vterm)
;;;; config
(use-package vterm
  :hook (vterm-mode . (lambda () (setq evil-insert-state-modes nil))))

(use-package eshell
  :hook ((eshell-first-time-mode . (lambda () (yas-minor-mode -1)))
         (((eshell-mode shell-mode) . (lambda () (corfu-mode -1)))))
  :init
  (setopt
   password-cache-expiry 3600
   eshell-prefer-lisp-functions t
   ;; password-cache 5
   password-cache-expiry 3600)
  :config
  (require 'em-tramp))

;;; emacs
(use-package emacs
  :hook (((Info-mode prog-mode evil-org-mode html-ts-mode ibuffer-mode imenu-list-minor-mode dired-mode LaTeX-mode) . (lambda () (setq display-line-numbers 'visual)))
         ((prog-mode html-ts-mode) . (lambda () (setq indent-tabs-mode nil))))
  :mode ("init.el" . (lambda () (emacs-lisp-mode) (outline-minor-mode 1) (evil-close-folds)))
  :config
  (setopt
   use-short-answers t
   undo-limit 400000
   undo-strong-limit 3000000
   undo-outer-limit 48000000
   native-comp-async-report-warnings-error nil
   backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
   indent-tabs-mode nil
   x-stretch-cursor t
   window-combination-resize t
   sentence-end-double-space nil
   doc-view-resolution 200
   enable-recursive-minibuffers t
   read-extended-command-predicate #'command-completion-default-include-p
   minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (scroll-bar-mode -1)
  (global-auto-revert-mode 1)
  (savehist-mode 1)
  (with-eval-after-load 'mule-util
    (setq
     truncate-string-ellipsis "..."))
  (add-to-list 'custom-enabled-themes 'tango-dark)
  (load-theme 'tango-dark)
  (blink-cursor-mode 0)
  (add-to-list 'exec-path (concat user-emacs-directory "bin/"))
  (add-to-list 'Info-directory-list (concat user-emacs-directory "info/"))
  :diminish outline-minor-mode)

