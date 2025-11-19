;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name (string-trim (shell-command-to-string "git config --global user.name"))
      user-mail-address (string-trim (shell-command-to-string "git config --global user.email")))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13 :weight 'normal))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq doom-env-file "~/.doom.d/environment")
(if (file-exists-p "~/.doom.d/environment")
    (doom-load-envvars-file "~/.doom.d/environment"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
(map! :map vertico-map "C-j" #'vertico-insert)

(map! "C-:" #'hippie-expand)
(map! "M-n" #'forward-paragraph)
(map! "M-p" #'backward-paragraph)
(map! "C-s" #'tim/consult-line)
(map! "²" #'+popup/toggle)

(setq projectile-globally-ignored-directories '("__build"
                                                ".cache"
                                                "venv"
                                                ".venv"
                                                "node_modules"
                                                ".idea"
                                                ".vscode"
                                                ".git"
                                                ".mypy_cache"
                                                ".pytest_cache"
                                                "build"
                                                "dist"))
(setq tab-width 2)
(setq typescript-indent-level tab-width)
(setq c-basic-offset tab-width)
(setq js-indent-level tab-width)

;; deactivate project indexing on tramp remote
;; Basically resolve the huge connection time on tramp
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

(use-package! sops
  :bind (("C-c C-s" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-e" . sops-edit-file))
  :init (global-sops-mode 1))

(use-package! vundo)
(map! "C-x u" #'vundo)
;;; Personnal Stuf

(defun tim/consult-line ()
  "When region is selected use it as candidate."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (set-mark-command 0)
        (consult-line (buffer-substring-no-properties beg end)))
    (consult-line)))

(defun tim/find-current-build-folder ()
  "Return the build folder __build in current project"
   (let* ((project-root (or (locate-dominating-file default-directory "__build")
                            (locate-dominating-file default-directory "build")
                            (locate-dominating-file default-directory "compile_commands.json") ; for clangd
                            (locate-dominating-file default-directory ".clangd" ) ; for clangd
                            (locate-dominating-file default-directory ".clang-tidy" ) ; for clang-tidy
                            (locate-dominating-file default-directory "compile_flags.txt") ; for ccls
                            (locate-dominating-file default-directory "compile_commands.json") ; for ccls
                            (locate-dominating-file default-directory "compile_flags.txt") ; for ccls
                            nil)))
     (when project-root
       ; the following will prevent to find the compile_commands if it's elsewhere of build/Debug
       (let ((compile-file (directory-files-recursively (concat project-root "build/Debug") "compile_commands.json")))
         (when compile-file
           (file-name-directory (car compile-file)))))))

(defun tim/provision-docker-container ()
  (interactive)
  (kill-new (with-temp-buffer
              (insert-file-contents "/home/bineau/init_docker_bash.sh")
              (buffer-string)))
  (when (string-match-p "vterm.*" (buffer-name))
    (vterm-yank)))

(setq lsp-clients-clangd-executable "/home/bineau/.emacs.d/.local/etc/lsp/clangd/clangd_15.0.6/bin/clangd")

(defun tim/add-compile-json-clangd-path (args)
  "Used as advice, add compile command directory in the build folder for clangd"
  (let ((build-folder (tramp-file-local-name (tim/find-current-build-folder))))
    (if build-folder
        `(,@args ,(concat "--compile-commands-dir=" build-folder))
      args)))

(defun int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

;; (dap-register-debug-template "Rust::GDB Run Configuration"
;;                              (list :type "gdb"
;;                                    :request "launch"
;;                                    :name "GDB::Run"
;;                            :gdbpath "rust-gdb"
;;                                    :target nil
;;                                    :cwd nil))

(advice-add 'lsp-clients--clangd-command :filter-return #'tim/add-compile-json-clangd-path)
;; (advice-remove 'lsp-clients--clangd-command #'tim/add-compile-json-clangd-path) ; for debug purpose
;;
; Better clangd
(setq lsp-clients-clangd-args '("-j=4"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--query-driver=/**/*"
                                "--header-insertion-decorators=0"))

;; MIGHT NEED TO REMOVE THAT IN THE FUTURE AS IT MAY BE FIXED UPSTREAM
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; (add-hook! 'doom-init-ui-hook
;;            :append ;; ensure it gets added to the end.
;;            #'(lambda () (require 'uniquify) (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

(defun tim/lab--alert (msg)
  ;; (message ">> lab.el :: %s" msg)
  (when (require 'alert nil t)
    (alert msg
           :title "Gitlab"
           :icon "/mnt/c/Users/bineau/AppData/Local/Emacs-Toast/gitlab.png"
           :severity 'urgent)))

(defun tbi/get-gdb-breakpoint ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (let ((filename (or (file-name-nondirectory (buffer-file-name)) (buffer-name)))
        (line-number (line-number-at-pos)))
    (let ((result (format "%s:%d" filename line-number)))
      (kill-new result) ; Copy to kill ring
      (message "%s" result)))) ; Display a message

(map! :desc "Get the GDB breakpoint position"
      "C-c f g" #'tbi/get-gdb-breakpoint)

(use-package! lab
  :defer t
  :init
  ;; Required.
  (setq lab-host "https://gitlab.kudelski.com"
        ;; Required.
        ;; See the following link to learn how you can gather one for yourself:
        ;; https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html#create-a-personal-access-token
        lab-token (getenv "READ_REG_TOKEN")
        lab-git-host "https://git.kudelski.com")
  (define-advice magit-push-current-to-pushremote (:after (&rest _) start-watching-pipeline)
    (lab-watch-pipeline-for-last-commit))
  (defalias 'lab--alert 'tim/lab--alert))

(use-package! alert)

;; (use-package! alert-toast
;;   :init
;;   (setq alert-default-style 'toast))


;; Optional, but useful. See the variable documentation.
;; (setq lab-group "YOUR-GROUP-ID")

(after! lsp-rust
  (setq lsp-rust-analyzer-lru-capacity 100
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-reborrow-hints t
        lsp-rust-analyzer-display-closure-return-type-hints nil
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t
        lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy")

  (after! lsp
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]cdk\\.out")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__build\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]htmlcov\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].pytest_cache\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].ruff_cache\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].pbm-build\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].gitlab-ci-local\\'"))

  (after! magit
    (setq magit-format-file-function #'magit-format-file-nerd-icons)
    ;; (require 'pretty-magit) ;; to fix
    )

  (setopt mcp-hub-servers
          '(("github" .
             (:command
              "docker"
              :args
              ("run" "-i" "--rm" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server")
              :env
              (:GITHUB_PERSONAL_ACCESS_TOKEN
               (tbi/read-file-to-string-with-lf (file-name-concat doom-user-dir ".github_pat"))))))
          )

  (use-package! pet
    :config
    (add-hook 'python-base-mode-hook 'pet-mode -10))

  ;; (use-package! nix-mode)

  (use-package! copilot
  :init (setq copilot-node-executable "~/.local/share/nvm/v24.9.0/bin/node")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("M-<right>" . 'copilot-accept-completion-by-word)
              ("M-f" . 'copilot-accept-completion-by-word)
              ("C-e" . 'copilot-accept-completion-by-line)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion)))

  (use-package! lsp-biome)
  (use-package! swagg)
  (use-package! copilot-chat)
  (use-package! pyenv-mode)
  (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)

  (defun tbi/read-file-to-string-with-lf (filepath)
    "Read the entire content of FILEPATH into a string, replacing all line breaks with \\n."
    (with-temp-buffer
      (insert-file-contents filepath)
      (let ((content (buffer-string)))
        (replace-regexp-in-string "\r?\n" "\\\\n" content))))

  (setq copilot-chat-commit-prompt (tbi/read-file-to-string-with-lf (file-name-concat doom-user-dir "commit_prompt.md")))

  (defun tbi/projectile-add-known-projects (dirs)
    "Add multiple DIRS to Projectile's known projects."
    (interactive
     (list (read-from-minibuffer "Enter directories (separated by space): ")))
    (let ((dir-list (if (stringp dirs)
                        (split-string dirs " " t)
                      dirs)))
      (dolist (dir dir-list)
        (if (file-directory-p dir)
          (projectile-add-known-project (expand-file-name dir))
        (error "%s is not a valid dir" dir)))))

  ;; (tbi/projectile-add-known-projects '(
  ;;                                      "~/prog/products/OriginDisruptionPOC"
  ;;                                      "~/prog/products/QMBrowserSDK"
  ;;                                      "~/prog/products/framework"
  ;;                                      "~/prog/products/gatekeeperapi/"
  ;;                                      "~/prog/products/gatekeeperapi"
  ;;                                      "~/prog/products/packer"
  ;;                                      "~/prog/products/screen-tracking"
  ;;                                      ))
  (require 'dap-cpptools))

;; TO get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
