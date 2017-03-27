;;; -*- lexical-binding: t -*-

;; pt - project tree

(require 'dt-utils)

(defconst pt:projects-path  "~/Projects")

(dt:define-remote/interactive
 pt:new-project!
 pt:reload-current-project!
 pt:select-by-name!
 
 pt:select-next-leaf!
 pt:select-prev-leaf!
 pt:select-next-leaf-4!
 pt:select-prev-leaf-4!
 
 pt:select-next-intr!
 pt:select-prev-intr!

 pt:switch-to-next-section!
 pt:switch-to-prev-section!
 
 pt:lift-current-node!
 pt:lower-current-node!

 pt:rename!
 
 pt:remove-from-tree!
 pt:delete!

 pt:create-test!
 pt:toggle-test!
 pt:delete-test!
 
 pt:run-test-at-background!
 pt:run-module-at-foreground!
 pt:interrupt-execution!
 
 pt:initialize-git-repository!
 pt:switch-to-current-project-node!
 )

(defface pt:leaf-face 
  '(( t :inherit dt:ebuffer-node-face :foreground "azure3")) "")

(defface pt:intr-1-face  
  '(( t :inherit dt:ebuffer-node-face :foreground "red3" :weight ultra-bold)) "")

(defface pt:intr-2-face  
  '(( t :inherit dt:ebuffer-node-face :foreground "forest green" :weight ultra-bold)) "")

(defface pt:intr-3-face  
  '(( t :inherit dt:ebuffer-node-face :foreground "dodger blue" :weight ultra-bold)) "")

(defface pt:intr->3-face  
  '(( t :inherit dt:ebuffer-node-face :foreground "royal blue" :weight ultra-bold)) "")


(defface pt:root-unmerged-face  
  '(( t :inherit dt:header-face :foreground "Orange")) "")


(defface pt:indentation-marker-face
  '(( t :inherit dt:ebuffer-node-face :foreground "gray20")) "")


(let ((global-ctrl-p-map (make-sparse-keymap)))
  (define-key global-ctrl-p-map (kbd "p") #'pt:new-project-from-existing-dir!)
  (define-key global-ctrl-p-map (kbd "i") #'pt:new-project)
  (define-key global-ctrl-p-map (kbd "l") #'pt:load-project!)

  (define-key (current-global-map) (kbd "C-p") global-ctrl-p-map))

(define-minor-mode pt-mode
  "project tree mode"
  :init-value nil
					;:lighter " CN"
  :keymap
  (let ((pt-map (make-sparse-keymap))
        (ctrl-p-map (make-sparse-keymap)))

    (define-key ctrl-p-map (kbd "n") #'pt:add-file!)
    (define-key ctrl-p-map (kbd "a") #'pt:add-directory!)
    (define-key ctrl-p-map (kbd "d") #'pt:delete!)
    (define-key ctrl-p-map (kbd "u") #'pt:reload-current-project!)
    

    (define-key pt-map (kbd "C-p") ctrl-p-map)

    (define-key pt-map (kbd "s-;") #'pt:select-prev-intr!)
    (define-key pt-map (kbd "s-'") #'pt:select-next-intr!)
    (define-key pt-map (kbd "s-[") #'pt:select-prev-leaf!)
    (define-key pt-map (kbd "s-]") #'pt:select-next-leaf!)
    (define-key pt-map (kbd "s-{") #'pt:select-prev-leaf-4!)
    (define-key pt-map (kbd "s-}") #'pt:select-next-leaf-4!)

    (define-key pt-map (kbd "s-1") #'pt:switch-by-shortcut-1!)
    (define-key pt-map (kbd "s-2") #'pt:switch-by-shortcut-2!)
    (define-key pt-map (kbd "s-3") #'pt:switch-by-shortcut-3!)
    (define-key pt-map (kbd "s-4") #'pt:switch-by-shortcut-4!)

    (define-key pt-map (kbd "s-d") #'pt:switch-to-next-section!)
    (define-key pt-map (kbd "s-a") #'pt:switch-to-prev-section!)
    
    (define-key pt-map (kbd "M-s-[") #'pt:lift-current-node!)
    (define-key pt-map (kbd "M-s-]") #'pt:lower-current-node!)
    
    (define-key pt-map (kbd "s-<tab>") #'pt:toggle-test!)
    
    (define-key pt-map (kbd "s-\\") #'pt:run-module-at-foreground!)
    (define-key pt-map (kbd "s-|") #'pt:run-test-at-background!)
    (define-key pt-map (kbd "C-\\") #'pt:interrupt-execution!)
    
    (define-key pt-map (kbd "M-c") #'gt:commit!)
    pt-map)
  (set (make-local-variable 'project-buffer-modified-p) nil))





(defun pt:new-project-from-existing-dir! ()
  (interactive)
  (let ((default-directory pt:projects-path)
	(insert-default-directory nil))
    (dt:call! 'pt:new-project-from-existing-dir!
              (directory-file-name (read-directory-name "new project name: ")))))

(defun pt:add-directory! ()
  (interactive)
  (let ((default-directory (dt:call 'pt:entered-directory-path))
	(insert-default-directory nil))
    (dt:call! 'pt:add-directory!
              (directory-file-name (read-directory-name "new directory name: ")))))

(defun pt:add-file! ()
  (interactive)
  (let ((default-directory (dt:call 'pt:entered-directory-path))
	(insert-default-directory nil))
    (dt:call! 'pt:add-file!
              (read-file-name "new file name: "))))


(defun pt:load-project! ()
  (interactive)
  (let ((default-directory pt:projects-path)
	(insert-default-directory nil))
    (dt:call! 'pt:load-project! (read-directory-name "project name: "))))


(defun pt:switch-by-shortcut-1! ()
  (interactive)
  (dt:call! 'pt:switch-by-shortcut! ?1))

(defun pt:switch-by-shortcut-2! ()
  (interactive)
  (dt:call! 'pt:switch-by-shortcut! ?2))

(defun pt:switch-by-shortcut-3! ()
  (interactive)
  (dt:call! 'pt:switch-by-shortcut! ?3))

(defun pt:switch-by-shortcut-4! ()
  (interactive)
  (dt:call! 'pt:switch-by-shortcut! ?4))


(defvar pt:mod-status-updater (run-at-time nil 0.1 #'pt:update-modification-status))
(defun pt:update-modification-status ()
  (let ((modifiedp (buffer-modified-p)))
    (when (and (local-variable-p 'project-buffer-modified-p)
	       (not (eq project-buffer-modified-p modifiedp)))
      (setq project-buffer-modified-p modifiedp)
      (if modifiedp
	  (dt:call! 'pt:switch-on-indicator! 0)
	(dt:call! 'pt:switch-off-indicator! 0)))))


(defadvice switch-to-buffer (before pt-hook
				    (&rest args))
  (pt:update-modification-status))



(defconst pt:buffer (generate-new-buffer "project-tree"))

(with-current-buffer pt:buffer
  (setq fit-window-to-buffer-horizontally t)
  (setq right-fringe-width 3)
  (font-lock-mode 1))

(defadvice set-window-buffer (after set-project-tree-window-margin
				    (window buffer-or-name &optional keep-margins))
  (when (eq (typecase buffer-or-name
	     (string (get-buffer buffer))
	     (buffer buffer-or-name))
	    pt:buffer)
    (set-window-margins window 0 4)))


(ad-activate #'set-window-buffer)

(defconst wl:main-layout
  (wlf:layout 
   '(| (:right-size 40)
       source-code
       project-tree)
   '((:name source-code)
     (:name project-tree
            :buffer pt:buffer))))

(delete-other-windows)

(defun pt:init-main-layout ()
  (wlf:refresh wl:main-layout))

(defun pt:set-source-code-buffer (buffer)
  (wlf:set-buffer wl:main-layout 'source-code buffer))

;; (defadvice compile-goto-error (after pt-hook
;; 				     (&rest args))
;;   (message ">>>> %s" (buffer-name (current-buffer)))

;;   (when pt-mode
;;     (dt:call pt:select-by-name!
;; 				     (list (buffer-name (current-buffer))))))



(ad-activate #'switch-to-buffer)
;; (ad-activate #'compile-goto-error) 




(defun pt:make-buffer (path)
  (let ((buffer (find-file-noselect path t)))
    (with-current-buffer buffer
      (pt-mode))
    buffer)) 

(defun pt:init-file-buffer (path new-name new-mode-line-buffer-identification-parts)
  (pt:rename-file-buffer (pt:make-buffer path)
        		 new-name
        		 new-mode-line-buffer-identification-parts))






(defun pt:rename-file-buffer (buffer new-name new-mode-line-buffer-identification-parts)
  (with-current-buffer buffer
    (rename-buffer new-name)
    (setq mode-line-buffer-identification
          (cl-destructuring-bind ((base-string base-face)
                                  (name-string name-face))
              new-mode-line-buffer-identification-parts
            (let ((normalized-base-string (cond 
                                           ((> (length base-string) 30)
                                            (format "%.27s..." base-string))
                                           (t base-string))))
              (concat (propertize normalized-base-string 'font-lock-face base-face)
                      (propertize name-string 'font-lock-face name-face)))))
    nil))

(defvar pt:exec-proc nil)
(defvar pt:proc-inspector-timer (run-at-time nil 0.1 #'pt:inspect-proc))
(cancel-timer pt:proc-inspector-timer)


(defun pt:inspect-proc ()
  (let ((status (process-status pt:exec-proc)))
    (case status
      (run nil)
      (exit (when pt:exec-proc
	      (dt:call! 'pt:on-exit-status!
                        (process-exit-status pt:exec-proc))
	      (cancel-timer pt:proc-inspector-timer)
	      
	      ;; (with-current-buffer (process-name pt:exec-proc)
	      ;; 	(setq buffer-read-only t))
	      
	      (set1 pt:exec-proc nil)))
      (otherwise (dt:call! 'pt:on-unexpected-status!
                           status)))))


(defconst pt:racket-programm "racket")
(defconst pt:racket-background-command (format "sleep 0.1 && %s" pt:racket-programm))



(defun pt:racket-test-output-filter (proc out)
  (let ((buffer (get-buffer (process-name proc))))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
	
	(when (string-match (rx (1+ digit) " success(es) "
				(group (1+ digit)) " failure(s) "
				(group (1+ digit)) " error(s)") out)
	  (dt:call! 'pt:on-test-result!
                    (+ (string-to-int (match-string-no-properties 1 out))
                       (string-to-int (match-string-no-properties 2 out)))))
	
	(princ (format "%s\n" out) buffer)))))




(defun pt:execute-test-at-background (name path)
  (setq pt:exec-proc (start-process-shell-command name name (format "%s %s"
								    pt:racket-background-command
								    path)))

  (with-current-buffer name
    (let ((buffer-read-only nil))
      (princ "\n" (get-buffer name))))

  (set-process-filter pt:exec-proc #'pt:racket-test-output-filter)
  (timer-activate pt:proc-inspector-timer))

(defun pt:execute-test-at-foreground (name path)
  (pt:execute-test-at-background name path)
  (select-window (display-buffer name)))

(defun pt:execute-module-at-foreground (name path)
  (setq pt:exec-proc (start-process name name pt:racket-programm path))
  (timer-activate pt:proc-inspector-timer)
  
  (select-window (display-buffer name)))


(defun pt:interrupt-process ()
  (interrupt-process pt:exec-proc))


(defun show-test-exec-buffer ()
  (interactive)
  (let ((buffer (get-buffer (format "*exec %s*" (dt:call 'pt:test-buffer-name)))))
    (when buffer
      (let ((win (get-buffer-window buffer)))
	(cond
	 ((window-live-p win)
	  (delete-window win))
	 
	 (t (display-buffer buffer)))))))


(defun pt:init-exec-buffer (name)
  (let ((exec-buffer (get-buffer name)))
    
    (when exec-buffer
      
      (when (window-live-p (get-buffer-window exec-buffer))
	(delete-window (get-buffer-window exec-buffer)))
      
      (kill-buffer exec-buffer))
    
    (with-current-buffer (generate-new-buffer name)
      (compilation-mode)
      (font-lock-mode))))


(provide 'dt-project)


