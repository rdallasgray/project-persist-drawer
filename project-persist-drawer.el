;;;###autoload
(define-minor-mode project-persist-drawer-mode
  "Use a project drawer with project-persist."
  :global t
  :group 'project-persist
  (if project-persist-drawer-mode
      (project-persist-drawer-on)
    (project-persist-drawer-off)))

(defun project-persist-drawer--no-adaptor ()
  (message "project-persist-drawer: no adaptor loaded"))

;; (defun project-persist-drawer-get-window ()
;;   (project-persist-drawer--no-adaptor))

;; (defun project-persist-drawer-open (dir)
;;   (project-persist-drawer--no-adaptor))

;; (defun project-persist-drawer-before-open (dir)
;;   (project-persist-drawer--no-adaptor))

;; (defun project-persist-drawer-after-open (dir)
;;   (project-persist-drawer--no-adaptor))

(defun project-persist-drawer--do-open ()
  (let ((project-root project-persist-current-project-root-dir))
    (setq default-directory project-root)
    (project-persist-drawer-before-open project-root)
    (project-persist-drawer-open project-root)
    (project-persist-drawer-after-open project-root)))

(defun project-persist-drawer-on ()
  (eval-after-load 'project-persist
    '(progn
       (add-hook 'project-persist-after-load-hook 'project-persist-drawer--do-open))))

(defun project-persist-drawer-off ()
  (eval-after-load 'project-persist
    '(progn
       (remove-hook 'project-persist-after-load-hook 'project-persist-drawer--do-open))))

(provide 'project-persist-drawer)
