#project-persist-drawer
Use a project drawer with [project-persist](https://github.com/rdallasgray/project-persist).

## Adaptors
At present only one adaptor is available --
[project-persist-drawer-adaptor-sr-speedbar](https://github.com/rdallasgray/project-persist-drawer-adaptor-sr-speedbar),
which uses [sr-speedbar](https://github.com/emacsmirror/sr-speedbar)
to display the project drawer.

An adaptor must implement the following functions:
```elisp
(eval-after-load 'project-persist-drawer
  '(progn
    (defun project-persist-drawer-get-window ()
      "Return the window associated with the project drawer.")

    (defun project-persist-drawer-open (dir)
      "Open the project drawer in DIR.")

    (defun project-persist-drawer-before-open (dir)
      "Function run before the drawer is opened in DIR.")

    (defun project-persist-drawer-after-open (dir)
      "Function run after the drawer is opened in DIR.")))
```

The function declarations should be wrapped in an `eval-after-load` block to ensure project-persist-drawer is loaded first.
