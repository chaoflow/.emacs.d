(eval-after-load 'setup-python
  '(progn
     ;; `ac-auto-show-menu': Short timeout because the menu is great.
     (setq ac-auto-show-menu 0.2)

     ;; `ac-quick-help-delay': I'd like to show the menu right with the
     ;; completions, but this value should be greater than
     ;; `ac-auto-show-menu' to show help for the first entry as well.
     (setq ac-use-quick-help t
           ac-quick-help-delay 0.5)))
