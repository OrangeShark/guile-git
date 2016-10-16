;; The 'nil' configuration applies to all modes.
((nil . ((indent-tabs-mode . nil)
         (tab-width . 2)
         (eval . (progn
                   (put 'with-directory 'scheme-indent-function 1)
                   (put 'with-repository 'scheme-indent-function 1))))))
