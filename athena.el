(require 'cl-lib)
(require 's)

(setq latin-to-greek-lookup '(("a" . alpha)
                              ("b" . beta)
                              ("g" . gamma)
                              ("d" . delta)
                              ("e" . epsilon)
                              ("z" . zeta)
                              ("h" . eta)
                              ("q" . theta)
                              ("i" . iota)
                              ("k" . kappa)
                              ("l" . lambda)
                              ("m" . mu)
                              ("n" . nu)
                              ("x" . xi)
                              ("o" . omicron)
                              ("p" . pi)
                              ("r" . rho)
                              ("s" . sigma)
                              ("t" . tau)
                              ("u" . upsilon)
                              ("f" . phi)
                              ("c" . chi)
                              ("y" . psi)
                              ("w" . omega)))

(defun get-case (letter)
  (if (equal letter (downcase letter)) 'small 'capital))

;; base - the base Greek letter name as a symbol
;; breathing - 'smooth, 'rough or nil
;; accent - 'acute, 'grave, or 'circumflex
;; diaeresis - t or nil
;; example call - (get-character 'alpha :breathing 'smooth :accent 'acute)
;; (cl-defun get-greek-letter (name case &key (breathing nil) (accent nil) (diearesis nil) ()))

(defun get-unicode-name (letter-name case)
  (format "GREEK %s LETTER %s"
          (if (eq case 'capital)
              "CAPITAL"
            "SMALL")
          (upcase (symbol-name letter-name))))

;; smooth breathing - PSILI
;; rough breathing - DASIA
;; grave - VARIA
;; acute - OXIA
;; circumflex - PERISPOMENI
;; iota - YPOGEGRAMMENI
;;      - PROSGEGRAMMENI if capital wtf...
(cl-defun diacritics-string
    (letter-case &key
                 (breathing nil)
                 (accent nil)
                 (diaeresis nil)
                 (iota-subscriptum nil))
  (setq diacritics (list (cond
                          ((eq breathing 'smooth) "PSILI")
                          ((eq breathing 'rough) "DASIA"))
                         (when diaeresis "DIALYTIKA")
                         (cond
                          ((eq accent 'acute) "OXIA")
                          ((eq accent 'grave) "VARIA")
                          ((eq accent 'circumflex) "PERISPOMENI"))
                         (when iota-subscriptum
                           (if (eq letter-case 'small)
                               "YPOGEGRAMMENI"
                             "PROSGEGRAMMENI"))))
  (string-join (remove nil diacritics)
               " AND "))

(cl-defun get-greek-letter-name
    (letter-case letter-name
                 &key
                 (breathing nil)
                 (accent nil)
                 (diaeresis nil)
                 iota-subscriptum
                 nil)
  (if (not (or breathing accent diaeresis iota-subscriptum))
      (get-unicode-name letter-name letter-case)
    (format "%s WITH %s"
            (get-unicode-name letter-name letter-case)
            (diacritics-string letter-case :breathing breathing
                               :accent accent
                               :diaeresis diaeresis
                               :iota-subscriptum iota-subscriptum))))

(setq holy-regex "[aeiouyhwbgdzqklmnxprstfc][\\(\\)]?\\+?[\\/\\\]?j?")

(defun parse (string)
  (mapcar (lambda (group)
            (let* ((first-char (substring group 0 1))
                   (letter-case (get-case first-char))
                   (greek-letter (cdr (assoc first-char latin-to-greek-lookup)))
                   (letter-properties (list letter-case greek-letter)))
              (progn
                (when (s-contains-p ")" group)
                  (setq letter-properties (plist-put letter-properties :breathing 'smooth)))
                (when (s-contains-p "(" group)
                  (setq letter-properties (plist-put letter-properties :breathing 'rough)))
                (when (s-contains-p "+" group)
                  (setq letter-properties (plist-put letter-properties :diaeresis t)))
                (when (s-contains-p "/" group)
                  (setq letter-properties (plist-put letter-properties :accent 'acute)))
                (when (s-contains-p "\\" group)
                  (setq letter-properties (plist-put letter-properties :accent 'grave)))
                (when (s-contains-p "^" group)
                  (setq letter-properties (plist-put letter-properties :accent 'circumflex)))
                (when (s-contains-p "j" group)
                  (setq letter-properties (plist-put letter-properties :iota-subscriptum t)))
                letter-properties)))
          (s-slice-at holy-regex string)))

(defun hellenize-word (string)
  (s-join ""
          (mapcar (lambda (properties)
                    (char-to-string (char-from-name (apply #'get-greek-letter-name properties))))
                  (parse string))))
 
(defun hellenize (string)
  (s-join " " (let ((words (s-split " " string)))
                (mapcar #'hellenize-word words))))


(defun hellenize-region ()
  (interactive)
  (let* ((region (buffer-substring (region-beginning) (region-end)))
         (hellenized (hellenize region)))
    (replace-region-contents (region-beginning) (region-end) (lambda () (hellenize region)))))
