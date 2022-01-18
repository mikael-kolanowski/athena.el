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
  (if (equal letter (downcase letter))
      'small
    'capital))

;; base - the base Greek letter name as a symbol
;; breathing - 'smooth, 'rough or nil
;; accent - 'acute, 'grave, or 'circumflex
;; diaeresis - t or nil
;; example call - (get-character 'alpha :breathing 'smooth :accent 'acute)
;; (cl-defun get-greek-letter (name case &key (breathing nil) (accent nil) (diearesis nil) ()))


;; smooth breathing - PSILI
;; rough breathing - DASIA
;; grave - VARIA
;; acute - OXIA
;; circumflex - PERISPOMENI
;; iota - YPOGEGRAMMENI
;;      - PROSGEGRAMMENI if capital wtf...
(cl-defun unicode-name
    (letter-case letter-name
                 &key
                 (breathing nil)
                 (accent nil)
                 (diaeresis nil)
                 (final nil)
                 (iota-subscriptum nil))
  "Get the Unicode name of the Greek character corresponding to the provided properties"
  (let ((has-diacritics (or breathing accent diaeresis iota-subscriptum))
        (is-final-sigma (and (eq letter-name 'sigma)
                             (not (eq letter-case 'capital))
                             final))
        (diacritics (s-join " AND "
                            (remove nil
                                    (list (case breathing
                                            (smooth "PSILI")
                                            (rough "DASIA"))
                                          (case accent
                                            (acute "OXIA")
                                            (grave "VARIA")
                                            (circumflex "PERISPOMENI"))
                                          (when iota-subscriptum
                                            (if (eq letter-case 'small)
                                                "YPOGEGRAMMENI"
                                              "PROSGEGRAMMENI")))))))
    (cond
     (is-final-sigma "GREEK SMALL LETTER FINAL SIGMA")
     ((not has-diacritics)
      (format "GREEK %s LETTER %s"
              (upcase (symbol-name letter-case))
              (upcase (symbol-name letter-name))))
     (t (format "GREEK %s LETTER %s WITH %s"
                (upcase (symbol-name letter-case))
                (upcase (symbol-name letter-name))
                diacritics)))))

(setq holy-regex "[aeiouyhwbgdzqklmnxprstfcAEIOUYHWBGDZQKLMNXPRSTFC][\\(\\)]?\\+?[\\/\\\]?j?")

(defun parse-word (string)
  (let ((splitting-regex (rx "aeiouyhwbgdzqklmnxprstfc")))
    (mapcar (lambda (group)
              (let* ((first-char (substring group 0 1))
                     (letter-case (get-case first-char))
                     (greek-letter (cdr (assoc (downcase first-char) latin-to-greek-lookup)))
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
            (s-slice-at holy-regex string))))

(defun parse (string)
  (let ((words (s-split "\s+" string)))
    (->> words
         (-map #'parse-word)
         ;; Set the FINAL key if the last letter in a word is sigma
         (--map (--map-last (eq (elt it 1) 'sigma)
                            (plist-put it :final t)
                            it)))))

(defun hellenize (string)
  (->> string
       parse
       (-map (lambda (word)
               (->> word
                    (--map (apply #'unicode-name it))
                    (-map #'char-from-name)
                    (-map #'char-to-string)
                    (s-join ""))))
       (s-join " ")))

(defun hellenize-region (start end)
  (interactive "r")
  (let ((substring (buffer-substring start end)))
    (replace-region-contents start
                             end
                             (lambda ()
                               (hellenize substring)))))

(defun insert-polytonic-greek-text (string)
  (interactive "sEnter text to transliterate: ")
  (insert (hellenize string)))
