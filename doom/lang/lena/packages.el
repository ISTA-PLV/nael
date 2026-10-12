;; -*- no-byte-compile: t; -*-
;;; lang/lena/packages.el

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3 in the Lena repository.

(package! lena
  :recipe (:host github :repo "ISTA-PLV/nael"
           :files ("lena/*.el" "lena/lena.info" "lena/lena.texi"
                   "lena/NOTICE" "lena/LICENSE*")))

(when (modulep! :editor evil)
  (package! lena-evil
    :recipe (:host github :repo "ISTA-PLV/nael"
             :files ("lena-evil/*.el" "lena-evil/LICENSE"))))

