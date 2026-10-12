;; -*- no-byte-compile: t; -*-
;;; lang/nael/packages.el

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3 in the Nael repository.

(package! nael
  :recipe (:host github :repo "ISTA-PLV/nael"
           :files ("nael/*.el" "nael/nael.info" "nael/nael.texi"
                   "nael/NOTICE" "nael/LICENSE*")))

