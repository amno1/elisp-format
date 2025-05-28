;; packages.cl
;; Copyright (C) 2025  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(uiop:define-package #:elisp-format-extrinsic-test
  (:use #:common-lisp
        #:parachute
        #:elisp-format-extrinsic)
  (:shadowing-import-from #:elisp-format-extrinsic
                          #:format #:formatter)
  (:export))

;;; packages.cl ends here
