;;; ddragon-tests.el --- Tests for ddragon.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang

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

(require 'ert)
(require 'ddragon)

(ert-deftest ddragon-versions ()
  (should (member "9.15.1" (ddragon-versions))))

(ert-deftest ddragon-file-version ()
  (should (= (ddragon-file-version "Ahri_15.jpg") 15)))

(ert-deftest ddragon-file-sort ()
  (should (equal (ddragon-file-sort '("Ahri_1.jpg" "Ahri_14.jpg" "Ahri_2.jpg"))
                 '("Ahri_1.jpg" "Ahri_2.jpg" "Ahri_14.jpg"))))

(provide 'ddragon-tests)
;;; ddragon-tests.el ends here
