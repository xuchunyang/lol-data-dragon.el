;;; lol-data-dragon-tests.el --- Tests for lol-data-dragon.el        -*- lexical-binding: t; -*-

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

;; Tests for lol-data-dragon.el

;;; Code:

(require 'ert)
(require 'lol-data-dragon)

(ert-deftest lol-data-dragon-versions ()
  (should (member "9.15.1" (lol-data-dragon-versions))))

(ert-deftest lol-data-dragon-file-version ()
  (should (= (lol-data-dragon-file-version "Ahri_15.jpg") 15)))

(provide 'lol-data-dragon-tests)
;;; lol-data-dragon-tests.el ends here
