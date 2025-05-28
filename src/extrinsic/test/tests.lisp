;; tests.lisp
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

(in-package #:elisp-format-extrinsic-test)

;; (def-test-group |b-directive|
;;   (deftests invistra-emacs-lisp-extrinsic:format
;;     "%b %b"      2  3   => "10 11"
;;     "%2$b %1$b"  3  2   => "11 10"
;;     "%3b %3b"    2  3   => " 10  11"
;;     "%3b %-3b"   2  3   => " 10 11 "
;;     "%+b %b"     2  3   => "+10 11"
;;     "%+b %+b"    2 -3   => "+10 -11"
;;     "%03b %-03b" 2  3   => "0010 11  "))

(def-test-group |c-directive|
    (deftests elisp-format-extrinsic:format
      "%c"      #\a => "a"
      "%c"       97 => "a"
      "%2c"      97 => " a"
      "%-2c"     97 => "a "
      "%+#02.2c" 97 => " a"
      "%c%c%c" 1 2 3 => ""
      "%1$c %2$2c"  #\a #\b => "a  b"
      "%2$c %1$-2c" #\a #\b => "b a "))

(def-test-group |d-directive|
    (deftests elisp-format-extrinsic:format
      "%#d"        0      => "0"
      "%-#.4d"     1      => "0001"
      "%#08.4d"    1      => "00000001"
      "%#8.4d"     1      => "    0001"
      "%-#8.4d"    1      => "0001    "
      "%d %d"      1  2   => "1 2"
      "%d %d"     0.1 1.9 => "0 1"
      "%2$d %1$d"  1  2   => "2 1"
      "%2d %2d"    1  2   => " 1  2"
      "%2d %-2d"   1  2   => " 1 2 "
      "%+d %d"     1  2   => "+1 2"
      "%+d %+d"    1 -2   => "+1 -2"
      "%03d %-03d" 1  2   => "001 2  "))

(def-test-group |o-directive|
    (deftests elisp-format-extrinsic:format
      "%#o"        0      => "0"
      "%#o"        1      => "01"
      "%#.o"       0      => "0"
      "%#.o"       1      => "01"
      "%#4.4o"     0      => "0000"
      "%o %o"      1  2   => "1 2"
      "%o %o"     0.1 1.9 => "0 1"
      "%2$o %1$o"  1  2   => "2 1"
      "%2o %2o"    1  2   => " 1  2"
      "%2o %-2o"   1  2   => " 1 2 "
      "%+o %o"     1  2   => "+1 2"
      "%+o %+o"    1 -2   => "+1 -2"
      "%.4o"       1      => "0001"
      "%#4.4o"     1      => "0001"
      "%03o %-03o" 1  2   => "001 2  "))

(def-test-group |x-directive|
    (deftests elisp-format-extrinsic:format
      "%#x"           0     => "0"
      "%#4.4x"        0     => "0000"
      "%x %x"        10  11 => "a b"
      "%2$x %1$x"    10  11 => "b a"
      "%2x %2x"      10  11 => " a  b"
      "%2x %-2x"     10  11 => " a b "
      "%+x %x"       10  11 => "+a b"
      "%+x %+x"      10 -11 => "+a -b"
      "%03x %-03x"   10  11 => "00a b  "
      "%+#x %#+x"    10 -11 => "+0xa -0xb"
      "%0#3x %#-03x" 10  11 => "0xa 0xb"
      "%.4x"          1     => "0001"
      "%#4.4x"        1     => "0x0001"
      "%x"      (expt 16 8) => "100000000"))

(def-test-group |X-directive|
    (deftests elisp-format-extrinsic:format
      "%#X"           0     => "0"
      "%#4.4X"        0     => "0000"
      "%X %X"        10  11 => "A B"
      "%2$X %1$X"    10  11 => "B A"
      "%2X %2X"      10  11 => " A  B"
      "%2X %-2X"     10  11 => " A B "
      "%+X %X"       10  11 => "+A B"
      "%+X %+X"      10 -11 => "+A -B"
      "%03X %-03X"   10  11 => "00A B  "
      "%+#X %#+X"    10 -11 => "+0XA -0XB"
      "%0#3X %#-03X" 10  11 => "0XA 0XB"
      "%.4X"         10     => "000A"
      "%#4.4X"       10     => "0X000A"
      "%X"      (expt 16 8) => "100000000"))

(def-test-group |f-directive|
    (deftests elisp-format-extrinsic:format
      "%f %f"       1        0          => "1.000000 0.000000"
      "%2$f %1$f" 0.1        1.9        => "1.900000 0.100000"
      "%2$f %1$f" 1111111.1  1111111.9  => "1111111.900000 1111111.100000"
      "%2$f %1$f" 11111111.1 11111111.9 => "11111111.900000 11111111.100000"
      "%12f %12f"   1        2          => "    1.000000     2.000000"
      "%012f %-012f"1        2          => "00001.000000 2.000000    "
      "%+f %+f"     1        2          => "+1.000000 +2.000000"
      "%+f %+f"    -1       -2          => "-1.000000 -2.000000"
      "%+.f %+.0f"  1       -2          => "+1 -2"
      "%+.2f %+.3f" 1       -2          => "+1.00 -2.000"))

(def-test-group |e-directive|
    (deftests elisp-format-extrinsic:format
      "%e %e"       1        0          => "1.000000e+00 0.000000e+00"
      "%2$e %1$e" 0.1        1.9        => "1.900000e+00 1.000000e-01"
      "%2$e %1$e" 1111111.1  1111111.9  => "1.111112e+06 1.111111e+06"
      "%2$e %1$e" 11111111.1 11111111.9 => "1.111111e+07 1.111111e+07"
      "%12e %12e"   1        2          => "1.000000e+00 2.000000e+00"
      "%012e %-012e"1        2          => "1.000000e+00 2.000000e+00"
      "%+e %+e"     1        2          => "+1.000000e+00 +2.000000e+00"
      "%+e %+e"    -1       -2          => "-1.000000e+00 -2.000000e+00"
      "%e"          123456.9            => "1.234569e+05"
      "%#e"         123456.9            => "1.234569e+05"
      "%.e"         123456.9            => "1e+05"
      "%#.e"        123456.9            => "1.e+05"
      "%.0e"        123456.9            => "1e+05"
      "%.1e"        123456.9            => "1.2e+05"
      "%.2e"        123456.9            => "1.23e+05"
      "%.3e"        123456.9            => "1.235e+05"
      "%.6e"        123456.9            => "1.234569e+05"
      "%.7e"        123456.9            => "1.2345690e+05"
      "%.8e"        123456.9            => "1.23456900e+05"))

(def-test-group |g-directive|
    (deftests elisp-format-extrinsic:format
      "%g %g"       1        0          => "1 0"
      "%2$g %1$g" 0.1        1.9        => "1.9 0.1"
      "%12g %12g"   1        2          => "           1            2"
      "%012g %-012g"1        2          => "000000000001 2           "
      "%+g %+g"     1        2          => "+1 +2"
      "%+g %+g"    -1       -2          => "-1 -2"
      "%g"          123456              => "123456"
      "%g"          1.234567            => "1.23457"
      "%g"          12.1234567          => "12.1235"
      "%g"          123456.1234567      => "123456"
      "%g"          123456.9            => "123457"
      "%g"          0.999999            => "0.999999"
      "%g"          0.9999999           => "1"
      "%g"          1.999999            => "2"
      "%g"          9.999999            => "10"
      "%g"         99.999999            => "100"
      "%g"      99999.999999            => "100000"
      "%g"     999999.999999            => "1e+06"
      "%g"    9999999.9999999           => "1e+07"
      "%.g"         123456.9            => "1e+05"
      "%.0g"        123456.9            => "1e+05"
      "%.1g"        123456.9            => "1e+05"
      "%.2g"        123456.9            => "1.2e+05"
      "%.3g"        123456.9            => "1.23e+05"
      "%.6g"        123456.9            => "123457"
      "%.7g"        123456.9            => "123456.9"
      "%.8g"        123456.9            => "123456.9"
      "%g"          1234567             => "1.23457e+06"
      "%2$g %1$g" 1111111.1  1111111.9  => "1.11111e+06 1.11111e+06"
      "%2$g %1$g" 11111111.1 11111111.9 => "1.11111e+07 1.11111e+07"))

(def-test-group |s-directive|
    (deftests elisp-format-extrinsic:format
      "%s"      #\a => "97"
      "%.0s"     11 => ""
      "%.1s"     11 => "1"
      "%.2s"     11 => "11"
      "%.3s"     11 => "11"
      "%s"       97 => "97"
      "%2s"      97 => "97"
      "%-2s"     97 => "97"
      "%3s"      97 => " 97"
      "%-3s"     97 => "97 "
      "%+#02.2s" 97 => "97"
      "%s%s%s" 1 2 3 => "123"
      "%S" "hello"   => "\"hello\""
      "%1$s %2$2s"  #\a #\b => "97 98"
      "%2$s %1$-2s" #\a #\b => "98 97"))

(def-test-group |S-directive|
    (deftests elisp-format-extrinsic:format
      "%S"      #\a => "97"
      "%S"       97 => "97"
      "%S"     1.00 => "1.0"
      ;;"%S"    .0001 => "0.0001" <-- not impl<
      "%S" "hello" => "\"hello\""
      "%1$S %2$S"   #\a #\b => "97 98"
      "%2$-4S %1$4S" #\a #\b => "98     97"))

;;; tests.lisp ends here
