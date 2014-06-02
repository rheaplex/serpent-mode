;;; dev-serpent-mode.el - Code to generate regular expressions for serpent-mode
;;
;; Copyright (C) 2014 Rob Myers <rob@robmyers.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keywords

(concat "\\<"
	(regexp-opt '("code" "else" "elif" "if" "init" "return" "stop" "suicide" "while"))
	"\\>")

;; Variables

(concat "\\<"
	(regexp-opt '("block.prevhash" "block.number" "block.timestamp"
                  "block.difficulty" "block.coinbase" "block.gaslimit"
                  "tx.gasprice" "tx.origin" "tx.gas" "msg.datasize"
                  "msg.sender" "msg.value" "contract.balance" "msg.data"
                  "contract.storage"))
	"\\>")

;; Functions

(concat "\\<\\(" 
        (regexp-opt '("array" "byte" "bytes" "create" "getch" "msg"
                      "send" "setch" "sha3"))
	"\\)\\s-*(")
