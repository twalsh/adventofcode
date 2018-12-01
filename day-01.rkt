#!/usr/bin/env racket
#lang racket/base
(require "advent-utils.rkt") 

(define lines (apply + (map string->number (read-input "input-01.txt"))))

lines
