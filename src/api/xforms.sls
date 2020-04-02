;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: LGPL-2.1-or-later

(library (scforms api xforms)
  (export xforms)
  (import (rnrs base)
          (pffi))

  (define xforms (open-shared-object "libforms.so")))
