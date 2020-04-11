;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: LGPL-2.1-or-later

;;; Rewrite of XBasic.h of XForms v1.4

(library (scforms api xbasic)
  (export)
  (import (rnrs)
          (for (pffi) run expand)
          (scforms api xforms)
          (scforms api basic)
          (scforms misc)
          (scforms define))

  ;; HACK: Type of XID not entirely clear?
  (defines
    (window unsigned-int)
    (colormap unsigned-int)
    (gc unsigned-int)
    (pixmap pointer))

  (defines
    (fl-xor #x6)
    (fl-copy #x3)
    (fl-and #x1))

  (define fl-mindepth 1)

  (defines
    (fl-illegalvisual -1)
    (fl-staticgray 0)
    (fl-grayscale 1)
    (fl-staticcolor 2)
    (fl-pseudocolor 3)
    (fl-truecolor 4)
    (fl-directcolor 5)
    (fl-defaultvisual 10))

  (defines
    (fl-northwest 1)
    (fl-north 2)
    (fl-northeast 3)
    (fl-west 4)
    (fl-east 6)
    (fl-southwest 7)
    (fl-south 8)
    (fl-southeast 9)
    (fl-nogravity 10)
    (fl-forgetgravity 10))

  (define (fl-is-gray v)
    (c-if (or (= v fl-grayscale)
              (= v fl-staticgray))))
  (define (fl-is-rgb v)
    (c-if (or (= v fl-truecolor)
              (= v fl-directcolor))))

  (define fl-max-colors 1024)

  (define-foreign-struct fl-state-t ; Renamed to avoid clash
    (fields
     (pointer xvinfo)
     (pointer cur-fnt)
     (colormap colormap)
     (window trailblazer)
     (int vclass)
     (int depth)
     (int rgb-bits)
     (int dithered)
     (int pcm)
     (pointer gc)
     (pointer textgc)
     (gc dimmedgc)
     (pointer lut)
     (unsigned-int rshift)
     (unsigned-int rmask)
     (unsigned-int rbits)
     (unsigned-int gshift)
     (unsigned-int gmask)
     (unsigned-int gbits)
     (unsigned-int bshift)
     (unsigned-int bmask)
     (unsigned-int bbits)))

  (define-foreign-variable xforms
    pointer fl_display)
  (define-foreign-variable xforms
    int fl_screen)
  (define-foreign-variable xforms
    window fl_root)
  (define-foreign-variable xforms
    window fl_vroot)
  (define-foreign-variable xforms
    int fl_scrh)
  (define-foreign-variable xforms
    int fl_scrw)
  (define-foreign-variable xforms
    int fl_vmode)

  ;; TODO: fl_visual fl_colormap fl_get_visual fl_get_colormap
  ;;   fl_get_gc fl_default_window

  (define-foreign-variable xforms
    pointer fl_state)
  (define-foreign-variable xforms
    pointer fl_ul_magic_char)
  (define-foreign-variable xforms
    double fl_dpi)

  (define-foreign-procedure xforms
    int fl_mode_capable (int int))

  (define-foreign-struct fl-pixmap
    (fields
     (pixmap pixmap)
     (window win)
     (pointer visual)
     (fl-coord x)
     (fl-coord y)
     (fl-coord z)
     (fl-coord w)
     (fl-coord h)
     (int depth)
     (fl-color dbl-background)
     (fl-color pixel)))

  (defines
    (fl-max-fontsizes 10)
    (fl-max-fontname-length 80))

  (define-foreign-struct fl-font
    (fields
     (pointer fs)
     (pointer size)
     (short nsize)
     (pointer fname)))

  (defines
    (fl-point pointer)
    (fl-rect pointer))

  (define-foreign-procedure xforms
    void fl_rectangle (int fl-coord fl-coord fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_rectbound (fl-coord fl-coord fl-coord fl-coord fl-color))
  (define (fl-rectf x y w h c)
    (fl-rectangle 1 x y w h c))
  (define (fl-rect x y w h c)
    (fl-rectangle 0 x y w h c))

  (define-foreign-procedure xforms
    void fl_roundrectangle (int fl-coord fl-coord fl-coord fl-coord fl-color))
  (define (fl-roundrectf x y w h c)
    (fl-roundrectangle 1 x y w h c))
  (define (fl-roundrect x y w h c)
    (fl-roundrectangle 0 x y w h c))

  (define-foreign-procedure xforms
    void fl_polygon (int pointer int fl-color))
  (define (fl-polyf p n c)
    (fl-polygon 1 p n c))
  (define (fl-poly p n c)
    (fl-polygon 0 p n c))
  (define (fl-polybound p n c)
    (fl-polygon 1 p n c)
    (fl-polygon 0 p n fl-black))

  (define-foreign-procedure xforms
    void fl_lines (pointer int fl-color))
  (define-foreign-procedure xforms
    void fl_line (fl-coord fl-coord fl-coord fl-coord fl-color))

  (define-foreign-procedure xforms
    void fl_point (fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_points (pointer int fl-color))

  (define-foreign-procedure xforms
    void fl_dashedlinestyle (pointer int))

  (define-foreign-procedure xforms
    void fl_update_display (int))

  (define (fl-diagline x y w h c)
    (fl-line x y (sub1 (+ x w)) (sub1 (+ y h)) c))

  (defines
    (fl-solid 0)
    (fl-userdash 1)
    (fl-userdoubledash 2)
    (fl-dot 3)
    (fl-dotdash 4)
    (fl-dash 5)
    (fl-longdah 6))

  (define-foreign-procedure xforms
    void fl_linewidth (int))
  (define-foreign-procedure xforms
    void fl_linestyle (int))
  (define-foreign-procedure xforms
    void fl_drawmode (int))

  (define-foreign-procedure xforms
    int fl_get_linewidth (void))
  (define-foreign-procedure xforms
    int fl_get_linestyle (int))
  (define-foreign-procedure xforms
    int fl_get_drawmode (void))

  (defines
    (fl-set-linewidth fl-linewidth)
    (fl-set-linestyle fl-linestyle)
    (fl-set-drawmode fl-drawmode))

  (define-foreign-procedure xforms
    void fl_circ (fl-coord fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_circf (fl-coord fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_circbound (fl-coord fl-coord fl-coord fl-color))

  (define-foreign-procedure xforms
    void fl_ovalbound (int fl-coord fl-coord fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_ovalbound (fl-coord fl-coord fl-coord fl-coord fl-color))
  (define-foreign-procedure xforms
    void fl_ovalarc (int fl-coord fl-coord fl-coord fl-coord int int fl-color))
  (define (fl-ovalf x y w h c)
    (fl-oval 1 x y w h c))
  (define (fl-ovall x y w h c)
    (fl-oval 0 x y w h c))

  ;; To be continued
  )
