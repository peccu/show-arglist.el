show-arglist --- Showing arglist of function at point
=====================================================
「カーソルのある関数がどんな引数を取るか」をヘッダ行(elscreenがスクリーン名を表示してるところ)に表示します．

Install
-------
load-pathに置いたら.emacsとかinit.elにこんな感じで書きます．

  (require 'show-arglist)
  (show-arglist-mode t)

Repository
----------
http://github.com/peccu/show-arglist.el