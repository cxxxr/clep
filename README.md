# clep
S式のgrep

## 使い方
    ;; sample.lispからシンボルのsetfがある式を探す
    (clep:clep-files '(setf (:lisp symbolp) . (:*))
                     "sample.lisp")

    ;; a.lisp, b.lisp, c.lispから#'(lambda ...)の式を探す
    (clep:clep-files '(function (lambda . (:*)))
                     '("a.lisp" "b.lisp" "c.lisp"))

    ;; 引数が2つのifをfoo.lispとbar.lispから探す
    (clep:clep-files '(if (:*) (:*)) '("foo.lisp" "bar.lisp"))

    ;; 束縛がないletを探す
    (clep:clep-files '(let () . (:*)) (list "a.lisp" "b.lisp" "c.lisp"))

## ライセンス
[MIT](https://github.com/cxxxr/repl/blob/master/LICENSE)
