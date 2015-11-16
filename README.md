# clep
S式のgrep

## 使い方
    ;; sample.lispからシンボルのsetfがある式を探す
    (clep:clep-file '(setf (:lisp symbolp) . (:*))
               "sample.lisp")

    ;; カレントディレクトリの*.lispのファイルから
    ;; #'(lambda ...)の式を探す
    (clep:clep-file '(function (lambda . (:*))))

    ;; 引数が2つのifをfoo.lispとbar.lispから探す
    (clep:clep-file '(if (:*) (:*)) "foo.lisp" "bar.lisp")

    ;; 束縛がないletを探す
    ;; 2つ目以降のファイルを指定する引数はリストにも対応
    (clep:clep-file '(let () . (:*)) (list "a.lisp" "b.lisp" "c.lisp"))

## ライセンス
[MIT](https://github.com/cxxxr/repl/blob/master/LICENSE)
