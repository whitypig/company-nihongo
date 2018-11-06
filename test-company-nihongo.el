(ert-deftest company-nihongo--test-get-candidates-in-current-buffer ()
  ""
  (let ((lst (sort '("のABC" "のdef" "の店舗" "の店長") #'string<)))
    ;; prefix is "の"
    (with-temp-buffer
      (insert "都心の店舗の店長。")
      (newline)
      (insert "アルファベットのABCのdef.")
      (should
       (equal
        (sort (company-nihongo--get-candidates-in-current-buffer "の") #'string<)
        lst))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))
    ;; prefix is "ノ"
    (with-temp-buffer
      (insert "あいうノうえおノかきく。")
      (newline)
      (insert "これノ南側ノ西側。")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ノ") #'string<)
              (sort '("ノうえお" "ノかきく" "ノ南側" "ノ西側") #'string<)))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))
    ;; prefix is "ト"
    (with-temp-buffer
      (insert "ストレス")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ス") #'string<)
              (sort '("ストレス") #'string<)))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))

    (with-temp-buffer
      (insert "ジェヒョン")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ジ") #'string<)
              (sort '("ジェヒョン") #'string<)))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))

    ;; // たぶん違う。
    ;; // ボードは例えば以下のようになっているはずである。
    ;; // 白の希望は黒に変換されているので、
    ;; // 対角線上の正方形のみが1以上の値を持つはずである。
    (with-temp-buffer
      (insert "ボードは例えば以下のようになっているはずである。")
      (newline)
      (insert "白の希望は黒に変換されているので、")
      (newline)
      (insert "対角線上の正方形のみが1以上の値を持つはずである。")
      (newline)
      (insert "*** 「する。###東京三菱")
      (newline)
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "つ")
              '("つはずである")))
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "さ")
              '("されているので")))
      ;; Should we forbid that "うになっているはずである" is collected
      ;; as a candidate?
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "う")
              '("うになっているはずである")))
      (should
       (null (company-nihongo--get-candidates-in-current-buffer "する")))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))
    (with-temp-buffer
      (insert "キャピタル・ゲイン・・コレって連結される？")
      (newline)
      (should
       (null (company-nihongo--get-candidates-in-current-buffer "・")))
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "キャ")
              '("キャピタル" "キャピタル・ゲイン")))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))))

(ert-deftest company-nihongo--test-split-buffer-string ()
  ""
  (let ((ret nil)
        (expeted '("*** 「" "する" "。###" "東京三菱" "」
**** " "これを" "バッファ" "に" "追加" "して" "、" "確認" "したら" "、「" "東京三菱" "」" "が" "補完" "されてしまった" "。
**** " "この" "状態" "で" "、" "company-nihongo-separator-regexp" "company" "nihongo" "separator" "regexp" "に" "、「#」" "を" "追加" "してみる" "。
**** " "補完" "される" "。
")))
    (with-temp-buffer
      (insert "*** 「する。###東京三菱」
**** これをバッファに追加して、確認したら、「東京三菱」が補完されてしまった。
**** この状態で、company-nihongo-separator-regexpに、「#」を追加してみる。
**** 補完される。
")
      (setq ret (company-nihongo--split-buffer-string (current-buffer)))
      (should
       (eq (length ret) (length expeted)))
      (should
       (equal ret expeted)))
    (with-temp-buffer
      (insert "新・用語辞典および旧・用語辞典")
      (newline)
      (insert "キャピタル・ゲイン")
      (newline)
      (setq ret (company-nihongo--split-buffer-string (current-buffer)))
      (should
       (equal
        ret
        '("新" "・" "用語辞典" "および" "旧" "・" "用語辞典" "
" "キャピタル・ゲイン" "キャピタル" "ゲイン" "
"))))
    (with-temp-buffer
      (insert "キャピタル・ゲイン・・コレって連結される？")
      (should
       (equal (sort (company-nihongo--split-buffer-string (current-buffer))
                    #'string<)
              (sort '("キャピタル" "ゲイン" "キャピタル・ゲイン"
                      "コレ" "って" "連結" "される" "？")
                    #'string<))))))

;; (cl-loop for word in (company-nihongo--get-word-list (current-buffer))
;;          do (message "word=%s" word))

(ert-deftest company-nihongo--test-get-word-list ()
  (with-temp-buffer
      (insert "新・用語辞典および旧・用語辞典")
      (newline)
      (insert "キャピタル・ゲイン")
      (newline)
      (setq ret (company-nihongo--get-word-list (current-buffer)))
      (should
       (equal
        (sort ret #'string<)
        (sort '("新" "用語辞典" "用語辞典および" "および" "および旧" "旧" "用語辞典"
                "キャピタル・ゲイン" "キャピタル" "ゲイン")
              #'string<))))
  (with-temp-buffer
    (insert "あれ・・コレって連結される？")
    (should
     (equal (sort (company-nihongo--get-word-list (current-buffer))
                  #'string<)
            (sort '("あれ" "コレ" "って" "コレって" "連結" "って連結" "される" "連結される")
                  #'string<))))
  (with-temp-buffer
    (insert "キャピタル・ゲイン・・コレって連結される？")
    (should
     (equal (nreverse (company-nihongo--get-word-list (current-buffer)))
            '("キャピタル・ゲイン" "キャピタル" "ゲイン"
                        "コレ" "コレって" "って" "って連結" "連結" "連結される" "される")))))

(ert-deftest company-nihongo--test-make-regexp ()
  (cl-flet ((get-regexp (prefix)
                        (cdr (company-nihongo--make-regexp prefix))))
    (should
     (string-match-p (get-regexp "キャピ") "キャピタル・ゲイン・・コレ"))
    (should
     (string-match-p (get-regexp "キャピ") "キャピタル・ゲイン・コレ"))))