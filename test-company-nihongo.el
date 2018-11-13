(defun company-nihongo--test-get-test-buffer ()
  (get-buffer-create "*company-nihongo-test*"))

(ert-deftest company-nihongo--test-get-candidates-in-current-buffer$ ()
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
      (should
       (null (company-nihongo--get-candidates-in-current-buffer "・")))
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "キャ")
              '("キャピタル" "キャピタル・ゲイン")))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))
    (with-temp-buffer
      (insert "新・コンピューター")
      (should
       (equal (company-nihongo--get-candidates-in-current-buffer "コ")
              '("コンピューター")))
      (company-nihongo--clear-tables-for-buffer (current-buffer)))))

(ert-deftest company-nihongo--test-split-buffer-string$ ()
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
       (equal ret
              '("新" "・" "用語辞典" "および" "旧" "・" "用語辞典" "
" "キャピタル" "・" "ゲイン" "
"))))
    (with-temp-buffer
      (insert "キャピタル・ゲイン・・コレって連結される？")
      (should
       (equal (sort (company-nihongo--split-buffer-string (current-buffer))
                    #'string<)
              (sort '("キャピタル" "・" "ゲイン" "・・" "コレ" "って" "連結" "される" "？")
                    #'string<))))
    (with-temp-buffer
      (insert "新・コンピューター")
      (should
       (equal (sort (company-nihongo--split-buffer-string (current-buffer)) #'string<)
              (sort '("新" "・" "コンピューター") #'string<))))
    (with-temp-buffer
      (insert "キャピタル・ゲイン")
      (should
       (equal (company-nihongo--split-buffer-string (current-buffer))
              '("キャピタル" "・" "ゲイン"))))
    ))

;; (cl-loop for word in (company-nihongo--get-word-list (current-buffer))
;;          do (message "word=%s" word))

(ert-deftest company-nihongo--test-get-word-list$ ()
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
                "キャピタル" "ゲイン" "キャピタル・ゲイン")
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
            '("キャピタル" "キャピタル・ゲイン" "ゲイン"
              "コレ" "コレって" "って" "って連結" "連結" "連結される" "される"))))
  (with-temp-buffer
    (insert "あれコレそれドレ")
    (should
     (equal (nreverse (company-nihongo--get-word-list (current-buffer)))
            '("あれ" "あれコレ" "コレ" "コレそれ" "それ" "それドレ" "ドレ"))))
  (with-temp-buffer
    (insert "プレミアム・ゴールド・スペシャル・プラチナ・メンバーシップ")
    (should
     (equal (nreverse (company-nihongo--get-word-list (current-buffer)))
            '("プレミアム" "プレミアム・ゴールド" "ゴールド"
              "プレミアム・ゴールド・スペシャル" "スペシャル"
              "プレミアム・ゴールド・スペシャル・プラチナ"
              "プラチナ" "プレミアム・ゴールド・スペシャル・プラチナ・メンバーシップ"
              "メンバーシップ"))))
  )

(ert-deftest company-nihongo--test-make-regexp$ ()
  (cl-flet ((get-regexp (prefix)
                        (cdr (company-nihongo--make-regexp prefix))))
    (should
     (string-match-p (get-regexp "キャピ") "キャピタル・ゲイン・・コレ"))
    (should
     (string-match-p (get-regexp "キャピ") "キャピタル・ゲイン・コレ"))))


(ert-deftest company-nihongo--test-is-3-consective-p$ ()
  (should
   (company-nihongo--is-3-consective-p "アイ"
                                       "うえ"
                                       "yeah"
                                       company-nihongo-separator-regexp))
  (should
   (company-nihongo--is-3-consective-p "トップレベル"
                                       "の"
                                       "heading"
                                       company-nihongo-separator-regexp))
  (should
   (null (company-nihongo--is-3-consective-p "あい"
                                             "うえ"
                                             "yeah"
                                             company-nihongo-separator-regexp)))
  (should
   (null (company-nihongo--is-3-consective-p "・"
                                             "あい"
                                             "ueo"
                                             company-nihongo-separator-regexp))))

(ert-deftest company-nihongo--test-get-word-list-3$ ()
  (with-temp-buffer
    (insert "トップレベルのheading")
    (should
     (equal (nreverse (company-nihongo--get-word-list (current-buffer)))
            '("トップレベル" "トップレベルの" "トップレベルのheading"
              "の" "のheading" "heading")))))

(ert-deftest company-nihongo--test-split-buffer-string-dup-words$ ()
  (with-current-buffer (company-nihongo--test-get-test-buffer)
    (erase-buffer)
    (goto-char (point-min))
    (insert "トップレベルのheading")
    (should
     (equal (company-nihongo--split-buffer-string (current-buffer))
            '("トップレベル" "の" "heading")))))

(ert-deftest company-nihongo--test-get-candidates$ ()
  (with-current-buffer (company-nihongo--test-get-test-buffer)
    (erase-buffer)
    (c-mode)
    (insert "新・コンビネーション")
    (company-nihongo--register-hashtable (current-buffer)))
  (with-temp-buffer
    (c-mode)
    (should
     (null (company-nihongo--get-candidates "・")))
    (should
     (equal
      (company-nihongo--get-candidates-in-buffer "コ"
                                                 (company-nihongo--test-get-test-buffer))
      '("コンビネーション"))))
  (company-nihongo--clear-tables-for-buffer (company-nihongo--test-get-test-buffer)))

(ert-deftest company-nihongo--test--process-katakana-word$ ()
  (should
   (equal (company-nihongo--process-kanakana-word "・・・アイウ・・エ・オ")
          '("・・" "アイウ" "・・" "エ" "・" "オ")))
  (should
   (equal (company-nihongo--process-kanakana-word "・ハロー")
          '("・" "ハロー")))
  (should
   (equal (company-nihongo--process-kanakana-word "・")
          '("・")))
  (should
   (equal (company-nihongo--process-kanakana-word "キャピタル・ゲイン・・コレ")
          '("キャピタル" "・" "ゲイン" "・・" "コレ")))
  (should
   (equal (company-nihongo--process-kanakana-word "・キャピタル・ゲイン・・コレ・・")
          '("・" "キャピタル" "・" "ゲイン" "・・" "コレ" "・・"))))
