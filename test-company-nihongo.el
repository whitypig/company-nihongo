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
        lst)))
    ;; prefix is "ノ"
    (with-temp-buffer
      (insert "あいうノうえおノかきく。")
      (newline)
      (insert "これノ南側ノ西側。")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ノ") #'string<)
              (sort '("ノうえお" "ノかきく" "ノ南側" "ノ西側") #'string<))))
    ;; prefix is "ト"
    (with-temp-buffer
      (insert "ストレス")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ス") #'string<)
              (sort '("ストレス") #'string<))))

    (with-temp-buffer
      (insert "ジェヒョン")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ジ") #'string<)
              (sort '("ジェヒョン") #'string<))))

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
              '("うになっているはずである"))))))