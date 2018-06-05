(ert-deftest company-nihongo--test-get-candidates-in-current-buffer ()
  ""
  (let ((lst (sort '("の" "の" "の" "の" "のABC" "のdef" "の店舗" "の店長") #'string<)))
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
              (sort '("ノ" "ノ" "ノ" "ノ" "ノうえお" "ノかきく" "ノ南側" "ノ西側") #'string<))))
    ;; prefix is "ト"
    (with-temp-buffer
      (insert "まさかのストレス")
      (should
       (equal (sort (company-nihongo--get-candidates-in-current-buffer "ス") #'string<)
              (sort '("ストレス") #'string<))))))