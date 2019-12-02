(ns aoc2019.day1)

;; Day 1 Part I

(def modules-masses
  [121656
   110933
   80850
   137398
   76307
   50450
   124691
   86449
   145386
   148648
   68909
   134697
   109636
   115718
   134485
   89267
   64829
   109070
   84257
   109010
   97574
   98363
   123029
   105568
   114500
   92041
   128869
   148350
   144605
   91862
   134417
   54710
   147843
   121914
   127855
   74545
   89596
   106562
   69863
   147082
   135724
   111637
   68869
   103685
   99453
   80908
   136020
   64974
   125159
   87504
   62499
   73294
   128811
   121567
   54673
   66647
   66871
   71228
   101622
   130675
   69025
   146118
   79970
   118267
   122279
   89523
   62965
   148036
   119625
   127056
   54980
   143581
   103274
   83064
   125131
   54362
   115851
   139103
   140674
   69616
   81353
   116441
   73898
   51403
   137019
   93146
   67273
   138182
   126680
   148683
   127805
   111741
   102219
   99603
   90453
   147581
   102136
   109913
   144899
   140572])

(defn fuel-req [mass]
  (let [res (-> mass (quot 3)  (- 2))]
    (if (neg? res)
      0
      res)))

(defn add-mass-fuel-req-to-total [total mass] (-> mass fuel-req (+ total)))

(defn modules-mass-total-fuel-req [modules-masses]
  (reduce add-mass-fuel-req-to-total 0 modules-masses))

(modules-mass-total-fuel-req modules-masses)

;; Day 1 Part II

(defn add-mass-fuel-req-to-total*
  [total mass]
  (let [additional-fuel-req (fuel-req mass)]
    (if (zero? additional-fuel-req)
      total
      (add-mass-fuel-req-to-total* (+ total additional-fuel-req)
                                   additional-fuel-req))))

(defn total-fuel-req
  [modules-masses]
  (reduce add-mass-fuel-req-to-total* 0 modules-masses))

(total-fuel-req modules-masses)