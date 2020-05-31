(ns proto.descryptors.samples)



(def used-by
  ["Microsoft"
   "Kekfield Enterprise"
   "Bionic SRL"
   "Amazon"
   "SpaceX"])



(def team
  [["Whitehouse" #{:founder}]
   ["Andrei Bolotov" #{:founder}]
   ["Ingh Huiangh Wu"]
   ["George Michael"]
   ["Sliserin Clikford"]
   ["Sauron Del Piero"]
   ["Satoshi"]])




(def wallets
  [["BitBox"   "https://google.com"]
   ["BRD"      "https://google.com"]
   ["Edge"     "https://google.com"]
   ["Electrum" "https://google.com"]
   ["KeepKey"  "https://google.com"]
   ["Ledger Nano S" "https://google.com"]
   ["Trezor"   "https://google.com"]
   ["Armory"   "https://google.com"]
   ["Wasabi"   "https://google.com"]
   ["Coin.Space" "https://google.com"]])




(def exchanges
  [{:name "EXMO" :pair "BTC/USD" :vol24 "2300"
    :price 13.33 :vol-percent "12%" :category "--"
    :fee "0.1%" :updated "23h ago"}

   {:name "Coinbase" :pair "BTC/USD" :vol24 "2300"
    :price 13.33 :vol-percent "12%" :category "--"
    :fee "0.1%" :updated "23h ago"}

   {:name "Bitfinex" :pair "BTC/USD" :vol24 "2300"
    :price 13.33 :vol-percent "12%" :category "--"
    :fee "0.1%" :updated "23h ago"}

   {:name "Binance" :pair "ETH/BTC" :vol24 "3456"
    :price 24.40 :vol-percent "12%" :category "--"
    :fee "1.3%" :updated "23h ago"}

   {:name "Kraken" :pair "ETH/BTC" :vol24 "3456"
    :price 24.40 :vol-percent "12%" :category "--"
    :fee "1.3%" :updated "23h ago"}

   {:name "Boing" :pair "MNR/USD" :vol24 "343"
    :price 24.40 :vol-percent "12%" :category "--"
    :fee "1.3%" :updated "23h ago"}])





(def tweets
  [{:score 320 :num-comments 20
    :title (str "I really hope that this new technology will bring peace to the world. "
                "In case this doesn't happen, we're doomed!")
    :subtitle "@rippleman"}

   {:score 3110 :num-comments 2
    :title (str "Is EOS that good? Should we all invest in it? "
                "Check this new article on SlashDot.org.")
    :subtitle "@Cryptopia"}

   {:score 10 :num-comments 1
    :title "Bitcoin bitcoin bitcoin... Bitcoin bitcoin. And so on..."
    :subtitle "@HiddenUser"}

   {:score 320 :num-comments 20
    :title (str "I really hope that this new technology will bring peace to the world. "
                "In case this doesn't happen, we're doomed!...")
    :subtitle "@rippleman"}

   {:score 3110 :num-comments 2
    :title (str "Is EOS that good? Should we all invest in it? "
                "Check this new article on SlashDot.org. yep")
    :subtitle "@Cryptopia"}

   {:score 10 :num-comments 1
    :title "Bitcoin bitcoin bitcoin... Bitcoin bitcoin. And so on... Alright."
    :subtitle "@HiddenUser"}])




(def reddit-news
  [{:score 301 :num-comments 420
    :title "Bitcoin has been hacked."
    :subtitle "posted by u/satoshi"}

   {:score 23 :num-comments 100
    :title "EOS declared that gold is worthless."
    :subtitle "posted by u/rich_and_rich"}

   {:score 13 :num-comments 120
    :title "EOS declared that gold is worthless. True."
    :subtitle "posted by u/rich_and_rich"}

   {:score 33 :num-comments 150
    :title "EOS declared that gold is worthless. False."
    :subtitle "posted by u/rich_and_rich"}

   {:score 1022 :num-comments 500
    :title "Stellar released a new client."
    :subtitle "posted by u/musk"}])





(def similar-coins
  ["bitcoin" "ethereum" "monero" "zcash" "eos" "lisk" "ripple"])
