module Spec (main) where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import qualified Main as Main

import Test.Hspec

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "getRules" $ do
    it "returns the parsed rules from rules.json" $ do
      rules <- Main.getRules
      rules `shouldBe` [ Main.Rule { Main.urls = [ Main.URL "facebook.com"
                                                 , Main.URL "www.facebook.com"
                                                 , Main.URL "twitter.com"
                                                 , Main.URL "www.twitter.com"
                                                 , Main.URL "youtube.com"
                                                 , Main.URL "www.youtube.com"
                                                 , Main.URL "instagram.com"
                                                 , Main.URL "www.instagram.com"
                                                 ]
                                   , Main.allowed = []
                                   }
                       , Main.Rule { Main.urls = [ Main.URL "chat.googleplex.com" ]
                                   , Main.allowed = []
                                   }
                       ]

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
