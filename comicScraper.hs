{-# LANGUAGE OverloadedStrings #-}
import Data.List (isInfixOf)
import Data.Sequence
import Data.Char
import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative
main :: IO()
main = do
    links <- getLinks "http://www.gla.ac.uk//schools/computing/staff"
    details <- return (fmap (mapM getDetails) links)
    sequence_ details
    print links



--Function for scraping the main link
getLinks :: String -> IO (Maybe [String])
getLinks a = scrapeURL a scrapeLinks

-- Function for scraping each individual staff link
getDetails :: String -> IO (Maybe [String])
getDetails a = scrapeURL a scrapeDetails


scrapeDetails :: Scraper String [String]
scrapeDetails =  
    chroots "h1" $ do
        name <- text anySelector
        let n = "name: " ++ name
        return n


scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector
        guard ("schools/computing/staff" `isInfixOf` altText)
        let url = "http://www.gla.ac.uk/" ++ altText
        return url









{-
scrapeStaffURL :: Scraper String [[String]]
scrapeStaffURL =
    chroots ("a") scrapeTitle


scrapeTitle :: Scraper String [String]
scrapeTitle = do
    staffLink <- attr "href" anySelector
    guard ("schools/computing/staff/" `isInfixOf` staffLink)
    let url = "www.gla.ac.uk" ++ staffLink
    print url

    -}







{-}
import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)
import Data.Char  

main :: IO ()
main = do
    links <- getLinks "http://www.gla.ac.uk//schools/computing/staff/"
    details <- return (fmap (mapM getDetails) links)
    sequence_ details
    print "done"

getLinks :: String -> IO (Maybe [String])
getLinks a = scrapeURL a scrapeLinks

getDetails :: String -> IO (Maybe [String])
getDetails a = scrapeURL a scrapeDetails

scrapeDetails :: Scraper String [String]
scrapeDetails =  
    chroots "h1" $ do
        name <- text anySelector
        let n = "name: " ++ name
        print 
        return n


scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector
        guard ("schools/computing/staff" `isInfixOf` altText)
        let url = "http://www.gla.ac.uk/" ++ altText
        return url

        --}