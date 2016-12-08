{-
  Name:   Nicholas Saunderson
  Email:  2057559s@student.gla.ac.uk
-}
{-# LANGUAGE OverloadedStrings #-}
import Text.LaTeX
import Data.List (isInfixOf)
import Data.List.Split
import Data.List (intercalate)
import Data.Char
import Text.HTML.Scalpel
import Data.Maybe
import Control.Monad
import Control.Applicative

main :: IO()
main = do
    {- Scrapes the main url and staff link: www.gla.ac.uk/ + schools/computing/staff, using the getLinks function and stores within links -}
    links <- getLinks (mainUrl++staffUrl)

    {- The getDetails function is used here to scrape each staff url. It maps over every element within the links list and extracts the 
       relevent information associated with telephone numbers and names using the defined functions. sequence is used
       to take IO out of the list. 
    -}
    details <- sequence $ (fmap (mapM getDetails) links)

    {- map1 maps the monadic list into a standard list.
       map2 converts the tuple into a string. Each string represents a name and a phone number. 
       map3 then uses a filter to remove the null elements 
    -}
    let map1 =  map fromJust (fromJust details)
    let map2 = map (map toString) map1
    let map3 = (filter (not . null) map2)

    {- flatten is used to convert the list of lists into the format: one list of strings  -}
    let flatten = intercalate []
    let flat = flatten $ flatten [map3]
    putStrLn "Completed"

    {- The list is then written to directory.txt, this is used later to export to the latex directory.tex file -}
    writeFile "directory.txt" $ unlines flat

    {- Generates the LaTeX and writes it out to a file called directory.tex. -}
    text <- execLaTeXT doc
    renderFile "directory.tex" text


{- This function converts the tuple of strings to a single string -}
toString :: (String, String) -> String
toString (name, phone) = name ++ ", " ++ phone

mainUrl :: String
mainUrl = "http://www.gla.ac.uk/"

staffUrl :: String
staffUrl = "schools/computing/staff"

otherStaffUrl :: String
otherStaffUrl = "/?action"

--Function for scraping the main link
getLinks :: String -> IO (Maybe [String])
getLinks a = scrapeURL a scrapeLinks

-- Function for scraping each individual staff link
getDetails :: String -> IO (Maybe [(String, String)])
getDetails a = scrapeURL a scrapeDetails


{- This function looks for a div tag with the id=mainContent, it then uses the predefined getAttributes function to retrieve the content 
   contained within the mainContent id
-}
scrapeDetails :: Scraper String [(String, String)]
scrapeDetails =  
    chroots ("div" @: ["id" @= "mainContent"]) getAttributes
        

{- This function stores the name from the h1 tag. It stores all of the contents within the id sp_contactInfo. 
   The telephone string is then passed into the getPhoneNumber function, which extracts the telephone number. 
-}
getAttributes :: Scraper String (String, String)
getAttributes = do
    name <- text "h1"
    telephone <- text ("div" @: ["id" @= "sp_contactInfo"]) 
    return (name, getPhoneNumber telephone)

{- This function retrieves the phone number from the string by checking for the '\ntelephone:'' pattern. If it exists 
   then the phone number gets formatted by the formatPhoneNumber function. 
-}
getPhoneNumber :: String -> String 
getPhoneNumber telephoneString = do
    if ("\ntelephone: " `isInfixOf` telephoneString )
        then (formatPhoneNumber telephoneString)
        else ("-")

{- Simple format function which extracts the phone number from the string using the splitOn function from the haskell Data.List library -}
formatPhoneNumber :: String -> String
formatPhoneNumber telephoneString = (splitOn "\n" ((splitOn ": " telephoneString)!!1))!!0


{- This function is used to scrape the links of individual staff web pages. It looks for the tag 'a', altText stores the staff name or unique staff url id.
   It uses a guard to check if the staff url is contained within altText, and the same with otherStaffUrl. If there is a match then altText is 
   appended to the end of the main url, and the url is returned.
-}
scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector
        guard (staffUrl `isInfixOf` altText || otherStaffUrl `isInfixOf` altText)
        let url = mainUrl ++ altText
        return url


{-  This function deals with the latex code generation -}
doc :: Monad m => LaTeXT m ()
doc = do 
    thePreamble
    document theBody

{- Populates the content of the preamble of the latex document. Uses the verbatium package to output the text file
   to a .tex file.
-}
thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article 
    usepackage [] "verbatim"
    author "Nicholas Saunderson - 2057559s"
    title "Function Programming 4 - Phone Book"

{- Populates the body of the latex file with the contents of directory.txt -}
theBody :: Monad m => LaTeXT m ()
theBody = do
    maketitle
    raw "\\verbatiminput{directory.txt}"
