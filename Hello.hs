-- Inspared by https://lhbg-book.link/

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title."
    ( append_
        (h1_ "Heading")
        ( append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )

-- A complete Html document.
newtype Html = Html String

-- A type for html structures, such as head, h1, p, etc.
newtype Structure = Structure String

-- Title is just a type name alias.
type Title = String

-- Combine html tags.
html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" title)
            <> el "body" (getStructureString content)
        )
    )

-- Because,
-- Structure :: String -> Structure
-- el "p" :: String -> String
-- So,
-- a ~ String
-- b ~ String
-- c ~ Structure
-- Because,
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- So,
-- (.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)
-- So,
-- Structure . el "p" :: String -> Structure
p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

-- A function that take a tag and content, and wraps the content with the tag.
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- Replace <> operator. Take two Structures and return a third Structure.
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

-- Converting back Html to String. So we can display it in our browser.
render :: Html -> String
render html =
  case html of
    Html str -> str