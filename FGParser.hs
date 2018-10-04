module  FGParser  where

import  Data.Char
import  Data.Maybe
import  Data.Map as M

import  FirstGrammar
import  SParser

spName :: SParser String
spName = do 
           c <- spsatisfy isLetter "Letter expected"
           cs <- spmany (spsatisfy isAlphaNum "Letter or digit expected" <|>
                         spchar '_')
           return (c : cs)

stringItem :: SParser Char
stringItem =  ( do 
                  spnot (spOneOf "\\\"") "stringItem with ' character"
                  spRead )  <|>
              ( do 
                  _ <- spchar '\\'
                  c <- spOneOf  "\\\"nrt"

                  return $ case c of
                             't' -> '\t'
                             'r' -> '\r'
                             'n' -> '\n'
                             _   -> c )


spString :: SParser String
spString = do
             _ <- spchar '"'
             s <- spmany  stringItem 
             _ <- spchar '"'
             return s

comment :: SParser String
comment = do 
             h <- spchar '#'
             s <- spmany 
                     (spnot (spchar '\n') "end of line in comment" >> spRead)
             return (h: s)

omitSpaces :: SParser String
omitSpaces = do
                ws <- spmany (spmany1 (spOneOf " \t\r\n") <|> comment)
                return (concat ws)

spaces :: SParser String                
spaces = do
                ws <- spmany1 (spmany1 (spOneOf " \t\r\n") <|> comment)
                return (concat ws)

spPrefix :: String -> SParser PredMode
spPrefix choices = do 
                        p <- spmaybe (spOneOf choices) 
                        return $ maybe Normal (\c -> case c of
                                                  '!' -> Not
                                                  '&' -> Predicate
                                                  _ -> Normal) p

classItem :: SParser Char
classItem =    ( do 
                        spnot (spOneOf "\\'") "classItem with ' character"
                        spRead )  <|>
                ( do 
                        _ <- spchar '\\'
                        c <- spOneOf  "\\'nrt"

                        return ( case c of
                                        't' -> '\t'
                                        'r' -> '\r'
                                        'n' -> '\n'
                                        _   -> c ))

spCharacterClass :: SParser String
spCharacterClass = do
                _ <- spchar '\''
                s <- spmany  classItem
                _ <- spchar '\''
                return s

spScalar :: SParser Element 
-- TODO - refactor
spScalar = ( do
                pref <- spPrefix "&"
                _ <- omitSpaces
                n <- spName
                sym <- spmaybe (omitSpaces >> spchar '$' >> spName >>= 
                           \name -> return ('$':name))
                return  (Element pref (Reference n sym)) ) <|>
           ( do
                _ <- spchar '!'
                _ <- omitSpaces
                n <- spName
                return  (Element Not (Reference n Nothing))) <|>

           ( do
                pref <- spPrefix "!&"
                _ <- omitSpaces
                s <- spString
                return  (Element pref (Terminal s)))  <|>

           ( do
                pref <- spPrefix "&"
                _ <- omitSpaces
                c <- spCharacterClass
                sym <- spmaybe (omitSpaces >> spchar '$' >> spName >>= 
                                  \name -> return ('$':name))
                return  (Element pref (Class c sym)) ) <|>

           ( do
                _ <- spchar '!'
                _ <- omitSpaces
                c <- spCharacterClass
                return  (Element Not (Class c Nothing))) <|>

           ( do
                pref <- spPrefix "&"
                _ <- omitSpaces
                _ <- spchar '.'
                sym <- spmaybe (omitSpaces >> spchar '$' >> 
                                  spName >>= \name -> return ('$':name))
                return  (Element pref (Char (fromMaybe "" sym)) )) <|>

           ( do
                _ <- spchar '!'
                _ <- omitSpaces
                _ <- spchar '.'
                return  (Element Not (Char "")))

spRepMode :: SParser RepMode
spRepMode = do
                c <- spOneOf "*+"
                case  c of
                        '*' -> return Star
                        '+' -> return Plus
                        _   -> error "Impossible repetition character"

spElements :: SParser [Element]
spElements = do 
                _ <- omitSpaces
                el <- spElement `spSepBy1` spaces
                _ <- omitSpaces
                return el

spElement :: SParser Element
spElement =  ( do 
                 pref <- spPrefix "!&"
                 _ <- omitSpaces
                 v <- spVector
                 return  (Element pref v) )  <|> spScalar

spVector :: SParser RawElement
spVector = (do
                name <- spName 
                _ <- omitSpaces
                rep <- spRepMode
                return  (Repetition rep [ Element Normal 
                                  (Reference name Nothing) ])) <|>
           (do
                s <- spString 
                _ <- omitSpaces
                rep <- spRepMode
                return  (Repetition rep [Element Normal (Terminal s)]))  <|>
           (do 
                _ <- spchar '['
                el <- spElements
                _ <- spchar ']'
                return  (Repetition Option el)) <|>
           (do 
                _ <- spchar '('
                el <- spElements
                rep <- spchar ')' >> omitSpaces >> spRepMode
                return  (Repetition rep el))  <|>
           (do 
                c <- spCharacterClass
                _ <- omitSpaces
                rep <- spRepMode
                return  (Repetition rep [Element Normal (Class c Nothing)]))

spPattern :: SParser Pattern
spPattern = do
               ch <- sppredicate (spOneOf "\"$[(")
               case ch of
                 '$' -> do
                     _ <- spchar '$'
                     _ <- omitSpaces
                     name <- spName
                     rep <- spmaybe (omitSpaces >> spRepMode)
                     case rep of
                             Nothing -> return  (PReference  ('$':name))
                             Just r -> return  (PRepetition r 
                                                  [PReference ('$':name)] )
                 '[' -> do
                     _ <- spchar '['
                     _ <- omitSpaces
                     ps <- spPattern `spSepBy` omitSpaces
                     _ <- omitSpaces
                     _ <- spchar ']'
                     return (PRepetition Option ps)
                 '(' -> do
                     _ <- spchar '('
                     _ <- omitSpaces
                     ps <- spPattern `spSepBy` omitSpaces
                     rep <- omitSpaces >> spchar ')' >> 
                                        omitSpaces >> spRepMode
                     return (PRepetition rep ps)
                 '"' -> do
                     s <- spString
                     rep <- spmaybe (omitSpaces >> spRepMode)

                     case rep of
                             Nothing -> return  (PTerminal  s)
                             Just r -> return  (PRepetition r [PTerminal s])

                 ec  -> error ("Impossible character: " ++ [ec])

spPatternBlock :: SParser [Pattern]
spPatternBlock = do
                        _ <- spchar '{'
                        _ <- omitSpaces
                        ps <- spPattern `spSepBy` omitSpaces
                        _ <- omitSpaces
                        _ <- spchar '}'
                        return  ps
                                        
spClause :: SParser Clause
spClause = (do
                el <- spElements
                mp <- spmaybe (omitSpaces >> spPatternBlock)
                case  mp of
                        Nothing -> return  (Clause el [])
                        Just ps -> return  (Clause el ps)) <|>
           (do 
                _ <- omitSpaces
                ps <- spPatternBlock
                return  (Clause [] ps))
                
spProduction :: SParser (String, Production)
spProduction = do
                  name <- spName 
                  _ <- omitSpaces
                  _ <- spchar '='
                  _ <- omitSpaces
                  cs <- spClause `spSepBy1` (omitSpaces >> 
                                        spchar '|' >> omitSpaces)
                  return  (name, Production name cs)
                        
spGrammar :: SParser Grammar
spGrammar = do
                _ <- omitSpaces
                ps <- spProduction `spSepBy1` (omitSpaces >> 
                                                spchar ';' >> omitSpaces)
                _ <- omitSpaces >> spmaybe (spchar ';') >> omitSpaces
                _ <- spnot spRead "eof"
                return  (Grammar (M.fromList ps))


