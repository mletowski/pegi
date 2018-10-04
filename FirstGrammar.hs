module FirstGrammar  (  Grammar(..), 
                        Element(..), 
                        PredMode(..), 
                        Pattern(..), 
                        Production(..), 
                        Clause(..), 
                        RawElement(..), 
                        RepMode(..), 

                        SPResult(..), 

                        parse
                        
                     )  where

import Data.Map as M
import Data.Maybe
import Data.List as L

import SParser

newtype Grammar = Grammar (Map String Production)
        deriving Show

data  RepMode = Star | Plus | Option
        deriving Show

data  Pattern =    PTerminal   String 
                |  PReference  String
                |  PRepetition RepMode [Pattern]
        deriving Show

data  RawElement   = Terminal String 
                   | Char String                   
                   | Class String (Maybe String)  
                   | Reference String (Maybe String)
                   | Repetition RepMode [Element]
        deriving Show

data  PredMode = Normal | Predicate | Not
        deriving Show

data  Element = Element PredMode RawElement
        deriving Show

data  Clause = Clause [Element] [Pattern]
        deriving Show

data  Production = Production String [Clause]
        deriving Show

--              RowData scalars rows-lists
data  RowData = RowData (Map String String) [[RowData]]
        deriving Show

--    Scalar name value | Vector rowdata
data  ElemData = Scalar String String
               | Array [RowData]
        deriving Show

aelement :: RawElement -> Element
aelement = Element Normal

--  expand pattern rowdata rest -> Either error result
expand :: [Pattern] -> RowData -> String -> SPResult  String
expand [] _ r = SPOK "" r
expand (PTerminal s: ps) rd r = 
        case  expand ps rd r  of
                SPOK t r' ->  SPOK (s ++ t) r'
                err -> err

expand (PReference name: ps) (RowData scalars vectors) r = do
        let  value = M.lookup name scalars
        case  value of
            Nothing -> SPError ("Unknown symbol in pattern: " ++ name ++ 
                                   ", scalars: " ++ show scalars) r

            Just  v -> let  res =  expand ps (RowData scalars vectors)  r
                       in   case res of
                            SPOK t r'  -> SPOK (v ++ t) r'
                            SPError e r' -> SPError e r'


expand  (PRepetition  repMode  ps: ps')  rowdata  r  = 
        case  expandRepetition  repMode ps rowdata r  of
                SPError e r' -> SPError e r'
                SPOK  e1 r' ->  case  expand ps' rowdata r'  of
                                        SPError e' r'' -> SPError e' r''
                                        SPOK es r'' -> SPOK (e1 ++ es) r''

expandRepetition :: RepMode -> [Pattern] -> RowData -> 
                                  String -> SPResult String

expandRepetition  _ [] _ r = SPOK "" r
                
expandRepetition  repMode  ps  (RowData scalars (v: vs)) r =
        case  expandRow  repMode  ps v   of
             Just x   -> SPOK x r
             Nothing  -> expandRepetition  repMode  ps  (RowData scalars vs) r

expandRepetition  Option _ (RowData _ []) r = SPOK "" r

expandRepetition  Star   _ (RowData _ []) r = SPOK "" r

expandRepetition  Plus   _ (RowData _ []) r = SPError "No match for pattern" r

expandRow :: RepMode -> [Pattern] -> [RowData] -> Maybe String
expandRow  _  []  _ = Just ""
expandRow  Option _ [] = Just ""
expandRow  Star   _ [] = Just ""
expandRow  Plus   _ [] = Nothing

expandRow  Plus   p (r: rs) =
        case  expand  p  r  ""  of
                SPError _ _ -> Nothing
                SPOK x _ -> do
                                xs <- expandRow Star p rs
                                return  (x ++ xs)

expandRow  Star   p (r: rs) =
        case  expand  p  r  ""  of
                SPError _ _ -> Nothing
                SPOK x _ -> do
                                xs <- expandRow Star p rs
                                return  (x ++ xs)

expandRow  Option   p [r] =
        case  expand  p  r  ""  of
                SPError _ _ -> Nothing
                SPOK x _ -> Just x

expandRow  _ _ _ = Nothing                

parseElemF :: Map String (SParser String) -> Element -> 
                                String -> SPResult ElemData

parseElemF _ (Element Normal (Char symbol)) (c: cs) = 
        SPOK (Scalar symbol [c]) cs

parseElemF _ (Element Normal (Char _)) "" = SPError "No more data" ""

parseElemF _ (Element Normal (Terminal t)) s = 
        case  stripPrefix t s of
                Just rest ->  SPOK (Scalar "" t) rest
                Nothing   ->  SPError ("Expected " ++ show t) s

parseElemF _ (Element Normal (Class chars _)) "" = 
                SPError ("Expected one of " ++ show chars) ""

parseElemF _ (Element Normal (Class chars symbol)) (c: cs) = 
        if c `elem` chars 
                then SPOK (Scalar (fromMaybe "" symbol) [c]) cs
                else SPError ("Expected one of " ++ show chars) (c: cs)

parseElemF  parsers  (Element Normal (Reference symbol result_name)) s = let
        name = fromMaybe ("$" ++ symbol) result_name
        p = M.lookup symbol parsers
        in case p of
             Just (SParser parser) -> case parser s of
                                        SPOK x r -> SPOK (Scalar name x) r
                                        SPError err r -> SPError err r
             Nothing -> SPError ("Unknown symbol " ++ symbol) s

parseElemF parsers (Element Normal (Repetition rep es)) s = 
        let row1 = sparse (parseRow parsers es) s in
            case  (row1, rep) of   
               (SPError _ _, Option) -> SPOK (Array []) s
               (SPError _ _, Star) -> SPOK (Array []) s
               (SPError err r, Plus) -> SPError err r
               (SPOK rowdata s1, Option)  -> SPOK (Array [rowdata]) s1
               (SPOK rowdata s1, _)  -> 
                  let  r = parseElemF parsers (Element Normal 
                                (Repetition Star es)) s1
                  in case r of 
                     SPError _ _ -> SPOK (Array [rowdata]) s1
                     SPOK (Array t) s2 -> SPOK (Array (rowdata:t)) s2
                     SPOK x _ -> SPError ("Impossible match: " ++ show x) s

parseElemF productions (Element Predicate e) s = 
        case  parseElemF productions (aelement e) s  of
                SPOK x _ -> SPOK x s
                err -> err 

parseElemF productions (Element Not e) s = 
        case parseElemF productions (aelement e) s of
                SPOK _ _ -> SPError "Negation failure" s
                SPError _ _ -> SPOK (Scalar "" "") s

parseElem :: Map String (SParser String) -> Element -> SParser ElemData
parseElem  parsers  e = SParser $ parseElemF parsers e

--  addResult  elem-data row-data = row-data
addResult :: ElemData -> RowData -> RowData
addResult (Scalar "" _) rd = rd
addResult (Scalar name x) (RowData scalars vectors) = 
        RowData (M.insert name x scalars) vectors

--  we want vectors in RowData to be sorted - empty vectors at end 
addResult (Array []) (RowData scalars vectors) = 
        RowData scalars (vectors ++ [[]])

addResult (Array xs) (RowData scalars vectors) = 
        RowData scalars (xs: vectors)

--  parseRow productions row text -> Just (rowdata, rest of text)
parseRow :: Map String (SParser String) -> [Element] -> SParser RowData
parseRow _ [] = SParser (SPOK (RowData (fromList []) []))
parseRow parsers (e: es) = do
        e_data <- parseElem parsers e 
        es_data <- parseRow parsers es
        return  (addResult e_data es_data)

parseClause :: Map String (SParser String) -> Clause -> SParser String

parseClause  parsers (Clause elements patt) =
        SParser (\s -> case  sparse (parseRow parsers elements) s  of
                        SPError err r -> SPError err r
                        SPOK rowdata rest -> case expand patt rowdata rest of
                                        SPError err' r' -> SPError err' r'
                                        SPOK x r''   -> SPOK x r'')

parseClauses :: Map String (SParser String) -> [Clause] -> 
                                        String -> SParser String

parseClauses _ [] name = SParser (SPError ("No more clauses for " ++ name))
parseClauses parsers (c: cs) name = SParser ( \s -> 
        let  res = sparse (parseClause parsers c) s in
        case  res of 
                SPError _ _ -> sparse (parseClauses parsers cs name) s
                SPOK x r -> SPOK x r )

compileProduction :: Map String (SParser String) -> 
        Production -> SParser String
compileProduction  pmap  (Production name clauses) = 
        parseClauses pmap clauses name

--  compileGrammar  grammar -> parsers
compileGrammar :: Grammar -> Map String (SParser String)
compileGrammar (Grammar pmap) = let
        parsers = M.map (compileProduction parsers) pmap
        in  parsers

--  gparser  grammar  goal_name  ->  the parser
gparser :: Grammar -> String -> SParser String
gparser  grammar goal  = 
        let parsers = compileGrammar grammar
        in  fromMaybe  (SParser (SPError ("Unknown symbol: " ++ goal)))
                        (M.lookup goal parsers)

parse :: Grammar -> String -> String -> SPResult String
parse  grammar  goal = sparse $ gparser grammar goal
