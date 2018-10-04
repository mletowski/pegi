module  SParser  (  SPResult(..), 
                    SParser(..), 
                    
                    sparse, 
                    spchar, 
                    spmany, 
                    spmany1, 
                    spnot, 
                    sppredicate, 
                    spmaybe, 
                    spsatisfy, 
                    spRead, 
                    spOneOf, 
                    spSepBy, 
                    spSepBy1, 

                    (<|>)
                        
                 )  where

import  Control.Monad (liftM, ap)

data  SPResult a = SPOK a String            -- SPOK result rest
                 | SPError String String    -- SPError message rest
  deriving Show

newtype  SParser a = SParser (String -> SPResult a)

instance Functor SParser where
  fmap = liftM

instance Applicative SParser where
  pure  = return
  (<*>) = ap

spbind :: SParser a -> (a -> SParser b) -> SParser b
spbind  (SParser  spA)  atospB  =  SParser (\s -> case spA s of
                                                        SPOK a rest -> case atospB a of
                                                                SParser bp -> bp rest
                                                        SPError e r -> SPError e r)

instance  Monad  SParser where
        (>>=) = spbind
        return x = SParser (SPOK x)
        fail err = SParser (SPError err)

sparse :: SParser a -> String -> SPResult a
sparse (SParser f) = f 

(<|>) :: SParser a -> SParser a -> SParser a
(<|>) (SParser p1) (SParser p2) = SParser (\s -> case p1 s of
                                                        SPError _ _ -> p2 s
                                                        ok -> ok)

spnot :: SParser a -> String -> SParser ()
spnot (SParser f) message = SParser (\s -> case f s of
                                        SPError _ _ -> SPOK () s
                                        _ -> SPError ("negation failed:" ++ message) s)

-- sppredicate p acts like p, but does not consume the input
sppredicate :: SParser a -> SParser a
sppredicate (SParser p) = SParser $ \s -> case p s of
                                                SPOK x _ -> SPOK x s
                                                err -> err

readMany :: (String -> SPResult a) -> String -> SPResult [a]
readMany f s = case f s of
                     SPOK x rest -> case  readMany f rest  of
                                        SPOK xs rests -> SPOK (x: xs) rests
                                        SPError _ _ -> SPOK [x] rest
                     SPError _ _ -> SPOK [] s
                

spmany :: SParser a -> SParser [a]
spmany (SParser f) = SParser (readMany f)

spmany1 :: SParser a -> SParser [a]
spmany1 p = do
                x <- p
                xs <- spmany p
                return (x: xs)

spmaybe :: SParser a -> SParser (Maybe a)
spmaybe p = (do
                x <- p
                return (Just x)) <|> return Nothing

spRead :: SParser Char 
spRead = SParser (\s -> case s of
                   (c: cs) -> SPOK c cs
                   "" -> SPError "Unexpected end of input" s)

spsatisfy :: (Char -> Bool) -> String -> SParser Char
spsatisfy f err = do
                x <- spRead 
                if f x then return x else fail err

spchar :: Char -> SParser Char
spchar c = spsatisfy (== c) ("Expected character " ++ [c])

spOneOf :: String -> SParser Char
spOneOf s = spsatisfy (`elem` s) ("Expected one of " ++ show s)


spSepBy1 :: SParser a -> SParser sep -> SParser [a]
spSepBy1 p sep = do
                    x <- p
                    xs <- spmany (sep >> p)
                    return (x: xs)

spSepBy :: SParser a -> SParser sep -> SParser [a]
spSepBy p sep = spSepBy1 p sep <|> return []

