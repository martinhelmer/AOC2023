module BSArray (BSArray
               , rows
               , cols
               , row
               , BSArray.lookup
               , makeBSarray
               , lookupMaybe
               , BSArray.elemIndex
               , flatIndex
               ) where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

type Index = (Int, Int)

newtype Row = Row Int
newtype Col = Col Int

data BSArray = BSArray
    { contents :: ByteString
    , _rows :: Row
    , _cols :: Col
    }

instance  Show BSArray where
    show bsa = show (contents bsa)

rows :: BSArray -> Int
rows (BSArray _ (Row rows') (Col _)) = rows'

cols :: BSArray -> Int
cols (BSArray _ (Row _) (Col cols')) = cols'


makeBSarray :: ByteString -> BSArray
makeBSarray s = BSArray s (Row rows') (Col cols')
    where (cols', rows') = case (B.elemIndex '\n' s , B.last s == '\n') of
            (Nothing, _) -> (B.length s, 1)
            (Just l ,True)  -> (l, B.length s `div` (l+1))
            (Just l, False) -> (l, (B.length s+1) `div` (l+1))


lookup :: BSArray -> Index ->  Char
lookup (BSArray s _ (Col cols')) (ir, ic) | ir < 0 = error "Negative row index!" 
                                          | ic < 0 = error "Negative column index!"
                                          | otherwise = B.index s (ic + ir * (cols' +1))

lookupMaybe :: BSArray -> Index -> Maybe Char
lookupMaybe a@(BSArray _ (Row rows') (Col cols')) ix@(ir, ic)
    | ir < 0 || ir >= rows' || ic < 0 || ic >= cols' = Nothing
    | otherwise = Just (BSArray.lookup a ix)

elemIndex :: BSArray -> Char -> Maybe Index
elemIndex bs c = case i of
        Nothing -> Nothing
        Just ix -> Just (ix `div` (cols bs + 1 ), ix `rem` (cols bs + 1 ))
    where i = B.elemIndex c (contents bs)

flatIndex :: BSArray -> (Int, Int) -> Int
flatIndex bs (row', col) = rows bs * row' + col

row :: BSArray -> Int -> ByteString
row bs rownum = let cols' = cols bs in
        B.take cols' . B.drop (rownum*(cols' +1)) $ contents bs
    