{-# LANGUAGE OverloadedStrings #-}

module TFG_maybeMonad where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath





-------- SIMBOLOS U OTRO CODIGO --------

maybeMonad :: LaTeXC l => l
maybeMonad= verbatim (pack t) where
  t =
    "    return :: a -> Maybe a \n" 
    <> "    return x  = Just x \n\n"
    <> "    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b \n"
    <> "    m >>= g = case m of \n"
    <> "                 Nothing -> Nothing \n"
    <> "                 Just x  -> g x \n"


bind :: LaTeXC l => l
bind = scriptsize (fbox (math "(>>=)"))



ejLog1 :: LaTeXC l => l
ejLog1 = verbatim (pack t) where
  t =
    " > log 1000 \n"
    <> " 6.907755278982137 \n"
    <> " > log (-1000) \n"
    <> " ''ERROR'' -- runtime error \n"

safeLog :: LaTeXC l => l
safeLog = verbatim (pack t) where
  t =
    " safeLog :: (Floating a, Ord a) => a -> Maybe a \n"
    <> " safeLog x \n"
    <> "     | x > 0    = Just (log x) \n"
    <> "     | otherwise = Nothing \n"

ejLog2 :: LaTeXC l => l
ejLog2 = verbatim (pack t) where
  t =
    " > safeLog 1000 \n"
    <> " Just 6.907755278982137 \n"
    <> " > safeLog -1000 \n"
    <> " Nothing \n"

ejDB :: LaTeXC l => l
ejDB = verbatim (pack t) where
  t =
    " phonebook :: [(String, String)] \n"
    <> " phonebook = [ (`Bob',   `01788 665242'), \n"
    <> "               (`Fred',  `01624 556442'), \n"
    <> "               (`Alice', `01889 985333'), \n"
    <> "               (`Jane',  `01732 187565') ] \n"

lookupDef :: LaTeXC l => l
lookupDef = verbatim (pack t) where
  t =
    " lookup :: Eq a => a  -- a key \n"
    <> "        -> [(a, b)]   -- the lookup table to use \n"
    <> "        -> Maybe b    -- the result of the lookup \n"

ejLookUp :: LaTeXC l => l
ejLookUp = verbatim (pack t) where
  t =
    " Prelude> lookup `Bob' phonebook \n"
    <> " Just `01788 665242' \n"
    <> " Prelude> lookup `Jane' phonebook \n"
    <> " Just `01732 187565' \n"
    <> " Prelude> lookup `Zoe' phonebook \n"
    <> " Nothing \n"

getRN :: LaTeXC l => l
getRN = verbatim (pack t) where
  t = 
    " getRegistrationNumber :: String       -- their name \n"
    <> "                       -> Maybe String -- their Reg.Num. \n"
    <> " getRegistrationNumber name =  \n"
    <> "   lookup name phonebook >>= \n"
    <> "     (\\number -> lookup number governmentDatabase) \n"

getTaxOwed :: LaTeXC l => l
getTaxOwed = verbatim (pack t) where
  t =
    " getTaxOwed :: String       -- their name \n"
    <> "           -> Maybe Double -- the amount of tax they owe \n"
    <> " getTaxOwed name =  \n"
    <> "   lookup name phonebook >>= \n"
    <> "     (\\number -> lookup number governmentDatabase) >>= \n"
    <> "       (\\registration -> lookup registration taxDatabase) \n"

getTaxOwed_DO :: LaTeXC l => l
getTaxOwed_DO = verbatim (pack t) where
  t = 
    " getTaxOwed name = do \n"
    <> "   number       <- lookup name phonebook \n"
    <> "   registration <- lookup number governmentDatabase \n"
    <> "   lookup registration taxDatabase \n"

exitMaybe :: LaTeXC l => l
exitMaybe = verbatim (pack t) where
  t =
    " zeroAsDefault :: Maybe Int -> Int \n"
    <> " zeroAsDefault mx = case mx of \n"
    <> "     Nothing -> 0 \n"
    <> "     Just x -> x \n"

zeroAsDefault :: LaTeXC l => l
zeroAsDefault = verbatim (pack t) where
  t =
    " zeroAsDefault :: Maybe Int -> Int \n"
    <> " zeroAsDefault mx = fromMaybe 0 mx \n"

dispRes :: LaTeXC l => l
dispRes = verbatim (pack t) where
  t =
    " displayResult :: Maybe Int -> String \n"
    <> " displayResult mx = maybe s1 ((s2++).show) mx \n"
    <> "   where \n"
    <> "     s1 = `There was no result' \n"
    <> "     s2 = `The result was'  \n \n"

probandoDispRes :: LaTeXC l => l
probandoDispRes = verbatim (pack t) where
  t =
    " Prelude> :t maybe \n"
    <> " maybe :: b -> (a -> b) -> Maybe a -> b \n"
    <> " Prelude> displayResult (Just 10) \n"
    <> " `The result was 10' \n"
    <> " Prelude> displayResult Nothing \n"
    <> " `There was no result' \n"



----------------------------------------





anexo4 :: LaTeXC l => l
anexo4 = 
  chapter "Appendix: the Maybe monad"
  <> "We introduced monads using " <> fbox "Maybe" <> " as an example. The " <> fbox "Maybe" <> " monad represents computations which might " <> qts "go wrong" 
  <> " by not returning a value. For reference, here are our definitions of " <> fbox "return" <> " and " <> bind <> " for " <> fbox "Maybe" <> " as we saw in the main body:"
  <> footnote ( "The definitions in the actual instance in " <> fbox "Data.Maybe" <> " are written a little differently, but are fully equivalent to these. " )
  <> maybeMonad
  <> section "Safe functions" <> safeFunctions
  <> section "Lookup tables" <> lookupTables
  <> section "Open monads" <> openMonads
  <> section "Maybe and safety" <> safety




safeFunctions :: LaTeXC l => l
safeFunctions =
  "The " <> fbox "Maybe" <> " datatype provides a way to make a safety wrapper around " <> textit "partial functions" <> ", that is, functions which can fail to work for a range of arguments. For example, "
  <> fbox "head" <> " and " <> fbox "tail" <> " only work with non-empty lists. Another typical case, which we will explore in this section, are mathematical functions like "
  <> fbox "sqrt" <> " and " <> fbox "log" <> "; (as far as real numbers are concerned) these are only defined for non-negative arguments."
  <> ejLog1
  <> "To avoid this crash, a " <> qts "safe" <> " implementation of log could be:"
  <> safeLog
  <> ejLog2
  <> "We could write similar " <> qts "safe functions" <> " for all functions with limited domains such as division, square-root, and inverse trigonometric functions ("
  <> fbox "safeDiv" <> ", " <> fbox "safeSqrt" <> ", " <> fbox "safeArcSin" <> ", etc. all of which would have the same " <> textit "type" <> " as " <> fbox "safeLog" <> " but definitions specific to their constraints)"
  <> par <> "If we wanted to combine these monadic functions, the cleanest approach is with monadic composition and point-free style:"
  <> verbatim ( pack " safeLogSqrt = safeLog <=< safeSqrt " )
  <> "Written in this way, " <> fbox "safeLogSqrt" <> " resembles a lot its unsafe, non-monadic counterpart:"
  <> verbatim ( pack " unsafeLogSqrt = log . sqrt " ) 




lookupTables :: LaTeXC l => l
lookupTables =
  "A lookup table relates " <> textit "keys" <> " to " <> textit "values" <> ". You look up a value by knowing its key and using the lookup table. For example, you might have a phone book application with a lookup table where contact names are keys to corresponding phone numbers."
  <> "\n An elementary way of implementing lookup tables in Haskell is to use a list of pairs: " <> fbox "[(a, b)]" <> ". Here " <> fbox "a" <> " is the type of the keys, and " <> fbox "b" <> " the type of the values."
  <> footnote ( " Check " <> fbox "Data.Map" <> " for a different, and potentially more useful, implementation." )
  <> "Here's how the phone book lookup table might look like:"
  <> ejDB
  <> "The most common thing you might do with a lookup table is look up values. Everything is fine if we try to look up " <> qts "Bob"<> ", " <> qts "Fred" <> ", " <> qts "Alice" <> " or " <> qts "Jane" 
  <> " in our phone book, but what if we were to look up " <> qts "Zoe" <> "? \n"
  <> "Zoe isn't in our phone book, so the lookup would fail. Hence, the Haskell function to look up a value from the table is a " <> fbox "Maybe" <> " computation (it is available from Prelude): "
  <> lookupDef
  <> "Let us explore some of the results from lookup:"
  <> ejLookUp
  <> "Now let's expand this into using the full power of the monadic interface. Say, we're now working for the government, and once we have a phone number from our contact, we want to look up this phone number in a big, government-sized lookup table to find out the registration number of their car. This, of course, will be another "
  <> fbox "Maybe" <> "-computation. But if the person we're looking for isn't in our phone book, we certainly won't be able to look up their registration number in the governmental database."
  <> par <> "What we need is a function that will take the results from the first computation and put it into the second lookup " <> textit "only" <> " if we get a successful value in the first lookup. Of course, our final result should be " <> fbox "Nothing" <> " if we get " <> fbox "Nothing" <> " from either of the lookups."
  <> getRN
  <> "If we then wanted to use the result from the governmental database lookup in a third lookup (say we want to look up their registration number to see if they owe any car tax), then we could extend our " <> fbox "getRegistrationNumber" <> " function:"
  <> getTaxOwed
  <> "Or, using the " <> fbox "do" <> "-block style:"
  <> getTaxOwed_DO
  <> "Let's just pause here and think about what would happen if we got a " <> fbox "Nothing" <> " anywhere. By definition, when the first argument to "
  <> bind <> " is " <> fbox "Nothing" <> ", it just returns " <> fbox "Nothing" <> " while ignoring whatever function it is given. Thus, a "
  <> fbox "Nothing" <> " at any stage in the large computation will result in a " <> fbox "Nothing" <> " overall, regardless of the other functions. After the first "
  <> fbox "Nothing" <> " hits, all " <> bind <> "s will just pass it to each other, skipping the other function arguments"
  <> ". The technical description says that the structure of the " <> fbox "Maybe" <> " monad " <> textbf "propagates failures" <> "."




openMonads :: LaTeXC l => l
openMonads =
  "Another trait of the " <> fbox "Maybe" <> " monad is that it is " <> textbf "open" <> ": if we have a " <> fbox "Just" <> " value, we can see the contents and extract the associated values through pattern matching."
  <> exitMaybe 
  <> "This usage pattern of replacing " <> fbox "Nothing" <> " with a default is captured by the " <> fbox "fromMaybe" <> " function in " <> fbox "Data.Maybe" <> "."
  <> zeroAsDefault
  <> "The " <> fbox "maybe" <> " Prelude function allows us to do it in a more general way, by supplying a function to modify the extracted value."
  <> dispRes
  <> probandoDispRes
  <> "This possibility makes sense for " <> fbox "Maybe" <> ", as it allows us to recover from failures. Not all monads are open in this way; often, they are designed to hide unnecessary details. " 
  <> fbox "return" <> " and " <> bind <> " alone do not allow us to extract the underlying value from a monadic computation, and so it is perfectly possible to make a " 
  <> qts "no-exit" <> " monad, from which it is never possible to extract values. The most obvious example of that is the " <> fbox "IO" <> " monad."




safety :: LaTeXC l => l
safety =
  "We have seen how " <> fbox "Maybe" <> " can make code safer by providing a graceful way to deal with failure that does not involve runtime errors. Does that mean we should always use " <> fbox "Maybe" <> " for everything? Not really."
  <> par <> "When you write a function, you are able to tell whether it might fail to produce a result during normal operation of the program"
  <> footnote ( "With " <> qts "normal operation" <> " we mean to exclude failure caused by uncontrollable circumstances in the real world, such as memory exhaustion or a dog chewing the printer cable. " )
  <> ", either because the functions you use might fail (as in the examples in this chapter) or because you know some of the argument or intermediate result values do not make sense (for instance, imagine a calculation that is only meaningful if its argument is less than 10)"
  <> ". If that is the case, by all means use " <> fbox "Maybe" <> " to signal failure; it is far better than returning an arbitrary default value or throwing an error."
  <> par <> "Now, adding " <> fbox "Maybe" <> " to a result type without a reason would only make the code more confusing and no safer. The type signature of a function with unnecessary "
  <> fbox "Maybe" <> " would tell users of the code that the function could fail when it actually can't."
  <> "\n Of course, that is not as bad a lie as the opposite one (that is, claiming that a function will not fail when it actually can), but we really want honest code in " <> textit "all" <> " cases"
  <> ". Furthermore, using " <> fbox "Maybe" <> " forces us to propagate failure (with " <> fbox "fmap" <> " or monadic code) and eventually handle the failure cases "
  <> "(using pattern matching, the " <> fbox "maybe" <> " function, or " <> fbox "fromMaybe" <> " from " <> fbox "Data.Maybe" <> "). If the function cannot actually fail, coding for failure is an unnecessary complication."









