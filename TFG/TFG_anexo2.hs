{-# LANGUAGE OverloadedStrings #-}

module TFG_anexo2 where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class


import TFG_monadasHaskell (comillas, comillas')


anexo2 :: LaTeXC l => l
anexo2 = 
  chapter "Appendix: Full Monad documentation"
  <> "The full " <> fbox "Monad" <> " code, as found in the Prelude documentation when searching " <> comillas' <> "Monad" <> comillas' <> " in Hoogle"
  <> documentacionMonadas


documentacionMonadas :: LaTeXC l => l
documentacionMonadas = verbatim (pack texto) where 
  texto =
    "{- | The 'Monad' class defines the basic operations over a /monad/, \n "
    <> "a concept from a branch of mathematics known as /category theory/. \n "
    <> "From the perspective of a Haskell programmer, however, it is best to \n "
    <> "think of a monad as an /abstract datatype/ of actions. \n "
    <> "Haskell's @do@ expressions provide a convenient syntax for writing \n "
    <> "monadic expressions. \n \n "

    <> "Instances of 'Monad' should satisfy the following laws: \n \n "

    <> "* @'return' a '>>=' k  =  k a@ \n "
    <> "* @m '>>=' 'return'  =  m@ \n "
    <> "* @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@ \n \n "

    <> "Furthermore, the 'Monad' and 'Applicative' operations should relate as follows: \n \n "

    <> "* @'pure' = 'return'@ \n "
    <> "* @('<*>') = 'ap'@ \n \n "

    <> "The above laws imply: \n \n "

    <> "* @'fmap' f xs  =  xs '>>=' 'return' . f@ \n "
    <> "* @('>>') = ('*>')@ \n \n "

    <> "and that 'pure' and ('<*>') satisfy the applicative functor laws. \n \n "

    <> "The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO' \n "
    <> "defined in the " <> [comillas] <> "Prelude" <> [comillas] <> " satisfy these laws. \n "
    <> "-}  \n "
    <> "class Applicative m => Monad m where \n "
    <> "    -- | Sequentially compose two actions, passing any value produced \n "
    <> "    -- by the first as an argument to the second. \n "
    <> "    (>>=)       :: forall a b. m a -> (a -> m b) -> m b \n \n "

    <> "    -- | Sequentially compose two actions, discarding any value produced \n "
    <> "    -- by the first, like sequencing operators (such as the semicolon) \n "
    <> "    -- in imperative languages. \n "
    <> "    (>>)        :: forall a b. m a -> m b -> m b \n "
    <> "    m >> k = m >>= \\_ -> k -- See Note [Recursive bindings for Applicative/Monad] \n "
    <> "    {-# INLINE (>>) #-} \n \n "

    <> "    -- | Inject a value into the monadic type. \n "
    <> "    return      :: a -> m a \n "
    <> "    return      = pure \n \n "

    <> "    -- | Fail with a message.  This operation is not part of the \n "
    <> "    -- mathematical definition of a monad, but is invoked on pattern-match \n "
    <> "    -- failure in a @do@ expression. \n "
    <> "    fail        :: String -> m a \n "
    <> "    fail s      = error s \n \n "

    <> "{- Note [Recursive bindings for Applicative/Monad] \n "
    <> "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  \n \n "

    <> "The original Applicative/Monad proposal stated that after \n "
    <> "implementation, the designated implementation of (>>) would become \n \n "

    <> "  (>>) :: forall a b. m a -> m b -> m b \n "
    <> "  (>>) = (*>) \n \n "

    <> "by default. You might be inclined to change this to reflect the stated \n "
    <> "proposal, but you really shouldn't! Why? Because people tend to define \n "
    <> "such instances the /other/ way around: in particular, it is perfectly \n "
    <> "legitimate to define an instance of Applicative (*>) in terms of (>>), \n "
    <> "which would lead to an infinite loop for the default implementation of \n "
    <> "Monad! And people do this in the wild. \n \n "

    <> "This turned into a nasty bug that was tricky to track down, and rather \n "
    <> "than eliminate it everywhere upstream, it's easier to just retain the \n "
    <> "original default. \n \n "

    <> "-}"

