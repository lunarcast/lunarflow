module Lunarflow.Label where

-- TODO: Think more about how good / bad of an idea this is.
-- WARNING: Don't use this for actual docs, just for conveying 
-- a small amoung of info to the programmer which actually calls the function.
-- | Dummy typeclass used for giving extra info to the programmer.
class Label (a :: Symbol)

instance label :: Label a
