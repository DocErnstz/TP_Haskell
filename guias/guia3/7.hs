absoluto :: Float -> Float
absoluto a | a >= 0 = a
           | a < 0 = -a

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float 
distanciaManhattan (a, b, c) (d, e, f) = absoluto (a-d) + absoluto (b-e) + absoluto (c-f)